module Flora.Import.Package where

import Control.Monad.Except
import qualified Data.ByteString as B
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Transact
import Distribution.PackageDescription (PackageDescription, UnqualComponentName,
                                        allLibraries, benchmarks, depPkgName,
                                        executables, targetBuildDepends,
                                        testSuites, unUnqualComponentName)
import qualified Distribution.PackageDescription as Cabal hiding (PackageName)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty
import Distribution.Types.Benchmark
import Distribution.Types.Executable
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Library
import Distribution.Types.LibraryName
import Distribution.Types.TestSuite
import Distribution.Types.Version
import Optics.Core

import qualified Data.UUID as UUID
import Flora.Import.Types
import Flora.Model.Package (getPackageByNamespaceAndName)
import Flora.Model.Package.Component as Component
import Flora.Model.Package.Orphans ()
import Flora.Model.Package.Types
import Flora.Model.Release
import Flora.Model.Requirement (Requirement (..), RequirementId (..),
                                RequirementMetadata (..), flag)
import Flora.Model.User
import Flora.Publish

-- | This tuple represents the package that depends on any associated dependency/requirement.
-- It is used in the recursive loading of Cabal files
type DependentName = (Namespace, PackageName)

coreLibraries :: Set PackageName
coreLibraries = Set.fromList
  [ PackageName "Cabal"
  , PackageName "Win32"
  , PackageName "array"
  , PackageName "base"
  , PackageName "binary"
  , PackageName "bytestring"
  , PackageName "containers"
  , PackageName "deepseq"
  , PackageName "ghc-bignum"
  , PackageName "ghc-prim"
  , PackageName "parsec"
  , PackageName "rts"
  , PackageName "stm"
  , PackageName "text"
  ]

-- 1. Load the .cabal file at the given path
-- 2. Translate it to a GenericPackageDescription
-- 3. Extract a 'Package', see if it already exists
-- 4. Extract a 'Release'
-- 5. Extract multiple 'PackageComponent's
-- 6. Extract multiple 'Requirement's
-- 7. Insert everything
importCabal :: UserId    -- ^ The UserId of the stand-in user for Hackage, for instance.
            -> Namespace -- ^ The namespace to which the package will belong.
            -> PackageName  -- ^ Name of the package and of the .cabal file
            -> FilePath -- ^ Directory where to find the .cabal files
            -> DBT IO Package
importCabal userId namespace packageName directory = do
  genDesc <- liftIO $ loadFile (directory <> T.unpack (display packageName) <> ".cabal")
  result <- runExceptT $ do
    package <- lift (getPackageByNamespaceAndName namespace packageName)
                 >>= \case
                         Nothing -> cabalToPackage userId (genDesc ^. #packageDescription) namespace packageName
                         Just package -> pure package
    release <- lift $
      getReleaseByVersion (package ^. #packageId) (genDesc ^. #packageDescription ^. #package ^. #pkgVersion)
                >>= \case
                        Nothing -> createRelease (package ^. #packageId)  (genDesc ^. #packageDescription ^. #package ^. #pkgVersion)
                        Just release -> pure release
    componentsAndRequirements <- extractComponents userId directory (namespace, packageName) (flattenPackageDescription genDesc) (release ^. #releaseId) (package ^. #name)
    let components = fmap fst componentsAndRequirements
    let requirements = foldMap snd componentsAndRequirements
    pure (package, release, components, requirements)
  case result of
    Left err -> error $ "Encountered error during import: " <> show err
    Right (package, release, components, rs) ->
      let pId = package ^. #packageId
          requirements = fmap (\r -> r & #packageId .~ pId) rs
       in publishPackage requirements components release package

loadFile :: FilePath -> IO GenericPackageDescription
loadFile path = fromJust . parseGenericPackageDescriptionMaybe <$> B.readFile path

extractComponents :: UserId
                  -> FilePath
                  -> DependentName
                  -> PackageDescription -- ^ Description from the parsed .cabal file
                  -> ReleaseId -- ^ Id of the release we're inserting
                  -> PackageName -- ^ Name of the package to which the release belongs
                  -> ExceptT ImportError (DBT IO) [(PackageComponent, [Requirement])]
extractComponents userId directory dependentName pkgDesc releaseId packageName = do
  libComps        <- traverse (extractFromLib        userId directory dependentName releaseId packageName) (allLibraries pkgDesc)
  executableComps <- traverse (extractFromExecutable userId directory dependentName releaseId) (executables pkgDesc)
  testSuiteComps  <- traverse (extractFromTestSuite  userId directory dependentName releaseId) (testSuites pkgDesc)
  benchmarkComps  <- traverse (extractFromBenchmark  userId directory dependentName releaseId) (benchmarks pkgDesc)
  -- foreignLibComps  <- traverse (extractFromforeignLib releaseId) (foreignLibs pkgDesc)
  pure $ libComps <> executableComps <> testSuiteComps <> benchmarkComps -- <> foreignLibComps

extractFromLib :: UserId
               -> FilePath
               -> DependentName
               -> ReleaseId -- ^
               -> PackageName -- ^
               -> Library -- ^
               -> ExceptT ImportError (DBT IO) (PackageComponent, [Requirement])
extractFromLib userId directory dependentName releaseId packageName library = do
  let dependencies = library ^. #libBuildInfo ^. #targetBuildDepends
  let libraryName = getLibName $ library ^. #libName
  let componentType = Component.Library
  let canonicalForm = CanonicalComponent libraryName componentType
  component <- createComponent releaseId canonicalForm
  requirements <- traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
  pure (component, requirements)
  where
    getLibName :: LibraryName -> Text
    getLibName LMainLibName        = display packageName
    getLibName (LSubLibName lname) = T.pack $ unUnqualComponentName lname

extractFromExecutable :: UserId
                      -> FilePath
                      -> DependentName
                      -> ReleaseId
                      -> Executable
                      -> ExceptT ImportError (DBT IO) (PackageComponent, [Requirement])
extractFromExecutable userId directory dependentName releaseId executable = do
  let dependencies = executable ^. #buildInfo ^. #targetBuildDepends
  let executableName = getExecutableName $ executable ^. #exeName
  let componentType = Component.Executable
  let canonicalForm = CanonicalComponent executableName componentType
  component <- createComponent releaseId canonicalForm
  requirements <- traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
  pure (component, requirements)
  where
    getExecutableName :: UnqualComponentName -> Text
    getExecutableName execName = T.pack $ unUnqualComponentName execName

extractFromTestSuite :: UserId
                     -> FilePath
                     -> DependentName
                     -> ReleaseId -- ^
                     -> TestSuite -- ^
                     -> ExceptT ImportError (DBT IO) (PackageComponent, [Requirement])
extractFromTestSuite userId directory dependentName releaseId testSuite = do
  let dependencies = testSuite ^. #testBuildInfo ^. #targetBuildDepends
  let testSuiteName = getTestSuiteName $ testSuite ^. #testName
  let componentType = Component.TestSuite
  let canonicalForm = CanonicalComponent testSuiteName componentType
  component <- createComponent releaseId canonicalForm
  requirements <- traverse (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
  pure (component, requirements)
  where
    getTestSuiteName :: UnqualComponentName -> Text
    getTestSuiteName testSuiteName = T.pack $ unUnqualComponentName testSuiteName

extractFromBenchmark :: UserId
                     -> FilePath
                     -> DependentName
                     -> ReleaseId -- ^
                     -> Benchmark -- ^
                     -> ExceptT ImportError (DBT IO) (PackageComponent, [Requirement])
extractFromBenchmark userId directory dependentName releaseId benchmark = do
  let dependencies = benchmark ^. #benchmarkBuildInfo ^. #targetBuildDepends
  let benchmarkName = getBenchmarkName $ benchmark ^. #benchmarkName
  let componentType = Component.Benchmark
  let canonicalForm = CanonicalComponent benchmarkName componentType
  component <- createComponent releaseId canonicalForm
  requirements <- traverse
    (\dependency -> depToRequirement userId directory dependentName dependency (component ^. #componentId)) dependencies
  pure (component, requirements)
  where
    getBenchmarkName :: UnqualComponentName -> Text
    getBenchmarkName benchName = T.pack $ unUnqualComponentName benchName

-- extractFromforeignLib :: ReleaseId -- ^
--                       -> PackageId -- ^
--                       -> ForeignLib -- ^
--                       -> ExceptT ImportError (DBT IO) (PackageComponent, [Requirement])
-- extractFromforeignLib releaseId packageId foreignLib = do
--   let dependencies = foreignLib ^. #foreignLibBuildInfo ^. #targetBuildDepends
--   let foreignLibName = getforeignLibName $ foreignLib ^. #foreignLibName
--   let componentType = Component.ForeignLib
--   let canonicalForm = CanonicalComponent foreignLibName componentType
--   component <- createComponent releaseId canonicalForm
--   requirements <- traverse (\dependency -> depToRequirement packageId dependency (component ^. #componentId)) dependencies
--   pure (component, requirements)
--   where
--     getforeignLibName :: UnqualComponentName -> Text
--     getforeignLibName foreignLibName = T.pack $ unUnqualComponentName foreignLibName

depToRequirement :: UserId -> FilePath -> DependentName -> Cabal.Dependency -> ComponentId -> ExceptT ImportError (DBT IO) Requirement
depToRequirement userId directory (dependentNamespace, dependentPackageName) cabalDependency packageComponentId = do
  let name = PackageName $ T.pack $ Cabal.unPackageName $ depPkgName cabalDependency
  let namespace = if Set.member name coreLibraries then Namespace "haskell" else Namespace "hackage"
  liftIO $ T.putStrLn $ "[+] (@" <> display dependentNamespace <> "/" <> display dependentPackageName <> ") Requiring @" <> display namespace <> "/" <> display name <> "…"
  result <- lift $ getPackageByNamespaceAndName namespace name
  case result of
    Just Package{packageId} -> do
      liftIO $ T.putStrLn $ "[+] (@" <> display dependentNamespace <> "/" <> display dependentPackageName <> ") Dependency @" <> display namespace <> "/" <> display name <> " is in the database (" <> (T.pack . show $ packageId) <> ")"
      requirementId <- RequirementId <$> liftIO UUID.nextRandom
      let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
      let metadata = RequirementMetadata{ flag = Nothing }
      pure Requirement{..}
    Nothing -> do
      -- Checking if the package depends on itself
      if (dependentNamespace, dependentPackageName) == (namespace, name)
      then do
        let packageId = PackageId UUID.nil
        liftIO $ T.putStrLn $ "[!] (@" <> display dependentNamespace <> "/" <> display dependentPackageName <> ") A sub-component depends on the package itself."
        requirementId <- RequirementId <$> liftIO UUID.nextRandom
        let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
        let metadata = RequirementMetadata{ flag = Nothing }
        pure Requirement{..}
      else do
        liftIO $ T.putStrLn $ "[!] (@" <> display dependentNamespace <> "/" <> display dependentPackageName <> ") Dependency @" <> display namespace <> "/" <> display name <> " does not exist in the database, trying to import it from " <> T.pack directory
        package <- lookupCabalFile userId namespace name directory
        let packageId = package ^. #packageId
        requirementId <- RequirementId <$> liftIO UUID.nextRandom
        let requirement = display $ prettyShow $ Cabal.depVerRange cabalDependency
        let metadata = RequirementMetadata{ flag = Nothing }
        pure Requirement{..}

-- | This function is used if the package of a requirement isn't already in the system.
-- Give it a directory where .cabal files are, a package name,
-- and if a .cabal file matches (case-sensitive) it will be imported.
lookupCabalFile :: UserId -> Namespace -> PackageName -> FilePath -> ExceptT ImportError (DBT IO) Package
lookupCabalFile userId namespace packageName directory =
  lift $ importCabal userId namespace packageName directory

createComponent :: ReleaseId -> CanonicalComponent -> ExceptT ImportError (DBT IO) PackageComponent
createComponent releaseId canonicalForm = do
  componentId <- ComponentId <$> liftIO UUID.nextRandom
  pure PackageComponent{..}

createRelease :: PackageId -> Version -> DBT IO Release
createRelease packageId version = do
  releaseId <- ReleaseId <$> liftIO UUID.nextRandom
  timestamp <- liftIO getCurrentTime
  let archiveChecksum = mempty
  let createdAt = timestamp
  let updatedAt = timestamp
  pure Release{..}

cabalToPackage :: UserId
               -> PackageDescription
               -> Namespace
               -> PackageName
               -> ExceptT ImportError (DBT IO) Package
cabalToPackage ownerId packageDesc namespace name = do
  timestamp <- liftIO getCurrentTime
  packageId <- PackageId <$> liftIO UUID.nextRandom
  sourceRepos <- getRepoURL (PackageName $ display $ packageDesc ^. #package ^. #pkgName) (packageDesc ^. #sourceRepos)
  let license = Cabal.license packageDesc
  let homepage = Just (display $ packageDesc ^. #homepage)
  let documentation = ""
  let bugTracker = Just (display $ packageDesc ^. #bugReports)
  let metadata = PackageMetadata{..}
  let synopsis = display $ packageDesc ^. #synopsis
  let createdAt = timestamp
  let updatedAt = timestamp
  pure $ Package{..}

-- getPackageName :: GenericPackageDescription -> ExceptT ImportError (DBT IO) PackageName
-- getPackageName genDesc = do
--   let pkgName = display $ genDesc ^. #packageDescription ^. #package ^. #pkgName
--   case parsePackageName pkgName of
--     Nothing   -> throwError $ InvalidPackageName pkgName
--     Just name -> pure name

getRepoURL :: PackageName -> [Cabal.SourceRepo] -> ExceptT ImportError (DBT IO) [Text]
getRepoURL _ []       = pure []
getRepoURL _ (repo:_)    = pure [display $ fromMaybe mempty (repo ^. #repoLocation)]
