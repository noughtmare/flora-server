{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.PackageSpec where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Optics.Core
import SpecHelpers (migrate)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted

import Flora.Import.Package
import Flora.Model.Package (getPackageByNamespaceAndName, getPackageDependents)
import Flora.Model.Package.Types
import Flora.Model.User
import Flora.UserFixtures

spec ::  Spec
spec = describeDB migrate "packages" $ do
  itDB "Insert @haskell/base and its dependencies, and fetch it" $ do
    importCabal (hackageUser ^. #userId) (Namespace "haskell") (PackageName "base") "./test/fixtures/Cabal/"
    result <- getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
    result `shouldSatisfy` (\x -> isJust x)
  itDB "Insert @haskell/containers and its dependencies" $ do
    importCabal (hackageUser ^. #userId) (Namespace "haskell") (PackageName "containers") "./test/fixtures/Cabal/"
    result <- getPackageByNamespaceAndName (Namespace "haskell") (PackageName "containers")
    result `shouldSatisfy` (\x -> isJust x)
  -- itDB "Load @haskell/array" $ do
  --   importCabal (hackageUser ^. #userId) (Namespace "haskell") (PackageName "array") "./test/fixtures/Cabal/"
  --   result <- getPackageByNamespaceAndName (Namespace "haskell") (PackageName "array")
  --   (result ^? _Just % #name) `shouldBe` Just (PackageName "array")
  -- itDB "Fetch the dependents of ghc-prim" $ do
  --   result <- Set.fromList . Vector.toList <$> getPackageDependents (Namespace "haskell") (PackageName "ghc-prim")
  --   Set.map (view #name) result `shouldBe` Set.fromList [PackageName "base", PackageName "ghc-bignum", PackageName "base", PackageName "bytestring", PackageName "integer-gmp", PackageName "binary"]
  -- itDB "Fetch the dependents of array" $ do
  --   result <- Set.fromList . Vector.toList <$> getPackageDependents (array ^. #namespace) (array ^. #name)
  --   result `shouldBe` Set.fromList [stm, deepseq, containers, binary]
