{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.Requirement where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (Entity, insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField,
                                             fromJSONField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import Database.PostgreSQL.Simple.Types (Only (Only))
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics (Generic)

import Data.Data
import Flora.Model.Package.Component
import Flora.Model.Package.Types
import Flora.Model.Release (ReleaseId)

newtype RequirementId = RequirementId { getRequirementId :: UUID }
  deriving (Eq, Show, FromField, ToField, FromJSON, ToJSON)
    via UUID

data Requirement = Requirement
  { -- | Unique identifier of this requirement in the database
    requirementId      :: RequirementId
  , -- | Package component that depends on this requirement
    packageComponentId :: ComponentId
  , -- | Package that is being depended on
    packageId          :: PackageId
  , -- | The human-readable version range expression of this requirement
    requirement        :: Text
  , -- | Additional metadata, like flags
    metadata           :: RequirementMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving (Entity)
    via (GenericEntity '[TableName "requirements"] Requirement)

data RequirementMetadata = RequirementMetadata
  { flag :: Maybe Text
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving anyclass (ToJSON, FromJSON)

instance FromField RequirementMetadata where
  fromField = fromJSONField

instance ToField RequirementMetadata where
  toField = toJSONField

insertRequirement :: Requirement -> DBT IO ()
insertRequirement = insert @Requirement

getRequirements :: ReleaseId
                   -- ^ Id of the release for which we want the dependencies
                -> DBT IO (Vector (Namespace, PackageName, Text))
                   -- ^ Returns a vector of (Namespace, Name, Version requirement)
getRequirements relId = query Select
  [sql|
  select distinct p.namespace, p.name, req.requirement
    from requirements as req
    inner join packages as p
      on req."package_id" = p."package_id"
    inner join package_components as pc
      on req."package_id" = pc."package_id"
    inner join releases as rel
      on rel."release_id" = req."release_id"
    inner join packages as p2
      on p2."package_id" = rel."package_id"
  where req."release_id" = ?
  |] (Only relId)
