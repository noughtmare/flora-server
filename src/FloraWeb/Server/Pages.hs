module FloraWeb.Server.Pages where

import Control.Monad.Reader
import Lucid
import Network.HTTP.Types
import Optics.Core
import Servant.API.Generic
import Servant.Server.Generic

import FloraWeb.Routes.Pages
import FloraWeb.Server.Auth
import qualified FloraWeb.Server.Pages.Packages as Packages
import qualified FloraWeb.Server.Pages.Sessions as Sessions
import FloraWeb.Templates
import FloraWeb.Templates.Error
import qualified FloraWeb.Templates.Pages.Home as Home

server :: ToServant Routes' (AsServerT FloraPageM)
server = genericServerT Routes'
  { home = homeHandler
  , about = aboutHandler
  , admin = ensureUser adminHandler
  , sessions = Sessions.server
  , packages = Packages.server
  }

ensureUser :: FloraAdminM a -> FloraPageM a
ensureUser adminM = do
  Session{sessionId, mUser} <- ask
  case mUser of
    Nothing -> renderError forbidden403
    Just user ->
      withReaderT (\Session{webEnvStore} -> ProtectedSession{..}) adminM

homeHandler :: FloraPageM (Html ())
homeHandler = do
  session <- ask
  templateDefaults <- fromSession session defaultTemplateEnv
  let templateEnv = templateDefaults & (#displayNavbarSearch .~ False)
  render templateEnv Home.show

aboutHandler :: FloraPageM (Html ())
aboutHandler = do
  session <- ask
  templateDefaults <- fromSession session defaultTemplateEnv
  let (templateEnv :: TemplateEnv) = templateDefaults
        & (#activeElements % #aboutNav .~ True)
  render templateEnv Home.about

adminHandler :: FloraAdminM (Html ())
adminHandler = undefined
