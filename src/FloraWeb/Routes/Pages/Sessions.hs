module FloraWeb.Routes.Pages.Sessions where

import Data.Text
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid
import Web.FormUrlEncoded

import Flora.Model.PersistentSession
import Flora.Model.User.Orphans ()

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { new    :: mode :- "new" :> Get '[HTML] (Html ())
  , create :: mode :- "new" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] (Html ())
  , delete :: mode :- "delete" :> Capture "session_id" PersistentSessionId :> Post '[HTML] (Html ())
  } deriving stock (Generic)

data LoginForm = LoginForm
  { email    :: Text
  , password :: Text
  , remember :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromForm, ToForm)
