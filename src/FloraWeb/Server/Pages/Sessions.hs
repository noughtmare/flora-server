module FloraWeb.Server.Pages.Sessions where

import Control.Monad.Reader
import Data.Password.Argon2
import qualified Data.Text as T
import Data.Text.Display
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Optics.Core
import Servant.API.Generic
import Servant.Server.Generic

import Flora.Environment
import Flora.Model.PersistentSession
import Flora.Model.User
import Flora.Model.User.Orphans ()
import FloraWeb.Routes.Pages.Sessions
import FloraWeb.Server.Auth
import FloraWeb.Server.Util
import FloraWeb.Session
import FloraWeb.Templates
import FloraWeb.Templates.Pages.Sessions as Sessions

server :: ToServant Routes' (AsServerT FloraPageM)
server = genericServerT Routes'
  { new = newSessionHandler
  , create = createSessionHandler
  , delete = deleteSessionHandler
  }

newSessionHandler :: FloraPageM (Html ())
newSessionHandler = do
  session <- ask
  let mUser = session ^. #mUser
  case mUser of
    Nothing -> do
      liftIO $ putStrLn "[+] No user logged-in"
      render (fromSession session defaultTemplateEnv) Sessions.newSession
    Just u -> do
      liftIO $ putStrLn $ "[+] User is already logged: " <> show u
      redirect Nothing "/"

createSessionHandler :: LoginForm -> FloraPageM (Html ())
createSessionHandler LoginForm{email, password, remember} = do
  session <- ask
  let FloraEnv{pool} = session ^. #floraEnv
  mUser <- liftIO $ withPool pool $ getUserByEmail email
  case mUser of
    Nothing -> do
      liftIO $ putStrLn "[+] Couldn't find user"
      let templateEnv =
            fromSession session defaultTemplateEnv
              & (#flashError ?~ mkError "Could not authenticate")
      render templateEnv Sessions.newSession
    Just user ->
      if validatePassword (mkPassword password) (user ^. #password)
      then do
        liftIO $ putStrLn "[+] User connected!"
        sessionId <- persistSession pool (session ^. #sessionId) (user ^. #userId)
        redirect (Just (craftCookie sessionId remember)) "/"
      else do
        liftIO $ putStrLn "[+] Couldn't authenticate user"
        let templateEnv = fromSession session defaultTemplateEnv
                            & #flashError ?~ mkError "Could not authenticate"
        render templateEnv Sessions.newSession

deleteSessionHandler :: PersistentSessionId -> FloraPageM (Html ())
deleteSessionHandler sessionId = do
  liftIO $ putStrLn $ T.unpack $ "[+] Logging-off session " <> display sessionId
  FloraEnv{pool} <- asks (\s -> s ^. #floraEnv)
  liftIO $ withPool pool $ deleteSession sessionId
  redirect (Just emptySessionCookie) "/"
