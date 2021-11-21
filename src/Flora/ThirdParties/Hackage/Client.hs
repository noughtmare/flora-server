{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
module Flora.ThirdParties.Hackage.Client where

import Servant.API ()
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic

import Control.Arrow ((>>>))
import Data.Text
import Flora.ThirdParties.Hackage.API as API

(//)
  :: (m ~ AsClientT n)
  => GenericServant routes m
  => (a -> ToServant routes m)
  -> (routes m -> b)
  -> (a -> b)
f // f' = f >>> fromServant >>> f'

(/:) :: (a -> b -> c) -> b -> a -> c
(/:) = flip

hackageClient :: (HackageAPI (AsClientT ClientM) -> a) -> a
hackageClient = ($ genericClient)

listHackageUsers :: ClientM [HackageUserObject]
listHackageUsers = hackageClient API.listUsers

getHackageUser :: Text -> ClientM HackageUserDetailsObject
getHackageUser username = hackageClient (API.withUser /: username)
