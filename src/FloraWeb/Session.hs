module FloraWeb.Session where

import qualified Data.UUID as UUID
import Web.Cookie

import Flora.Model.PersistentSession
import Servant (Header, Headers, addHeader)

-- | This function builds a cookie with the provided content
craftSessionCookie :: PersistentSessionId -- ^ Cookie content
                   -> Bool                -- ^ Remember the cookie for 1 week
                   -> SetCookie
craftSessionCookie (PersistentSessionId content) rememberSession =
  defaultSetCookie
      { setCookieValue = UUID.toASCIIBytes content
      , setCookieName  = "flora_server_session"
      , setCookieHttpOnly = True
      , setCookieSameSite = Just sameSiteStrict
      , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
      }

emptySessionCookie :: SetCookie
emptySessionCookie = defaultSetCookie
  { setCookieName = "flora_server_session"
  , setCookieValue = ""
  , setCookieMaxAge = Just 0
  }

addCookie :: SetCookie
          -> a
          -> Headers '[Header "Set-Cookie" SetCookie] a
addCookie cookies continuation = addHeader cookies continuation

deleteCookie :: a -> Headers '[Header "Set-Cookie" SetCookie] a
deleteCookie continuation = addHeader emptySessionCookie continuation
