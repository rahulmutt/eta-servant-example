module Main where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.HTTP.Types                 (status200)
import Servlet.Jetty
import Servant

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

-- /users
type UserAPI =  "users" :> Get '[JSON] [User]
           -- :<|> Raw

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = return users
    -- :<|> appSimple

app :: Application
app = serve userAPI server

appSimple :: Application
appSimple _ respond = respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main :: IO ()
main = runJetty 9000 app
