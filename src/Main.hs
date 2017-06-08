module Main where

import Control.Monad.Trans.Except

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Servlet.Handler.Jetty

import Servant
import Servant.Docs

import Data.List
import GHC.Generics

data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)

instance FromJSON Position
instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage
instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { name :: String
  , email :: String
  , age :: Int
  , interested_in :: [String]
  } deriving (Show, Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)

instance FromJSON Email
instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = email c
        subject' = "Hey " ++ name c ++ ", we miss you!"
        body'    = "Hi " ++ name c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (age c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (interested_in c)
                ++ " products? Give us a visit!"

type SubAPI = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
          :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
          :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

subApi :: Proxy SubAPI
subApi = Proxy

subServer :: Server SubAPI
subServer = position
       :<|> hello
       :<|> marketing

  where position :: Int -> Int -> Handler Position
        position a b = return (Position a b)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)

type DocsAPI = SubAPI :<|> Raw

instance ToCapture (Capture "x" Int) where
  toCapture _ = DocCapture "x" "(integer) position on the x axis"

instance ToCapture (Capture "y" Int) where
  toCapture _ = DocCapture "y" "(integer) position on the y axis"

instance ToSample Position where
  toSamples _ = singleSample (Position 3 14)

instance ToParam (QueryParam "name" String) where
  toParam _ =
    DocQueryParam "name"
                  ["Alp", "John Doe", "..."]
                  "Name of the person to say hello to."
                  Normal

instance ToSample HelloMessage where
  toSamples _ =
    [ ("When a value is provided for 'name'", HelloMessage "Hello, Alp")
    , ("When 'name' is not specified", HelloMessage "Hello, anonymous coward")
    ]

ci :: ClientInfo
ci = ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]

instance ToSample ClientInfo where
  toSamples _ = singleSample ci

instance ToSample Email where
  toSamples _ = singleSample (emailForClient ci)

api :: Proxy DocsAPI
api = Proxy

docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] subApi

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

server :: Server DocsAPI
server = subServer :<|> (Tagged serveDocs)

  where serveDocs _ respond =
          respond $ responseLBS ok200 [plain] docsBS

        plain = ("Content-Type", "text/plain")

app :: Application
app = serve api server

main :: IO ()
main = run 9000 app
