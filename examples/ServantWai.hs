{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

-- | Configure an Application with a CORS policy
app :: Application
app = (cors (const $ Just policy)) $ serve api server
    where policy = CorsResourcePolicy
                    { corsOrigins = Nothing
                    , corsMethods = ["POST"]
                    , corsRequestHeaders = []
                    , corsExposedHeaders = Nothing
                    , corsMaxAge = Nothing
                    , corsVaryOrigin = False
                    , corsRequireOrigin = False
                    , corsIgnoreFailures = True
                    }

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

main :: IO ()
main = run 8080 app
