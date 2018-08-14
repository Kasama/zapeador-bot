{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import Data.Aeson          (ToJSON, toJSON, FromJSON, (.=), object)
import Data.Proxy          (Proxy (Proxy))
import Data.Text           (Text)
import Data.Maybe
import GHC.Generics        (Generic)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API         (Post, ReqBody, Header, JSON, (:>))
import Servant.Client      ( BaseUrl (BaseUrl)
                           , Scheme (Http, Https)
                           , ClientM
                           , mkClientEnv
                           , client
                           , runClientM
                           )

-- url = BaseUrl Http "vemdezapbe.be" 80 "api/v1.0" :: BaseUrl
-- url = BaseUrl Http "postman-echo.com" 80 "" :: BaseUrl
url = BaseUrl Http "localhost" 3000 "" :: BaseUrl

data ZapRequest = ZapRequest
                { message  :: Text
                , mood     :: String
                , strength :: Int
                , rate     :: Float
                , tweet    :: Bool
                } deriving (Show, Generic)
instance ToJSON ZapRequest where
  toJSON ZapRequest{..} = object [ "zap"      .= message  -- {
                                 , "mood"     .= mood
                                 , "strength" .= strength
                                 , "rate"     .= rate
                                 , "tweet"    .= tweet
                                 ]                        -- }

data ZapResponse = ZapResponse
                 { version     :: Text
                 , zap         :: Text
                 , gemidao     :: Maybe Text
                 , requestTime :: Text
                 } deriving (Show, Generic)
instance FromJSON ZapResponse

type ZapAPI = "api" :> ReqBody '[JSON] ZapRequest :> Post '[JSON] ZapResponse
zapAPI = Proxy :: Proxy ZapAPI

zapClient :: ZapRequest -> ClientM ZapResponse
zapClient = client zapAPI

clientEnv = do manager <- newManager defaultManagerSettings
               return   $ mkClientEnv manager url

apiClient :: ClientM a -> IO (Maybe a)
apiClient api = do response <- clientEnv >>= runClientM api
                   case response of
                     Left  err  -> do print err
                                      return Nothing
                     Right todo -> return $ Just todo
