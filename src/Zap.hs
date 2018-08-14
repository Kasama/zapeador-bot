{-# LANGUAGE RecordWildCards #-}

module Zap ( zapear
           ) where

import Data.Text (Text, pack)

import qualified Client as Zap


zap' :: Bool -> Float -> Int -> String -> Text -> IO (Maybe Zap.ZapResponse)
zap' tweet rate strength mood message = Zap.apiClient api
                                        where api     = Zap.zapClient request
                                              request = Zap.ZapRequest{..}

simpleZap :: Text -> IO (Maybe Zap.ZapResponse)
simpleZap = zap' False 0.7 4 "happy"

zapear :: Text -> IO Text
zapear msg = do response <- simpleZap msg
                case response of
                  Nothing       -> return $ pack "Erro Zapeando Texto"
                  Just response -> return $ Zap.zap response
