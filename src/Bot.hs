module Bot
    ( runBot
    ) where

import Telegram.Bot.API ( Update
                        , Token
                        , defaultTelegramClientEnv
                        )
import Telegram.Bot.Simple ((<#))
import qualified Telegram.Bot.Simple as Bot
import qualified Telegram.Bot.Simple.UpdateParser as Update
import System.IO.Unsafe (unsafePerformIO)
import Zap (zapear)

import Data.Text
import qualified Data.Text as Text

type Model = ()

data Action
    = NoOp
    | Test Text

type UpdateHandler = Update -> Model -> Maybe Action
type ActionHandler = Action -> Model -> Bot.Eff Action Model
type Bot = Bot.BotApp Model Action

botApp :: UpdateHandler -> ActionHandler -> Bot
botApp updateToAction handleAction =
  Bot.BotApp { Bot.botInitialModel = ()
             , Bot.botAction       = updateToAction
             , Bot.botHandler      = handleAction
             , Bot.botJobs         = []
             }

updateToAction :: UpdateHandler
updateToAction update _ =
  case Update.updateMessageText update of
    Just text -> Just (Test text)
    Nothing   -> Nothing

handleActionIO :: Action -> Model -> IO (Bot.Eff Action Model)
handleActionIO action model =
  case action of
    NoOp    -> return $ pure model
    Test msg -> do reply <- zapear msg
                   return $ model <# do Bot.replyText reply
                                        return NoOp

handleAction :: Action -> Model -> Bot.Eff Action Model
handleAction action model =
  case action of
    NoOp     -> pure model
    Test msg -> model <# do Bot.replyText (unsafePerformIO $ zapear msg)
                            return NoOp

ioBot = botApp updateToAction handleAction

-- zapBot :: Bot.BotApp Model Action
-- zapBot = do actionHandler <- handleAction
--             return $ Bot.BotApp
--                    { Bot.botInitialModel = ()
--                    , Bot.botAction = updateToAction
--                    , Bot.botHandler = actionHandler
--                    , Bot.botJobs = []
--                    } where
--                     updateToAction :: Update -> Model -> Maybe Action
--                     updateToAction update _ =
--                       case Update.updateMessageText update of
--                         Just text -> Just (Test text)
--                         Nothing   -> Nothing
--
--                     handleAction :: Action -> Model -> IO (Bot.Eff Action Model)
--                     handleAction action model =
--                       case action of
--                         NoOp -> return $ pure model
--                         Test msg -> return $ model <# do Bot.replyText msg
--                                                          return NoOp


runBot :: Token -> IO ()
runBot token = do env <- defaultTelegramClientEnv token
                  Bot.startBot_ ioBot env
