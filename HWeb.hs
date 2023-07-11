{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
-- import Web.Scotty
import qualified Data.Map as M
import Web.Scotty.Trans
import System.Environment (getArgs)

data Config = Config
    { counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case M.lookup k m of
    Just i  -> (M.insert k (i + 1) m, i + 1)
    Nothing -> (M.insert k 1 m, 1)

countKey :: Text -> IORef (M.Map Text Integer) -> IO Integer
countKey k m = atomicModifyIORef' m (bumpBoomp k)

app :: Scotty ()
app = do
    get "/:key" $ do
        unprefixed <- param "key"
        config <- lift ask
        let key' = prefix config <> unprefixed
        newInteger <- liftIO $ countKey key' (counts config)
        html $ mconcat [ "<h1>Success! Count was: "
                       , TL.pack $ show newInteger
                       , "</h1>"
                       ]

-- tshow :: Show a => a -> Text
-- tshow = TL.pack . show

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR = flip runReaderT config
    scottyT 3000 runR app