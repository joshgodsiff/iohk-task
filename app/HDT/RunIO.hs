module HDT.RunIO where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.STM
import Control.Monad.Free (foldFree)
import Data.Text as T
import qualified Data.Text.IO as T
import HDT.Agent
import System.IO
import Text.Printf (printf)

tshowLn :: Show a => a -> T.Text
tshowLn = T.pack . printf "%s\n" . show

oneSecond :: Int
oneSecond = 1000000

interpretAgent :: Show msg => TChan msg -> Agent msg () -> IO ()
interpretAgent bcast agent = do
  localChan <- atomically $ dupTChan bcast
  foldFree (go bcast localChan) agent
  where
    go :: Show msg => TChan msg -> TChan msg -> AgentF msg x -> IO x
    go bcast' readC agentF = case agentF of
      Delay a -> do
        threadDelay oneSecond
        pure a
      Broadcast m a -> do
        T.putStr $ tshowLn m
        atomically $ writeTChan bcast' m
        pure a
      Receive a -> do
        m <- atomically $ readTChan readC
        pure (a m)

-- | Function @'runIO' agents@ runs each agent in the given list
--  concurrently in the @'IO'@-monad.
--  Broadcast should be realized using a @'TChan' msg@ which is shared amongst
--  the threads running each agent,
--  and @'runIO'@ should only return if and when every agent has returned.
--
--  The operations should be interpreted as follows:
--
--   * @'delay'@ should delay execution for one second.
--   * @'broadcast' msg@ should broadcast @msg@ via the shared @'TChan'@
--     and additionally log the message to the console. Doing this naively
--     could lead to garbled output, so care must be taken to ensure sequential access
--     to the console.
--   * @'receive'@ should block the thread until a message is received on the
--     shared @'TChan'@.
runIO ::
  Show msg =>
  -- | The agents to run concurrently.
  [Agent msg ()] ->
  IO ()
runIO agents = do
  hSetBuffering stdout LineBuffering
  bchan <- newBroadcastTChanIO
  forConcurrently_ agents $ interpretAgent bchan