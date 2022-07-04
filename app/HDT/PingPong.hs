module HDT.PingPong where

import HDT.Agent

-- |The message type used by agents @'ping'@ and @'pong'@.
data PingPongMessage =
      Ping -- ^Message used by the @'ping'@ agent, which the @'pong'@ agent waits for.
    | Pong -- ^Message used by the @'pong'@ agent, which the @'ping'@ agent waits for.
    deriving (Show)

-- |Agent @'ping'@ starts by broadcasting a @'Ping'@ message, then
-- waits for a @'Pong'@ message and repeats.
-- Note how it guards against the possibility of receiving its own broadcasts!
ping :: Agent PingPongMessage ()
ping = delay >> broadcast Ping >> go
  where
    go = do
        msg <- receive
        case msg of
            Ping -> go
            Pong -> ping

-- |Agent @'pong'@ waits for a @'Ping'@ message, the broadcasts a @'Pong'@ message
-- and repeats.
pong :: Agent PingPongMessage ()
pong = do
    msg <- receive
    case msg of
        Ping -> delay >> broadcast Pong >> pong
        Pong -> pong