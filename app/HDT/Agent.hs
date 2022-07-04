{-# LANGUAGE DeriveFunctor #-}

module HDT.Agent where

import Control.Monad.Free

data AgentF msg a
  = Delay a
  | Broadcast msg a
  | Receive (msg -> a)
  deriving Functor

-- |An @'Agent' msg a@ is an abstract process that can send and receive broadcast
-- messages of type @msg@ and will eventually return a result of type @a@.
-- Define the type such that @'Agent' msg@ is a /free monad/ supporting the operations
-- @'delay'@, @'broadcast'@ and @'receive'@ below.
--
-- Agents can be used to model concurrent agents that communicate and coordinate
-- via message exchange over a broadcast channel.
type Agent msg = Free (AgentF msg)

-- The resulting monad should be /free/ and support operations
-- @'delay'@, @'broadcast'@ and @'receive'@ described below.
-- instance Monad (Agent msg) where

-- |Delay for one timestep.
delay :: Agent msg ()
delay = liftF $ Delay ()

-- |Broadcast a message.
broadcast :: msg          -- ^The message to broadcast.
          -> Agent msg ()
broadcast msg = liftF $ Broadcast msg ()

-- |Wait for a broadcast and return the received message.
receive :: Agent msg msg
receive = liftF $ Receive id