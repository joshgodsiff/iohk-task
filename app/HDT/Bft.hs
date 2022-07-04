module HDT.Bft where

import Control.Monad (when)
import HDT.Agent
import HDT.Blockchain

-- | The type of messages used for communication in the BFT-protocol.
data BftMessage
  = -- | Message used by the @'clock'@ to broadcast the current time.
    Time Slot
  | -- | Message used by a @'node'@ to announce a new @'Chain'@.
    NewChain Chain
  deriving (Show)

-- | The nodes do not keep track of time by themselves, but instead rely on
--  the @'clock'@ agent, which broadcasts the beginning of each new @'Slot'@
--  using @'Time'@-messages. The agent should start with @'Slot' 0@ and run forever.
--  __TODO:__ Implement @'clock'@.
clock :: Agent BftMessage a
clock = go 0
  where
    go t = do
      broadcast (Time t)
      delay
      go (t + 1)

-- | A @'node'@ participating in the BFT-protocol. It should start with the @'Genesis'@
--  chain at @'Slot' 0@ and run forever.
--  __TODO:__ Implement @'node'@.
node ::
  -- | Total number of nodes.
  NumNodes ->
  -- | Identifier of /this/ node.
  NodeId ->
  Agent BftMessage ()
node numNodes nodeId = go 0 Genesis
  where
    go slot' c = do
      msg <- receive
      case msg of
        Time s -> do
          when (slotLeader numNodes s == nodeId) $
            broadcast . NewChain $ c :> Block s nodeId
          go s c
        NewChain newC ->
          if chainValid numNodes slot' newC && (chainLength newC > chainLength c)
            then go (currentSlot newC) newC
            else go slot' c

currentSlot :: Chain -> Slot
currentSlot Genesis = 0
currentSlot (_ :> b) = slot b