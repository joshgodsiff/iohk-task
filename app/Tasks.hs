{-# OPTIONS_HADDOCK show-extensions  #-}
{-# OPTIONS_GHC -Wno-dodgy-exports   #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

{-|
Module      : HDT.Tasks
Description : IOHK Haskell Developer Test
Copyright   : (c) Lars Brünjes, 2020
Maintainer  : lars.bruenjes@iohk.io
Stability   : experimental
Portability : portable

This module contains the IOHK Haskell Developer Test. Please complete all the
to-do's!
-}
module HDT.Tasks
    ( -- * @Agent@s
      -- | We start by implementing a free monad,
      -- which we will use to model a concurrent agent than
      -- can communicate and coordinate with other agents by
      -- exchanging broadcast messages.
      Agent (..)
    , delay
    , broadcast
    , receive
      -- * Ping-pong example
      -- |As an example of using agents, agents @'ping'@ and @'pong'@ defined below
      -- keep sending @'Ping'@ and @'Pong'@ messages back and forth between themselves.
    , PingPongMessage (..)
    , ping
    , pong
      -- * An @'IO'@-interpreter for agents.
      -- |In order to actually /run/ agents, we need to /interpret/ the free
      -- @'Agent'@ monad. We start with an interpreter in @'IO'@,
      -- which runs each agent in a list of agents in its own thread
      -- and implements messaging by utilizing a shared @'TChan'@ broadcast channel.
      --
      -- We will then be able to run our ping-pong example:
      --
      -- >>> runIO [ping, pong]
      -- Ping
      -- Pong
      -- Ping
      -- Pong
      -- Ping
      -- ...
    , runIO
      -- * Ouroboros Bft
      -- | We use agents to implement a simplified version of the
      -- <https://iohk.io/en/research/library/papers/ouroboros-bfta-simple-byzantine-fault-tolerant-consensus-protocol/ Ouroboros-BFT>
      -- blockchain consensus protocol.
      --
      -- Ouroboros-BFT works as follows: A fixed number of @n@ /nodes/ participate in the
      -- protocol. They collaborate on building a /blockchain/ by adding /blocks/,
      -- and the protocol ensures that the nodes will agree on a /common prefix/
      -- (all nodes agree on the prefix of the chain, but may disagree on a few blocks
      -- towards the end).
      -- This will work as long as at least two thirds of the nodes follow the protocol.
      --
      -- Time is divided in /slots/ of a fixed length, and in each slot, one node is the
      -- /slot leader/ with the right to create the next block.
      -- Slots leaders are determined in a round-robin fashion: Node 0
      -- can create a block in Slot 0, Node 1 in Slot 1, Node @(n-1)@ in Slot @(n-1)@,
      -- Node 0 in Slot @n@ and so on.
      -- When a node is slot leader, it adds a block to its current chain and
      -- broadcasts the new chain to the other nodes.
      --
      -- Each node holds on to a current chain (all nodes start with the chain
      -- just consisting of the /genesis block/). When a node receives a new chain
      -- from another node, it checks the new chain for /validity/ and
      -- adopts it as its own chain /if it is longer than its own chain/.
      --
      -- A chain is /valid/ if
      --
      --  * The timestamps are stricly increasing,
      --  * All blocks have been created by the slot leader of that slot and
      --  * The newest block's timestamp is not from the future.
      --
      -- In reality, nodes would use digital signatures to sign the blocks
      -- they create, and each block would contain a /payload/, but we
      -- want to keep matters as simple as possible.
      --
      -- >>> runIO $ clock : [node 3 nid | nid <- [0,1,2]]
      -- Time 0
      -- Time 1
      -- NewChain (Genesis :> {1 1})
      -- Time 2
      -- NewChain (Genesis :> {1 1} :> {2 2})
      -- Time 3
      -- NewChain (Genesis :> {1 1} :> {2 2} :> {3 0})
      -- Time 4
      -- NewChain (Genesis :> {1 1} :> {2 2} :> {3 0} :> {4 1})
      -- Time 5
      -- NewChain (Genesis :> {1 1} :> {2 2} :> {3 0} :> {4 1} :> {5 2})
      -- Time 6
      -- NewChain (Genesis :> {1 1} :> {2 2} :> {3 0} :> {4 1} :> {5 2} :> {6 0})
      -- ...
      , Slot
      , NodeId
      , Block (..)
      , Chain (..)
      , chainLength
      , slotLeader
      , chainValid
      , clock
      , node
      -- * A /pure/ interpreter for agents.
      -- |We also want to be able to interpret a list of agents in a
      -- /pure and deterministic/ fashion.
      --
      -- When we try this with our ping-pong example, we will be able to do:
      --
      -- >>> take 5 $ runPure [ping, pong]
      -- [(1,Ping),(2,Pong),(3,Ping),(4,Pong),(5,Ping)]
      , runPure
    )  where

import Control.Concurrent.STM
import Numeric.Natural        (Natural)
import Text.Printf            (printf)

-- |An @'Agent' msg a@ is an abstract process that can send and receive broadcast
-- messages of type @msg@ and will eventually return a result of type @a@.
-- Define the type such that @'Agent' msg@ is a /free monad/ supporting the operations
-- @'delay'@, @'broadcast'@ and @'receive'@ below.
--
-- Agents can be used to model concurrent agents that communicate and coordinate
-- via message exchange over a broadcast channel.
data Agent msg a

-- |__TODO:__ Provide a @'Functor'@ instance for @'Agent' msg@.
instance Functor (Agent msg) where

-- |__TODO:__ Provide an @'Applicative'@ instance for @'Agent' msg@.
instance Applicative (Agent msg) where

-- |__TODO:__ Provide a @'Monad'@ instance for @'Agent' msg@.
-- The resulting monad should be /free/ and support operations
-- @'delay'@, @'broadcast'@ and @'receive'@ described below.
instance Monad (Agent msg) where

-- |Delay for one timestep.
-- __TODO:__ Implement @'delay'@.
delay :: Agent msg ()
delay = error "TODO: implement delay"

-- |Broadcast a message.
-- __TODO:__ Implement @'broadcast'@.
broadcast :: msg          -- ^The message to broadcast.
          -> Agent msg ()
broadcast = error "TODO: implement broadcast"

-- |Wait for a broadcast and return the received message.
-- __TODO:__ Implement @'receive'@.
receive :: Agent msg msg
receive = error "TODO: implement receive"

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

-- |Function @'runIO' agents@ runs each agent in the given list
-- concurrently in the @'IO'@-monad.
-- Broadcast should be realized using a @'TChan' msg@ which is shared amongst
-- the threads running each agent,
-- and @'runIO'@ should only return if and when every agent has returned.
--
-- The operations should be interpreted as follows:
--
--  * @'delay'@ should delay execution for one second.
--  * @'broadcast' msg@ should broadcast @msg@ via the shared @'TChan'@
--    and additionally log the message to the console. Doing this naively
--    could lead to garbled output, so care must be taken to ensure sequential access
--    to the console.
--  * @'receive'@ should block the thread until a message is received on the
--    shared @'TChan'@.
--
-- __TODO:__ Implement @'runIO'@.
runIO :: Show msg
      => [Agent msg ()] -- ^The agents to run concurrently.
      -> IO ()
runIO = error "TODO: implement runIO"

-- |Time is divided into @'Slot'@s.
type Slot = Natural

-- |Each node has a @'NodeId'@.
-- If there are @n@ nodes, their id's will be 0, 1, 2,...,@(n-1)@.
type NodeId = Natural

-- |The blockchain is built from @'Block'@s.
data Block = Block
    { slot    :: Slot   -- ^Timestamp indicating when the block was created.
    , creator :: NodeId -- ^Identifies the node that created the block.
    }

instance Show Block where
    show b = printf "{%d %d}" (slot b) (creator b)

infixl 5 :>

-- |A blockchain can either be empty (just the genesis block) or contain @'Block'@s.
data Chain =
      Genesis
    | Chain :> Block

instance Show Chain where
    showsPrec _ Genesis  = showString "Genesis"
    showsPrec d (c :> b) = showParen (d > 10) $ showsPrec 0 c . showString " :> " . showString (show b)

-- |Computes the length of a @'Chain'@.  __TODO:__ Implement @'chainLength'@.
--
-- >>> chainLength Genesis
-- 0
-- >>> chainLength $ Genesis :> Block 2 2 :> Block 3 0
-- 2
chainLength :: Chain -> Int
chainLength = error "TODO: implement chainLength"

-- |Computes the slot leader. __TODO:__ Implement @'slotLeader'@.
--
-- >>> slotLeader 3 0
-- 0
-- >>> slotLeader 3 1
-- 1
-- >>> slotLeader 3 3
-- 0
slotLeader :: Int    -- ^Total number of nodes.
           -> Slot   -- ^The @'Slot'@.
           -> NodeId -- ^Identifies the node that has the right to create a block
                     --  in the given @'Slot'@.
slotLeader = error "TODO: implement slotLeader"

-- |Determines whether a chain is valid.  __TODO:__ Implement @'chainValid'@.
--
-- >>> chainValid 3 4 $ Genesis :> Block 10 1
-- False
-- >>> chainValid 3 14 $ Genesis :> Block 10 1
-- True
-- >>> chainValid 3 14 $ Genesis :> Block 10 2
-- False
-- >>> chainValid 3 14 $ Genesis :> Block 3 1 :> Block 10 1
-- False
-- >>> chainValid 3 14 $ Genesis :> Block 3 0 :> Block 10 1
-- True
chainValid :: Int   -- ^Total number of nodes.
           -> Slot  -- ^Current slot.
           -> Chain -- ^Chain to validate.
           -> Bool
chainValid = error "TODO: implement chainValid"

-- |The type of messages used for communication in the BFT-protocol.
data BftMessage =
      Time Slot      -- ^Message used by the @'clock'@ to broadcast the current time.
    | NewChain Chain -- ^Message used by a @'node'@ to announce a new @'Chain'@.
    deriving Show

-- |The nodes do not keep track of time by themselves, but instead rely on
-- the @'clock'@ agent, which broadcasts the beginning of each new @'Slot'@
-- using @'Time'@-messages. The agent should start with @'Slot' 0@ and run forever.
-- __TODO:__ Implement @'clock'@.
clock :: Agent BftMessage a
clock = error "TODO: implement clock"

-- |A @'node'@ participating in the BFT-protocol. It should start with the @'Genesis'@
-- chain at @'Slot' 0@ and run forever.
-- __TODO:__ Implement @'node'@.
node :: Int                -- ^Total number of nodes.
     -> NodeId             -- ^Identifier of /this/ node.
     -> Agent BftMessage a
node = error "TODO: implement node"

-- |Interprets a list of agents in a /pure/ fashion,
-- returning the list of all broadcasts (with their timestamps).
--
-- __Hint:__ It might be helpful to keep track of
--
--  * all active agents,
--  * all delayed agents and
--  * all agents waiting for a broadcast.
--
-- __TODO:__ Implement @'runPure'@.
runPure :: [Agent msg ()]   -- ^The agents to run.
        -> [(Natural, msg)] -- ^A list of all broadcasts, represented by
                            -- pairs containing a timestamp and the message that was sent.
runPure = error "TODO: implement runPure"
