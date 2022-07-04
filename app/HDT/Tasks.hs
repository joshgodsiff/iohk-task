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

      , PartitionedAgents (..)
    )  where

import HDT.Agent
import HDT.PingPong
import HDT.RunIO
import HDT.Blockchain
import HDT.Bft
import HDT.RunPure