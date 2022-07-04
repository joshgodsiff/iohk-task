{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}

module HDT.RunPure where

import Control.Monad.Free (Free (..))
import Data.Function ((&))
import Data.Sequence
  ( Seq (..),
    (|>),
  )
import Data.Sequence qualified as S
import HDT.Agent
import Numeric.Natural (Natural)

-- | Interprets a list of agents in a /pure/ fashion,
--  returning the list of all broadcasts (with their timestamps).
--
--  __Hint:__ It might be helpful to keep track of
--
--   * all active agents,
--   * all delayed agents and
--   * all agents waiting for a broadcast.
--
--  __TODO:__ Implement @'runPure'@.
runPure :: [Agent msg ()]   -- ^The agents to run.
        -> [(Natural, msg)] -- ^A list of all broadcasts, represented by
                            -- pairs containing a timestamp and the message that was sent.
runPure =
  zip [1 ..] . step . partitionAgents . S.fromList . fmap (\a -> PureAgent {queue = S.Empty, agent = a})

step :: PartitionedAgents (PureAgent msg a) -> [msg]
step pa = case fst next of
  Nothing -> step newAgents
  Just m  -> m : step newAgents
  where
    (nextAgent, rm) = popAgent pa
    next = maybe (Nothing, Nothing) interpret nextAgent
    newAgents = case next of
      (Nothing, Nothing) -> rm
      (Just m, Just a)   -> partitionAgent agent a rm & enqueueMsg m
      (Nothing, Just a)  -> partitionAgent agent a rm
      (Just m, Nothing)  -> pa & enqueueMsg m

enqueueMsg :: msg -> PartitionedAgents (PureAgent msg a) -> PartitionedAgents (PureAgent msg a)
enqueueMsg m = fmap (\a -> a {queue = queue a |> m})

interpret :: PureAgent msg a -> (Maybe msg, Maybe (PureAgent msg a))
interpret pa = case agent pa of
  Pure _            -> (Nothing, Nothing)
  (Free f) -> case f of
    Delay a         -> (Nothing,  pure $ pa {agent = a})
    Broadcast msg a -> (pure msg, pure $ pa {agent = a})
    Receive g -> case queue pa of
      S.Empty       -> (Nothing,  pure pa)
      (msg :<| ms)  -> (Nothing,  pure $ PureAgent {queue = ms, agent = g msg})

partitionAgents :: Seq (PureAgent msg a) -> PartitionedAgents (PureAgent msg a)
partitionAgents = foldr (partitionAgent agent) mempty

partitionAgent :: (c -> Agent msg a) -> c -> PartitionedAgents c -> PartitionedAgents c
partitionAgent g c pa = case g c of
  Pure _          -> pa
  Free ag -> case ag of
    Broadcast _ _ -> pa {broadcasting = broadcasting pa |> c}
    Receive _     -> pa {receiving = receiving pa |> c}
    Delay _       -> pa {delayed = delayed pa |> c}

popAgent ::
  PartitionedAgents (PureAgent msg b) ->
  (Maybe (PureAgent msg b), PartitionedAgents (PureAgent msg b))
popAgent pa@PartitionedAgents {broadcasting = (n :<| nx)} = (pure n, pa {broadcasting = nx})
popAgent pa@PartitionedAgents {receiving    = (n :<| nx)}
  | not . S.null $ queue n                                = (pure n, pa {receiving = nx})
popAgent pa@PartitionedAgents {delayed      = (n :<| nx)} = (pure n, pa {delayed = nx})
popAgent pa@PartitionedAgents {}                          = (Nothing, pa)

data PartitionedAgents agents = PartitionedAgents
  { broadcasting :: Seq agents
  , delayed :: Seq agents
  , receiving :: Seq agents
  } deriving (Functor)

instance Semigroup (PartitionedAgents a) where
  a <> b = PartitionedAgents
    { broadcasting = broadcasting a <> broadcasting b
    , receiving = receiving a <> receiving b
    , delayed = delayed a <> delayed b
    }

instance Monoid (PartitionedAgents a) where
  mempty = PartitionedAgents
      { broadcasting = mempty
      , receiving = mempty
      , delayed = mempty
      }

deriving instance Show a => Show (PartitionedAgents a)

data PureAgent msg a = PureAgent
  { agent :: Agent msg a,
    queue :: S.Seq msg
  }
