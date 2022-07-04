module HDT.Blockchain where

import Data.Foldable as F hiding (toList)
import Numeric.Natural (Natural)
import Text.Printf (printf)

-- | Time is divided into @'Slot'@s.
type Slot = Natural

-- | Each node has a @'NodeId'@.
--  If there are @n@ nodes, their id's will be 0, 1, 2,...,@(n-1)@.
type NodeId = Natural

-- | The total number of nodes participating in the chain
type NumNodes = Natural

-- | The blockchain is built from @'Block'@s.
data Block = Block
    { slot    :: Slot   -- ^Timestamp indicating when the block was created.
    , creator :: NodeId -- ^Identifies the node that created the block.
    }

instance Show Block where
  show b = printf "{%d %d}" (slot b) (creator b)

infixl 5 :>

-- | A blockchain can either be empty (just the genesis block) or contain @'Block'@s.
data Chain
  = Genesis
  | Chain :> Block

instance Show Chain where
  showsPrec _ Genesis = showString "Genesis"
  showsPrec d (c :> b) = showParen (d > 10) $ showsPrec 0 c . showString " :> " . showString (show b)

-- | Computes the length of a @'Chain'@.
--  >>> chainLength Genesis
--  0
--  >>> chainLength $ Genesis :> Block 2 2 :> Block 3 0
--  2
chainLength :: Chain -> NumNodes
chainLength = castNum . F.length . toList

-- | Computes the slot leader.
--  >>> slotLeader 3 0
--  0
--  >>> slotLeader 3 1
--  1
--  >>> slotLeader 3 3
--  0
slotLeader :: NumNodes -- ^Total number of nodes.
           -> Slot     -- ^The @'Slot'@.
           -> NodeId   -- ^Identifies the node that has the right to create a block
                       --  in the given @'Slot'@.
slotLeader numNodes s = s `mod` numNodes

-- | Determines whether a chain is valid.
--  >>> chainValid 3 4 $ Genesis :> Block 10 1
--  False
--  >>> chainValid 3 14 $ Genesis :> Block 10 1
--  True
--  >>> chainValid 3 14 $ Genesis :> Block 10 2
--  False
--  >>> chainValid 3 14 $ Genesis :> Block 3 1 :> Block 10 1
--  False
--  >>> chainValid 3 14 $ Genesis :> Block 3 0 :> Block 10 1
--  True
chainValid :: NumNodes -- ^Total number of nodes.
           -> Slot     -- ^Current slot.
           -> Chain    -- ^Chain to validate.
           -> Bool
chainValid numNodes s chain = tsSI && bCBL && lBNFF
  where
    tsSI = timestampsStrictlyIncreasing chain
    bCBL = blockCreatedByLeader numNodes chain
    lBNFF = latestBlockNotFromFuture s chain

timestampsStrictlyIncreasing :: Chain -> Bool
timestampsStrictlyIncreasing = snd . F.foldr f (0, True) . toList
  where
    f blk (slt, b) = (slot blk, b && slot blk > slt)

blockCreatedByLeader :: NumNodes -> Chain -> Bool
blockCreatedByLeader numNodes = F.all (\b -> creator b == slotLeader numNodes (slot b)) . toList

latestBlockNotFromFuture :: Slot -> Chain -> Bool
latestBlockNotFromFuture _    Genesis  = True
latestBlockNotFromFuture time (_ :> b) = time >= slot b

toList :: Chain -> [Block]
toList Genesis = []
toList (c :> b) = b : toList c

castNum :: (Integral a, Num b) => a -> b
castNum = fromInteger . toInteger