
module Data.Array.Repa.Repr.LazyTreeSplitting
        ( L, Array (..)
        , fromRope, toRope, ropeFromList, ropeToList
        )
where
import Data.Array.Repa.Shape
import Data.Array.Repa.Base hiding (toList)
import Data.Array.Repa.Eval hiding (fromList, toList)
import Data.Array.Repa.Index
import Control.Monad
import qualified Data.Rope as T
import qualified Data.Rope.Seq as TS
import Control.DeepSeq

--TODO: FIX
-- | Unboxed arrays are represented as unboxed vectors.
--
--   The implementation uses @Data.Vector.Unboxed@ which is based on type
--   families and picks an efficient, specialised representation for every
--   element type. In particular, unboxed vectors of pairs are represented
--   as pairs of unboxed vectors.
--   This is the most efficient representation for numerical data.
--
data L

linearIndex' :: Int -> T.Rope a -> a
linearIndex' n (T.Leaf x) = TS.index x n
linearIndex' n (T.Cat s d l r) =
    if (T.length l) <= n
    then linearIndex' n l
    else linearIndex' (n - (T.length l)) r

--TODO: FIX
-- | Read elements from an unboxed vector array.
instance Source L a where
 data Array L sh a --TODO: Restrict to DIM1? Can we do DIM2+?
        = ARope !sh !(T.Rope a)

 linearIndex (ARope _ vec) ix
        = linearIndex' ix vec
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (ARope sh vec) ix
        = linearIndex' ix vec
 {-# INLINE unsafeLinearIndex #-}

 extent (ARope sh _)
        = sh
 {-# INLINE extent #-}

 deepSeqArray (ARope sh vec) x 
  = sh `deepSeq` vec `seq` x
 {-# INLINE deepSeqArray #-}


--TODO: Do we need to restrict Rope class to contain values that can be Show'n and Read?
deriving instance (Show sh, Show e)
        => Show (Array L sh e)

--deriving instance (Read sh, Read e)
--        => Read (Array L sh e)

-- | O(1)
fromRope :: Shape sh => sh -> T.Rope a -> Array L sh a
fromRope sh rp = ARope sh rp
{-# INLINE fromRope #-}

-- | O(1)
toRope :: Shape sh => Array L sh a -> T.Rope a
toRope (ARope sh rp) = rp
{-# INLINE toRope #-}

ropeFromList :: Shape sh => sh -> [a] -> Array L sh a
ropeFromList sh xs = ARope sh (let x = T.fromList xs in x `seq` x) --(ix1 (length xs)) (T.fromList xs)
{-# INLINE ropeFromList #-}

ropeToList :: Shape sh => Array L sh a -> [a]
ropeToList (ARope _ rp) = let x = T.toList rp in x `seq` x
{-# INLINE ropeToList #-}