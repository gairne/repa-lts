
module Data.Array.Repa.Repr.LazyTreeSplitting
        ( L, Array (..)
        )
where
import Data.Array.Repa.Shape            as R
import Data.Array.Repa.Base             as R
import Data.Array.Repa.Eval             as R
import Data.Array.Repa.Repr.Delayed     as R
import Control.Monad
import qualified Data.Rope as T
import qualified Data.Rope.Seq TS

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

linearIndex' :: Int -> Rope a -> a
linearIndex' n (Leaf x) = TS.index x n
linearIndex' n (Cat s d l r) =
    if (T.length l) <= n
    then linearIndex' n l
    else linearIndex' (n - (T.length l)) r

--TODO: FIX
-- | Read elements from an unboxed vector array.
instance Source L a where
 data Array L sh a --TODO: Restrict to DIM1? Can we do DIM2+?
        = ARope !sh !(Rope a)

 linearIndex (ARope sh vec) ix
        = linearIndex' (size sh) vec ix
 {-# INLINE linearIndex #-}

 unsafeLinearIndex (ARope _ vec) ix
        = linearIndex' (size sh) vec ix
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

deriving instance (Read sh, Read e)
        => Read (Array L sh e)
