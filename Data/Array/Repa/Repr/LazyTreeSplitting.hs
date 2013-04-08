
module Data.Array.Repa.Repr.LazyTreeSplitting
        ( L, Array (..)
        , fromListToTree)
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


-- Fill -----------------------------------------------------------------------
-- | Filling of unboxed vector arrays.
instance U.Unbox e => Target U e where
 data MVec U e 
  = ()

 newMVec n
  = error "newMVec: Should not be used"
 {-# INLINE newMVec #-}

 unsafeWriteMVec v ix
  = error "unsafeWriteMVec: Should not be used"
 {-# INLINE unsafeWriteMVec #-}

 unsafeFreezeMVec sh v     
  = error "unsafeFreezeMVec: Should not be used"
 {-# INLINE unsafeFreezeMVec #-}

 deepSeqMVec v x
  = error "deepSeqMVec: Should not be used"
 {-# INLINE deepSeqMVec #-}

 touchMVec _ 
  = return ()
 {-# INLINE touchMVec #-}

loadPLTS :: Shape sh => Array L sh e -> 

-- Load -----------------------------------------------------------------------
-- | Compute all elements in an array.
instance Shape sh => Load D sh e where
 loadP (ADelayed sh getElem) mvec
  = mvec `deepSeqMVec`
    do  traceEventIO "Repa.loadP[LTS]: start"
--        fillChunkedP (size sh) (unsafeWriteMVec mvec) (getElem . fromIndex sh)
--        touchMVec mvec
        traceEventIO "Repa.loadP[LTS]: end"
 {-# INLINE [4] loadP #-}

 loadS (ADelayed sh getElem) mvec
  = mvec `deepSeqMVec`
    do  traceEventIO "Repa.loadS[LTS]: start"
--        fillLinearS (size sh) (unsafeWriteMVec mvec) (getElem . fromIndex sh)
--        touchMVec mvec
        traceEventIO "Repa.loadS[LTS]: end"
 {-# INLINE [4] loadS #-}


-- Conversions ----------------------------------------------------------------
-- | Sequential computation of array elements..
--
--   * This is an alias for `computeS` with a more specific type.
--
computeUnboxedS
        :: ( Shape sh
           , Load r1 sh e, U.Unbox e)
        => Array r1 sh e -> Array U sh e
computeUnboxedS = computeS
{-# INLINE computeUnboxedS #-}


-- | Parallel computation of array elements.
--
--   * This is an alias for `computeP` with a more specific type.
--
computeUnboxedP
        :: ( Shape sh
           , Load r1 sh e, Monad m, U.Unbox e)
        => Array r1 sh e -> m (Array U sh e)
computeUnboxedP = computeP
{-# INLINE computeUnboxedP #-}


-- | O(n). Convert a list to an unboxed vector array.
-- 
--   * This is an alias for `fromList` with a more specific type.
--
fromListUnboxed
        :: (Shape sh, U.Unbox a)
        => sh -> [a] -> Array U sh a
fromListUnboxed = R.fromList
{-# INLINE fromListUnboxed #-}


-- | O(1). Wrap an unboxed vector as an array.
fromUnboxed
        :: (Shape sh, U.Unbox e)
        => sh -> U.Vector e -> Array U sh e
fromUnboxed sh vec
        = AUnboxed sh vec
{-# INLINE fromUnboxed #-}


-- | O(1). Unpack an unboxed vector from an array.
toUnboxed
        :: U.Unbox e
        => Array U sh e -> U.Vector e
toUnboxed (AUnboxed _ vec)
        = vec
{-# INLINE toUnboxed #-}
