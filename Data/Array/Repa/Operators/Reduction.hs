{-# LANGUAGE BangPatterns, ExplicitForAll, TypeOperators, MagicHash #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Array.Repa.Operators.Reduction
	( foldS,        foldP, reduceLTS
	, foldAllS,     foldAllP
	, sumS,         sumP
	, sumAllS,      sumAllP
        , equalsS,      equalsP)
where
import Data.Array.Repa.Base
import Data.Array.Repa.Index
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa.Operators.Mapping        as R
import Data.Array.Repa.Shape		        as S
import qualified Data.Vector.Unboxed	        as V
import qualified Data.Vector.Unboxed.Mutable    as M
import Prelude				        hiding (sum)
import qualified Data.Array.Repa.Eval.Reduction as E
import Data.Array.Repa.Repr.LazyTreeSplitting
import qualified Data.Rope as RP
import Control.Monad.Par
import System.IO.Unsafe
import GHC.Exts
import Control.DeepSeq

-- fold ----------------------------------------------------------------------
-- | Sequential reduction of the innermost dimension of an arbitrary rank array.
--
--   Combine this with `transpose` to fold any other dimension.
foldS   :: (Shape sh, Source r a, Elt a, Unbox a)
        => (a -> a -> a)
        -> a
        -> Array r (sh :. Int) a
        -> Array U sh a

foldS f z arr
 = arr `deepSeqArray`
   let  sh@(sz :. n') = extent arr
        !(I# n)       = n'
   in unsafePerformIO
    $ do mvec   <- M.unsafeNew (S.size sz)
         E.foldS mvec (\ix -> arr `unsafeIndex` fromIndex sh (I# ix)) f z n
         !vec   <- V.unsafeFreeze mvec
         now $ fromUnboxed sz vec
{-# INLINE [1] foldS #-}


-- | Parallel reduction of the innermost dimension of an arbitray rank array.
--
--   The first argument needs to be an associative sequential operator.
--   The starting element must be neutral with respect to the operator, for
--   example @0@ is neutral with respect to @(+)@ as @0 + a = a@.
--   These restrictions are required to support parallel evaluation, as the
--   starting element may be used multiple times depending on the number of threads.
foldP   :: (Shape sh, Source r a, Elt a, Unbox a, Monad m)
        => (a -> a -> a)
        -> a
        -> Array r (sh :. Int) a
        -> m (Array U sh a)

foldP f z arr 
 = arr `deepSeqArray`
   let  sh@(sz :. n) = extent arr
   in   case rank sh of
           -- specialise rank-1 arrays, else one thread does all the work.
           -- We can't match against the shape constructor,
           -- otherwise type error: (sz ~ Z)
           --
           1 -> do
                x       <- foldAllP f z arr
                now $ fromUnboxed sz $ V.singleton x

           _ -> now
              $ unsafePerformIO 
              $ do mvec   <- M.unsafeNew (S.size sz)
                   E.foldP mvec (\ix -> arr `unsafeIndex` fromIndex sh ix) f z n
                   !vec   <- V.unsafeFreeze mvec
                   now $ fromUnboxed sz vec
{-# INLINE [1] foldP #-}


-- foldAll --------------------------------------------------------------------
-- | Sequential reduction of an array of arbitrary rank to a single scalar value.
--
foldAllS :: (Shape sh, Source r a, Elt a, Unbox a)
	=> (a -> a -> a)
	-> a
	-> Array r sh a
	-> a

foldAllS f z arr 
 = arr `deepSeqArray`
   let  !ex     = extent arr
        !(I# n) = size ex
   in   E.foldAllS 
                (\ix -> arr `unsafeIndex` fromIndex ex (I# ix))
                f z n 
{-# INLINE [1] foldAllS #-}


-- | Parallel reduction of an array of arbitrary rank to a single scalar value.
--
--   The first argument needs to be an associative sequential operator.
--   The starting element must be neutral with respect to the operator,
--   for example @0@ is neutral with respect to @(+)@ as @0 + a = a@.
--   These restrictions are required to support parallel evaluation, as the
--   starting element may be used multiple times depending on the number of threads.
foldAllP 
        :: (Shape sh, Source r a, Elt a, Unbox a, Monad m)
	=> (a -> a -> a)
	-> a
	-> Array r sh a
	-> m a

foldAllP f z arr 
 = arr `deepSeqArray`
   let  sh = extent arr
        n  = size sh
   in   return
         $ unsafePerformIO 
         $ E.foldAllP (\ix -> arr `unsafeIndex` fromIndex sh ix) f z n
{-# INLINE [1] foldAllP #-}


-- sum ------------------------------------------------------------------------
-- | Sequential sum the innermost dimension of an array.
sumS	:: (Shape sh, Source r a, Num a, Elt a, Unbox a)
	=> Array r (sh :. Int) a
	-> Array U sh a
sumS = foldS (+) 0
{-# INLINE [3] sumS #-}


-- | Parallel sum the innermost dimension of an array.
sumP	:: (Shape sh, Source r a, Num a, Elt a, Unbox a, Monad m)
	=> Array r (sh :. Int) a
	-> m (Array U sh a)
sumP = foldP (+) 0 
{-# INLINE [3] sumP #-}


-- sumAll ---------------------------------------------------------------------
-- | Sequential sum of all the elements of an array.
sumAllS	:: (Shape sh, Source r a, Elt a, Unbox a, Num a)
	=> Array r sh a
	-> a
sumAllS = foldAllS (+) 0
{-# INLINE [3] sumAllS #-}


-- | Parallel sum all the elements of an array.
sumAllP	:: (Shape sh, Source r a, Elt a, Unbox a, Num a, Monad m)
	=> Array r sh a
	-> m a
sumAllP = foldAllP (+) 0
{-# INLINE [3] sumAllP #-}


-- Equality ------------------------------------------------------------------
instance (Shape sh, Eq sh, Source r a, Eq a) => Eq (Array r sh a) where
 (==) arr1 arr2
        =   extent arr1 == extent arr2
        && (foldAllS (&&) True (R.zipWith (==) arr1 arr2))


-- | Check whether two arrays have the same shape and contain equal elements,
--   in parallel.
equalsP :: (Shape sh, Eq sh, Source r1 a, Source r2 a, Eq a, Monad m) 
        => Array r1 sh a 
        -> Array r2 sh a
        -> m Bool
equalsP arr1 arr2
 = do   same    <- foldAllP (&&) True (R.zipWith (==) arr1 arr2)
        return  $ (extent arr1 == extent arr2) && same


-- | Check whether two arrays have the same shape and contain equal elements,
--   sequentially.
equalsS :: (Shape sh, Eq sh, Source r1 a, Source r2 a, Eq a) 
        => Array r1 sh a 
        -> Array r2 sh a
        -> Bool
equalsS arr1 arr2
        =   extent arr1 == extent arr2
        && (foldAllS (&&) True (R.zipWith (==) arr1 arr2))

reduceLTS :: (Shape sh, Source L a, NFData a) => (a -> a -> a) -> a -> Array L sh a -> a
reduceLTS f z rope = runPar $ RP.reduceLTS f z (toRope rope)