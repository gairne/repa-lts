{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

#define PHASE_STREAM [1]
#define PHASE_INNER  [0]

#define INLINE_STREAM INLINE PHASE_STREAM
#define INLINE_INNER  INLINE PHASE_INNER

#define SPECIALIZE 1

module Data.Rope.Seq (
  Seq,
  fromVector, toVector,
  fromList, toList,
  empty, singleton, last, length, append, split, splitAt, index,

  Progress(..), ZipProgress(..),

  map, mapUntil, filter, filterUntil, reduce, reduceUntil, zipWith, zipWithUntil, resumeZipWithUntil
 ) where

import Prelude hiding (last, length, filter, map, mapM, splitAt, zipWith)

import Control.DeepSeq
import Control.Monad.Par
import Control.Monad.ST
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic (Step(..), Stream(..), foldl1M', foldlM')
import Data.Vector.Fusion.Stream.Size (smaller)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import Control.Monad (liftM)


data Progress a b = More a
                  | Finished b
 deriving (Show)

data ZipProgress a b c = Hungry a
                       | Awaiting b
                       | ZipFinished c
 deriving (Show)

-- | The 'Seq' data type ensures that its elements are in WHNF
data Seq a where
    Seq        :: V.Vector a -> Seq a
#if SPECIALIZE
    Seq_Double :: U.Vector Double -> Seq Double
#endif /* SPECIALIZE */


instance Show a => Show (Seq a) where
    showsPrec p (Seq v) =
        showParen (p > 10) $
        showString "Seq " . showsPrec 11 v

#if SPECIALIZE
    showsPrec p (Seq_Double v) =
        showParen (p > 10) $
        showString "Seq_Double " . showsPrec 11 v
#endif /* SPECIALIZE */

--instance Read a => Read (Seq a) where
--    readsPrec p (Seq v) =
--        readParen (p > 10) $
--        readString "Seq " . readsPrec 11 v

-- #if SPECIALIZE
--    readsPrec p (Seq_Double v) =
--        readParen (p > 10) $
--        readString "Seq_Double " . readsPrec 11 v
-- #endif /* SPECIALIZE */

instance NFData (Seq a) where
    rnf (Seq v)        = v `seq` ()
#if SPECIALIZE
    rnf (Seq_Double v) = v `seq` ()
#endif /* SPECIALIZE */

fromVector :: G.Vector v a => v a -> Seq a
{-# INLINE fromVector #-}
fromVector = Seq . forceConvert

#if SPECIALIZE
{-# RULES

"fromVector [Double]" fromVector = fromVectorDouble

 #-}

index :: Seq a -> Int -> a
index (Seq v) i = v G.! i
index (Seq_Double v) i = v G.! i

fromVectorDouble :: U.Vector Double -> Seq Double
{-# INLINE fromVectorDouble #-}
fromVectorDouble v = Seq_Double v
#endif /* SPECIALIZE */

toVector :: G.Vector v a => Seq a -> v a
{-# INLINE toVector #-}
toVector (Seq v)        = G.convert v
#if SPECIALIZE
toVector (Seq_Double v) = G.convert v
#endif /* SPECIALIZE */

fromList :: [a] -> Seq a
{-# INLINE fromList #-}
fromList xs = Seq (G.fromList xs)

toList :: Seq a -> [a]
{-# INLINE toList #-}
toList (Seq xs)        = G.toList xs
toList (Seq_Double xs) = G.toList xs

empty :: Seq a
{-# INLINE empty #-}
empty = Seq (G.empty)

singleton :: a -> Seq a
singleton x = Seq (G.singleton x)

last :: Seq a -> a
{-# INLINE last #-}
last (Seq v)        = G.last v
#if SPECIALIZE
last (Seq_Double v) = G.last v
#endif /* SPECIALIZE */

length :: Seq a -> Int
{-# INLINE length #-}
length (Seq v)        = G.length v
#if SPECIALIZE
length (Seq_Double v) = G.length v
#endif /* SPECIALIZE */

append :: Seq a -> Seq a -> Seq a
{-# INLINE append #-}
append (Seq v)        (Seq w)        = Seq (v G.++ w)
#if SPECIALIZE
append (Seq_Double v) (Seq_Double w) = Seq_Double (v G.++ w)
append (Seq v)        (Seq_Double w) = Seq (v G.++ G.convert w)
append (Seq_Double v) (Seq w)        = Seq (G.convert v G.++ w)
#endif /* SPECIALIZE */

split :: Seq a -> (Seq a, Seq a)
{-# INLINE split #-}
split (Seq w)        = let (u, v) = G.splitAt (G.length w `div` 2) w in (Seq u, Seq v)
#if SPECIALIZE
split (Seq_Double w) = let (u, v) = G.splitAt (G.length w `div` 2) w in (Seq_Double u, Seq_Double v)
#endif /* SPECIALIZE */

splitAt :: Int -> Seq a -> (Seq a, Seq a)
{-# INLINE splitAt #-}
splitAt n (Seq w)        = let (u, v) = G.splitAt n w in (Seq u, Seq v)
#if SPECIALIZE
splitAt n (Seq_Double w) = let (u, v) = G.splitAt n w in (Seq_Double u, Seq_Double v)
#endif /* SPECIALIZE */

-- | Map a function over a 'Seq'
map :: (a -> b) -> Seq a -> Seq b
{-# INLINE map #-}
map f (Seq v)        = (Seq . G.unstream . S.inplace (mapM (return . f)) . G.stream) v
#if SPECIALIZE
map f (Seq_Double v) = (Seq . G.unstream . S.inplace (mapM (return . f)) . G.stream) v
#endif /* SPECIALIZE */

reduce :: (a -> a -> a) -> a -> Seq a -> a
{-# INLINE reduce #-}
reduce f k (Seq v) = G.foldl' f k v --G.unstream . S.inplace (foldrM (\a b -> return (f a b))) v
#if SPECIALIZE
reduce f k (Seq_Double v) = G.foldl1' f v --(G.unstream . S.inplace (foldl1M' (return . f)) . G.stream) v --G.foldl1' f k v
#endif /* SPECIALIZE */


-- | Filter a 'Seq'
filter :: (a -> Bool) -> Seq a -> Seq a
{-# INLINE filter #-}
filter f (Seq v)        = (Seq . G.unstream . S.inplace (filterM (return . f)) . G.stream) v
#if SPECIALIZE
filter f (Seq_Double v) = (Seq . G.unstream . S.inplace (filterM (return . f)) . G.stream) v
#endif /* SPECIALIZE */

zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
{-# INLINE zipWith #-}
zipWith f (Seq u) (Seq v) = Seq $ G.zipWith f u v --(Seq . G.unstream . (zipWithM (return . f))) (G.stream u) (G.stream v)
#if SPECIALIZE
zipWith f (Seq_Double u) (Seq_Double v) = Seq $ G.unstream $ (zipWithM (\x -> \y -> return $ f x y) (G.stream u) (G.stream v))

-- (Seq . G.unstream . S.inplace (zipWithM (return . f))) . ((G.stream u) (G.stream v)) --Seq $ U.zipWith f u v --
#endif /* SPECIALIZE */

-- | Map a monadic function over a 'Seq'
mapM :: forall m a b . Monad m => (a -> m b) -> Stream m a -> Stream m b
{-# INLINE_STREAM mapM #-}
mapM f (Stream step (s0 :: s) n) = Stream step' s0 n
  where
    {-# INLINE_INNER step' #-}
    step' :: s -> m (Step s b)
    step' s = do
        r <- step s
        case r of
          Yield x s' -> do  y <- f x
                            y `seq` return $ Yield y s'
          Skip    s' -> return $ Skip s'
          Done       -> return Done

-- | Drop elements which do not satisfy the monadic predicate
filterM :: forall m a . Monad m => (a -> m Bool) -> Stream m a -> Stream m a
{-# INLINE_STREAM filterM #-}
filterM f (Stream step s n) = Stream step' s n--(toMax n)
  where
    {-# INLINE_INNER step' #-}
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  b <- f x
                                  return $ if b then Yield x s'
                                                else Skip    s'
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done

-- data SPEC = SPEC
-- #define EMPTY_STREAM (\s -> ERROR s emptyStream)
-- emptyStream :: String
-- {-# NOINLINE emptyStream #-}
-- emptyStream = "empty stream"


-- -- | Left fold over a non-empty 'Stream' with a strict accumulator and a
-- -- monadic operator
-- foldl1M' :: Monad m => (a -> a -> m a) -> Stream m a -> m a
-- {-# INLINE_STREAM foldl1M' #-}
-- foldl1M' f (Stream step s sz) = foldl1M'_loop SPEC s
--   where
--     foldl1M'_loop !sPEC s
--       = do
--           r <- step s
--           case r of
--             Yield x s' -> foldlM' f x (Stream step s' (sz - 1))
--             Skip    s' -> foldl1M'_loop SPEC s'
--             Done       -> EMPTY_STREAM "foldl1M'"

-- -- | Left fold with a strict accumulator and a monadic operator
-- foldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
-- {-# INLINE_STREAM foldlM' #-}
-- foldlM' m z (Stream step s _) = foldlM'_loop SPEC z s
--   where
--     foldlM'_loop !sPEC z s
--       = z `seq`
--         do
--           r <- step s
--           case r of
--             Yield x s' -> do { z' <- m z x; foldlM'_loop SPEC z' s' }
--             Skip    s' -> foldlM'_loop SPEC z s'
--             Done       -> return z

-- | Zip two 'Stream's with the given monadic function
zipWithM :: forall m a b c . Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE_STREAM zipWithM #-}
zipWithM f (Stream stepa sa na) (Stream stepb sb nb)
  = Stream step (sa, sb, Nothing) (smaller na nb)
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, Nothing) = liftM (\r ->
                               case r of
                                 Yield x sa' -> Skip (sa', sb, Just x)
                                 Skip    sa' -> Skip (sa', sb, Nothing)
                                 Done        -> Done
                             ) (stepa sa)

    step (sa, sb, Just x)  = do
                               r <- stepb sb
                               case r of
                                 Yield y sb' ->
                                   do
                                     z <- f x y
                                     return $ Yield z (sa, sb', Nothing)
                                 Skip    sb' -> return $ Skip (sa, sb', Just x)
                                 Done        -> return $ Done


#if SPECIALIZE
{-# RULES

"map [Double]" map = mapDouble
"reduce [Double]" reduce = reduceDouble
"filter [Double]" filter = filterDouble
"zipWith [Double]" zipWith = zipWithDouble

 #-}

mapDouble :: (Double -> Double) -> Seq Double -> Seq Double
mapDouble f (Seq v)        = Seq_Double (U.map f (U.convert v))
mapDouble f (Seq_Double v) = Seq_Double (U.map f v)

reduceDouble :: (Double -> Double -> Double) -> Double -> Seq Double -> Double
reduceDouble f z (Seq v)        = U.foldl1' f (U.convert v)
reduceDouble f z (Seq_Double v) = U.foldl1' f v

filterDouble :: (Double -> Bool) -> Seq Double -> Seq Double
filterDouble f (Seq v)        = Seq_Double (U.filter f (U.convert v))
filterDouble f (Seq_Double v) = Seq_Double (U.filter f v)

zipWithDouble :: (Double -> Double -> Double) -> Seq Double -> Seq Double -> Seq Double
zipWithDouble f (Seq u) (Seq v)               = Seq_Double (U.zipWith f (U.convert u) (U.convert v))
zipWithDouble f (Seq_Double u) (Seq v)        = Seq_Double (U.zipWith f u (U.convert v))
zipWithDouble f (Seq u) (Seq_Double v)        = Seq_Double (U.zipWith f (U.convert u) v)
zipWithDouble f (Seq_Double u) (Seq_Double v) = Seq_Double (U.zipWith f u v)
#endif /* SPECIALIZE */

mapUntilV  ::  forall v1 v2 a b . (G.Vector v1 a, G.Vector v2 b)
           =>  Par Bool
           ->  (a -> b)
           ->  v1 a
           ->  Progress (v2 b, v1 a) (v2 b)
{-# INLINE mapUntilV #-}
mapUntilV cond f v1 = runST $ do
    mv2 <- GM.new len
    go mv2 0
  where
    go  ::  G.Mutable v2 s b
        ->  Int
        ->  ST s (Progress (v2 b, v1 a) (v2 b))
    go mv2 !i | i >= len = do
      v2 <- G.freeze mv2
      return $ Finished v2

    go mv2 !i = do
      if runPar cond
        then do  v2 <- G.freeze mv2
                 return $ More (G.take i v2, G.drop i v1)
        else do  let x = f (v1 G.! i)
                 x `seq` GM.write mv2 i x
                 go mv2 (i+1)

    len :: Int
    len = G.length v1

zipWithUntilV :: forall v1 v2 v3 a b c . (G.Vector v1 a, G.Vector v2 b, G.Vector v3 c)
              => Par Bool
              -> (a -> b -> c)
              -> v1 a
              -> v2 b
              -> ZipProgress (v3 c, (v1 a, v2 b), Int) (v3 c, v1 a, Int) (v3 c, v2 b)
{-# INLINE zipWithUntilV #-}
zipWithUntilV cond f v1 v2 = runST $ do
    mv3 <- GM.new len
    go mv3 0
  where
    go :: G.Mutable v3 s c
       -> Int
       -> ST s (ZipProgress (v3 c, (v1 a, v2 b), Int) (v3 c, v1 a, Int) (v3 c, v2 b))
    go mv3 !i | i >= len = do
      v3 <- G.freeze mv3
      return $ ZipFinished (v3, G.drop i v2)

    go mv3 !i | i >= clen = do
      v3 <- G.freeze mv3
      return $ Awaiting (G.take i v3, G.drop i v1, i)

    go mv3 !i = do
      if runPar cond
        then do v3 <- G.freeze mv3
                return $ Hungry (G.take i v3, (G.drop i v1, G.drop i v2), i)
        else do let x = f (v1 G.! i) (v2 G.! i)
                x `seq` GM.write mv3 i x
                go mv3 (i+1)

    len :: Int
    len = G.length v1
    clen :: Int
    clen = G.length v2

resumeZipWithUntilV :: forall v1 v2 v3 a b c . (G.Vector v1 a, G.Vector v2 b, G.Vector v3 c)
              => Par Bool
              -> (a -> b -> c)
              -> v1 a
              -> v2 b
              -> v3 c
              -> Int
              -> ZipProgress (v3 c, (v1 a, v2 b), Int) (v3 c, v1 a, Int) (v3 c, v2 b)
{-# INLINE resumeZipWithUntilV #-}
resumeZipWithUntilV cond f v1 v2 v3 n = runST $ do
    mv3' <- G.thaw v3
    mv3 <- GM.grow mv3' len
    go mv3 0
  where
    go :: G.Mutable v3 s c
       -> Int
       -> ST s (ZipProgress (v3 c, (v1 a, v2 b), Int) (v3 c, v1 a, Int) (v3 c, v2 b))
    go mv3 !i | i >= len = do
      v3 <- G.freeze mv3
      return $ ZipFinished (v3, G.drop i v2)

    go mv3 !i | i >= clen = do
      v3 <- G.freeze mv3
      return $ Awaiting (G.take i v3, G.drop i v1, i)

    go mv3 !i = do
      if runPar cond
        then do v3 <- G.freeze mv3
                return $ Hungry (G.take i v3, (G.drop i v1, G.drop i v2), i)
        else do let x = f (v1 G.! i) (v2 G.! i)
                x `seq` GM.write mv3 i x
                go mv3 (i+1)

    len :: Int
    len = (G.length v1)
    clen :: Int
    clen = (G.length v2)

filterUntilV :: forall v1 v2 a . (G.Vector v1 a, G.Vector v2 a) => Par Bool -> (a -> Bool) -> v1 a -> Progress (v2 a, v1 a) (v2 a)
{-# INLINE filterUntilV #-}
filterUntilV hungryF filterF v1 = runST $ do
    mv2 <- GM.new len -- We need to rebalance the rope later to change the size.
    go mv2 0 0
    
  where
    go :: G.Mutable v2 s a -> Int -> Int -> ST s (Progress (v2 a, v1 a) (v2 a))
    go mv2 !i !j | i >= len = do
      let mv2' = GM.take j mv2
      v2 <- G.freeze mv2'
      --v2 <- G.freeze mv
      --let mv2' = GM.new j
      --    mv2'' = G.take j v2
      --v2 <- G.freeze mv2''
      return $ Finished v2
    go mv2 !i !j = do
      if runPar hungryF
        then do v2 <- G.freeze mv2
                return $ More (G.take i v2, G.drop i v1)
        else do let x = filterF (v1 G.! i)
                if x then do GM.write mv2 j (v1 G.! i); go mv2 (i+1) (j+1)
                     else do go mv2 (i+1) j
    len :: Int
    len = G.length v1

reduceUntilV  ::  forall v1 a . (G.Vector v1 a)
           =>  Par Bool
           ->  (a -> a -> a)
           ->  a
           ->  v1 a
           ->  Progress (a, v1 a) a
{-# INLINE reduceUntilV #-}
reduceUntilV cond f k v1 = runST $ do
    go k 0
  where
    go  ::  a
        ->  Int
        ->  ST s (Progress (a, v1 a) a)
    go k !i | i >= len = do
      return $ Finished k

    go k !i = do
      if runPar cond
        then do  return $ More (k, G.drop i v1)
        else do  let k' = f k (v1 G.! i)
                 go k' (i+1)

    len :: Int
    len = G.length v1

-- ----------------------

mapUntil  ::  forall a b . Par Bool
          ->  (a -> b)
          ->  Seq a
          ->  Progress (Seq b, Seq a) (Seq b)
{-# INLINE mapUntil #-}
mapUntil cond f (Seq u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq v, Seq w)
      Finished v   -> Finished (Seq v)

mapUntil cond f (Seq_Double u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq v, Seq_Double w)
      Finished v   -> Finished (Seq v)

#if SPECIALIZE
{-# RULES

"mapUntil [Double]" mapUntil = mapUntilDouble

 #-}

mapUntilDouble  ::  Par Bool
                ->  (Double -> Double)
                ->  Seq Double
                ->  Progress (Seq Double, Seq Double) (Seq Double)
{-# INLINE mapUntilDouble #-}
mapUntilDouble cond f (Seq u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq_Double v, Seq w)
      Finished v   -> Finished (Seq_Double v)

mapUntilDouble cond f (Seq_Double u) =
    case mapUntilV cond f u of
      More (v, w)  -> More (Seq_Double v, Seq_Double w)
      Finished v   -> Finished (Seq_Double v)
#endif /* SPECIALIZE */


filterUntil :: forall a . Par Bool -> (a -> Bool) -> Seq a -> Progress (Seq a, Seq a) (Seq a)
{-# INLINE filterUntil #-}
filterUntil hungryCond filterCond (Seq u) =
  case filterUntilV hungryCond filterCond u of
    More (v, w) -> More (Seq v, Seq w)
    Finished v  -> Finished (Seq v)

#if SPECIALIZE
{-# RULES

"filterUntil [Double]" filterUntil = filterUntilDouble

 #-}

filterUntilDouble :: Par Bool -> (Double -> Bool) -> Seq Double -> Progress (Seq Double, Seq Double) (Seq Double)
{-# INLINE filterUntilDouble #-}
filterUntilDouble hungryCond filterCond (Seq u) =
  case filterUntilV hungryCond filterCond u of
    More (v, w) -> More (Seq_Double v, Seq w)
    Finished v  -> Finished (Seq_Double v)

filterUntilDouble hungryCond filterCond (Seq_Double u) =
  case filterUntilV hungryCond filterCond u of
    More (v, w) -> More (Seq_Double v, Seq_Double w)
    Finished v  -> Finished (Seq_Double v)
#endif /* SPECIALIZE */


reduceUntil  ::  forall a . Par Bool
          ->  (a -> a -> a)
          ->  a
          ->  Seq a
          ->  Progress (a, Seq a) a
{-# INLINE reduceUntil #-}
reduceUntil cond f k (Seq u) =
    case reduceUntilV cond f k u of
      More (v, w)  -> More (v, Seq w)
      Finished v   -> Finished (v)

#if SPECIALIZE
{-# RULES

"reduceUntil [Double]" reduceUntil = reduceUntilDouble

 #-}
reduceUntilDouble  ::  Par Bool
          ->  (Double -> Double -> Double)
          ->  Double
          ->  Seq Double
          ->  Progress (Double, Seq Double) Double
{-# INLINE reduceUntilDouble #-}
reduceUntilDouble cond f k (Seq u) =
    case reduceUntilV cond f k u of
      More (v, w)  -> More (v, Seq w)
      Finished v   -> Finished (v)

reduceUntilDouble cond f k (Seq_Double u) =
    case reduceUntilV cond f k u of
      More (v, w)  -> More (v, Seq_Double w)
      Finished v   -> Finished (v)
#endif /* SPECIALIZE */


zipWithUntil :: forall a b c . Par Bool
          -> (a -> b -> c)
          -> Seq a
          -> Seq b
          -> ZipProgress (Seq c, (Seq a, Seq b), Int) (Seq c, Seq a, Int) (Seq c, Seq b)
{-# INLINE zipWithUntil #-}
zipWithUntil cond f (Seq u) (Seq v) =
    case zipWithUntilV cond f u v of
      Hungry (x, (y, z), i) -> Hungry (Seq x, (Seq y, Seq z), i)
      Awaiting (v, y, i) -> Awaiting (Seq v, Seq y, i)
      ZipFinished (v, z) -> ZipFinished (Seq v, Seq z)

#if SPECIALIZE
{-# RULES

"zipWithUntil [Double]" zipWithUntil = zipWithUntilDouble

 #-}
zipWithUntilDouble :: Par Bool
          -> (Double -> Double -> Double)
          -> Seq Double
          -> Seq Double
          -> ZipProgress (Seq Double, (Seq Double, Seq Double), Int) (Seq Double, Seq Double, Int) (Seq Double, Seq Double)
{-# INLINE zipWithUntilDouble #-}
zipWithUntilDouble cond f (Seq u) (Seq v) =
    case zipWithUntilV cond f u v of
      Hungry (x, (y, z), i) -> Hungry (Seq_Double x, (Seq y, Seq z), i)
      Awaiting (v, y, i) -> Awaiting (Seq_Double v, Seq y, i)
      ZipFinished (v, z) -> ZipFinished (Seq_Double v, Seq z)

zipWithUntilDouble cond f (Seq_Double u) (Seq_Double v) =
    case zipWithUntilV cond f u v of
      Hungry (x, (y, z), i) -> Hungry (Seq_Double x, (Seq_Double y, Seq_Double z), i)
      Awaiting (v, y, i) -> Awaiting (Seq_Double v, Seq_Double y, i)
      ZipFinished (v, z) -> ZipFinished (Seq_Double v, Seq_Double z)
#endif /* SPECIALIZE */



resumeZipWithUntil :: forall a b c . Par Bool
          -> (a -> b -> c)
          -> Seq a
          -> Seq b
          -> Seq c
          -> Int
          -> ZipProgress (Seq c, (Seq a, Seq b), Int) (Seq c, Seq a, Int) (Seq c, Seq b)
{-# INLINE resumeZipWithUntil #-}
resumeZipWithUntil cond f (Seq u) (Seq v) (Seq p) i =
    case resumeZipWithUntilV cond f u v p i of
      Hungry (x, (y, z), i) -> Hungry (Seq x, (Seq y, Seq z), i)
      Awaiting (v, y, i) -> Awaiting (Seq v, Seq y, i)      
      ZipFinished (v, z) -> ZipFinished (Seq v, Seq z)

#if SPECIALIZE
{-# RULES

"resumeZipWithUntil [Double]" resumeZipWithUntil = resumeZipWithUntilDouble

 #-}
resumeZipWithUntilDouble :: Par Bool
          -> (Double -> Double -> Double)
          -> Seq Double
          -> Seq Double
          -> Seq Double
          -> Int
          -> ZipProgress (Seq Double, (Seq Double, Seq Double), Int) (Seq Double, Seq Double, Int) (Seq Double, Seq Double)
{-# INLINE resumeZipWithUntilDouble #-}
resumeZipWithUntilDouble cond f (Seq u) (Seq v) (Seq p) i =
    case resumeZipWithUntilV cond f u v p i of
      Hungry (x, (y, z), i) -> Hungry (Seq x, (Seq y, Seq z), i)
      Awaiting (v, y, i) -> Awaiting (Seq v, Seq y, i)      
      ZipFinished (v, z) -> ZipFinished (Seq v, Seq z)

resumeZipWithUntilDouble cond f (Seq_Double u) (Seq_Double v) (Seq_Double p) i =
    case resumeZipWithUntilV cond f u v p i of
      Hungry (x, (y, z), i) -> Hungry (Seq_Double x, (Seq_Double y, Seq_Double z), i)
      Awaiting (v, y, i) -> Awaiting (Seq_Double v, Seq_Double y, i)      
      ZipFinished (v, z) -> ZipFinished (Seq_Double v, Seq_Double z)

#endif /* SPECIALIZE */

forceConvert :: (G.Vector v a, G.Vector w a) => v a -> w a
{-# INLINE forceConvert #-}
forceConvert = G.unstream . forceStream . G.stream

forceStream :: forall m a . Monad m => Stream m a -> Stream m a
{-# INLINE_STREAM forceStream #-}
forceStream (Stream step (s0 :: s) n) = Stream step' s0 n
  where
    {-# INLINE_INNER step' #-}
    step' :: s -> m (Step s a)
    step' s = do
        r <- step s
        case r of
          Yield x s' -> x `seq` return $ Yield x s'
          Skip    s' -> return $ Skip s'
          Done       -> return Done


-- -- | Zip two 'Stream's with the given monadic function
-- zipWithM :: forall m a b c . Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
-- {-# INLINE_STREAM zipWithM #-}
-- zipWithM f (Stream stepa sa na) (Stream stepb sb nb)
--   = Stream step (sa, sb, Nothing) (smaller na nb)
--   where
--     {-# INLINE_INNER step #-}
--     step (sa, sb, Nothing) = liftM (\r ->
--                                case r of
--                                  Yield x sa' -> Skip (sa', sb, Just x)
--                                  Skip    sa' -> Skip (sa', sb, Nothing)
--                                  Done        -> Done
--                              ) (stepa sa)

--     step (sa, sb, Just x)  = do
--                                r <- stepb sb
--                                case r of
--                                  Yield y sb' ->
--                                    do
--                                      z <- f x y
--                                      return $ Yield z (sa, sb', Nothing)
--                                  Skip    sb' -> return $ Skip (sa, sb', Just x)
--                                  Done        -> return $ Done

-- data SPEC = SPEC

-- -- | Right fold with a monadic operator
-- foldrM :: forall m a . Monad m => (a -> a -> m a) -> a -> Stream m a -> m a
-- {-# INLINE_STREAM foldrM #-}
-- foldrM f z (Stream step s _) = foldrM_loop SPEC s
--   where
--     foldrM_loop !sPEC s
--       = do
--           r <- step s
--           case r of
--             Yield x s' -> f x =<< foldrM_loop SPEC s'
--             Skip    s' -> foldrM_loop SPEC s'
--             Done       -> return z
