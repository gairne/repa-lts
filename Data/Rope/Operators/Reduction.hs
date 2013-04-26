{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rope.Operators.Reduction
 (reduceLTS)
where

import Data.Rope.Rope

import Debug.Trace       (trace, traceEvent)

import qualified Data.Rope.Seq as Seq
import Data.Rope.Seq (Progress(..), Seq)

import Control.Monad.Par

import Control.DeepSeq

import Prelude hiding (length, splitAt)
import qualified Prelude as P

import Data.List (foldl')

data ReduceCtx a = RCTop
                 | RCLeft (ReduceCtx a) (Rope a)
                 | RCRight a (ReduceCtx a)
 deriving (Show)

data ReduceCur a = ReduceCur a (Seq a) (ReduceCtx a)

reduceCtxLength :: ReduceCtx a -> Int
reduceCtxLength RCTop = 0
reduceCtxLength (RCLeft  ctx' rrp)  = reduceCtxLength ctx' + length rrp
reduceCtxLength (RCRight lrp  ctx') = reduceCtxLength ctx' --reduceCtxLength ctx' +++ (length lrp, 0)

reduceCtxDepth :: ReduceCtx a -> Int
reduceCtxDepth  RCTop = 0
reduceCtxDepth (RCLeft  ctx' rrp)  = reduceCtxDepth ctx' + depth rrp
reduceCtxDepth (RCRight lrp  ctx') = reduceCtxDepth ctx' -- +++ (depth lrp, 0)

reduceCurLength :: ReduceCur a -> Int
reduceCurLength (ReduceCur p useq ctx) =
    reduceCtxLength ctx + Seq.length useq

reduceCurDepth :: ReduceCur a -> Int
reduceCurDepth (ReduceCur p useq ctx) =
    reduceCtxDepth ctx + 1

reduceLeftmost :: Rope a -> ReduceCtx a -> (Seq a, ReduceCtx a)
reduceLeftmost (Leaf xs)         ctx = (xs, ctx)
reduceLeftmost (Cat _ _ lrp rrp) ctx = reduceLeftmost lrp (RCLeft ctx rrp)

reduceNext :: (a -> a -> a) -> a -> ReduceCtx a -> Progress (Seq a, ReduceCtx a) a
reduceNext f v RCTop              = Finished v
reduceNext f v (RCLeft ctx' rrp)  = let (xs, ctx'') = reduceLeftmost rrp (RCRight v ctx')
                             in
                               More (xs, ctx'')
reduceNext f ov (RCRight v ctx') = reduceNext f (f ov $! v) ctx'

rsplitCur :: ReduceCur a -> (Rope a, Rope a, [a])
rsplitCur cur =
  let n = reduceCurLength cur `div` 2
      UnzipReduceCtx ls rs ds = rcurUnzip cur
      (rps1, mrp, k, rps2) = divideRopes rs n
      (mls, mrs) = splitAt mrp k --rsplitAtAsCur mrp k
      (rp1, _) = encodeRopes (rps1 ++ [mls])
      (rp2, _) = encodeRopes ([mrs] ++ rps2)
  in
    (rp1, rp2, ls) --MapCurReb ls ds mds n1 n2 l1 l2)

reduceLTSUntil :: forall a . Par Bool
            -> (a -> a -> a)
            -> a
            -> Rope a
            -> Par (Progress (ReduceCur a) a)
reduceLTSUntil cond f z rp =
    m xs ctx
  where
    mSeq :: a -> ReduceCtx a -> a
    mSeq x ctx = case reduceNext f x ctx of
                    Finished p' -> p'
                    More (xs', ctx') -> mSeq (Seq.reduce f z xs') ctx'

    m :: Seq a -> ReduceCtx a -> Par (Progress (ReduceCur a) a)
    m xs ctx =
        case Seq.reduceUntil cond f z xs of
          Finished p' -> p' `seq` case reduceNext f p' ctx of --Completed this leaf, move onto next
                                   Finished red' -> return $ Finished red'
                                   More (seq', ctx') -> m seq' ctx'
          More (p', useq') -> if reduceCurLength (ReduceCur p' useq' ctx) >= 2
                                 then return $ More (ReduceCur p' useq' ctx)
                                 else return $ Finished (mSeq (Seq.reduce f p' useq') ctx)

    xs :: Seq a
    ctx :: ReduceCtx a
    (xs, ctx) = reduceLeftmost rp RCTop

data UnzipReduceCtx a = UnzipReduceCtx [a] [Rope a] [Dir]
 deriving (Show)

type UnzipReduceCur a = UnzipReduceCtx a

rctxUnzip :: ReduceCtx a -> UnzipReduceCtx a
rctxUnzip RCTop         = UnzipReduceCtx [] [] []
rctxUnzip (RCLeft c r)  = let UnzipReduceCtx ls rs ds = rctxUnzip c
                         in
                           UnzipReduceCtx ls (r:rs) (L:ds)
rctxUnzip (RCRight l c) = let UnzipReduceCtx ls rs ds = rctxUnzip c
                         in
                           UnzipReduceCtx (l:ls) rs (R:ds)

rcurUnzip :: ReduceCur a -> UnzipReduceCur a
rcurUnzip (ReduceCur p useq ctx) =
    UnzipReduceCtx (p:ls) (Leaf useq:rs) ds
  where
    (UnzipReduceCtx ls rs ds) = rctxUnzip ctx

rctxZip :: UnzipReduceCtx a -> ReduceCtx a
rctxZip (UnzipReduceCtx [] [] []) = RCTop
rctxZip (UnzipReduceCtx ls (r:rs) (L:ds)) = RCLeft (rctxZip (UnzipReduceCtx ls rs ds)) r
rctxZip (UnzipReduceCtx (l:ls) rs (R:ds)) = RCRight l (rctxZip (UnzipReduceCtx ls rs ds))

rcurZip :: UnzipReduceCur a -> ReduceCur a
rcurZip (UnzipReduceCtx (p:ls) (Leaf useq:rs) ds) =
    ReduceCur p useq (rctxZip (UnzipReduceCtx ls rs ds))



-- splitAtAsReduceCur :: Rope a -> Int -> MapCur a a
-- splitAtAsReduceCur rp n =
--     s rp RCTop n
--   where
--     s (Leaf seq) ctx n = let (lseq, rseq) = Seq.splitAt n seq
--                          in
--                            (MapCur lseq rseq ctx)
--     s (Cat _ _ lrp rrp) ctx n
--         | n < length rrp = s lrp (RCLeft ctx rrp) n
--         | otherwise      = s rrp (RCRight lrp ctx) (n - length lrp)



-- rsplitAtAsCur :: Rope a -> Int -> (Rope a, Rope a)
-- rsplitAtAsCur rp n =
--     s rp RCTop n
--   where
--     s :: Rope a -> ReduceCtx a -> Int -> (Rope a, Rope a)
--     s (Leaf seq) ctx n = let (lseq, rseq) = Seq.splitAt n seq
--                          in
--                            (lseq, rseq)
--     s (Cat _ _ lrp rrp) ctx n
--         | n < length rrp = s lrp (RCLeft ctx rrp) n
--         | otherwise      = s rrp (RCRight lrp ctx) (n - length lrp)



reduceLTS :: forall a . NFData a => (a -> a -> a) -> a -> Rope a -> Par a
reduceLTS bop init rp0 =
    go rp0
  where
    go :: Rope a -> Par a
    go rp@(Leaf u)
        | len < mAX_LEAF_SIZE = do prog <- reduceLTSUntil isHungry' bop init rp --isHungry bop init rp
                                   case prog of
                                     Finished red' -> return red'
                                     More cur' -> do let (rp1, rp2, reb) = rsplitCur cur'
                                                     prp1' <- spawn (reduceLTS bop init rp1)
                                                     prp2' <- spawn (reduceLTS bop init rp2)
                                                     v1' <- get prp1'
                                                     v2' <- get prp2'
                                                     return $ bop v1' (foldl' bop init reb) `bop` v2' --root (join rp1' rp2' reb)
        | otherwise            = do let (v, w) = Seq.splitAt (len `div` 2) u
                                    iv' <- spawn_ $ go (Leaf v)
                                    iw' <- spawn_ $ go (Leaf w)
                                    v' <- get iv'
                                    w' <- get iw'
                                    return $ v' `bop` w' --v' `cat` w'
      where
        len :: Int
        len = Seq.length u

    go (Cat l d v w) = do iv' <- spawn $ go v
                          iw' <- spawn $ go w
                          v' <- get iv'
                          w' <- get iw'
                          return $ v' `bop` w' --Cat l d v' w'

isFalse :: Par Bool
isFalse = do --x <- put False
             --y <- get x
             return False
isHungry' :: Par Bool
isHungry' = do return runParHungry