{-# LANGUAGE ScopedTypeVariables #-}

module Data.Rope.Operators.Zipping
 (zipWithLTS)

where

import Data.Rope.Rope

import Debug.Trace       (trace, traceEvent)

import qualified Data.Rope.Seq as Seq
import Data.Rope.Seq (Progress(..), Seq, ZipProgress(..))

import Control.Monad.Par

import Prelude hiding (length, splitAt)
import qualified Prelude as P

import Data.Rope.Operators.Mapping (UnzipMapCtx(..))
import qualified Data.Rope.Operators.Mapping as Map

data ZCTop
data ZCLeft
data ZCRight
data ZRTop
data ZRLeft
data ZRRight

--data ZipCtxs x y z = Contexts (ResultCtx x z) (ConsumedCtx y)  
type ZipCtxs x y z = (ResultCtx x z, ConsumedCtx y)

data ResultCtx a b = ZRTop
                | ZRLeft  (ResultCtx a b) (Rope a)
                | ZRRight (Rope b)     (ResultCtx a b)
 deriving (Show)

data ConsumedCtx d = ZCTop
                     | ZCLeft (ConsumedCtx d) (Rope d)
                     | ZCRight (ConsumedCtx d)

--Interrupted during processing of a Seq because someone was hungry. Split into processed and unprocessed parts.
data ZipCur a b c = ZipCur (Seq c) (Seq a, Seq b) (ZipCtxs a b c)
-- deriving (Show)

data UnzipZipCtxs a b c = UnzipZipCtxs [Rope c] [Rope a] [Rope b] [Dir] [Dir]
 deriving (Show)

type UnzipZipCur a b c = UnzipZipCtxs a b c

data ZipCurReb c = ZipCurReb [Rope c]
  [Dir] [Dir] {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  [Dir] [Dir] {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
 deriving (Show)

data ResCur a c = ResCur (Seq c) (Seq a) (ResultCtx a c)

data UnzipResCtx a c = UnzipResCtx [Rope c] [Rope a] [Dir]
type UnzipResCur a c = UnzipResCtx a c

plug :: Rope a -> ResultCtx a a -> Rope a
plug rp ZRTop = rp
plug rp (ZRLeft  ctx' rrp)  = plug (rp `ncat` rrp) ctx'
plug rp (ZRRight lrp  ctx') = plug (lrp `ncat` rp) ctx'


-- ctxDepth :: MapCtx a b -> (Int, Int)
-- ctxDepth  MCTop = (0, 0)
-- ctxDepth (MCLeft  ctx' rrp)  = ctxDepth ctx' +++ (0, depth rrp)
-- ctxDepth (MCRight lrp  ctx') = ctxDepth ctx' +++ (depth lrp, 0)

-- curDepth :: MapCur a b -> (Int, Int)
-- curDepth (MapCur pseq useq ctx) =
--     ctxDepth ctx +++ (1, 1)

-- lengthRight :: MapCur a b -> Int
-- lengthRight cur = snd (curLength cur)

-- depthRight :: MapCur a b -> Int
-- depthRight cur = snd (curDepth cur)

--

splitCur :: ZipCur a b c -> (Rope a, Rope a, Rope b, Rope b, ZipCurReb c)
splitCur cur =
  let n = snd (curLength cur) `div` 2
      UnzipZipCtxs ps as bs ads bds = curUnzip cur
      (rps1A, mrpA, kA, rps2A) = divideRopes as n
      n1A = P.length rps1A
      UnzipMapCtx mlsA mrsA mdsA = Map.curUnzip (Map.splitAtAsCur mrpA kA)
      n2A = P.length mrsA
      (rp1A, l1A) = encodeRopes (rps1A ++ mlsA)
      (rp2A, l2A) = encodeRopes (mrsA ++ rps2A)
      (rps1B, mrpB, kB, rps2B) = divideRopes bs n
      n1B = P.length rps1B
      UnzipMapCtx mlsB mrsB mdsB = Map.curUnzip (Map.splitAtAsCur mrpB kB)
      n2B = P.length mrsB
      (rp1B, l1B) = encodeRopes (rps1B ++ mlsB)
      (rp2B, l2B) = encodeRopes (mrsB ++ rps2B)
  in (rp1A, rp2A, rp1B, rp2B, ZipCurReb ps ads mdsA n1A n2A l1A l2A bds mdsB n1B n2B l1B l2B) -- Do we need the knowledge to rebuild the consumed bit? I think not.

join :: Rope a -> Rope a -> ZipCurReb c -> ResCur a c --, ResCur a c)
join rp1 rp2 (ZipCurReb ps ads mdsA n1A n2A l1A l2A bds msdB n1B n2B l1B l2B) =
    let  xs1A = decodeRope rp1 l1A
         rps1A = take n1A xs1A
         mlsA = drop n1A xs1A
         xs2A = decodeRope rp2 l2A
         mrsA = take n2A xs2A
         rps2A = drop n2A xs2A
         mrpA = root (rescurZip (UnzipResCtx mlsA mrsA mdsA))
         as = rps1A ++ [mrpA] ++ rps2A
         
         --xs1B = decodeRope rp1B l1B
         --rps1B = take n1B xs1B
         --mlsB = drop n1B xs1B
         --xs2B = decodeRope rp2B l2B
         --mrsB = take n2B xs2B
         --rps2B = drop n2B xs2B
         --mrpB = Map.root (curZip (Map.UnzipMapCtx mlsB mrsB mdsB))
         --bs = rps1B ++ [mrpB] ++ rps2B
    in
      rescurZip (UnzipResCtx ps as ads)--, rescurZip (UnzipResCtx ps bs bds))

root :: ResCur c c -> Rope c
root (ResCur xs ys ctx) = plug (Leaf (xs `Seq.append` ys)) ctx

-- Ported to Zip:

ctxUnzip :: ZipCtxs a b c -> UnzipZipCtxs a b c
ctxUnzip (ZRTop, ZCTop)        = UnzipZipCtxs [] [] [] [] []
ctxUnzip ((ZRLeft c r), cctx)  = let UnzipZipCtxs ps as bs ads bds = ctxUnzip (c, cctx)
                                 in UnzipZipCtxs ps (r:as) bs (L:ads) bds
ctxUnzip ((ZRRight l c), cctx) = let UnzipZipCtxs ps as bs ads bds = ctxUnzip (c, cctx)
                                 in UnzipZipCtxs (l:ps) as bs (R:ads) bds
ctxUnzip (ZRTop, (ZCLeft c r)) = let UnzipZipCtxs ps as bs ads bds = ctxUnzip (ZRTop, c)
                                 in UnzipZipCtxs ps as (r:bs) ads (L:bds)
ctxUnzip (ZRTop, (ZCRight c))  = let UnzipZipCtxs ps as bs ads bds = ctxUnzip (ZRTop, c)
                                 in UnzipZipCtxs ps as bs ads (R:bds)

curUnzip :: ZipCur a b c -> UnzipZipCur a b c
curUnzip (ZipCur pseq (useqA, useqB) ctx) = UnzipZipCtxs (Leaf pseq:ps) (Leaf useqA:as) (Leaf useqB:bs) ads bds
  where
    (UnzipZipCtxs ps as bs ads bds) = ctxUnzip ctx

ctxZip :: UnzipZipCtxs a b c -> ZipCtxs a b c
ctxZip c = (ctxZipFst c, ctxZipSnd c)

ctxZipFst :: UnzipZipCtxs a b c -> ResultCtx a c
ctxZipFst (UnzipZipCtxs [] [] _ [] _) = ZRTop
ctxZipFst (UnzipZipCtxs ps (a:as) _ (L:ads) _) = ZRLeft (ctxZipFst (UnzipZipCtxs ps as [] ads [])) a
ctxZipFst (UnzipZipCtxs (p:ps) as _ (R:ads) _) = ZRRight p (ctxZipFst (UnzipZipCtxs ps as [] ads []))

ctxZipSnd :: UnzipZipCtxs a b c -> ConsumedCtx b
ctxZipSnd (UnzipZipCtxs _ _ [] _ []) = ZCTop
ctxZipSnd (UnzipZipCtxs _ _ (b:bs) _ (L:bds)) = ZCLeft (ctxZipSnd (UnzipZipCtxs [] [] bs [] bds)) b
ctxZipSnd (UnzipZipCtxs _ _ bs _ (R:bds)) = ZCRight (ctxZipSnd (UnzipZipCtxs [] [] bs [] bds))

curZip :: UnzipZipCur a b c -> ZipCur a b c
curZip (UnzipZipCtxs (Leaf pseq:ps) (Leaf useqA:as) (Leaf useqB:bs) ads bds) = ZipCur pseq (useqA, useqB) (ctxZip (UnzipZipCtxs ps as bs ads bds))

resctxZip :: UnzipResCtx a b -> ResultCtx a b
resctxZip (UnzipResCtx [] [] []) = ZRTop
resctxZip (UnzipResCtx ls (r:rs) (L:ds)) = ZRLeft (resctxZip (UnzipResCtx ls rs ds)) r
resctxZip (UnzipResCtx (l:ls) rs (R:ds)) = ZRRight l (resctxZip (UnzipResCtx ls rs ds))

rescurZip :: UnzipResCur a c -> ResCur a c
rescurZip (UnzipResCtx (Leaf pseq:ps) (Leaf useqA:as) ads) = ResCur pseq useqA (resctxZip (UnzipResCtx ps as ads))

ctxLength :: ZipCtxs a b c -> (Int, Int)
ctxLength (ZRTop, _) = (0, 0)
ctxLength ((ZRLeft  ctx' rrp), cc)  = ctxLength (ctx', cc) +++ (0, length rrp)
ctxLength ((ZRRight lrp  ctx'), cc) = ctxLength (ctx', cc) +++ (length lrp, 0)


--
-- Exported
--
--next :: Rope b -> MapCtx a b -> Progress (Seq a, MapCtx a b) (Rope b)
--next rp MCTop              = Finished rp
--next rp (MCLeft ctx' rrp)  = let (xs, ctx'') = leftmost rrp (MCRight rp ctx')
--                             in
--                               More (xs, ctx'')
--next rp (MCRight lrp ctx') = next (lrp `ncat` rp) ctx'

--leftmost :: Rope a -> MapCtx a b -> (Seq a, MapCtx a b)
--leftmost (Leaf xs)         ctx = (xs, ctx)
--leftmost (Cat _ _ lrp rrp) ctx = leftmost lrp (MCLeft ctx rrp)

nextResult :: Rope c -> ZipCtxs a b c -> Progress (Seq a, ZipCtxs a b c) (Rope c)
nextResult rp (ZRTop, _) = Finished rp
nextResult rp ((ZRLeft ctx' rrp), cctx) = let (xs, ctx'') = leftmostResult rrp ((ZRRight rp ctx'), cctx)
                                              in More (xs, ctx'')
nextResult rp ((ZRRight lrp ctx'), cctx) = nextResult (lrp `ncat` rp) (ctx', cctx)

nextConsumed :: ZipCtxs a b c -> (Seq b, ZipCtxs a b c)
--nextConsumed (ZCTop, _) = Finished c_rp
nextConsumed (rctx, (ZCLeft ctx' rrp)) = let (xs, ctx'') = leftmostConsumed rrp (rctx, (ZCRight ctx'))
                                         in (xs, ctx'')
nextConsumed (rctx, (ZCRight ctx')) = nextConsumed (rctx, ctx')

leftmost2 :: Rope a -> Rope b -> ZipCtxs a b c -> (Seq a, Seq b, ZipCtxs a b c)
leftmost2 (Leaf xs) (Leaf ys) ctx = (xs, ys, ctx)
leftmost2 (Leaf xs) (Cat _ _ lrp rrp) (rctx, cctx) = leftmost2 (Leaf xs) lrp (rctx, (ZCLeft cctx rrp))
leftmost2 (Cat _ _ lrp rrp) x (rctx, cctx) = leftmost2 lrp x ((ZRLeft rctx rrp), cctx)

leftmostResult :: Rope a -> ZipCtxs a b c -> (Seq a, ZipCtxs a b c)
leftmostResult (Leaf xs) ctx = (xs, ctx)
leftmostResult (Cat _ _ lrp rrp) (rctx, cctx) = leftmostResult lrp ((ZRLeft rctx rrp), cctx)

leftmostConsumed :: Rope b -> ZipCtxs a b c -> (Seq b, ZipCtxs a b c)
leftmostConsumed (Leaf xs) ctx = (xs, ctx)
leftmostConsumed (Cat _ _ lrp rrp) (rctx, cctx) = leftmostConsumed lrp (rctx, (ZCLeft cctx rrp))

curLength :: ZipCur a b c -> (Int, Int)
curLength (ZipCur pseq (useqA, useqB) ctx) =
    ctxLength ctx +++ (Seq.length pseq, Seq.length useqA)


--
-- Body
--

zipLTSUntil :: forall a b c . Par Bool
            -> (a -> b -> c)
            -> Rope a
            -> Rope b
            -> Int --Int offset.. We are processing starting from this 'index' in the rope.
            -> Par (Progress (ZipCur a b c) (Rope c))
zipLTSUntil cond f rpA rpB n =
    m cond xs ys ctx
  where
    m :: Par Bool -> Seq a -> Seq b -> ZipCtxs a b c -> Par (Progress (ZipCur a b c) (Rope c))
    m cond' xs ys ctx =
        case Seq.zipWithUntil cond' f xs ys of
          ZipFinished (pseq', useqB')            -> case nextResult (Leaf pseq') ctx of
                                                   Finished rp'     -> return $ Finished rp' --Assert useqB' is empty
                                                   More (xs', ctx') -> m cond' xs' useqB' ctx'
          Awaiting (pseq', useqA', i)         -> let (seqb, ctx') = nextConsumed ctx in r cond' useqA' seqb pseq' i ctx
          Hungry (pseq', (useqA', useqB'), n) -> if snd (curLength (ZipCur pseq' (useqA', useqB') ctx)) >= 2
                                                 then return $ More (ZipCur pseq' (useqA', useqB') ctx)
                                                 else 
                                                  r isFalse useqA' useqB' pseq' n ctx

    r :: Par Bool -> Seq a -> Seq b -> Seq c -> Int -> ZipCtxs a b c -> Par (Progress (ZipCur a b c) (Rope c))
    r cond' as bs cs n ctx = 
        case Seq.resumeZipWithUntil cond' f as bs cs n of
          ZipFinished (pseq', useqB')            -> case nextResult (Leaf pseq') ctx of
                                                   Finished rp'     -> return $ Finished rp' --Assert useqB' is empty
                                                   More (xs', ctx') -> m cond' xs' useqB' ctx'
          Awaiting (pseq', useqA', i)         -> let (seqb, ctx') = nextConsumed ctx in r cond' useqA' seqb pseq' i ctx
          Hungry (pseq', (useqA', useqB'), n) -> if snd (curLength (ZipCur pseq' (useqA', useqB') ctx)) >= 2
                                                 then return $ More (ZipCur pseq' (useqA', useqB') ctx)
                                                 else 
                                                  r isFalse useqA' useqB' pseq' n ctx

    xs :: Seq a
    ys :: Seq b
    ctx :: ZipCtxs a b c
    (xs, ys, ctx) = leftmost2 rpA rpB (ZRTop, ZCTop)

zipWithLTS :: forall a b c . (a -> b -> c) -> Rope a -> Rope b -> Par (Rope c)
{-# INLINE zipWithLTS #-}
zipWithLTS f rpA rpB =
    go rpA rpB
  where
    go :: Rope a -> Rope b -> Par (Rope c)
    go (Cat l d v w) (Cat ll dd vv ww) | (length v == length vv) && (length w == length ww) = do 
                    iv' <- spawn $ go v vv
                    iw' <- spawn $ go w ww
                    v'  <- get iv'
                    w'  <- get iw'
                    return $ Cat l d v' w'

    go (Cat l d v w) (Cat ll dd vv ww) | l >= ll = do
                    let (lv, rv) =  splitAt v ll
                    iv' <- spawn $ go lv vv
                    iw' <- spawn $ go (rv `ncat` w) ww
                    v'  <- get iv'
                    w'  <- get iw'
                    return $ Cat l d v' w'

    go (Cat l d v w) (Cat ll dd vv ww) | l < ll = do
                    let (lvv, rvv) = splitAt vv l
                    iv' <- spawn $ go v lvv
                    iw' <- spawn $ go w (rvv `ncat` ww)
                    v'  <- get iv'
                    w'  <- get iw'
                    return $ Cat l d v' w'

    go rpA rpB = do prog <- zipLTSUntil isHungry' f rpA rpB 0
                    case prog of
                      Finished rp' -> return rp'
                      More cur@(ZipCur p (uA, uB) ctx) -> do
                                   let (lrp1, rrp1, lrp2, rrp2, reb) = splitCur cur
                                   prp1' <- spawn (zipWithLTS f lrp1 lrp2)
                                   prp2' <- spawn (zipWithLTS f rrp1 rrp2)
                                   rp1' <- get prp1'
                                   rp2' <- get prp2'
                                   return $ root (join rp1' rp2' reb)

-- zipWithLTS' :: forall a b c . (a -> b -> c) -> Int -> Rope a -> Rope b -> Par (Rope c)
-- {-# INLINE zipWithLTS' #-}
-- zipWithLTS' f n rpA rpB =
--     go n rpA rpB
--   where
--     go :: Int -> Rope a -> Rope b -> Par (Rope c)
--     go n rp@(Leaf u) rp'
--         | len < mAX_LEAF_SIZE = do  prog <-zipLTSUntil isHungry f rp rp' n
--                                     case prog of
--                                       Finished rp'' -> return $ traceEvent ("finished: "  ++ show (length rp'')) $ rp''
--                                       More cur'@(ZipCur _ _ _) -> do 
--                                                        let (rp1, rp2, reb) = splitCur cur'
--                                                        prp1' <- traceEvent ("spawn left: "  ++ show (length rp1)) $
--                                                                 spawn (zipWithLTS' f n' rp1 rp')
--                                                        prp2' <- traceEvent ("spawn right: " ++ show (length rp2)) $
--                                                                 spawn (zipWithLTS' f (n' + (length rp1) rp2 rp'))
--                                                        rp1'  <- get prp1'
--                                                        rp2'  <- get prp2'
--                                                        return $ root (join rp1' rp2' reb)

--         | otherwise           = do  let (v, w) = traceEvent "split" $ Seq.splitAt (len `div` 2) u
--                                     iv'  <- spawn $ go (Leaf v)
--                                     iw'  <- spawn $ go (Leaf w)
--                                     v'   <- get iv'
--                                     w'   <- get iw'
--                                     return $ traceEvent "ncat" $ v' `cat` w'
--       where
--         len :: Int
--         len = Seq.length u

--     go n (Cat l d v w) rp2 = do 
--                            iv' <- spawn $ go n v rp2
--                            iw' <- spawn $ go (n+(length v)) w rp2
--                            v' <- get iv'
--                            w' <- get iw'
--                            return $ traceEvent "Cat" $ Cat l d v' w'

isFalse :: Par Bool
isFalse = do --x <- put False
             --y <- get x
             return False
isHungry' :: Par Bool
isHungry' = do return runParHungry