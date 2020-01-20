data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show

t1 = (NodeB 1 (NodeB 2 EmptyB (NodeB 3 EmptyB EmptyB)) EmptyB)
t2 = NodeB 1 (NodeB 2 (NodeB 5 EmptyB EmptyB) (NodeB 3 (NodeB 8 EmptyB EmptyB) (NodeB 7 EmptyB EmptyB))) (NodeB 4 EmptyB EmptyB)

mirror :: TreeB a -> TreeB a
mirror EmptyB = EmptyB
mirror (NodeB a lt rt) = NodeB a (mirror rt) (mirror lt)

isSymmetricB tree = tree == mirror tree

instance Eq a => Eq (TreeB a) where
	EmptyB == EmptyB = True
	(NodeB x lt rt) == (NodeB x' lt' rt') = x == x' && lt == lt' && rt == rt'
	_ == _ = False
	
leafsB EmptyB = []
leafsB (NodeB x EmptyB EmptyB) = [x]
leafsB (NodeB x rt lt) = leafsB rt ++ leafsB lt

internalsB EmptyB = []
internalsB (NodeB x EmptyB EmptyB) = []
internalsB (NodeB x lt rt) = x : internalsB lt ++ internalsB rt


pretty :: (Show a) => TreeB a -> IO ()
pretty t  = putStrLn (unlines xss)
 where
   (xss,_,_) = pprint' t

pprint' EmptyB                   =  ([], 0, 0)
pprint' (NodeB x EmptyB EmptyB)  =  ([s], ls, ls-1)
  where
    s = show x
    ls = length s
pprint' (NodeB x l r)          =  (resultLines, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show x
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,lc) = pprint' l
    (rp,rw,rc) = pprint' r
    -- recurse
    (lw',lb) = if lw==0 then (1," ") else (lw,"/")
    (rw',rb) = if rw==0 then (1," ") else (rw,"\\")
    -- compute full width of this tree
    totLW = maximum [lw', swl,  1]
    totRW = maximum [rw', swr, 1]
    w = totLW + 1 + totRW
{-
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totLW
                -----------    totRW
     ----   -   -----------    w (total width)
-}
    -- get right column info that accounts for left side
    rc2 = totLW + 1 + rc
    -- make left and right tree same height
    llp = length lp
    lrp = length rp
    lp' = if llp < lrp then lp ++ replicate (lrp - llp) "" else lp
    rp' = if lrp < llp then rp ++ replicate (llp - lrp) "" else rp
    -- widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
    lp'' = map (\s -> if length s < totLW then nSpaces (totLW - length s) ++ s else s) lp'
    rp'' = map (\s -> if length s < totRW then s ++ nSpaces (totRW - length s) else s) rp'
    -- first part of line1
    line1 = if swl < lw' - lc - 1 then
                nSpaces (lc + 1) ++ nBars (lw' - lc - swl) ++ s
            else
                nSpaces (totLW - swl) ++ s
    -- line1 right bars
    lline1 = length line1
    line1' = if rc2 > lline1 then
                line1 ++ nBars (rc2 - lline1)
             else
                line1
    -- line1 right padding
    line1'' = line1' ++ nSpaces (w - length line1')
    -- first part of line2
    line2 = nSpaces (totLW - lw' + lc) ++ lb
    -- pad rest of left half
    line2' = line2 ++ nSpaces (totLW - length line2)
    -- add right content
    line2'' = line2' ++ " " ++ nSpaces rc ++ rb
    -- add right padding
    line2''' = line2'' ++ nSpaces (w - length line2'')
    resultLines = line1'' : line2''' : zipWith (\l r -> l ++ " " ++ r) lp'' rp''