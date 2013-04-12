-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module MiRanda.CeRNA where

mutameScore :: Int -> Int -> [[(Int,Int)]] -> Double
mutameScore len nmiR bss =
  if null bss'
  then 0
  else c1 * c2 * c3 * c4
  where
    bss' = map sort $ filter (not . null) bss
    nRNA = length bss'
    totalN = sum nForEach
    nForEach = map (fromIntegral . length) bss'
    c1 = (fromIntegral nRNA) / fromIntegral nmiR
    c2 = sum $ zipWith (/) nForEach $ map f1 bss'       -- unclear
    c3 = sum $ zipWith (/) (map ((^^2) . f1) bss') (map f2 bss')  -- unclear 
    c4 = (totalN - (fromIntegral nRNA) + 1) / totalN
    f1 ((a,b):[]) = let l = (fromIntegral (a + b)) / 2
                        leng = fromIntegral len
                    in max (leng-l) l
    f1 xs = let (a1,b1) = head xs
                (a2,b2) = last xs
            in (fromIntegral (a2+b2)) / 2 - 
               (fromIntegral (a1+b1)) / 2
    f2 ((a,b):[]) = let l = (fromIntegral (a + b)) / 2
                        leng = fromIntegral len
                    in l ^^ 2 + (leng - l) ^^ 2
    f2 (x:xs) = fst $ foldl' (\(acc,(a1,b1)) (a2,b2) ->
                                let l1 = fromIntegral (a1+b1) / 2
                                    l2 = fromIntegral (a2+b2) / 2
                                    acc' = acc + (l2 - l1) ^^ 2
                                in (acc',(a2,b2))
                             ) (0,x) xs
