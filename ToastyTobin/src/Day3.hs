module Day3 where

run :: IO ()
run = print . distance $ input

input :: Int
input = 265149

distance :: Int -> Int
distance 1 = 0
distance index = (ring-1) `quot` 2 + toCenter
  where
    ring = ringRoot . fromIntegral $ index
    toCenter = abs $ (ring*ring - index) `mod` (ring-1) - ((ring-1) `quot` 2)

ringRoot :: (Floating a, RealFrac a) => a -> Int
ringRoot = (+1) . (*2) . ceiling . (/2) . subtract 1 . sqrt
