module Utils.V2 (module LV2, module Linear.Metric, module Utils.V2) where

import Linear.V2 as LV2
import Linear.Metric

-- | 'unangle', but for integer vectors.
unangle' :: (Floating a, Ord a) => V2 Int -> a
unangle' = unangle . fmap fromIntegral

-- | 'perp', but clockwise instead of counter-clockwise.
perpcw :: Num a => V2 a -> V2 a
perpcw = negate . perp