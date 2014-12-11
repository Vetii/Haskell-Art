module Utils where

-- For the pattern 80% - 20%; etc.
separated :: (RealFrac a) => a -> [a]
separated x = [fx, 1.0 - fx]
    where fx = snd (properFraction x)  -- 2.3 -> 0.3

-- How to cut a list in two parts.
-- (Useful for separation of foreground / background
firstHalf :: [a] -> [a]
firstHalf list = take (floor (l / 2)) list
    where l = fromIntegral (length list)

lastHalf :: [a] -> [a]
lastHalf list = drop (floor (l / 2)) list
    where l = fromIntegral (length list)

