module Utils where

splitByComma :: String -> [String]
splitByComma = foldr (\c (s:ss) -> if c == ','
                                   then []:(s:ss)
                                   else (c:s):ss)
                     [[]]

(∘∘) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(∘∘) f g a b = f (g a b)
infixr 9 ∘∘

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c