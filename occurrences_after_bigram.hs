occurences_after_bigram :: (Eq a) => [a] -> a -> a -> [a]
occurences_after_bigram [] a b = []
occurences_after_bigram (x:[]) a b = []
occurences_after_bigram (x1:(x2:[])) a b = []
occurences_after_bigram (x1:(x2:(x3:xs))) a b |x1 == a && x2 == b = x3:(occurences_after_bigram (x2:(x3:xs)) a b) 
                                              |otherwise = occurences_after_bigram (x2:(x3:xs)) a b 
-- brauchen wir warscheinlich nicht: |x1 == a && x2 == b && xs == [] = [x3]

satz2wortlisteRaw ::  [Char] -> [Char] -> [[Char]]
satz2wortlisteRaw [] akku = [akku]
satz2wortlisteRaw (x:xs) akku | x /= ' ' = satz2wortlisteRaw xs (akku ++ [x])
                              |otherwise = akku : (satz2wortlisteRaw xs [])

-- Mr Rekursion
--      function []   = basisfall
--      function x:xs = Teillösung1 +++ Teillösung2 +++  Teillösung3 +++ .......... +++ Basisfall           

satz2wortliste :: [Char] -> [[Char]]
satz2wortliste xs = satz2wortlisteRaw xs []

occurences_after_bigram_in_sentences :: [Char] -> [Char] -> [Char] -> [[Char]]
occurences_after_bigram_in_sentences  sentence word1 word2 = occurences_after_bigram (satz2wortliste sentence) word1 word2

-- primitive rekursion oder einfache rekursion 
-- rekursionsfunktion x 0       = (lsg x)
-- rekursionsfunktion x n       =  (lsg x) ++ rekursionsfunktion x' n-1 
--          lösing setzt sich zusammen:   (lsg x) ++ (lsg x') ++ ..... ++ (lsg x0)           

 
--satz2wortliste2 :: [Char] -> [[Char]]
