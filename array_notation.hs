beaf3 :: (Integral a) => a -> a -> a -> a
beaf3 a 0 c  = 1 --not really needed
beaf3 a b 0  = a*b -- not really needed
beaf3 a 1 c  = a
beaf3 a b 1  = a^b
--beaf3 a b c  = beaf3 a (beaf3 a (pred b) c) (pred c)
--below has about half as many recursive calls
beaf3 a b c  = ((foldr (.) id .) . replicate') b (flip (beaf3 a) (pred c)) 1

beaf4 :: (Integral a) => a -> a -> a -> a -> a
beaf4 a 0 c d = 1 --not really needed
beaf4 a b 0 d = a*b --not really needed
beaf4 a b c 0 = a*b --not really needed
beaf4 a 1 c d = a
beaf4 a b 1 1 = a^b
beaf4 a b 1 d = beaf4 a a (beaf4 a (pred b) 1 d) (pred d)
beaf4 a b c d = beaf4 a (beaf4 a (pred b) c d) (pred c) d

--Helper Funtions
replicate' :: (Integral a) => a -> b -> [b]
replicate' 0 val = []
replicate' n val = val : (replicate' (pred n) val)
