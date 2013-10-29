-- Monoid : neutral element, append reducer

class JMonoid m where
    jmempty :: m
    jmappend :: m -> m -> m
    jmconcat :: [m] -> m
    jmconcat = foldr jmappend jmempty


-- some instances
instance JMonoid [a] where
    jmempty  = []
    jmappend = (++)

-- usage
r1 = [1,2] `jmappend` [1,2]

-- Oredring monoid
instance JMonoid Ordering where
    jmempty = EQ
    EQ `jmappend` r = r
    GT `jmappend` _ = GT
    LT `jmappend` _ = LT

lengthCompare xs ys = (length xs `compare` length ys) `jmappend`
                      (xs `compare` ys)

-- Sum

newtype JSum a = JSum { unJSum :: a } deriving (Eq, Show)

instance (Num a) => JMonoid (JSum a) where
    jmempty = JSum 0
    JSum x `jmappend` JSum y = JSum (x + y)  






