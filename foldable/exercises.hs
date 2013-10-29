-- Use monoid to fold data structures
import qualified Data.Foldable as F
import Data.Monoid

-- Define binary tree
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- if a are monoids then we fold the strcutue
instance F.Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Node x l r) = f x `mappend`
                             F.foldMap f l `mappend`
                             F.foldMap f r 


testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )

-- We can fold now  
r1 = F.foldr(\x acc -> x + acc) 0 testTree
-- reduce to a monoid value with foldMap
r2 = getAny $ F.foldMap(\x -> Any $ x == 3 ) testTree
-- nt effficiet : usage of ++
r3 = F.foldMap(\x -> [x]) testTree
-- consider foldr : usage of :
r4 = F.foldr((:)) [] testTree


