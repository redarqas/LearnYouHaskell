-- Monad gives an answer to how flatMap a data structure 
import Control.Monad

class JMonad m where 
    jreturn :: a -> m a
    -- (>>=)
    flatMap :: m a -> (a -> m b) -> m b
    -- (>>)
    noextract :: m a -> m b -> m b
    noextract x y = x `flatMap` (\_ -> y)
    jfail :: String -> m a
    jfail msg = error msg

instance JMonad Maybe where
    jreturn = Just
    flatMap Nothing _ = Nothing
    flatMap (Just x) f = f x
    jfail _ = Nothing

-- usage 
r1 = Just 3 `flatMap` (\x -> Just $ show x)
dor1 = do 
    x <- Just 3
    return $ show x

-- Le funanbule example
-- We conserve the contaxt of failing
-- We do not have to check if a value is Nothing or Just
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (l,r) 
       | abs (nl - r) > 3 = Nothing
       | otherwise = Just (nl,r) 
       where nl = l + b 
landRight :: Birds -> Pole -> Maybe Pole
landRight b (l,r) 
       | abs (nr - l) > 3 = Nothing
       | otherwise = Just (l,nr) 
       where nr = r + b

funanbule1 = return (0,0) >>= landLeft 2 >>= landRight 4 >>= landRight 5

-- noxtract (>>) : ignore previous argument and continue

funanbule2 = return (0,0) >>= landLeft 2 >> Just (0,0) >>= landLeft 3
-- do notation : in this example (>>=) syntaxe is more consistent
funanbule3 = do 
    f <- return (0,0)
    s <- landLeft 2 f
    t <- landRight 2 s
    return t

-- lists

instance JMonad [] where
    jreturn x = [x]
    flatMap xs f = concatMap f xs
    jfail _ = []

l1 = [1..5] >>= (\x -> [x, -x])
dol1 = do
    x <- [1..5]
    [x, -x]

-- MonadPlus for lists add guards fo do notation
sevensOnly = do 
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
-- list comprhension is a syntaxic sugar for list Monad
sevensOnly2 = [x | x <- [1..50], '7' `elem` show x]











