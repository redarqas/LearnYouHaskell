import Data.Monoid

data Chunk a = Chunk [a] | EOF deriving (Show, Eq)

data Step e a m b = Continue (Chunk a -> Iteratee e a m b)
                  | Yield b (Chunk a)
                  | Error e

newtype Iteratee e a m b =  Iteratee {runIteratee :: m (Step e a m b)}

-- Chunk as Monoid 

instance Monoid (Chunk a) where
    mempty = EOF 
    Chunk xs `mappend` Chunk ys = Chunk (xs ++ ys)
    _ `mappend` _ = EOF

instance Functor (Chunk) where
    fmap _ EOF = EOF
    fmap f (Chunk xs) =Chunk (fmap f xs)

instance (Show a, Show b, Show e) => Show (Step e a m b) where
    showsPrec d step = showParen (d > 10) $ case step of
        (Continue _) -> s "Continue"
        (Yield b chunk) -> s "Yield " . sp b . s " " . sp chunk 
        (Error err) -> s "Error " . sp err
        where
            s = showString
            sp :: Show a => a -> ShowS
            sp = showsPrec 11

instance Monad m => Monad (Step e a m) where
    return x = Iteratee . return . Yield x $ Chunk [] 