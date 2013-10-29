
-- Define a functor type classe

class JFunctor f where
  jfmap :: (a -> b) -> f a -> f b

-- define Maybe success / fail context (* -> *)  

data JMaybe a = JNothing | JJust a

instance JFunctor JMaybe where
    jfmap _ JNothing = JNothing

-- usage 

mapResult = fmap (+3) (Just 3)

instance JFunctor IO where
    jfmap f action = do
        x <- action
        return $ f x

-- If you ever find yourself binding the result of an I/O action to a name, 
-- only to apply a function to that and call that something else, consider using fmap 

ioprog = do 
    line <- fmap reverse getLine
    putStrLn line

-- Either : Success or error with explanation context
data JEither a b = JLeft a | JRight b

instance JFunctor (JEither a) where
    jfmap _ (JLeft x) = JLeft x
    jfmap f (JRight x) = JRight (f x)

instance JFunctor ((->) r) where
    jfmap =  (.)

-- Functor can be seen as lift of function taking one parameter
-- ie, with functors we can apply a fuction with one parameter 
-- to a parameter contained on a context
plus3 :: (Functor f, Num b) => f b -> f b
plus3 = fmap (+3)

r1 = plus3 (Just 3)
r2 = plus3 [1,2,3] 









