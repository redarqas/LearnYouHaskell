-- Define Applicative type class

class JApplicative f where
    jpure :: a -> f a
    (<#>) :: f (a -> b) -> f a -> f b

-- famp  == <##>    
(<##>) g x = jpure g <#> x

instance JApplicative Maybe where
    jpure x = Just x
    Nothing <#> _ = Nothing
    Just f <#> something = fmap f something

-- Usage 
r1 = jpure (+3) <#> (Just 4) 
r2 = jpure (+) <#> Just 3 <#> Just 5
r3 = (+) <##> Just 4 <#> Just 5

-- With applicatives we can apply a fuction to parameters  
-- contained on contexts

addthree :: (Num a) => a -> a -> a -> a
addthree x y z = x + y + z

r4 = addthree <##> Just 3 <#> Just 3 <#> Just 3

instance JApplicative [] where
    jpure x  = [x]
    fx <#> xs = [f x | f <- fx, x <- xs]

r5 = [(+1), (*2), (/2)] <#> [1,2,3]
r6 = [(+), (*), (/)] <#> [1,2,3] <#> [1,2,3]

-- We can remplace simple list comprehension 
-- with applicative
r7 = (+) <##> [1..5] <#> [5..10]

instance JApplicative IO where
    jpure = return
    fx <#> xs = do
          f <- fx
          x <- xs
          return $ f x 

-- That can be better to apply a function directly to applcative values 
-- than exatracting them and then apply the function

ioprog = do
    twoLines <- (++) <##> getLine <#> getLine
    putStrLn twoLines

instance JApplicative ((->) r) where
    jpure x = (\_ -> x)
    f <#> g = (\x -> f x (g x) ) 

fun1 = (+) <##> (/3) <#> (+4) 
r8 = fun1 3


newtype WrapList a = WrapList { unWrapList :: [a] } 

instance JApplicative (WrapList) where
    jpure x = WrapList (repeat x)
    WrapList fx <#> WrapList xs = WrapList $ zipWith(\f x -> f x) fx xs

-- We can use ZipList to extend to possiblities of zipwith
r9 = unWrapList $ (,,) <##> WrapList [1..3] <#> WrapList [1..3] <#> WrapList [1..3]  

-- With applicative we can lift any normal fuction into another one wich can 
-- be applied on applicative values

jliftA2 :: (JApplicative f) => (a -> b -> c) -> f a -> f b -> f c
jliftA2 f fa fb = f <##> fa <#> fb

-- A particular usage with cons function
r10 = jliftA2 (:) (Just 3) (Just [4,4])
-- A generalisation of this particular case
jsequence :: (JApplicative f) => [f a] -> f [a]
jsequence [] = jpure []
jsequence (x:xs) = (:) <##> x <#> jsequence xs

-- An useful example

r11 = and $ jsequence [(<10), (>0), odd] $ 7














