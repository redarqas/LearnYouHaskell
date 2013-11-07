import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO
import Control.Exception.Base
-- Define Iteratee : A stream processor, takes a number of elements
-- from an input stream, perform calculation and return the value

-- An element from a stream [StreamG el]
-- An element can be Empty, containing a value, or EOf for ending the stream
data StreamG el = Empty | El el | EOF 

-- Iteratee consumes element of type el and gives a result of type a
-- It has two states : Done and Continue
data IterV el a = Done a (StreamG el) | Cont (StreamG el -> IterV el a)

-- Let's define a function which feeds data to our Iteratee
enum :: IterV el a -> [el] -> IterV el a
enum i [] = i
enum i@(Done _ _) _ = i
enum (Cont k) (x:xs) = enum (k $ El x) xs

-- Categories of iteratees : 1 - finite number of inputs / 2- ends on EOF element / 3- Never ends
-- run fucntion checks if the iteratee is finished by sending EOF
-- Sent EOF comes after enum elemnts      

run :: IterV el a -> Maybe a
run (Done x _) = Just x
run (Cont k) = run' (k EOF)
       where run' (Done x _) = Just x
             run' _ = Nothing

-- Some examples 

--genericHead :: [a] -> a, the first input makes Iteratee in Done state
headIter :: IterV el (Maybe el)
headIter = Cont step
  where step (El el) = Done (Just el) Empty
        step Empty = Cont step
        step EOF = Done Nothing EOF
--usage
headr = run $ enum headIter [1..10]

-- drop :: Int -> [a] -> [a], do nothing with element just forward to next until 0 index
dropIter :: Int -> IterV el ()
dropIter 0 = Done () Empty
dropIter n = Cont step
  where
    step (El _) = dropIter (n-1)
    step Empty = Cont step
    step EOF = Done () EOF

--usage 
dropr = run $ enum (dropIter 1) [1..5]

-- length :: [a] -> Int :: Accumate number of element util EOF
lengthIter :: IterV el Int
lengthIter = Cont (step 0) 
    where step acc (El _) =  Cont (step (acc+1)) 
          step acc Empty = Cont (step acc)
          step acc EOF = Done acc EOF
-- usage

lengthr = run $ enum lengthIter [1..10]

-- Make Iteratee as instance of Functor / Applicative / Monad

instance  Functor (IterV el) where
    fmap f (Done x l) = Done (f x) l
    fmap f (Cont step) = Cont (fmap f . step)

instance Applicative (IterV el) where
    pure x = Done x Empty
    (Done f s) <*> i = fmap f i
    (Cont k) <*> i = Cont (\str -> k str <*> i)

instance Monad (IterV el) where
    return x = Done x Empty
    m >>= f = case m of 
        Done x str -> case f x of 
            Done x' _ -> Done x' str
            Cont k -> k str
        Cont k -> Cont (\str -> k str >>= f)


-- Define an Enumerator for IO Monad
-- Data for putting a  Iteratee under an IO
type EnumeratorM el m a = IterV el a -> m (IterV el a)

enumHandle :: Handle -> EnumeratorM Char IO a
enumHandle h iter = loop iter
     where
       loop i@(Done _ _) = return i
       loop i@(Cont k) = do
         isEOF <- hIsEOF h
         if isEOF then return i else hGetChar h >>= loop . k . El

enumFile :: FilePath -> EnumeratorM Char IO a
enumFile fp i = bracket
                (openFile fp ReadMode)
                (hClose)
                (flip enumHandle i)


lengthOfTwoFiles :: FilePath -> FilePath -> IO (Maybe Int)
lengthOfTwoFiles fp1 fp2 =
     fmap run $ ((enumFile fp1) >=> (enumFile fp2)) lengthIter

lengthOneFile fp = enumFile fp $ lengthIter 



