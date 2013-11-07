
import System.FilePath
import Control.Monad 
import System.Directory 
import System.Time
import System.IO
import Control.Exception
import Control.Applicative
import Data.List


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    properNames <- filter (`notElem` [".", ".."] ) <$> getDirectoryContents topdir
    paths <- forM properNames $  \name -> do
      let path = topdir </> name
      isDir <- doesDirectoryExist path
      if isDir 
        then getRecursiveContents path
        else return [path]
    return $ concat paths



simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p dir = do
   files <- filter p <$> getRecursiveContents dir
   return files

sfr = simpleFind (\path -> takeExtension path == ".txt") "."



type Predicate = FilePath -- path to directory entry 
               -> Permissions -- permissions
               -> Maybe Integer -- file size (Nothing if not file) 
               -> ClockTime -- last modified
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize f =  bracket (openFile f ReadMode) hClose $ \h -> Just <$> hFileSize h
                   

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p dir = getRecursiveContents dir >>= (\paths -> filterM check paths)
    where check path = do 
              perms <- getPermissions path
              size <- getFileSize path
              time <- getModificationTime path
              return $ p path perms size time


myTest :: Predicate 
myTest path _ (Just size) _ = takeExtension path == ".txt" && size > 100
myTest _ _ _ _ = False

rs = betterFind myTest "." 

-- DSL

type InfoP a = FilePath  
              -> Permissions 
              -> Maybe Integer 
              -> ClockTime 
              -> a


pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size 
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

btf = betterFind (sizeP `equalP` 133) 

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP binary info x fpath perms size time = info fpath perms size time `binary` x

eqP, ltP, gtP :: (Ord a) => InfoP a -> a -> InfoP Bool

eqP = liftP (==) 
ltP = liftP (<)
gtP = liftP (>)

btfeqP = betterFind (sizeP `eqP` 133) "."

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 bin f g a b c d = f a b c d `bin` g a b c d

andP = liftP2(&&)
orP = liftP2(||)

btf3 = betterFind ((sizeP `eqP` 133) `andP` (pathP `eqP` "./test.txt")) "."

constP :: a -> InfoP a
constP x _ _ _ _ = x

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' f i x = liftP2 f i (constP x)

-- new test
liftPath :: (FilePath -> a) -> InfoP a
liftPath f pt _ _ _ = f pt

myTest2 = (liftPath takeExtension `eqP` ".txt") `andP`
          (sizeP `gtP` 100)

btf4 = betterFind(myTest2) "."

(==?) = equalP
(&&?) = andP
(>?) = gtP
infix 4 ==?
infixr 3 &&?
infix 4 >?


myTest3 = liftPath takeExtension ==? ".txt" &&? sizeP >? 100

btfFinal = betterFind(myTest3) "."


-- File system traversal

data Info = Info {
  infoPath :: FilePath, 
  infoPerms :: Maybe Permissions , 
  infoSize :: Maybe Integer, 
  infoModTime :: Maybe ClockTime 
} deriving (Eq, Ord, Show)


maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle errorHandler (Just `liftM` act)
  where errorHandler :: SomeException -> IO (Maybe a)
        errorHandler _ = return Nothing

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)


getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents path = filter (`notElem` [".", ".."]) <$> getDirectoryContents path

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms 


traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <-  getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

alphaOrderer :: [Info] -> [Info]
alphaOrderer xs = sortBy (\f s -> infoPath f `compare` infoPath s) xs 

-- Iteratee 

data Iterate seed = Done {unwrap :: seed}
                  | Skip {unwrap :: seed}
                  | Continue {unwrap :: seed}

-- fold
type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= (\paths -> walk seed paths)
    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed' -> walk seed' names
        Continue seed' 
          | isDirectory info -> do
              next <- fold seed' path'
              case next  
















