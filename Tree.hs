import System.Directory
import System.FilePath
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import System.IO.Error
import Data.List
import Control.Monad

data Tree a = Leaf a | Node a [Tree a]


main :: IO ()
main = do
  (args, files) <- getArgs >>= parse
  
  -- Scan directory
  let maxLevel = case determineLevel args of
                  Just value -> read value
                  Nothing    -> -1 -- -1 means infinity
  trees <- mapM (scanDirectory maxLevel args) files
  
  -- Print out directory tree for every passed filepath
  (mapM_ putStrLn . foldr1 (++) . map printTree) trees
  
  -- Print out stats
  let stats = (printTreeStats . map treeStats) trees
  unless (Noreport `elem` args) (putStrLn stats)
  

-- | Scans and translates directory into Tree structure
scanDirectory :: Integer -> [Flag] -> FilePath -> IO (Tree String)
scanDirectory maxLevel args file = scanDirectory' file file maxLevel args

scanDirectory' :: FilePath -> String -> Integer -> [Flag] -> IO (Tree String)
scanDirectory' path dirName maxLevel args = do
  
  -- Get directory contents
  result <- try (getDirectoryContents path) :: IO (Either IOError [FilePath])
  case result of
    
    -- Print any IO exceptions in brackets after the directory name (Return a blank dir tree)
    Left ex  -> return (Node (dirName++" ["++ ioeGetErrorString ex ++"]") [])
    
    -- If everything's okay, continue
    Right allPaths -> do
      
      -- Sorts files alphabetically
      let sortIfShould = if Nosort `elem` args then id else sort

      -- Filters out hidden files
      let filterOutHiddenFiles = if All `elem` args then id else filter (\file -> not ("." `isPrefixOf` file))

      -- Filter out retrieved files if flags are set to do so
      let files = filterOutHiddenFiles. sortIfShould . filter (`notElem` [".", ".."]) $ allPaths

      -- Annotate the files with metadata
      annotatedFiles <- forM files $ \file -> do
        isDirectory <- doesDirectoryExist (path </> file)
        return (file, isDirectory)

      -- Filters out directories from the list of annotated files (i.e. (file, isDirectory) tuples)
      let filterDirsIfShould = if Dirsonly `elem` args then filter snd else id

      -- Sorts files so that the directories are shown first (doesn't work with Nosort flag)
      let comparator = (\(_, aIsDirectory) (_,_) -> if aIsDirectory then LT else GT)
      let placeDirectoriesOnTop = if (Dirsfirst `elem` args) && (Nosort `notElem` args) then sortBy comparator else id

      -- Traverse directory tree
      trees <- forM ((placeDirectoriesOnTop . filterDirsIfShould) annotatedFiles) $ \(file, isDirectory) -> do 
          let isLastLevel = maxLevel /= 1
          let nextMaxLevel = if maxLevel==(-1) then -1 else pred maxLevel -- -1 means infinity
          if isDirectory && isLastLevel
            then scanDirectory' (path </> file) file nextMaxLevel args
            else if isDirectory then return (Node file []) else return (Leaf file)

      return (Node dirName trees)


-- | Pretty print for directory trees
printTree :: Tree String -> [String]
printTree a = printTree' a False "" True

printTree' :: Tree String -> Bool -> String -> Bool -> [String]
printTree' (Leaf a) isLast level isRoot = [level ++ prefix ++ a] where
  prefix = if isRoot then "" else turn++"── "
  turn = if isLast then "└" else "├"
printTree' (Node a trees) isLast level isRoot = directory ++ subtree where
  directory = printTree' (Leaf a) isLast level isRoot
  subtree = if null trees   then []
                            else foldr1 (++) (map printTrees markedTrees)
  printTrees = \(x, willBeLast) -> printTree' x willBeLast (level++addLevel) False
  addLevel
    | isRoot = ""
    | isLast = "    "
    | otherwise = "\9474   "
  markedTrees = zip (init trees) (repeat False) ++ [(last trees, True)]


-- | Retrieves directory tree stats in the form of (totalFiles, totalDirectories) tuple
treeStats :: Tree String -> (Integer, Integer)
treeStats (Leaf _) = (1, 0)
treeStats (Node _ trees) = foldl (\(f1,d1) (f2,d2) -> (f1+f2, d1+d2)) (0,1) (map treeStats trees)


-- | Prints out stats
-- >>> e.g. [(1,1),[0,3]]
-- "4 directories, 1 file"
printTreeStats :: [(Integer, Integer)] -> String
printTreeStats stats = "\n"++ show d ++" "++directories++", "++ show f ++" "++files where
  f = (sum . fst . unzip) stats
  d = (sum . map pred . snd . unzip) stats -- Hmm, we shouldn't count the root dir
  directories = if d==1 then "directory" else "directories"
  files = if f==1 then "file" else "files"


-- | Argument flags
data Flag
  = Noreport 
  | Level String 
  | Dirsonly 
  | All      
  | Version  
  | Help     
  | Nosort   
  | Dirsfirst
  deriving (Eq, Show)


flags :: [OptDescr Flag]
flags =
  [Option ['L'] []            (ReqArg Level "level")
      "Descend only level directories deep."
  ,Option ['d'] []            (NoArg Dirsonly) 
      "List directories only."
  ,Option ['a'] []            (NoArg All)    
      "All files are listed."
  ,Option ['U'] []            (NoArg Nosort)
      "Leave files unsorted."
  ,Option [] ["noreport"]     (NoArg Noreport)
      "Turn off file/directory count at end of tree listing."
  ,Option [] ["dirsfirst"]    (NoArg Dirsfirst)
      "List directories before files (-U disables)."
  ,Option [] ["version"]      (NoArg Version)
      "Print version and exit.."
  ,Option ['h', '?'] ["help"] (NoArg Help)
      "Prints this help message."
  ]


-- | Parses arguments
--
-- Sources: (since I've not found a better library)
-- http://www.haskell.org/haskellwiki/Tutorials/Programming_Haskell/Argument_handling#Parsing_the_flags
-- http://cvs.haskell.org/Hugs/pages/libraries/base/System-Console-GetOpt.html
parse :: [String] -> IO ([Flag], [String])
parse argv = case getOpt Permute flags argv of
  (args,fs,[]) -> do
    let files = if null fs then ["."] else fs
    let level = case determineLevel args of
                  Just value -> read value
                  Nothing    -> 1 -- anything that passes validation
      
    parse' args flags files header level

  (_,_,errs)      -> do
    hPutStrLn stderr (concat errs ++ usageInfo header flags)
    exitWith (ExitFailure 1)

  where header = "Usage: tree [-L level] [-adU] [--noreport] [--dirsfirst] [--version] [--help] [<directory list>]"

parse' :: [Flag] -> [OptDescr a] -> t -> String -> Integer -> IO ([Flag], t)
parse' args flags files header level
  | Help `elem` args =    do  hPutStrLn stderr (usageInfo header flags)
                              exitWith ExitSuccess
  | Version `elem` args = do  putStrLn version
                              exitWith ExitSuccess
  | level <= 0 =          do  putStrLn "Invalid level, must be greater than 0."
                              exitWith (ExitFailure 1)
  | otherwise =           return (nub (concatMap set args), files)

set :: t -> [t]
set f = [f]

determineLevel :: [Flag] -> Maybe String
determineLevel [] = Nothing
determineLevel (Level s:_) = Just s
determineLevel (_:rest) = determineLevel rest

-- | Version info 
version :: String
version = "GNU tree 1.0\n\
          \Copyright (C) 2007 Free Software Foundation, Inc.\n\
          \License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
          \This is free software: you are free to change and redistribute it.\n\
          \There is NO WARRANTY, to the extent permitted by law."

