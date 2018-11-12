import System.Environment
import System.IO.Error
import Control.Exception
import Data.List (nub)

data WrongNumberOfArgumentsException = WrongNumberOfArgumentsException

instance Show WrongNumberOfArgumentsException where
  show e = "Wrong number of arguments!"

instance Exception WrongNumberOfArgumentsException

riskyAction :: IO ()
riskyAction = do
        contents <- readContents
        putStrLn $ show $ noOfLinesInFile contents

readContents :: IO String
readContents = do
                args <- getArgs
                if null args
                  then throw WrongNumberOfArgumentsException
                  else do
                    let fileName = head args
                    contents <- readFile fileName
                    return contents

catchWrongnumberOfArgumentsException :: WrongNumberOfArgumentsException -> IO ()
catchWrongnumberOfArgumentsException = putStrLn . show

exHdlr :: IOError -> IO ()
exHdlr = \ex -> case ex of
                  isDoesNotExistError -> putStrLn "The file doesn't exist!"
                  _                 -> ioError ex

main :: IO ()
main = riskyAction `catch` exHdlr `catch` catchWrongnumberOfArgumentsException

noOfLinesInFile :: String -> Int
noOfLinesInFile = length . lines

noOfWordsInFile :: String -> Int
noOfWordsInFile = length . words

noOfCharsInFile :: String -> Int
noOfCharsInFile = length

noOfDistWordsInFile :: String -> Int
noOfDistWordsInFile = length . nub . words

noOfLinesLongerThan80InFile :: String -> Int
noOfLinesLongerThan80InFile = length . filter ((>80) . length) . lines

noOfOccurancesOfWord :: String -> String -> Int
noOfOccurancesOfWord w = length . filter (== w) . words
