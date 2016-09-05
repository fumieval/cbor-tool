{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Term
import Data.List
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Lens hiding (List, Context(..))
import Control.Monad
import System.IO
import System.Console.ANSI
import qualified Data.Text as T
import System.Directory
import Debug.Trace
import Data.Char

data Doc = DStr String | DBrackets [Doc] | DBraces [Doc] | DDocs [Doc]

diagKeyValue :: Term -> Term -> Doc
diagKeyValue k v = DDocs [DStr $ setSGRCode [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
    , diag k
    , DStr $ setSGRCode [Reset]
    , DStr ": "
    , diag v]

diag :: Term -> Doc
diag (TInt i) = DStr $ show i
diag (TInteger i) = DStr $ show i
diag (TBytes s) = DStr $ show s
diag (TBytesI s) = DStr $ show s
diag (TString s) = DStr $ show s
diag (TStringI s) = DStr $ show s
diag (TList xs) = DBrackets (map diag xs)
diag (TListI xs) = DBrackets (map diag xs)
diag (TMap xs) = DBraces $ map (uncurry diagKeyValue) xs
diag (TMapI xs) = DBraces $ map (uncurry diagKeyValue) xs
diag (TTagged i x) = DDocs [DStr (show i), DStr "(", diag x, DStr ")"]
diag (TBool x) = DStr $ show x
diag TNull = DStr "()"
diag TUndef = DStr "undefined"
diag (TSimple x) = DStr $ show x
diag (THalf x) = DStr $ show x
diag (TFloat x) = DStr $ show x
diag (TDouble x) = DStr $ show x

showDoc :: Int -> Doc -> String
showDoc _ (DStr s) = s
showDoc n (DDocs ds) = concatMap (showDoc n) ds
showDoc _ (DBrackets []) = "[]"
showDoc n (DBrackets xs)
  | length (concat ss) < 80 = "[" ++ intercalate ", " ss ++ "]"
  | otherwise = intercalate "\n" $ _head %~ ("[ "++) $ (ss & _tail . traverse %~ ((replicate n ' ' ++ ", ")++)) ++ [replicate n ' ' ++ "]"]
  where
    ss = map (showDoc (n + 2)) xs
showDoc _ (DBraces []) = "{}"
showDoc n (DBraces xs)
  | length (concat ss) < 80 = "{" ++ intercalate ", " ss ++ "}"
  | otherwise = intercalate "\n" $ _head %~ ("{ "++) $ (ss & _tail . traverse %~ ((replicate n ' ' ++ ", ")++)) ++ [replicate n ' ' ++ "}"]
  where
    ss = map (showDoc (n + 2)) xs

showPattern :: Term -> String
showPattern (TString s) = T.unpack s
showPattern t = "'" ++ showDoc 0 (diag t) ++ "'"

completion :: Term -> Int -> [String] -> [String]
completion (TMap xs) 0 (s:_) = [d | (k, _) <- xs, let d = showPattern k, isPrefixOf s d]
completion (TMap xs) 0 _ = map (showPattern . fst) xs
completion (TMap xs) n ("_":ss) = [r | (_, v) <- xs, r <- completion v (n - 1) ss]
completion (TMap xs) n (s:ss) = [r | (k, v) <- xs, let d = showPattern k, s == d, r <- completion v (n - 1) ss]
completion _ _ _ = []

access :: Term -> [String] -> [Doc]
access (TMap xs) ("_":ss) = [r | (_, v) <- xs, r <- access v ss]
access (TMap xs) (s:ss) = [r | (k, v) <- xs, quoted s == showPattern k, r <- access v ss]
access t _ = [diag t]

quoted :: String -> String
quoted str
    | all (\c -> isAlphaNum c || c `elem` ("-./" :: String)) str = str
    | otherwise = "'" ++ concatMap (\c -> if c == '\'' then "\\'" else [c]) str ++ "'"

runCompletion :: Int -> [String] -> IO ()
runCompletion n ("--bash-completion-word" : "cbor-tool" : xs) = runCompletion (n - 1) xs
runCompletion 0 ("--bash-completion-word" : path : _) = getDirectoryContents "." >>= mapM_ putStrLn . filter (isPrefixOf path)
runCompletion 0 _ = getDirectoryContents "." >>= mapM_ putStrLn
runCompletion n ("--bash-completion-word" : path : xs) = do
    term <- deserialise <$> BL.readFile path
    mapM_ putStrLn $ completion term (n - 1) $ filter (/="--bash-completion-word") xs

parseArgs :: [String] -> IO ()
parseArgs ("--bash-completion-index" : n : xs) = runCompletion (read n) xs
parseArgs (path : xs) = do
    term <- deserialise <$> BL.readFile path
    mapM_ (putStrLn . showDoc 0) $ access term xs

main :: IO ()
main = getArgs >>= parseArgs
