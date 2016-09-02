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

main :: IO ()
main = do
  args <- getArgs
  hPutStrLn stderr $ show args
  if
    | "--multiple" `elem` args -> forever $ do
      n <- read <$> getLine
      term <- deserialise <$> BL.hGet stdin n
      putStrLn $ showDoc 0 $ diag term
    | otherwise -> do
      term <- deserialise <$> BL.getContents
      putStrLn $ showDoc 0 $ diag term
