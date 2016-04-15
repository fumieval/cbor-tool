{-# LANGUAGE OverloadedStrings #-}

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Term
import qualified Data.Text as T
import Data.List
import System.Environment (getArgs)
import Text.Trifecta
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Lens hiding (List, Context(..))
import Control.Monad
import System.IO (stdin)
import Data.Either (partitionEithers)
import Data.List.Lens

diag :: Term -> String
diag (TInt i) = show i
diag (TInteger i) = show i
diag (TBytes s) = show s
diag (TBytesI s) = show s
diag (TString s) = show s
diag (TStringI s) = show s
diag (TList xs) = "[" ++ intercalate ", " (map diag xs) ++ "]"
diag (TListI xs) = "[" ++ intercalate ", " (map diag xs) ++ "]"
diag (TMap xs) = "{" ++ intercalate ", " (map (\(k,v) -> diag k ++ ": " ++ diag v) xs) ++ "}"
diag (TMapI xs) = "{" ++ intercalate ", " (map (\(k,v) -> diag k ++ ": " ++ diag v) xs) ++ "}"
diag (TTagged i x) = show i ++ "(" ++ diag x ++ ")"
diag (TBool x) = show x
diag TNull = "()"
diag TUndef = "undefined"
diag (TSimple x) = show x
diag (THalf x) = show x
diag (TFloat x) = show x
diag (TDouble x) = show x

data Expr = Traverse
  | Index Int
  | Key String
  | Diag
  | Row
  | Indent
  | Context
  | Comp Expr Expr
  | List [Expr] deriving Show

data Val = VT Term
  | VS String
  | VCxt Val Val
  | VIndent Val
  | VList [Val]
  | VRow [Val]
  deriving Show

runExpr :: [Val] -> Expr -> Val -> [Val]
runExpr _ Context (VCxt c _) = [c]
runExpr _ Context _ = []
runExpr _ Traverse (VList xs) = xs
runExpr _ Traverse (VRow xs) = xs
runExpr _ Traverse (VT (TMap kvs)) = [VCxt (VT k) (VT v) | (k, v) <- kvs]
runExpr _ Traverse (VT (TMapI kvs)) = [VCxt (VT k) (VT v) | (k, v) <- kvs]
runExpr _ Traverse (VT (TList xs)) = map VT xs
runExpr _ Traverse (VT (TListI xs)) = map VT xs
runExpr _ (Key k0) (VT (TMap kvs)) = [VT v | (k, v) <- kvs, matchKey k0 k]
runExpr _ (Key k0) (VT (TMapI kvs)) = [VT v | (k, v) <- kvs, matchKey k0 k]
runExpr vs (Key k) (VCxt _ x) = runExpr vs (Key k) x
runExpr _ Diag v = [VS $ printVal v]
runExpr vs (Comp a b) t = concatMap (runExpr vs b) $ runExpr vs a t
runExpr _ Indent v = [VIndent v]
runExpr vs (List xs) t = [VList $ concatMap (flip (runExpr vs) t) xs]
runExpr _ Row (VList xs) = [VRow xs]
runExpr vs e (VCxt _ x) = runExpr vs e x
runExpr vs e (VIndent x) = VIndent <$> runExpr vs e x
runExpr _ Traverse v = error $ show v ++ " is not traversable"
runExpr _ (Key _) v = error $ show v ++ " is not a map"
runExpr _ t v = error $ show (t, v)

_VS f (VS s) = VS <$> f s
_VS _ x = pure x

matchKey :: String -> Term -> Bool
matchKey str (TString str') = str == T.unpack str'
matchKey _ _ = False

parseTerm :: Parser Expr
parseTerm = choice
  [ parens parseExpr
  , List <$> brackets (sepBy parseExpr (symbol ","))
  , Traverse <$ symbol "*"
  , Diag <$ symbol "diag"
  , Indent <$ symbol ">"
  , Context <$ symbol "?"
  , Row <$ symbol "row"
  , Index <$> fromInteger <$> natural
  , Key <$> stringLiteral
  ]

parseExpr :: Parser Expr
parseExpr = foldl Comp <$> parseTerm <*> many parseTerm

printVal :: Val -> String
printVal (VS x) = x
printVal (VT t) = diag t
printVal (VRow xs) = intercalate "\t" $ map printVal xs
printVal (VList xs) = "[" ++ intercalate ", " (map printVal xs) ++ "]"
printVal (VIndent v) = "  " ++ printVal v
printVal (VCxt _ x) = printVal x

main = do
  (src, flags) <- partitionEithers . map (matching (prefixed "--")) <$> getArgs
  case parseString (parseExpr <* eof) mempty $ unwords src of
    Success expr
      | "multiple" `elem` flags -> forever $ do
          n <- read <$> getLine
          term <- deserialise <$> BL.hGet stdin n
          mapM_ (putStrLn . printVal) $ runExpr [] expr $ VT term
      | otherwise -> do
          term <- deserialise <$> BL.getContents
          mapM_ (putStrLn . printVal) $ runExpr [] expr $ VT term
    Failure doc -> print doc
