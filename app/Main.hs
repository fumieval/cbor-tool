{-# LANGUAGE OverloadedStrings #-}

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Term
import qualified Data.Text as T
import Data.List
import System.Environment (getArgs)
import Text.Trifecta
import qualified Data.ByteString.Lazy as BL
import Control.Lens hiding (List, Context(..))

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


runExpr :: Expr -> Val -> [Val]
runExpr Context (VCxt c _) = [c]
runExpr Context _ = []
runExpr Traverse (VList xs) = xs
runExpr Traverse (VRow xs) = xs
runExpr Traverse (VT (TMap kvs)) = [VCxt (VT k) (VT v) | (k, v) <- kvs]
runExpr Traverse (VT (TMapI kvs)) = [VCxt (VT k) (VT v) | (k, v) <- kvs]
runExpr Traverse (VT (TList xs)) = map VT xs
runExpr Traverse (VT (TListI xs)) = map VT xs
runExpr (Key k0) (VT (TMap kvs)) = [VT v | (k, v) <- kvs, matchKey k0 k]
runExpr (Key k0) (VT (TMapI kvs)) = [VT v | (k, v) <- kvs, matchKey k0 k]
runExpr (Key k) (VCxt _ x) = runExpr (Key k) x
runExpr Diag v = [VS $ printVal v]
runExpr (Comp a b) t = concatMap (runExpr b) $ runExpr a t
runExpr Indent v = [VIndent v]
runExpr (List xs) t = [VList $ concatMap (flip runExpr t) xs]
runExpr Row (VList xs) = [VRow xs]
runExpr e (VCxt _ x) = runExpr e x
runExpr e (VIndent x) = VIndent <$> runExpr e x
runExpr Traverse v = error $ show v ++ " is not traversable"
runExpr (Key _) v = error $ show v ++ " is not a map"
runExpr t v = error $ show (t, v)

_VS f (VS s) = VS <$> f s
_VS _ x = pure x

matchKey :: String -> Term -> Bool
matchKey str (TString str') = str == T.unpack str'
matchKey _ _ = False

parseTerm :: Parser Expr
parseTerm = choice
    [ parens parseExpr
    , List <$> brackets (sepBy parseExpr (symbol ","))
    , Traverse <$ symbol "/"
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
    str <- unwords <$> getArgs
    term <- deserialise <$> BL.getContents
    case parseString (parseExpr <* eof) mempty str of
        Success expr -> do
            print expr
            mapM_ (putStrLn . printVal) $ runExpr expr $ VT term

        Failure doc -> print doc
