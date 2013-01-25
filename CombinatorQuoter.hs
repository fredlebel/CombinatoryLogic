{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CombinatorQuoter where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Data.Generics
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

-- ================================================================== --

type Symbol = String


-- Binary tree representing a CL fragment
data CLTree = App CLTree CLTree |   -- An application
              Leaf Symbol |         -- A single combinator symbol
              Point Symbol CLTree | -- An abstraction
              AntiTree String       -- AntiQuoting Tree
    deriving (Show, Eq, Typeable, Data)


-- Converts a string to a CLTree.  String can be in minimal parentheses format.
-- Using Parsec

parse_haskell_ident :: Parser Symbol
parse_haskell_ident = many lower


parse_letter_combinator :: Parser Symbol
parse_letter_combinator = do
    ch <- try letter <|> digit
    return [ch]

parse_symbol_combinator :: Parser Symbol
parse_symbol_combinator = do
    char '{'
    sym <- many1 (satisfy (/='}'))
    char '}'
    return $ "{" ++ sym ++ "}"

parse_leaf :: Parser CLTree
parse_leaf = do
    str <- try parse_letter_combinator <|> parse_symbol_combinator
    return $ Leaf str

parse_anti_tree :: Parser CLTree
parse_anti_tree = do
    -- string "$leaf:"
    char '<'
    id <- parse_haskell_ident
    char '>'
    return $ AntiTree id

parse_segment :: Parser CLTree
parse_segment = try parse_leaf <|> try parse_sub_expression <|> try parse_lambda <|> parse_anti_tree

parse_sub_expression :: Parser CLTree
parse_sub_expression = do
    char '('
    tree <- parse_tree
    char ')'
    return tree

parse_lambda :: Parser CLTree
parse_lambda = do
    char '['
    c <- try parse_letter_combinator <|> parse_symbol_combinator
    char '.'
    tree <- parse_tree
    char ']'
    return $ Point c tree

parse_tree :: Parser CLTree
parse_tree = do
    segment <- parse_segment
    tree <- continue_parsing segment
    return tree

    where
        continue_parsing leftTree =
            try ( do
                segment <- parse_segment
                continue_parsing $ App leftTree segment)
            <|>
            return leftTree

parse_combinator_expression :: Parser CLTree
parse_combinator_expression = do
    spaces
    tree <- parse_tree
    spaces
    eof
    return tree

readCLTree :: String -> Either String CLTree
readCLTree str =
    case parse parse_combinator_expression "" str of
        Right tree -> return tree
        Left err   -> Left $ show err

-- ==============================================================
-- QUASIQUOTER
-- ==============================================================

parseCLTree :: Monad m => (String, Int, Int) -> String -> m CLTree
parseCLTree (file, line, col) str =
    case runParser p () "" str of
      Left err  -> fail $ show err
      Right e   -> return e
    where
        p = do
                pos <- getPosition
                setPosition $
                    (flip setSourceName) file $
                    (flip setSourceLine) line $
                    (flip setSourceColumn) col $
                    pos
                parse_combinator_expression

-- Expressions

antiCLTreeExp :: CLTree -> Maybe (Q Exp)
antiCLTreeExp  (AntiTree id)  = Just $ varE (mkName id)
antiCLTreeExp  _                = Nothing
                
quoteCLTreeExp s = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))
    tree <- parseCLTree pos s
    dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree

-- Patterns

antiCLTreePat :: CLTree -> Maybe (Q Pat)
antiCLTreePat  (AntiTree id)  = Just $ varP (mkName id)
antiCLTreePat  _                = Nothing
                
quoteCLTreePat s = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))
    tree <- parseCLTree pos s
    dataToPatQ (const Nothing `extQ` antiCLTreePat) tree

-- The Quasi Quoter
cl :: QuasiQuoter
cl = QuasiQuoter
    {
        quoteExp = quoteCLTreeExp,
        quotePat = quoteCLTreePat
    }




