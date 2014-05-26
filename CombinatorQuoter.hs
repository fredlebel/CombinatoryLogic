{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CombinatorQuoter
( CLTree (..)
, Symbol
, parse_symbol
, cl
, readCLTree
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Control.Applicative ((<$), (<*), (*>), (<*>), (<$>), liftA, pure)

import Data.Generics
import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

-- ================================================================== --

type Symbol = String


-- Binary tree representing a CL fragment
data CLTree = App CLTree CLTree |   -- An application
              Leaf Symbol |         -- A single combinator symbol
              Point Symbol CLTree | -- An abstraction
              AntiTree String |     -- AntiQuoting Tree
              AntiPoint String CLTree      -- AntiQuoting Point symbol
    deriving (Show, Eq, Typeable, Data)


-- Converts a string to a CLTree.  String can be in minimal parentheses format.
-- Using Parsec

a <*. b = a <* char b
b .*> a = char b *> a
a <++> b = (++) <$> a <*> b
a <:> b = (:) <$> a <*> b

parse_letter_combinator :: Parser Symbol
parse_letter_combinator = pure <$> (try letter <|> digit)

parse_long_combinator :: Parser Symbol
parse_long_combinator = string "{" <++> many1 (noneOf "}") <++> string "}"

parse_symbol :: Parser Symbol
parse_symbol = try parse_letter_combinator <|> parse_long_combinator

parse_leaf :: Parser CLTree
parse_leaf = Leaf <$> parse_symbol

parse_anti_tree :: Parser CLTree
parse_anti_tree = AntiTree <$> ('<' .*> many1 (noneOf ">") <*. '>')

parse_segment :: Parser CLTree
parse_segment = try parse_leaf <|> try parse_sub_expression <|> try parse_lambda <|> try parse_anti_tree <|> parse_anti_lambda

parse_sub_expression :: Parser CLTree
parse_sub_expression = '(' .*> parse_tree <*. ')'

parse_lambda :: Parser CLTree
parse_lambda = Point <$> ('[' .*> parse_symbol <*. '.') <*> (parse_tree <*. ']')

parse_anti_lambda :: Parser CLTree
parse_anti_lambda = AntiPoint <$>
    (string "[<" *> many1 (noneOf ">") <* string ">.") <*>
    (parse_tree <*. ']')

parse_tree :: Parser CLTree
parse_tree = foldl1 App <$> many1 (try (spaces *> parse_segment))

parse_combinator_expression :: Parser CLTree
parse_combinator_expression = spaces *> parse_tree <* spaces <* eof

readCLTree :: String -> Either String CLTree
readCLTree str =
    case parse parse_combinator_expression str str of
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
--antiCLTreeExp  (AntiTree id)  = Just $ varE (mkName id)
antiCLTreeExp  (AntiTree splice) =
    case parseExp splice of
        Right exp   -> Just . return $ exp
        Left errStr -> fail errStr
antiCLTreeExp  (AntiPoint splice tree) =
    case parseExp splice of
        Right exp   -> Just $ [| Point $(return exp) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree) |]
        Left errStr -> fail errStr
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
antiCLTreePat  (AntiPoint id tree) = Just $ conP (mkName "CombinatorQuoter.Point") [varP (mkName id), dataToPatQ (const Nothing `extQ` antiCLTreePat) tree]
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




