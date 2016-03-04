{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module CombinatorQuoter
( CLTree (..)
, Symbol
, parse_symbol
, cl
, readCLTree
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Data.Generics
import Text.Parsec
import Text.Parsec.String

-- ================================================================== --

type Symbol = String

data CLTree = App CLTree CLTree |  -- An application
              Leaf Symbol |        -- A single combinator symbol
              Point Symbol CLTree  -- An abstraction
    deriving (Show, Eq, Typeable, Data)

-- Binary tree representing a CL fragment
data CLTree' = App' CLTree' CLTree' |   -- An application
              Leaf' Symbol |         -- A single combinator symbol
              Point' Symbol CLTree' | -- An abstraction
              AntiTree' String |     -- AntiQuoting Tree
              AntiPoint' String CLTree'      -- AntiQuoting Point' symbol
    deriving (Show, Eq, Typeable, Data)

-- Converts a string to a CLTree'.  String can be in minimal parentheses format.
-- Using Parsec

a <*. b = a <* char b
b .*> a = char b *> a
a <++> b = (++) <$> a <*> b
a <:> b = (:) <$> a <*> b

parse_letter_combinator :: Parser Symbol
parse_letter_combinator = pure <$> (try letter <|> digit <|> char '_')

parse_long_combinator :: Parser Symbol
parse_long_combinator = string "{" <++> many1 (noneOf "}") <++> string "}"

parse_symbol :: Parser Symbol
parse_symbol = try parse_letter_combinator <|> parse_long_combinator

class ParseableTree t where
    mkApp :: t -> t -> t
    mkLeaf :: Symbol -> t
    mkPoint :: Symbol -> t -> t

    parse_tree :: Parser t
    parse_tree = foldl1 mkApp <$> many1 (try (spaces *> parse_segment))

    parse_leaf :: Parser t
    parse_leaf = mkLeaf <$> parse_symbol

    parse_sub_expression :: Parser t
    parse_sub_expression = '(' .*> parse_tree <*. ')'

    parse_lambda :: Parser t
    parse_lambda = mkPoint <$> ('[' .*> parse_symbol <*. '.') <*> (parse_tree <*. ']')

    parse_segment :: Parser t

instance ParseableTree CLTree where
    mkApp = App
    mkLeaf = Leaf
    mkPoint = Point
    parse_segment = try parse_leaf <|> try parse_sub_expression <|> try parse_lambda

instance ParseableTree CLTree' where
    mkApp = App'
    mkLeaf = Leaf'
    mkPoint = Point'
    parse_segment = try parse_leaf <|> try parse_sub_expression <|> try parse_lambda <|> try parse_anti_tree <|> parse_anti_lambda

parse_anti_tree :: Parser CLTree'
parse_anti_tree = AntiTree' <$> ('<' .*> many1 (noneOf ">") <*. '>')

parse_anti_lambda :: Parser CLTree'
parse_anti_lambda = AntiPoint' <$>
    (string "[<" *> many1 (noneOf ">") <* string ">.") <*>
    (parse_tree <*. ']')

parse_combinator_expression :: (ParseableTree t) => Parser t
parse_combinator_expression = spaces *> parse_tree <* spaces <* eof

readCLTree :: String -> Either String CLTree
readCLTree str =
    case parse parse_combinator_expression str str of
        Right tree -> return tree
        Left err   -> Left $ show err

-- ==============================================================
-- QUASIQUOTER
-- ==============================================================

parseCLTree :: Monad m => (String, Int, Int) -> String -> m CLTree'
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

antiCLTreeExp :: CLTree' -> Maybe (Q Exp)
--antiCLTreeExp  (AntiTree' id)  = Just $ varE (mkName id)
antiCLTreeExp (AntiTree' splice) =
    case parseExp splice of
        Right expr   -> Just . return $ expr
        Left errStr -> fail errStr
antiCLTreeExp (AntiPoint' splice tree) =
    case parseExp splice of
        Right expr   -> Just $ [| Point $(return expr) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree) |]
        Left errStr -> fail errStr
antiCLTreeExp (App' l r) = Just $ [| App $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) l) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) r) |]
antiCLTreeExp (Leaf' sym) = Just $ [| Leaf $(litE (StringL sym)) |]
antiCLTreeExp (Point' sym tree) = Just $ [| Point $(litE (StringL sym)) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree) |]

quoteCLTreeExp s = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))
    tree <- parseCLTree pos s
    dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree

-- Patterns

antiCLTreePat :: CLTree' -> Maybe (Q Pat)
antiCLTreePat (AntiTree' str)  = Just $ if str == "_" then wildP else varP (mkName str)
antiCLTreePat (AntiPoint' str tree) = Just $ conP (mkName "CombinatorQuoter.Point") [varP (mkName str), dataToPatQ (const Nothing `extQ` antiCLTreePat) tree]
antiCLTreePat (App' l r) = Just $ conP (mkName "CombinatorQuoter.App") [dataToPatQ (const Nothing `extQ` antiCLTreePat) l, dataToPatQ (const Nothing `extQ` antiCLTreePat) r]
antiCLTreePat (Leaf' sym) = Just $ conP (mkName "CombinatorQuoter.Leaf") [litP (StringL sym)]
antiCLTreePat (Point' sym tree) = Just $ conP (mkName "CombinatorQuoter.Point") [litP (StringL sym), dataToPatQ (const Nothing) tree]

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
        quotePat = quoteCLTreePat,
        quoteType = error "Not supported",
        quoteDec = error "Not supported"
    }




