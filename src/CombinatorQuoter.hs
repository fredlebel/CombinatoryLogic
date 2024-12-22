{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module CombinatorQuoter
( CLTree (..)
, Symbol
, parseSymbol
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

a <*. b  = a <* char b
b .*> a  = char b *> a
a <++> b = (++) <$> a <*> b
a <:> b  = (:) <$> a <*> b

parseLetterCombinator :: Parser Symbol
parseLetterCombinator = pure <$> (try letter <|> digit <|> char '_')

parseLongCombinator :: Parser Symbol
parseLongCombinator = string "{" <++> many1 (noneOf "}") <++> string "}"

parseSymbol :: Parser Symbol
parseSymbol = try parseLetterCombinator <|> parseLongCombinator

class ParseableTree t where
    mkApp   :: t -> t -> t
    mkLeaf  :: Symbol -> t
    mkPoint :: Symbol -> t -> t

    parseTree :: Parser t
    parseTree = foldl1 mkApp <$> many1 (try (spaces *> parseSegment))

    parseLeaf :: Parser t
    parseLeaf = mkLeaf <$> parseSymbol

    parseSubExpression :: Parser t
    parseSubExpression = '(' .*> parseTree <*. ')'

    parseLambda :: Parser t
    parseLambda = mkPoint <$> ('[' .*> parseSymbol <*. '.') <*> (parseTree <*. ']')

    parseSegment :: Parser t

instance ParseableTree CLTree where
    mkApp        = App
    mkLeaf       = Leaf
    mkPoint      = Point
    parseSegment = try parseLeaf <|> try parseSubExpression <|> try parseLambda

instance ParseableTree CLTree' where
    mkApp        = App'
    mkLeaf       = Leaf'
    mkPoint      = Point'
    parseSegment = try parseLeaf <|> try parseSubExpression <|> try parseLambda <|> try parseAntiTree <|> parseAntiLambda

parseAntiTree :: Parser CLTree'
parseAntiTree = AntiTree' <$> ('<' .*> many1 (noneOf ">") <*. '>')

parseAntiLambda :: Parser CLTree'
parseAntiLambda = AntiPoint' <$>
    (string "[<" *> many1 (noneOf ">") <* string ">.") <*>
    (parseTree <*. ']')

parseCombinatorExpression :: (ParseableTree t) => Parser t
parseCombinatorExpression = spaces *> parseTree <* spaces <* eof

readCLTree :: String -> Either String CLTree
readCLTree str =
    case parse parseCombinatorExpression str str of
        Right tree -> return tree
        Left err   -> Left $ show err

-- ==============================================================
-- QUASIQUOTER
-- ==============================================================

parseCLTree :: MonadFail m => (String, Int, Int) -> String -> m CLTree'
parseCLTree (file, line, col) str =
    case runParser p () "" str of
      Left err  -> fail $ show err
      Right e   -> return e
    where
        p = do
                pos <- getPosition
                setPosition $
                    flip setSourceName file $
                    flip setSourceLine line $
                    setSourceColumn pos col
                parseCombinatorExpression

-- Expressions

antiCLTreeExp :: CLTree' -> Maybe (Q Exp)
--antiCLTreeExp  (AntiTree' id)  = Just $ varE (mkName id)
antiCLTreeExp (AntiTree' splice) =
    case parseExp splice of
        Right expr   -> Just . return $ expr
        Left errStr -> fail errStr
antiCLTreeExp (AntiPoint' splice tree) =
    case parseExp splice of
        Right expr   -> Just [| Point $(return expr) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree) |]
        Left errStr -> fail errStr
antiCLTreeExp (App' l r) = Just [| App $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) l) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) r) |]
antiCLTreeExp (Leaf' sym) = Just [| Leaf $(litE (StringL sym)) |]
antiCLTreeExp (Point' sym tree) = Just [| Point $(litE (StringL sym)) $(dataToExpQ (const Nothing `extQ` antiCLTreeExp) tree) |]

-- Patterns

antiCLTreePat :: CLTree' -> Maybe (Q Pat)
antiCLTreePat (AntiTree' str)  = Just $ if str == "_" then wildP else varP (mkName str)
antiCLTreePat (AntiPoint' str tree) = Just $ conP (mkName "CombinatorQuoter.Point") [varP (mkName str), dataToPatQ (const Nothing `extQ` antiCLTreePat) tree]
antiCLTreePat (App' l r) = Just $ conP (mkName "CombinatorQuoter.App") [dataToPatQ (const Nothing `extQ` antiCLTreePat) l, dataToPatQ (const Nothing `extQ` antiCLTreePat) r]
antiCLTreePat (Leaf' sym) = Just $ conP (mkName "CombinatorQuoter.Leaf") [litP (StringL sym)]
antiCLTreePat (Point' sym tree) = Just $ conP (mkName "CombinatorQuoter.Point") [litP (StringL sym), dataToPatQ (const Nothing) tree]

quoteCLTree :: (CLTree' -> Q b) -> String -> Q b
quoteCLTree dataToFn  s = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))
    tree <- parseCLTree pos s
    dataToFn tree

-- The Quasi Quoter
cl :: QuasiQuoter
cl = QuasiQuoter
    {
        quoteExp = quoteCLTree (dataToExpQ (const Nothing `extQ` antiCLTreeExp)),
        quotePat = quoteCLTree (dataToPatQ (const Nothing `extQ` antiCLTreePat)),
        quoteType = error "Not supported",
        quoteDec = error "Not supported"
    }




