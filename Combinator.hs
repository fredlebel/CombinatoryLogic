import System
import Data.Char
import qualified Data.Map as Map
import Control.Monad.Error

-- ========== GENERIC FUNCTIONS ========== --
-- Simple piping function
a |> f = f a

count a [] = 0
count a (x:xs)
    | a == x    = 1 + (count a xs)
    | otherwise = count a xs

-- Function to replace all occurences of a
-- certain item in a list with another item.
replace :: Eq a => a -> a -> [a] -> [a]
replace a b [] = []
replace a b (x:xs)
    | x == a    = b:(replace a b xs)
    | otherwise = x:(replace a b xs)

-- ================================================================== --

-- Binary tree representing a CL fragment
data CLTree = Branch CLTree CLTree | Leaf Char
    deriving (Show, Eq)

isCombinator :: Char -> Bool
isCombinator c = isAscii c && (isLetter c || isDigit c)

{-
-- Converts a string to a CLTree.  String must be in full parantheses format.
parse :: String -> Either String CLTree
parse str = do
    -- TODO, maybe validate that the returned string is empty.
    (_, tree) <- parseCombinator str
    return (tree)

parseCombinator :: String -> Either String (String, CLTree)
parseCombinator (c:cs)
    | isAscii c && isLetter c = Right (cs, Leaf c)
    | c == '('  = parseBranch (c:cs)
    | otherwise = Left ("Unexpected leading character '" ++ [c] ++ "' in: " ++ (c:cs))

stripChar :: Char -> String -> Either String String
stripChar ch [] = Left ("Unexpected end of string, expected '" ++ [ch] ++ "'")
stripChar ch (c:cs)
    | c == ch   = Right cs
    | otherwise = Left ("Unexpected leading character in \"" ++ (c:cs) ++ "\", expected '" ++ [ch] ++ "'")

parseBranch :: String -> Either String (String, CLTree)
parseBranch str = do
    str <- stripChar '(' str
    (str, leftValue) <- parseCombinator str
    (str, rightValue) <- parseCombinator str
    str <- stripChar ')' str
    return (str, Branch leftValue rightValue)
-}

-- Converts a string to a CLTree.  String can be in minimal parantheses format.
readCLTree :: String -> Either String CLTree
readCLTree str = do
    -- TODO, maybe validate that the returned string is empty.
    (_, tree) <- readCLTreeLeft str
    return (tree)

readCLTreeLeft :: String -> Either String (String, CLTree)
readCLTreeLeft [] = Left "Empty combinator string."
readCLTreeLeft (c:cs)
    -- If it's a leaf.
    | isCombinator c = do
        -- Create the starting left fragment.
        let leftFragment = Leaf c
        -- Continue parsing the string, now building to the right.
        readCLTreeRight leftFragment cs
    -- If it's the start of parantheses.
    | c == '('  = do
        (cs, leftFragment) <- readCLTreeLeft cs
        -- Continue parsing the string, now building to the right.
        readCLTreeRight leftFragment cs
    | otherwise = Left ("Unexpected leading character '" ++ [c] ++ "' in: " ++ (c:cs))

readCLTreeRight :: CLTree -> String -> Either String (String, CLTree)
readCLTreeRight leftFragment [] = return ([], leftFragment)
readCLTreeRight leftFragment (c:cs)
    -- If it's a leaf.
    | isCombinator c = do
        -- Create a new branch with this leaf on the right side.
        let tree = Branch leftFragment (Leaf c)
        -- Continue parsing the string.
        readCLTreeRight tree cs
    -- If it's the start of parantheses.
    | c == '('  = do
        (cs, rightFragment) <- readCLTreeLeft cs
        -- Create a new branch with this fragment on the right side.
        let tree = Branch leftFragment rightFragment
        -- Continue parsing the string.
        readCLTreeRight tree cs
    | c == ')'  = return (cs, leftFragment)
    | otherwise = Left ("Unexpected leading character '" ++ [c] ++ "' in: " ++ (c:cs))



-- Symbol lookup.
type CLSymbolMap = Map.Map Char CLTree

-- Compile a higher level CLTree with multiple combinators to one
-- that only uses SKI combinators.
compile :: Bool -> CLSymbolMap -> CLTree -> Either String CLTree
compile strict symbols (Leaf ch)
    | elem ch "SKI" = Right (Leaf ch) -- Keep as is.
    | otherwise = case Map.lookup ch symbols of
        Just symbolTree -> Right symbolTree
        Nothing         -> if strict
                           then Left ("Unrecognized combinator character : '" ++ [ch] ++ "'")
                           else Right (Leaf ch)
compile strict symbols (Branch l r) = do
    newL <- compile strict symbols l
    newR <- compile strict symbols r
    return $ Branch newL newR

-- Loads a combinator symbol.
-- Once loaded that symbol can be used in future combinators.
loadSymbol :: Char -> String -> CLSymbolMap -> Either String CLSymbolMap
loadSymbol ch str symbols = do
    tree <- readCLTree str
    normalizedTree <- compile True symbols tree
    return $ Map.insert ch normalizedTree symbols

-- CLTree reductions
reduceTree :: CLTree -> CLTree
-- I reduction
reduceTree (Branch (Leaf 'I') x) = x
-- K reduction
reduceTree (Branch (Branch (Leaf 'K') x) _) = x
-- S reduction
reduceTree (Branch (Branch (Branch (Leaf 'S') x) y) z) = (Branch (Branch x z) (Branch y z))
-- Any leaf doesn't reduce.
reduceTree (Leaf x) = (Leaf x)
-- Branch, reduce both sides
reduceTree (Branch l r) = Branch (reduceTree l) (reduceTree r)

findNormalForm :: CLTree -> CLTree
findNormalForm tree = if tree == newTree then tree else findNormalForm newTree
    where newTree = reduceTree tree

-- Convert CLTree to a string

-- Converts a CLTree into a string representation with full parentheses.
showCLTree :: CLTree -> String
showCLTree (Leaf c) = [c]
showCLTree (Branch l r) = "(" ++ (showCLTree l) ++ (showCLTree r) ++ ")"

-- Converts a CLTree into a string representation with minimal parentheses.
-- That means only right branches are enclosed in parentheses.
showCLTreeCompact :: CLTree -> String
showCLTreeCompact (Leaf c) = [c]
showCLTreeCompact (Branch l r@(Branch _ _)) = (showCLTreeCompact l) ++ "(" ++ (showCLTreeCompact r) ++ ")"
showCLTreeCompact (Branch l r)              = (showCLTreeCompact l) ++        (showCLTreeCompact r)

unitTest :: Either String CLTree
unitTest = do
    -- The R combinator here swaps two arguments.
    symbols <- loadSymbol 'R' "S(K(SI))(S(KK)I)" Map.empty
    f <- (readCLTree >=> compile False symbols) "R"
    x <- (readCLTree >=> compile False symbols) "x"
    y <- (readCLTree >=> compile False symbols) "y"
    let nf = findNormalForm $ (Branch (Branch f x) y)
    return nf
    -- return (Branch (Branch f x) y)

hardcodedSymbols :: CLSymbolMap
hardcodedSymbols =
    case result of
        Right symbols -> symbols
    where
        result = (
            -- Bxyz = x(yz)
            (loadSymbol 'B' "S(KS)K") >=>
            -- Cxyz = xzy
            (loadSymbol 'C' "S(BBS)(KK)") >=>
            -- Wxy = xyy
            (loadSymbol 'W' "SS(KI)") >=>
            -- Y combinator
            (loadSymbol 'Y' "SSK(S(K(SS(S(SSK))))K)") >=>
            -- Txy = yx
            (loadSymbol 'T' "S(K(SI))(S(KK)I)") >=>
            -- Pxyz = z(xy)
            (loadSymbol 'P' "BT") >=>
            -- Dxy0 = x, Dxy1 = y
            (loadSymbol 'D' "(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(S(KS)(S(KK)(KK)))(S(KK)(KK)))))(S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))") >=>
            (loadSymbol '0' "(KI)") >=>
            (loadSymbol '1' "SB(KI)") >=>
            (loadSymbol '2' "SB(SB(KI))") >=>
            (loadSymbol '3' "SB(SB(SB(KI)))") >=>
            (loadSymbol '4' "SB(SB(SB(SB(KI))))") >=>
            (loadSymbol '5' "SB(SB(SB(SB(SB(KI)))))") >=>
            (loadSymbol '6' "SB(SB(SB(SB(SB(SB(KI))))))") >=>
            (loadSymbol '7' "SB(SB(SB(SB(SB(SB(SB(KI)))))))") >=>
            (loadSymbol '8' "SB(SB(SB(SB(SB(SB(SB(SB(KI))))))))") >=>
            (loadSymbol '9' "SB(SB(SB(SB(SB(SB(SB(SB(SB(KI)))))))))")
            ) Map.empty

printResult :: (Show r) => Either String r -> IO()
printResult (Right r) = print r
printResult (Left l) = putStrLn ("Error! " ++ l)

main = do
    args <- getArgs

    {-
    putStrLn ""
    putStrLn "== Unittest. =="
    case unitTest of
        Right tree -> putStrLn . showCLTreeCompact $ tree
        Left str   -> putStrLn str

    putStrLn ""
    putStrLn "== Parsing command line combinator string. =="
    -}
    let inputCombinator  = args !! 0
    let result = readCLTree inputCombinator
    -- printResult $ result

    case result of
        (Left str) -> putStrLn str
        (Right tree) -> do
            putStrLn . showCLTree $ tree
            putStrLn . showCLTreeCompact $ tree
            case compile False hardcodedSymbols tree of
                Right compiledTree -> do
                    mapM_ (\n -> putStrLn . showCLTreeCompact . (!!  n) . (iterate reduceTree) $ compiledTree) [0..100]

    {-
    putStrLn ""
    putStrLn "== Church encoding test. =="
    let churchTest = do
        symbols <- (
            (loadSymbol 'A' "S(K(SI))(S(KK)I)") >=>
            (loadSymbol 'B' "S(KS)K")
            ) Map.empty
        n3 <- (readCLTree >=> compile False symbols) "((SB)((SB)((SB)(KI))))"
        return . findNormalForm $ (Branch (Branch n3 (Leaf 'x')) (Leaf 'y'))

    case churchTest of
        Right tree -> putStrLn . showCLTreeCompact $ tree
        Left str -> putStrLn str
    -}

{-

Argument swap.
./Combinator.exe "S(K(SI))(S(KK)I)xy"
Church #3 application.
./Combinator.exe "(SB)((SB)((SB)(KI)))xy"

     
(((S(KI))x)y)
S(KI)xy

((SK)I) = SKI
(S(KI)) = S(KI)

(S)((KI)(SB))II

Combinator.exe --symbol B "((S(KS))K)" --symbol A "((S(K(SI)))((S(KK))I))" --compact --nf "((SB)((SB)((SB)(KI))))"
Combinator.exe --symbol B "((S(KS))K)" --full --step 2 "((SB)((SB)((SB)(KI))))"

-- Manually deriving the D combinator since I couldn't find it online.
[\xyz.z(Ky)x]
= [\x.[\y.[\z.(z(Ky))x]]]
= [\x.[\y.(S[\z.z(Ky)][\z.x])]]
= [\x.[\y.(S(S[\z.z][\z.Ky])(Kx))]]
= [\x.[\y.(S(SI[\z.Ky])(Kx))]]
= [\x.[\y.(S(SI(S[\z.K][\z.y]))(Kx))]]
= [\x.[\y.(S(SI(S(KK)(Ky)))(Kx))]]
-- z abstraction is gone
= [\x.[\y.(S(SI(S(KK)(Ky))))(Kx)]]
= [\x.(S[\y.(S(SI(S(KK)(Ky))))][\y.(Kx)])]
= [\x.(S[\y.(S(SI(S(KK)(Ky))))](S[\y.K][\y.x]))]
= [\x.(S[\y.S(SI(S(KK)(Ky)))](S(KK)(Kx)))]
= [\x.(S(S[\y.S][\y.(SI(S(KK)(Ky)))])(S(KK)(Kx)))]
= [\x.(S(S(KS)(S[\y.SI]      [\y.(S(KK)(Ky))]    ))(S(KK)(Kx)))]
= [\x.(S(S(KS)(S(S(KS)(KI))  [\y.(S(KK)(Ky))]    ))(S(KK)(Kx)))]
= [\x.(S(S(KS)(S(S(KS)(KI))  (S[\y.S(KK)][\y.(Ky)])    ))(S(KK)(Kx)))]
= [\x.(S(S(KS)(S(S(KS)(KI))  (S[\y.S(KK)](S(KK)I))    ))(S(KK)(Kx)))]
= [\x.(S(S(KS)(S(S(KS)(KI))  (S(S[\y.S][\y.(KK)])(S(KK)I))    ))(S(KK)(Kx)))]
= [\x.(S(S(KS)(S(S(KS)(KI))  (S(S(KS)(S(KK)(KK)))(S(KK)I))    ))(S(KK)(Kx)))]
-- y abstraction is gone
= [\x.S(S(KS)(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I))))(S(KK)(Kx))]
= (S[\x.S(S(KS)(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I))))] [\x.S(KK)(Kx)])
= (S[\x.S(S(KS)(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I))))] (S[\x.S(KK)][\x.(Kx)]))
= (S[\x.S(S(KS)(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I))))] (S(S[\x.S][\x.(KK)])(S(KK)I)))
= (S[\x.S(S(KS)(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I))))] (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)[\x.(S(KS)(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I))))]) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S[\x.S(KS)] [\x.(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I)))])) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) [\x.(S(S(KS)(KI))(S(S(KS)(S(KK)(KK)))(S(KK)I)))])) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S[\x.S(S(KS)(KI))][\x.(S(S(KS)(S(KK)(KK)))(S(KK)I))]))) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)[\x.(S(KS)(KI))])[\x.(S(S(KS)(S(KK)(KK)))(S(KK)I))]))) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S[\x.S(KS)](S(KK)(KI))))[\x.(S(S(KS)(S(KK)(KK)))(S(KK)I))]))) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))[\x.(S(S(KS)(S(KK)(KK)))(S(KK)I))]))) (S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))    (S[\x.S(S(KS)(S(KK)(KK)))][\x.S(KK)I])     )))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))    (S[\x.S(S(KS)(S(KK)(KK)))](S[\x.S(KK)](KI)))     )))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))    (S[\x.S(S(KS)(S(KK)(KK)))](S(S(KS)(S(KK)(KK)))(KI)))     )))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))    (S(S(KS)[\x.(S(KS)(S(KK)(KK)))])      (S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))    (S(S(KS)(S[\x.S(KS)][\x.(S(KK)(KK))]))      (S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))    (S(S(KS)(S(S(KS)(S(KK)(KS)))[\x.(S(KK)(KK))]))      (S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))(S(S(KS)(S(S(KS)(S(KK)(KS)))    (S[\x.S(KK)][\x.(KK)])))      (S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))(S(S(KS)(S(S(KS)(S(KK)(KS)))    (S(S(KS)[\x.(KK)])(S(KK)(KK)))))      (S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
= (S(S(KS)(S(S(KS)(S(KK)(KS))) (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))(S(S(KS)(S(S(KS)(S(KK)(KS)))    (S(S(KS)(S(KK)(KK)))(S(KK)(KK)))))      (S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))
-- x abstraction is gone
= (S(S(KS)(S(S(KS)(S(KK)(KS)))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(KK)(KI))))(S(S(KS)(S(S(KS)(S(KK)(KS)))(S(S(KS)(S(KK)(KK)))(S(KK)(KK)))))(S(S(KS)(S(KK)(KK)))(KI))))))(S(S(KS)(S(KK)(KK)))(S(KK)I)))

-}


