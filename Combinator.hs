import System
import Data.Char
import qualified Data.Map as Map
import Control.Monad.Error

-- ========== GENERIC FUNCTIONS ========== --
-- Simple piping function
a |> f = f a

count :: (Eq a) => a -> [a] -> Int
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

-- Search in a map for the first key whose value matches the argument.
findFirstKey :: (Eq a) => a -> Map.Map k a -> Maybe k
findFirstKey toFind map = Map.foldrWithKey keyFinder Nothing map
    where keyFinder key value acc
            | toFind == value   = Just key
            | otherwise         = acc
    
    
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
    if count '(' str /= count ')' str
        then Left ("Parentheses mismatch in \"" ++ str ++ "\"")
        else do
            (remainingStr, tree) <- readCLTreeLeft str
            -- Validate that the returned string is empty.
            if null remainingStr
                then return tree
                else Left ("Unexpected trailing characters \"" ++ remainingStr ++ "\"")

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

-- Compacts a tree composed of only SKI using a symbol map.
-- Effectively the opposite of the function compile.
-- Starts from the top of the tree so it will always compact to the biggest symbol.
compactWithSymbols :: CLSymbolMap -> CLTree -> CLTree
compactWithSymbols symbols tree@(Leaf _) = tree
compactWithSymbols symbols tree@(Branch l r) =
    case findFirstKey tree symbols of
        Just ch -> Leaf ch
        Nothing -> Branch (compactWithSymbols symbols l) (compactWithSymbols symbols r)
    
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

-- Reduces the tree until it no longer changes.
-- Can loop forever if the tree has no normal forms.
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
            -- Ufx = x(ffx)
            (loadSymbol 'U' "(S(K(SI))(SII))") >=>
            -- Y combinator, Yx = x(Yx)
            (loadSymbol 'Y' "UU") >=>
            -- Txy = yx
            (loadSymbol 'T' "S(K(SI))(S(KK)I)") >=>
            -- Pxyz = z(xy)
            (loadSymbol 'P' "BT") >=>
            -- Dxy0 = x, Dxy1 = y
            (loadSymbol 'D' "(S(K(S(S(KS)(S(K(SI))(S(KK)K)))))(S(KK)K))") >=>
            (loadSymbol '0' "(KI)") >=>
            (loadSymbol '1' "SB(KI)") >=>
            (loadSymbol '2' "SB(SB(KI))") >=>
            (loadSymbol '3' "SB(SB(SB(KI)))") >=>
            (loadSymbol '4' "SB(SB(SB(SB(KI))))") >=>
            (loadSymbol '5' "SB(SB(SB(SB(SB(KI)))))") >=>
            (loadSymbol '6' "SB(SB(SB(SB(SB(SB(KI))))))") >=>
            (loadSymbol '7' "SB(SB(SB(SB(SB(SB(SB(KI)))))))") >=>
            (loadSymbol '8' "SB(SB(SB(SB(SB(SB(SB(SB(KI))))))))") >=>
            (loadSymbol '9' "SB(SB(SB(SB(SB(SB(SB(SB(SB(KI)))))))))") >=>
            
            -- Q = \yv.D (succ(v0)) (y(v0)(v1))
            (loadSymbol 'Q' "(S(K(S(S(KD)(S(K(SB))(SI(K0))))))(S(S(KS)(S(S(KS)K)(K((SI(K0))))))(K((SI(K1))))))") >=>
            -- R = \xyu.u(Qy)(D0x)1
            -- Rxy0 = x
            -- Rxy(k+1) = yk(Rxyk)
            (loadSymbol 'R' "(S(S(KS)(S(K(S(KS)))(S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))(S(KK)(S(KK)(D0))))))(K(K(K1))))")
            
            ) Map.empty

printResult :: (Show r) => Either String r -> IO()
printResult (Right r) = print r
printResult (Left l) = putStrLn ("Error! " ++ l)

main = do
    args <- getArgs

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
                    mapM_ (\n -> putStrLn . showCLTreeCompact . compactWithSymbols hardcodedSymbols . (!!  n) . (iterate reduceTree) $ compiledTree) [0..200]


{-

-- Plans for command line flags
Combinator.exe --symbol B "((S(KS))K)" --symbol A "((S(K(SI)))((S(KK))I))" --compact --nf "((SB)((SB)((SB)(KI))))"
Combinator.exe --symbol B "((S(KS))K)" --full --step 2 "((SB)((SB)((SB)(KI))))"

-- Manually deriving the D combinator since I couldn't find it online.
[\xyz.z(Ky)x]

1. [x].M = KM (if x is not a FV(M))
2. [x].x = I
3. [x].Ux = U (if x is not a FV(U))
4. [x].UV = S([x].U)([x].V) if (1. or 3. don't apply)

[\xyz.z(Ky)x]
= [\x.[\y.[\z.(z(Ky))x]]]
= [\x.[\y.(S[\z.z(Ky)][\z.x])]]
= [\x.[\y.(S(S[\z.z][\z.Ky])(Kx))]]
= [\x.[\y.(S(SI(K(Ky)))(Kx))]]
-- z abstraction is gone.
= [\x.[\y.S(SI(K(Ky)))(Kx)]]
= [\x.(S    [\y.S(SI(K(Ky)))]   [\y.(Kx)])]
= [\x.(S    (S(KS)[\y.(SI(K(Ky)))])   (K(Kx)))]
= [\x.(S(S(KS)(S(K(SI)) [\y.K(Ky)]  ))(K(Kx)))]
= [\x.(S(S(KS)(S(K(SI)) (S(KK)K)  ))(K(Kx)))]
-- y abstraction is gone.
= [\x.S(S(KS)(S(K(SI))(S(KK)K)))(K(Kx))]
= (S[\x.S(S(KS)(S(K(SI))(S(KK)K)))]     [\x.(K(Kx))])
= (S(K(S(S(KS)(S(K(SI))(S(KK)K)))))     (S[\x.K][\x.(Kx)]))
= (S(K(S(S(KS)(S(K(SI))(S(KK)K)))))     (S(KK)K))
-- x abstraction is gone
= (S(K(S(S(KS)(S(K(SI))(S(KK)K)))))(S(KK)K))


-- Deriving U \ux.x(uux)
[\u.[\x.x(uux)]]
= [\u.(SI[\x.uux])]
= [\u.(SI(uu))]
= (S[\u.SI][\u.(uu)])
= (S(K(SI))(S[\u.u][\u.u]))
= (S(K(SI))(SII))

Deriving Q \yv.D (succ(v0)) (y(v0)(v1))
= [\y.[\v.D ((SB)(v0)) (y(v0)(v1))]]
= [\y.(S[\v.D((SB)(v0))]    [\v.(y(v0)(v1))])]
= [\y.(S(S(KD)[\v.(SB)(v0)])    (S[\v.y(v0)][\v.(v1)]))]
= [\y.(S(S(KD)[\v.(SB)(v0)])    (S(S(Ky)[\v.v0]) (SI(K1))))]
= [\y.(S(S(KD)(S(K(SB))[\v.(v0)]))    (S(S(Ky)(SI(K0))) (SI(K1))))]
= [\y.(S(S(KD)(S(K(SB))(SI(K0))))    (S(S(Ky)(SI(K0))) (SI(K1))))]
-- v abstraction is gone
= [\y.S(S(KD)(S(K(SB))(SI(K0))))(S(S(Ky)(SI(K0)))(SI(K1)))]
= (S[\y.S(S(KD)(S(K(SB))(SI(K0))))]      [\y.(S(S(Ky)(SI(K0)))(SI(K1)))])
= (S(K(S(S(KD)(S(K(SB))(SI(K0))))))      (S[\y.S(S(Ky)(SI(K0)))](K((SI(K1))))))
= (S(K(S(S(KD)(S(K(SB))(SI(K0))))))      (S(S(KS)[\y.(S(Ky)(SI(K0)))])    (K((SI(K1))))))
= (S(K(S(S(KD)(S(K(SB))(SI(K0))))))      (S(S(KS)(S[\y.S(Ky)]   (K((SI(K0))))))    (K((SI(K1))))))
= (S(K(S(S(KD)(S(K(SB))(SI(K0))))))      (S(S(KS)(S(S(KS)[\y.Ky])   (K((SI(K0))))))    (K((SI(K1))))))
= (S(K(S(S(KD)(S(K(SB))(SI(K0))))))      (S(S(KS)(S(S(KS)K)   (K((SI(K0))))))    (K((SI(K1))))))
-- y abstraction is gone
= (S(K(S(S(KD)(S(K(SB))(SI(K0))))))(S(S(KS)(S(S(KS)K)(K((SI(K0))))))(K((SI(K1))))))

Deriving R \xyu.u(Qy)(D0x)1
= [\x.[\y.[\u.u(Qy)(D0x)1]]]
= [\x.[\y.(S[\u.u(Qy)(D0x)](K1))]]
= [\x.[\y.(S(S(SI(K(Qy)))    (K(D0x)))(K1))]]
-- u abstraction is gone.
= [\x.[\y.S(S(SI(K(Qy)))(K(D0x)))(K1)]]
= [\x.(S[\y.S(S(SI(K(Qy)))(K(D0x)))](K(K1)))]
= [\x.(S    (S(KS)[\y.S(SI(K(Qy)))(K(D0x))])    (K(K1)))]
= [\x.(S    (S(KS)(S(S(KS)[\y.SI(K(Qy))])   (K(K(D0x)))))    (K(K1)))]
= [\x.(S    (S(KS)(S(S(KS)  (S(K(SI))[\y.K(Qy)]))   (K(K(D0x)))))    (K(K1)))]
= [\x.(S    (S(KS)(S(S(KS)  (S(K(SI))(S(KK)Q)))   (K(K(D0x)))))    (K(K1)))]
-- y abstraction is gone.
= [\x.S(S(KS)(S(S(KS)(S(K(SI))(S(KK)Q)))(K(K(D0x)))))(K(K1))]
= (S[\x.S(S(KS)(S(S(KS)(S(K(SI))(S(KK)Q)))(K(K(D0x)))))](K(K(K1))))
= (S (S(KS)[\x.S(KS)(S(S(KS)(S(K(SI))(S(KK)Q)))(K(K(D0x))))])    (K(K(K1))))
= (S (S(KS)(S(K(S(KS)))     [\x.S(S(KS)(S(K(SI))(S(KK)Q)))(K(K(D0x)))]))    (K(K(K1))))
= (S (S(KS)(S(K(S(KS)))     (S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))[\x.K(K(D0x))])))    (K(K(K1))))
= (S (S(KS)(S(K(S(KS)))     (S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))   (S(KK)[\x.K(D0x)]))))    (K(K(K1))))
= (S (S(KS)(S(K(S(KS)))     (S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))   (S(KK)(S(KK)[\x.D0x])))))    (K(K(K1))))
= (S (S(KS)(S(K(S(KS)))     (S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))   (S(KK)(S(KK)(D0))))))    (K(K(K1))))
-- x abstraction is gone.
= (S(S(KS)(S(K(S(KS)))(S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))(S(KK)(S(KK)(D0))))))(K(K(K1))))


-}













