{-# LANGUAGE QuasiQuotes #-}

import System.Exit
import System.Environment
import System.Console.GetOpt
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import CombinatorQuoter

programHeader :: String
programHeader = "Combinatory Logic Reducer v1.1 [Frederic LeBel : March 9th, 2013]"

-- ========== GENERIC FUNCTIONS ========== --

-- Search in a map for the first key whose value matches the argument.
findFirstKey :: (Eq a) => a -> Map.Map k a -> Maybe k
findFirstKey toFind m = Map.foldrWithKey keyFinder Nothing m
    where keyFinder key value acc
            | toFind == value   = Just key
            | otherwise         = acc

-- Apply a function until the result settles
untilSettle :: (Eq e) => (e -> e) -> e -> e
untilSettle f e = if e == newE then e else untilSettle f newE
    where newE = f e

-- ================================================================== --


-- Parses a symbol definition returning a tuple containing (symbolStr, treeStr, normalFormBool)
splitSymbolDefinition :: String -> Either String (Symbol, String, Bool)
splitSymbolDefinition str =
    case parse parse_symbol_definition str str of
        Right result -> Right result
        Left err   -> Left $ show err
    where
        parse_symbol_definition = do
            sym <- parse_symbol
            opt <- try (char '=') <|> char '!'
            val <- many1 anyChar
            return $ (sym, val, opt == '!')

-- Symbol lookup.
type CLSymbolMap = Map.Map Symbol CLTree

-- Compile a higher level CLTree with multiple combinators to one
-- that only uses SKI combinators.
compile :: Bool -> CLSymbolMap -> CLTree -> Either String CLTree
compile strict symbols (Leaf sym)
    | (length sym == 1) && elem (head sym) "SKI" = Right (Leaf sym) -- Keep as is.
    | otherwise = case Map.lookup sym symbols of
        Just symbolTree -> Right symbolTree
        Nothing         -> if strict
                           then Left ("Unrecognized combinator character : '" ++ sym ++ "'")
                           else Right (Leaf sym)
compile strict symbols (App l r) = do
    newL <- compile strict symbols l
    newR <- compile strict symbols r
    return $ App newL newR
compile strict symbols (Point sym tree) = do
    newTree <- compile strict symbols tree
    return $ Point sym newTree


-- Compacts a tree composed of only SKI using a symbol map.
-- Effectively the opposite of the function compile.
-- Starts from the top of the tree so it will always compact to the biggest symbol.
compactWithSymbols :: CLSymbolMap -> CLTree -> CLTree
compactWithSymbols _ tree@(Leaf _) = tree
--compactWithSymbols symbols (Point sym subTree) = Point sym (compactWithSymbols symbols subTree)
compactWithSymbols symbols [cl| [<sym>.<subTree>] |] = [cl| [<sym>.<compactWithSymbols symbols subTree>] |]
compactWithSymbols symbols tree@(App l r) =
    case findFirstKey tree symbols of
        Just sym -> Leaf sym
        Nothing -> App (compactWithSymbols symbols l) (compactWithSymbols symbols r)

-- Loads a combinator symbol.
-- Once loaded that symbol can be used in future combinators.
loadStrSymbol :: Symbol -> String -> CLSymbolMap -> Either String CLSymbolMap
loadStrSymbol sym str symbols = do
    tree <- readCLTree str
    loadTreeSymbol sym tree symbols

loadTreeSymbol :: Symbol -> CLTree -> CLSymbolMap -> Either String CLSymbolMap
loadTreeSymbol sym tree symbols = do
    let cleanTree = reduceAllAbstractions tree
    normalizedTree <- compile True symbols cleanTree
    return $ Map.insert sym normalizedTree symbols

loadReducedSymbol :: Symbol -> String -> CLSymbolMap -> Either String CLSymbolMap
loadReducedSymbol sym str symbols = do
    tree <- readCLTree str
    let cleanTree = reduceAllAbstractions tree
    normalizedTree <- compile True symbols cleanTree
    let nf = findNormalForm normalizedTree
    return $ Map.insert sym nf symbols

-- CLTree reductions
lazyReduceTree :: CLTree -> CLTree
-- I reduction
lazyReduceTree [cl| I<x> |] = x
-- K reduction
lazyReduceTree [cl| K<x><_> |] = x
-- S reduction
lazyReduceTree [cl| S<x><y><z> |] = [cl| <x><z>(<y><z>) |]
-- App, reduce both sides
--lazyReduceTree (App l r) = App (lazyReduceTree l) (lazyReduceTree r)
lazyReduceTree [cl| <l><r> |] = [cl| <lazyReduceTree l><lazyReduceTree r> |]
-- Any leaf doesn't reduce.
lazyReduceTree (Leaf x) = (Leaf x)
-- An abstraction
lazyReduceTree (Point sym tree) = reduceAbstraction (sym, tree)

-- CLTree reductions
eagerReduceTree :: CLTree -> CLTree
-- I reduction
eagerReduceTree [cl| I<x> |] = eagerReduceTree x
-- K reduction
eagerReduceTree [cl| K<x><_> |] = eagerReduceTree x
-- S reduction
eagerReduceTree [cl| S<x><y><z> |] = [cl| <eagerReduceTree x><eagerReduceTree z>(<eagerReduceTree y><eagerReduceTree z>) |]
-- App, reduce both sides
--eagerReduceTree (App l r) = App (eagerReduceTree l) (eagerReduceTree r)
eagerReduceTree [cl| <l><r> |] = [cl| <eagerReduceTree l><eagerReduceTree r> |]
-- Any leaf doesn't reduce.
eagerReduceTree (Leaf x) = (Leaf x)
-- An abstraction
eagerReduceTree (Point sym tree) = reduceAbstraction (sym, tree)

-- Abstraction conversion.  Converts [x.xSx] -> (S(SI(KS))I)
reduceAbstraction :: (Symbol, CLTree) -> CLTree
reduceAbstraction (sym, tree) = if containsAbstraction tree then Point sym (lazyReduceTree tree) else reduceImpl tree
    where   reduceImpl [cl| <l><r> |]
                -- [x.M] -> KM if sym not FV(M)
                | not (fv sym l) && not (fv sym r)  = [cl| K(<l><r>) |] -- App (Leaf "K") (App l r)
                -- Eta transformation. [x.Ux] -> U
                | not (fv sym l) && r == (Leaf sym)   = l
                -- [x.UV] -> S[x.U][x.V] where x FV(U) || FV(V)
                | otherwise    = [cl| S[<sym>.<l>][<sym>.<r>] |] -- App (App (Leaf "S") (Point sym l)) (Point sym r)
            reduceImpl leaf@(Leaf x)
                -- [x.x] -> I
                | sym == x   = [cl| I |] --Leaf "I"
                -- [x.y] -> (Ky)
                | otherwise = [cl| K<leaf> |] -- App (Leaf "K") (Leaf x)
            -- Shouldn't happen because of the 'containsAbstraction' check above.
            reduceImpl (Point _ _) = undefined

            -- Checks if a given symbol is a free variable in a tree.
            fv sym' (Leaf x)    = sym' == x
            fv sym' (App l r)   = (fv sym' l) || (fv sym' r)
            fv sym' (Point _ x) = fv sym' x

            -- Checks if an abstraction is contained in a given tree.
            containsAbstraction (Leaf _)    = False
            containsAbstraction (App l r)   = (containsAbstraction l) || (containsAbstraction r)
            containsAbstraction (Point _ _) = True


reduceAllAbstractions :: CLTree -> CLTree
reduceAllAbstractions = untilSettle doRemoval
    where   doRemoval (App l r) = App (doRemoval l) (doRemoval r)
            doRemoval leaf@(Leaf _) = leaf
            doRemoval (Point sym tree) = reduceAbstraction (sym, tree)


-- Reduces the tree until it no longer changes.
-- Can loop forever if the tree has no normal forms.
findNormalForm :: CLTree -> CLTree
findNormalForm = untilSettle eagerReduceTree

-- Convert CLTree to a string

-- Converts a CLTree into a string representation with full parentheses.
showCLTree :: CLTree -> String
showCLTree (Leaf c) = c
showCLTree (App l r) = "(" ++ (showCLTree l) ++ (showCLTree r) ++ ")"
showCLTree (Point sym subTree) = "[" ++ sym ++ "." ++ (showCLTree subTree) ++ "]"

-- Converts a CLTree into a string representation with minimal parentheses.
-- That means only right applications are enclosed in parentheses.
showCLTreeCompact :: CLTree -> String
showCLTreeCompact (Leaf c) = c
showCLTreeCompact (App l r@(App _ _)) = (showCLTreeCompact l) ++ "(" ++ (showCLTreeCompact r) ++ ")"
showCLTreeCompact (App l r)           = (showCLTreeCompact l) ++        (showCLTreeCompact r)
showCLTreeCompact (Point sym subTree) = "[" ++ sym ++ "." ++ (showCLTreeCompact subTree) ++ "]"

hardcodedSymbols :: CLSymbolMap
hardcodedSymbols =
    case result of
        Right symbols -> symbols
        Left _ -> undefined
    where
        result = return Map.empty
            -- Bxyz = x(yz)
            >>= (loadTreeSymbol "B" [cl| S(KS)K |])
            -- Cxyz = xzy
            >>= (loadTreeSymbol "C" [cl| S(BBS)(KK) |])
            -- Wxy = xyy
            >>= (loadTreeSymbol "W" [cl| SS(KI) |])
            -- Ufx = x(ffx)
            >>= (loadTreeSymbol "U" [cl| (S(K(SI))(SII)) |])
            -- Y combinator, Yx = x(Yx)
            >>= (loadTreeSymbol "Y" [cl| UU |])
            -- Txy = yx
            >>= (loadTreeSymbol "T" [cl| S(K(SI))(S(KK)I) |])
            -- Pxyz = z(xy)
            >>= (loadTreeSymbol "P" [cl| BT |])
            -- Dxy0 = x, Dxy1 = y
            >>= (loadTreeSymbol "D" [cl| (S(K(S(S(KS)(S(K(SI))(S(KK)K)))))(S(KK)K)) |])
            >>= (loadTreeSymbol "0" [cl| (KI) |])
            >>= (loadTreeSymbol "1" [cl| SB(KI) |])
            >>= (loadTreeSymbol "2" [cl| SB(SB(KI)) |])
            >>= (loadTreeSymbol "3" [cl| SB(SB(SB(KI))) |])
            >>= (loadTreeSymbol "4" [cl| SB(SB(SB(SB(KI)))) |])
            >>= (loadTreeSymbol "5" [cl| SB(SB(SB(SB(SB(KI))))) |])
            >>= (loadTreeSymbol "6" [cl| SB(SB(SB(SB(SB(SB(KI)))))) |])
            >>= (loadTreeSymbol "7" [cl| SB(SB(SB(SB(SB(SB(SB(KI))))))) |])
            >>= (loadTreeSymbol "8" [cl| SB(SB(SB(SB(SB(SB(SB(SB(KI)))))))) |])
            >>= (loadTreeSymbol "9" [cl| SB(SB(SB(SB(SB(SB(SB(SB(SB(KI))))))))) |])

            -- Q = \yv.D (succ(v0)) (y(v0)(v1))
            >>= (loadTreeSymbol "Q" [cl| (S(K(S(S(KD)(S(K(SB))(SI(K0))))))(S(S(KS)(S(S(KS)K)(K((SI(K0))))))(K((SI(K1)))))) |])
            -- R = \xyu.u(Qy)(D0x)1
            -- Rxy0 = x
            -- Rxy(k+1) = yk(Rxyk)
            >>= (loadTreeSymbol "R" [cl| (S(S(KS)(S(K(S(KS)))(S(K(S(S(KS)(S(K(SI))(S(KK)Q)))))(S(KK)(S(KK)(D0))))))(K(K(K1)))) |])

-- Reduces a combinator for a given number of rounds or until it reaches a normal form.
runCombinator :: Int -> (CLTree -> CLTree) -> (CLTree -> String) -> CLTree -> [String]
runCombinator 0 _ outputFn tree = [outputFn tree, "\n..."]
runCombinator iterationCount reduceFn outputFn tree =
    if nextTree == tree
        then [output]
        else output:(runCombinator (iterationCount - 1) reduceFn outputFn nextTree)
    where
        nextTree = reduceFn tree
        output = outputFn tree
    -- Stop if reached normal form.


-- Program entrypoint.
main :: IO ()
main = do
    args <- getArgs

    case getOpt RequireOrder options args of
        ([], [], []) -> putStrLn $ usageInfo programHeader options
        (actions, _, _) -> do
            -- Execute all option functions, returning the final options.
            opts <- foldl (>>=) (return defaultOptions) actions

            case opts of
                Options {optCombinator = Nothing} -> do { putStrLn "Must define combinator argument."; exitWith $ ExitFailure 1}
                Options {
                    optSymbols = symbols,
                    optIterationCount = iterationCount,
                    optCompactFn = compactFn,
                    optPrintFn = printFn,
                    optPutStrFn = putStrFn,
                    optReduceFn = reduceFn,
                    optCombinator = Just tree } ->
                        putStrFn . runCombinator iterationCount reduceFn (printFn . compactFn symbols) $ tree


-- Dealing with program arguments and parsing them.

data Options = Options {
        optSymbols :: CLSymbolMap,
        optIterationCount :: Int,
        optCompactFn :: (CLSymbolMap -> CLTree -> CLTree),
        optPrintFn :: (CLTree -> String),
        optPutStrFn :: ([String] -> IO()),
        optReduceFn :: (CLTree -> CLTree),
        optCombinator :: Maybe CLTree
    }

defaultOptions :: Options
defaultOptions = Options {
        optSymbols = Map.empty,
        optIterationCount = 200,
        optCompactFn = compactWithSymbols,
        optPrintFn = showCLTreeCompact,
        optPutStrFn = mapM_ putStrLn,
        optReduceFn = lazyReduceTree,
        optCombinator = Nothing
    }

options :: [OptDescr (Options -> IO Options)]
options = [
        Option ['v'] ["version"]                (NoArg showVersion)                     "show version number",
        Option ['s'] ["symbol"]                 (ReqArg defineSymbol "SYMBOL")          "define a symbol and a combinator, ex: \"B=S(KS)K\"",
        Option ['S'] ["use_predefined_symbols"] (NoArg usePredefinedSymbols)            "use the predefined symbols",
        Option ['L'] ["SKI"]                    (NoArg showSKIOnly)                     "shows only SKI combinators, no symbols",
        Option ['P'] ["show_parentheses"]       (NoArg showAllParentheses)              "show full parentheses, ex: \"((SK)I)\"",
        Option ['f'] ["no_stop"]                (NoArg doNotStop)                       "reduce until reaching NF, no stop at 200 iterations",
        Option ['l'] ["last"]                   (NoArg showLastOnly)                    "only show the last line of the reduction",
        Option ['e'] ["eager"]                  (NoArg eagerReduce)                     "tree reduction is done eagerly",
        Option ['c'] ["combinator"]             (ReqArg defineCombinator "COMBINATOR")  "combinator to reduce"
    ]

showVersion :: Options -> IO Options
showVersion _ = do
    putStrLn programHeader
    exitWith ExitSuccess

defineSymbol :: String -> Options -> IO Options
defineSymbol symbolOptStr opt@(Options {optSymbols = symbols}) =
    case work symbolOptStr of
        Right newSymbols -> return $ opt {optSymbols = newSymbols}
        Left str -> do
            putStrLn str
            exitWith $ ExitFailure 1
    where
        work str = do
            (sym, symbolStr, reduceSymbol) <- splitSymbolDefinition str
            newSymbols <- (if reduceSymbol then loadReducedSymbol else loadStrSymbol) sym symbolStr symbols
            return newSymbols



usePredefinedSymbols :: Options -> IO Options
usePredefinedSymbols opt@(Options {optSymbols = symbols}) =
    return $ opt {optSymbols = Map.union symbols hardcodedSymbols}

showSKIOnly :: Options -> IO Options
showSKIOnly opt = return $ opt { optCompactFn = (\_ -> id) }


showAllParentheses :: Options -> IO Options
showAllParentheses opt = return $ opt {optPrintFn = showCLTree}

doNotStop :: Options -> IO Options
doNotStop opt = return $ opt {optIterationCount = -1}

showLastOnly :: Options -> IO Options
showLastOnly opt = return $ opt {optPutStrFn = putStrLn . last}

eagerReduce :: Options -> IO Options
eagerReduce opt = return $ opt {optReduceFn = eagerReduceTree}

defineCombinator :: String -> Options -> IO Options
defineCombinator combinatorStr opt@(Options {optSymbols = symbols})  =
    case return combinatorStr >>= readCLTree >>= compile False symbols of
        Right tree -> return $ opt {optCombinator = Just tree}
        Left str -> do
            putStrLn str
            exitWith $ ExitFailure 1

{-

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













