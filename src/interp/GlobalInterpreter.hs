-- vim:set foldmethod=marker foldmarker=--{,--}:
module GlobalInterpreter(runInterpreter
                        ,InterpConfig(..)
                        ,ApiMap
                        ,Manipulator
                        ,mkUniGraph
                        ) where

import Interpreter
import Language
import JoinApi(ApiMap, Manipulator)

import Control.Monad
import Control.Applicative
import Control.Arrow

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import qualified System.Random as R
import Debug.Trace

rootLocation = "@root"

--{ Helper functions

-- | Adds a global "machineId" message to a group of atoms.
makeIdMsg :: String -> [Atom] -> [Atom]
makeIdMsg machineId as =
 [DefA [ReactionD [VarJ "machineId" [VarP "k"]] 0 . Proc $ [MsgA "k" [toJoin machineId]]]
       (Proc as)
 ]

grandestParents :: [Context] -> M.Map String String
grandestParents ctxs =
  grandestParents' $ M.fromList (map (cLocation &&& cLocationParent) ctxs)
  where
    grandestParents' m = let m' = M.mapWithKey prop m
                          in if m' == m then m' else grandestParents' m'
     where prop k p = let p' = m M.! p
                      in if p == rootLocation
                            then k
                            else if p' == rootLocation
                                    then p
                                    else p'

-- | Generates a list of fresh StdGens
repStdGens ::  (R.RandomGen b) => Int -> b -> [b]
repStdGens n stdGen = snd . foldl (.) id funs $ (stdGen,[])
  where funs = replicate n (\(g,xs) -> (snd . R.next $ g, g:xs))

-- | Search for an element in a list, and remove it if found.
takeElem ::  (a -> Bool) -> [a] -> (Maybe a, [a])
takeElem p xs = takeElem' [] xs
  where takeElem' l [] = (Nothing, l)
        takeElem' l (x:xs) = if p x then (Just x, l++xs) else takeElem' (l++[x]) xs

comProb :: M.Map String [(String, Double)] -> String -> String -> Double
comProb graph n1 n2 =
    maybe 0 (\p -> 1/p) $ dijkstra graph (\(_, _, w) -> w) (\o n -> o * (1/n)) n1 n2 1

dijkstra graph wFun adder start dest initW =
    dijkstra' (Just start) (M.fromList [(start, (False, initW))])
  where
    -- graph maps vertices to edges. wFun maps edges to weights
    dijkstra' cur paths =  -- paths map vertexes to path lengths and visited/unvisited status
      case cur of
        Just cur' -> let edges  = outE graph cur'
                         curW   = snd . fromJust $ M.lookup cur' paths
                         paths' = updatePaths paths cur' curW edges
                     in dijkstra' (getLowestUnvisited paths) paths'
        Nothing   -> maybe Nothing (Just . snd) (M.lookup dest paths)

    getLowestUnvisited pths = maybe Nothing (Just . fst) $ M.foldrWithKey foldPaths Nothing pths

    foldPaths v (vis, w) old = if vis then old else
      maybe (Just (v,w)) (\o@(v',w') -> if v < v' then Just (v,w) else Just o) old

    -- Examine every edge, update the vertices pointed to and mark the current node as visited.
    updatePaths paths curV curW []     = M.update (\(_, len) -> Just (True, len)) curV paths
    updatePaths paths curV curW (e:es) =
        let w   = adder curW (wFun e)
            dV  = destV e
            dVData = M.lookup dV paths
         in case dVData of
              -- dV already visited
              Just (True, _)      -> updatePaths paths curV curW es
              -- org not visited, check for update
              Just (False, wPrev) -> if w < wPrev then
                updatePaths (M.update (\_ -> Just (False, w)) dV paths) curV curW es
                else updatePaths paths curV curW es
              Nothing   -> updatePaths (M.insert dV (False, w) paths) curV curW es

mkUniGraph :: [String] -> [(String, String, Double)] -> M.Map String [(String, Double)]
mkUniGraph vs es = mkGraph' M.empty vs [ e | (o,d,w) <- es, e <- [(o,d,w),(d,o,w)] ]
  where
    mkGraph' m []     _  = m
    mkGraph' m (v:vs) es =
      let (vEdgs, restE) = partition (\(v', _, _) -> v' == v) es
          vEdgs'         = [(d,w) | (_,d,w) <- vEdgs]
      in M.insert v vEdgs' (mkGraph' m vs restE)

outE g o = maybe [] (\e -> [(o,d,w) | (d,w) <- e]) (M.lookup o g)
destV (_, d, _)  = d
origV (o, _, _)  = o
--}

runInterpreter :: InterpConfig -> IO [Context]
runInterpreter conf = do
    let comP = M.fromList [ p | x <- M.keys $ comLinks conf,
                                y <- M.keys $ comLinks conf,
                                p <- [((x,y), comProb (comLinks conf) x y)]]
    stdGen <- R.newStdGen
    runInterpreter' comP 0 $ mkInitialCtxs stdGen (machineClasses conf) (initialMachines conf)
  where runInterpreter' comP n contexts = do
           [stdGen1, stdGen2, stdGen3, stdGen4] <- replicateM 4 R.newStdGen
           newAtms <- runExternals $ manipulators conf
           -- Execute API messages
           contexts' <- mapM (runApi $ apiMap conf) contexts
           -- Spawn new contexts for sublocations
           let cSpawned = concatMap (heatLocations stdGen1) contexts'
           -- Move contexts with a "go" atom
           let cMoved = map migrate cSpawned
           -- Map sublocation names to their grandest parent location name.
           let gp = grandestParents cMoved
           -- Handle fail handler registration and halting
           let cFailed =
                (registerFail >>> map halt >>> killFailed comP gp stdGen2) cMoved
           -- Exchange messages between locations
           let cExchanged = exchangeMessages comP gp stdGen3 cFailed
           let cStepped =
                (putMessages comP gp stdGen4 [(rootLocation,na) | na <- newAtms] >>>
                 map (execInterp conf)) cExchanged
           let cProgressed = if map cAtoms cStepped == map cAtoms contexts
                                then map (\x -> x {cTime = succ $ cTime x}) cStepped
                                else cStepped
           if maybe True (n/=) (breakAt conf)
                then runInterpreter' comP (n+1) cProgressed
                else return contexts

        mkInitialCtxs      _   _     [] = []
        mkInitialCtxs stdGen cls (m:ms) =
          let (stdGen', stdGen'') = R.split stdGen
              as = makeIdMsg (fst m) (cls M.! snd m)
              ctx = initContext []
                                as
                                (fst m)
                                rootLocation
                                []
                                stdGen'
          in ctx : mkInitialCtxs stdGen'' cls ms

{-
 - There are two types of "magic" devices:
 -   1. External events that manipulate the state of the interpreter
 -   2. Atoms with side effects in the outside world
 -
 - Through these two magic devices we can implement timeouts and keypresses.
 -}

-- | Remove atoms that match a magic word and run the corresponding function.
runApi :: ApiMap -> Context -> IO Context
runApi funMap ctx = do
  (atms, exports) <- unzip <$> mapM matchAtm (cAtoms ctx)
  return ctx{cAtoms = concat atms, cExportedNames = S.unions $ (cExportedNames ctx):exports }
  where
    matchAtm :: Atom -> IO ([Atom], S.Set String)
    matchAtm del@(DelayA d (Proc [atm])) =
        if d > cTime ctx
            then return ([del], S.empty)
            else do (as, set) <- matchAtm atm
                    return ([DelayA d (Proc as)], set)
    matchAtm atm@(MsgA nm exp) = maybe (return ([atm], S.empty)) (\f -> (,) <$> (f atm) <*> (pure $ freeVars atm))  (M.lookup nm funMap)
    matchAtm atm = return ([atm], S.empty)

runExternals :: [Manipulator] -> IO [Atom]
runExternals funs = do
  atms <- sequence funs
  return $ concat $ atms

-- |Marks a context as failed if it contains a halt<> atom.
halt :: Context -> Context
halt context =
  context { cFail = any isHalt $ cAtoms context }
    where isHalt (DelayA d (Proc [MsgA "halt" _])) = d <= cTime context
          isHalt (MsgA "halt" _) = True
          isHalt _ = False

-- |Propagates failure to subcontexts, and removes failed
killFailed :: M.Map (String, String) Double
              -> M.Map String String
              -> R.StdGen
              -> [Context]
              -> [Context]
killFailed comP gp rGen cs =
   let (failed, ok) = propagateFail cs
       msgs = concatMap (\ctx -> [(gp M.! cLocation ctx, MsgA fc []) | fc <- cFailureConts ctx])
                        failed
    in putMessages comP gp rGen msgs ok
     where propagateFail cs =
             let (failed, ok) = partition cFail cs
                 failNames = S.fromList $ map cLocation failed
                 cs' = map (markFailed failNames) ok
                 (failed', ok') = propagateFail cs'
              in if null failed
                    then ([], cs)
                    else (failed++failed', ok')

           markFailed failNames ctx =
             ctx { cFail = S.member (cLocation ctx) failNames }

-- |For all locations with a "go" atom, change its parent location
-- to the first argument of the "go" and substitute go<_, k> with k
migrate :: Context -> Context
migrate ctx =
  let (mMsg, as') = takeElem isGo $ cAtoms ctx
      isGo (MsgA "go" _) = True
      isGo _ = False
   in maybe
        ctx
        (\(MsgA _ ((VarE dest):(VarE cont):[]))
           -> ctx { cLocationParent = dest
                  , cAtoms = (MsgA cont []):as'})
        mMsg

-- |Find all fail<a, k> messages and register k in the cFailureConts of the
-- location with name a
registerFail :: [Context] -> [Context]
registerFail ctxs =
  -- for each context, remove and collect occurences of fail, and mark the failure
  -- continuation as exported
  let
    (fails, ctxs') = unzip $ map extractFails ctxs
  in
    map (updateCtx $ concat fails) ctxs'
  where
    isFail t (DelayA d (Proc [MsgA "fail" _])) = d <= t
    isFail t (MsgA "fail" _) = True
    isFail t _               = False

    extractFails :: Context -> ([(String, String)], Context)
    extractFails ctx =
      let (fails, atms') = partition (isFail $ cTime ctx) (cAtoms ctx)
          fails' = map exLocCont fails
      in (fails', ctx{cAtoms = atms',
                      cExportedNames = (S.fromList (snd . unzip $ fails'))
                                       `S.union` (cExportedNames ctx)})

    exLocCont :: Atom -> (String, String)
    exLocCont (MsgA _ [VarE loc, VarE cont]) = (loc,cont)
    exLocCont (DelayA _ (Proc [msg])) = exLocCont msg

    updateCtx :: [(String, String)] -> Context -> Context
    updateCtx fails ctx = let
      (_, failConts) = unzip . fst $ partition (((cLocation ctx) ==) . fst) fails
      in ctx{cFailureConts = failConts ++ (cFailureConts ctx)}

heatLocations stdGen context =
  let (locations, defs) = partition isLocationD $ cDefs context
      stdGens = repStdGens (length locations) stdGen
      exports = (S.unions . map definedVars $ defs)
                 `S.intersection`
                (S.unions . map freeVars $ locations)
      context' = context {cDefs = defs,
                          cExportedNames = cExportedNames context
                                          `S.union` exports}
   in context' : zipWith (mkContext $ cLocation context) stdGens locations
   where mkContext locParent stdGen (LocationD name ds (Proc as)) =
           let exports = S.unions . map definedVars $ filter isReactionD ds
            in initContext ds as name locParent (S.toList exports) stdGen

-- | Exchanges messages between a list of contexts.
exchangeMessages :: M.Map (String, String) Double
                    -> M.Map String String
                    -> R.StdGen
                    -> [Context]
                    -> [Context]
exchangeMessages comP gp rGen cs =
  let (cs', ms) = second concat . unzip $ map takeMessages cs
   in putMessages comP gp rGen ms cs'

takeMessages ::  Context -> (Context, [(String, Atom)])
takeMessages context =
  let dvs = S.unions . map definedVars $ cDefs context
      (locals, nonlocals) = partition (\m -> isLocal dvs m
                                             || not (hasDelay (cTime context) m))
                                      (cAtoms context)
      nonlocals' = map rmDelay nonlocals
      exports = S.unions $ map getExports nonlocals'
      context' = context { cAtoms = locals
                         , cExportedNames = cExportedNames context `S.union` exports }
   in (context', [(cLocation context, nl) | nl <- nonlocals'])
    where getExports (MsgA _ es) = S.unions $ map freeVars es
          hasDelay t (DelayA d (Proc _)) = d == t
          hasDelay _ _ = False
          rmDelay (DelayA d (Proc [atm])) = atm
          rmDelay x = trace (show x) x

putMessages :: M.Map (String, String) Double
               -> M.Map String String
               -> R.StdGen
               -> [(String, Atom)]
               -> [Context]
               -> [Context]
putMessages comP gp rGen ms [] = []
putMessages comP gp rGen ms (context:cs) =
 let (rGen', rGen'') = R.split rGen
     dvs = S.unions . map definedVars $ cDefs context
     (locals, ms') = partition (snd >>> isLocal dvs) $ ms
     succCom = map setDelay $ tryCom rGen' locals
     context' = context { cAtoms = cAtoms context ++ succCom }
  in context':putMessages comP gp rGen'' ms' cs
  where
    setDelay = DelayA (succ $ cTime context) . Proc . (:[])

    tryCom _  [] = []
    tryCom rg ((loc,a):ls) =
      let destLoc = gp M.! cLocation context
          loc' = gp M.! loc
       in if loc == rootLocation || loc' == destLoc
             then a:tryCom rg ls
             else case M.lookup (loc', destLoc) comP of
                    Nothing -> tryCom rg ls
                    Just cP -> let (p,rg') = R.randomR (0,1) rg
                                in if cP >= p then a:tryCom rg' ls
                                              else tryCom rg' ls

isLocal ::  S.Set String -> Atom -> Bool
isLocal dvs (DelayA d (Proc [msg@(MsgA _ _)])) = isLocal dvs msg
isLocal dvs (MsgA name _) = S.member name dvs
isLocal _ _ = True

