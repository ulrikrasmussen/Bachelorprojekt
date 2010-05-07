-- vim:set foldmethod=marker foldmarker=--{,--}:
module GlobalInterpreter(runInterpreter
                        ,InterpConfig(..)
                        ,ApiMap
                        ,Manipulator
                        ,defaultConfig) where

import Interpreter
import Language
import JoinApi(ApiMap, Manipulator)

import Control.Applicative
import Control.Arrow

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import qualified System.Random as R

--{ Helper functions

-- | Generates a list of fresh StdGens
repStdGens ::  (R.RandomGen b) => Int -> b -> [b]
repStdGens n stdGen = snd . foldl (.) id funs $ (stdGen,[])
  where funs = replicate n (\(g,xs) -> (snd . R.next $ g, g:xs))

-- | Search for an element in a list, and remove it if found.
takeElem ::  (a -> Bool) -> [a] -> (Maybe a, [a])
takeElem p xs = takeElem' [] xs
  where takeElem' l [] = (Nothing, l)
        takeElem' l (x:xs) = if p x then (Just x, l++xs) else takeElem' (l++[x]) xs
--}

rootLocation = "@root"


runInterpreter :: InterpConfig -> IO [Context]
runInterpreter conf = let
    comP = M.fromList [ p | x <- M.keys $ comLinks conf, 
                            y <- M.keys $ comLinks conf, 
                            p <- [((x,y), comProb (comLinks conf) x y)]]
  in do 
    (stdGen1, stdGen2) <- R.split <$> R.getStdGen
    -- Create the initial "machines"
    runInterpreter' comP stdGen2 0 $ mkInitialCtxs stdGen1 (machineClasses conf) (initialMachines conf) -- [initContext [] as rootLocation rootLocation [] stdGen1]
  where runInterpreter' comP stdGen n contexts = do
           (stdGen_, stdGen') <- return $ R.split stdGen
           (stdGen__, stdGen'') <- return $ R.split stdGen_
           (stdGen___, stdGen''') <- return $ R.split stdGen__
           (stdGen'''', stdGen''''') <- return $ R.split stdGen___
           newAtms <- runExternals $ manipulators conf
           contexts' <- mapM (runApi $ apiMap conf) contexts >>=
                        (concatMap (heatLocations stdGen'') >>>
                         registerFail >>>
                         map halt >>>
                         killFailed comP stdGen''' >>>
                         map migrate >>>
                         exchangeMessages comP stdGen'''' >>>
                         putMessages comP stdGen''''' [tagged| na <- newAtms, tagged <- [(rootLocation,na)]]  >>>
                         map (execInterp conf) >>>
                         return)
           if maybe (contexts /= contexts') (n/=) (breakAt conf)
                then runInterpreter' comP stdGen' (n+1) contexts'
                else return contexts

        mkInitialCtxs      _   _     [] = []
        mkInitialCtxs stdGen cls (m:ms) = let (stdGen, stdGen') = R.split stdGen 
          in (initContext [] [(M.findWithDefault undefined (mcClass m) cls)] (mcName m) rootLocation [] stdGen):(mkInitialCtxs stdGen' cls ms)

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
    where isHalt (MsgA "halt" _) = True
          isHalt _ = False

-- |Propagates failure to subcontexts, and removes failed
killFailed :: M.Map (String, String) Double -> R.StdGen -> [Context] -> [Context]
killFailed comP rGen cs =
   let (failed, ok) = propagateFail cs
       msgs = concatMap (\ctx -> [tagged| fc <- cFailureConts ctx, tagged <- [(cLocation ctx, MsgA fc [])]]) failed
    in putMessages comP rGen msgs ok
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
    isFail (MsgA "fail" _) = True
    isFail _               = False

    extractFails :: Context -> ([(String, String)], Context)
    extractFails ctx =
      let (fails, atms') = partition isFail (cAtoms ctx)
          fails' = map (\(MsgA _ ((VarE loc):(VarE cont):[])) -> (loc,cont)) fails
      in (fails', ctx{cAtoms = atms',
                      cExportedNames = (S.fromList (snd . unzip $ fails'))
                                       `S.union` (cExportedNames ctx)})

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
exchangeMessages :: M.Map (String, String) Double -> R.StdGen -> [Context] -> [Context]
exchangeMessages comP rGen cs =
  let (cs', ms) = second concat . unzip $ map takeMessages cs
   in putMessages comP rGen ms cs'

takeMessages ::  Context -> (Context, [(String, Atom)])
takeMessages context =
  let dvs = S.unions . map definedVars $ cDefs context
      (locals, nonlocals) = partition (isLocal dvs) $ cAtoms context
      exports = S.unions $ map getExports nonlocals
      context' = context { cAtoms = locals
                         , cExportedNames = cExportedNames context `S.union` exports }
   in (context', [tagged| nl <- nonlocals, tagged <- [(cLocation context, nl)]])
    where getExports (MsgA _ es) = S.unions $ map freeVars es

putMessages :: M.Map (String, String) Double -> R.StdGen -> [(String, Atom)] -> [Context] -> [Context]
putMessages comP rGen ms [] = []
putMessages comP rGen ms (context:cs) =
 let (rGen, rGen') = R.split rGen
     dvs = S.unions . map definedVars $ cDefs context
     (locals, ms') = partition (snd >>> isLocal dvs) $ ms
     succCom = tryCom rGen locals
     context' = context { cAtoms = cAtoms context ++ succCom }
  in context':putMessages comP rGen' ms' cs
  where
    tryCom _  [] = []
    tryCom rg ((loc,a):ls) = if loc == rootLocation then a:tryCom rg ls 
      else case M.lookup (loc, cLocation context) comP of
        Nothing -> tryCom rg ls
        Just cP -> let (p,rg') = R.randomR (0,1) rg in
                   if cP >= p then a:tryCom rg' ls else tryCom rg' ls

isLocal ::  S.Set String -> Atom -> Bool
isLocal dvs (MsgA name _) = S.member name dvs
isLocal _ _ = True

