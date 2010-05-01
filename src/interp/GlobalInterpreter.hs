-- vim:set foldmethod=marker foldmarker=--{,--}:
module GlobalInterpreter(runInterpreter
                        ,InterpConfig(..)
                        ,defaultConfig) where

import Interpreter
import Language

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


runInterpreter :: InterpConfig -> Proc -> IO [Context]
runInterpreter conf (Proc as) = do
  (stdGen1, stdGen2) <- R.split <$> R.getStdGen
  runInterpreter' stdGen2 0 [initContext [] as rootLocation rootLocation [] stdGen1]
   where runInterpreter' stdGen n ctx = do
           (stdGen', stdGen'') <- return $ R.split stdGen
           newAtms <- runExternals $ manipulators conf
           ctx' <- mapM (runApi $ apiMap conf) ctx >>=
                   (map (execInterp conf) >>>
                   concatMap (heatLocations stdGen'') >>>
                   registerFail >>>
                   map halt >>>
                   killFailed >>>
                   map migrate >>>
                   exchangeMessages >>>
                   putMessages newAtms >>>
                   return)
           if maybe (ctx /= ctx') (n/=) (breakAt conf)
                then runInterpreter' stdGen' (n+1) ctx'
                else return ctx

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
  atms <- concat <$> mapM matchAtm (cAtoms ctx)
  return ctx{cAtoms = atms}
  where
    matchAtm :: Atom -> IO [Atom]
    matchAtm atm@(MsgA nm exp) = maybe (return [atm]) ($ atm) (M.lookup nm funMap)
    matchAtm atm = return [atm]

runExternals :: [Manipulator] -> IO [Atom]
runExternals funs = do
  maybeAtms <- sequence funs
  return $ concat $ catMaybes maybeAtms

-- |Marks a context as failed if it contains a halt<> atom.
halt :: Context -> Context
halt context =
  context { cFail = any isHalt $ cAtoms context }
    where isHalt (MsgA "halt" _) = True
          isHalt _ = False

-- |Propagates failure to subcontexts, and removes failed
killFailed :: [Context] -> [Context]
killFailed cs =
   let (failed, ok) = propagateFail cs
       conts = concatMap cFailureConts failed
       msgs = map (\n -> MsgA n []) conts
    in putMessages msgs ok
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
exchangeMessages :: [Context] -> [Context]
exchangeMessages cs =
  let (cs', ms) = second concat . unzip $ map takeMessages cs
   in putMessages ms cs'

takeMessages ::  Context -> (Context, [Atom])
takeMessages context =
  let dvs = S.unions . map definedVars $ cDefs context
      (locals, nonlocals) = partition (isLocal dvs) $ cAtoms context
      exports = S.unions $ map getExports nonlocals
      context' = context { cAtoms = locals
                         , cExportedNames = cExportedNames context `S.union` exports }
   in (context', nonlocals)
    where getExports (MsgA _ es) = S.unions $ map freeVars es

putMessages ::  [Atom] -> [Context] -> [Context]
putMessages ms [] = []
putMessages ms (context:cs) =
 let dvs = S.unions . map definedVars $ cDefs context
     (locals, ms') = partition (isLocal dvs) $ ms
     context' = context { cAtoms = cAtoms context ++ locals }
  in context':putMessages ms' cs

isLocal ::  S.Set String -> Atom -> Bool
isLocal dvs (MsgA name _) = S.member name dvs
isLocal _ _ = True

