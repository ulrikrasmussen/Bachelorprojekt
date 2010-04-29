-- vim:set foldmethod=marker foldmarker=--{,--}:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter(runInterpreter, defaultConfig, InterpConfig(..)) where

import Language

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Arrow

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

import qualified System.Random as R

rootLocation = "@root"

--{ Helper functions

-- | Generates a list of fresh StdGens
repStdGens ::  (R.RandomGen b) => Int -> b -> [b]
repStdGens n stdGen = snd . foldl (.) id funs $ (stdGen,[])
  where funs = replicate n (\(g,xs) -> (snd . R.next $ g, g:xs))

--}

instance Applicative (State Context) where
    pure = return
    (<*>) = ap

instance Applicative (ErrorT String (State Context)) where
    pure = return
    (<*>) = ap

newtype JoinM a = J { runJoinM :: ErrorT String (State Context) a  }
    deriving (Monad, MonadState Context, MonadError String, Functor, Applicative)

data Context = Context { cDefs :: [Def] -- ^ Active definitions
                       , cAtoms :: [Atom] -- ^ Active atoms
                       , cFreshNames :: [String] -- ^ Infinite stream of fresh names
                       , cLog :: [String] -- ^ Debug log
                       , cStdGen :: R.StdGen -- ^ StdGen for non-determinism
                       , cLocation :: String -- ^ Location name.
                       , cLocationParent :: String -- ^ Name of parent location.
                       , cFail :: Bool -- ^ Indicates if the context is in a failed state.
                       , cExportedNames :: S.Set String -- ^ Names exported to other contexts.
                                                        --   Defs for these cannot be garbage
                                                        --   collected.
                       , cFailureConts :: [String] -- ^ The list of continuations triggered
                                                   --   upon calling halt<>
                       }

instance Show Context where
  show context =      "Loc  :\t" ++ cLocation context ++ " (" ++ cLocationParent context ++ ")"
               ++ "\n\nExps :\t" ++ (concat . intersperse ", " . S.toList $ cExportedNames context)
               ++ "\n\nDefs :\t" ++ (concat . intersperse "\n\t" . map show $ cDefs context)
               ++ "\n\nAtoms:\t" ++ (concat . intersperse "\n\t" . map show $ cAtoms context)
               ++ "\n\nFailure Cs:\t" ++ (concat . intersperse "\n\t" . map show $ cFailureConts context)

instance Eq Context where
  a == b = cDefs a == cDefs b && cAtoms a == cAtoms b
         && cLocation a == cLocation b && cFail a == cFail b

class (Monad m) => MonadJoin m where
  getDefs :: m [Def]
  rmDef :: Def -> m ()
  putDef :: Def -> m ()
  getAtoms :: m [Atom]
  rmAtom :: Atom -> m ()
  putAtom :: Atom -> m ()
  replaceDefs :: [Def] -> m ()
  replaceAtoms :: [Atom] -> m ()
  debug :: String -> m ()
  cleanDebug :: m()

  getFreshNames :: Int -> m [String]
  getExportedNames :: m (S.Set String)
  setExportedNames :: S.Set String -> m ()
  getStdGen :: m R.StdGen
  getLocation :: m String
  isFailed :: m Bool
  setFail :: m ()

instance MonadJoin JoinM where
  getDefs = gets cDefs
  rmDef def = modify $ \s -> s { cDefs = delete def $ cDefs s }
  putDef def = modify $ \s -> s { cDefs = def : cDefs s }

  getAtoms = gets cAtoms
  rmAtom atm = modify $ \s -> s { cAtoms = delete atm $ cAtoms s }
  putAtom atm = modify $ \s -> s { cAtoms = atm : cAtoms s }

  replaceAtoms atoms = modify $ \s -> s {cAtoms = atoms}
  replaceDefs defs = modify $ \s -> s {cDefs = defs}

  debug msg = modify $ \s -> s{cLog = msg : (cLog s)}
  cleanDebug = modify $ \s -> s{cLog = []}

  getFreshNames n = do
    (fns, fns') <- gets (splitAt n . cFreshNames)
    modify $ \s -> s {cFreshNames = fns'}
    return fns

  getExportedNames = gets cExportedNames

  setExportedNames exports = modify $ \s -> s { cExportedNames = exports }

  getStdGen = do
    (sg, sg') <- R.split <$> gets cStdGen
    modify $ \s -> s {cStdGen = sg'}
    return sg

  getLocation = gets cLocation

  isFailed = gets cFail

  setFail = do modify $ \s -> s{cFail = True}
               throwError "Location failed"


initContext ::    [Def]    -- initial definitions
              -> [Atom]    -- initial atoms
              -> String    -- initial location name
              -> String    -- initial parent location name
              -> [String]  -- exported names
              -> R.StdGen  -- random seed
              -> Context

initContext ds as locName locParent exports stdGen = Context {
        cDefs = ds
      , cAtoms = as
      , cFreshNames = freshNames
      , cLog = []
      , cStdGen = stdGen
      , cLocation = locName
      , cLocationParent = locParent
      , cFail = False
      , cExportedNames = S.fromList exports
      , cFailureConts = []
      }
    where freshNames = ["#" ++ locName ++ "#" ++ show i | i <- [1..]]

data InterpConfig = IC {
    runGC :: Bool
  , gcInterval :: Integer
  , breakAt :: Maybe Integer
  , nondeterministic :: Bool
} deriving (Show)

defaultConfig = IC {
    runGC = True
  , gcInterval = 1
  , breakAt = Nothing
  , nondeterministic = False
}

runInterpreter :: InterpConfig -> Proc -> IO [Context]
runInterpreter conf (Proc as) = do
  (stdGen1, stdGen2) <- R.split <$> R.getStdGen
  return $ runInterpreter' stdGen2 0 [initContext [] as rootLocation rootLocation [] stdGen1]
   where runInterpreter' stdGen n ctx =
           let (stdGen', stdGen'') = R.split stdGen
               -- Execute a step in each context, spawn off any new locations, and
               -- exchange messages between contexts.
               ctx' = map (execInterp conf) >>>
                      concatMap (heatLocations stdGen'') >>>
                      registerFail >>>
                      map halt >>>
                      killFailed >>>
                      map migrate >>>
                      exchangeMessages $ ctx
            in if maybe (ctx /= ctx') (n/=) (breakAt conf)
                  then runInterpreter' stdGen' (n+1) ctx'
                  else ctx

         halt :: Context -> Context
         halt context =
           context { cFail = any isHalt $ cAtoms context }
             where isHalt (MsgA "halt" _) = True
                   isHalt _ = False

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

         {-
          - For all locations with a "go"-atom, change its parent location
          - to the first argument of the "go" and substitute go<_, k> with k
          -}
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

         takeElem p xs = takeElem' [] xs
           where takeElem' l [] = (Nothing, l)
                 takeElem' l (x:xs) = if p x then (Just x, l++xs) else takeElem' (l++[x]) xs

         {-
          - Find all fail<a, k> and register k in the cFailureConts of a
          -}
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
             -- ([(locNm, failCont)], Context)
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

         mkContext locParent stdGen (LocationD name ds (Proc as)) =
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

-- Executes a single step of the interpreter. If the context is in a failed state,
-- nothing happens.
execInterp :: InterpConfig -> Context -> Context
execInterp conf context
    | cFail context = context
    | otherwise     = execState (runErrorT . runJoinM $ interp conf) context

-- |Performs a single step of interpretation
interp :: (MonadJoin m, Functor m, Applicative m)
                => InterpConfig -> m ()
interp conf = do
  when (runGC conf) $ garbageCollect
  when (nondeterministic conf) $ scrambleContext
  mapM_ heatAtom =<< getAtoms
  mapM_ applyReaction =<< getDefs

scrambleContext :: (MonadJoin m, Functor m, Applicative m) => m ()
scrambleContext = do
  replaceDefs =<< scramble <$> getStdGen <*> getDefs
  replaceAtoms =<< scramble <$> getStdGen <*> getAtoms
   where scramble stdGen xs =
            map snd $ sortBy (comparing fst) $ zip (R.randoms stdGen :: [Int]) xs

applyReaction :: (MonadJoin m, Functor m) => Def -> m ()
applyReaction d@(ReactionD js p) =
  sequence <$> mapM matchJoin js >>=
  maybe (return ()) (\xs ->
    do let (sigma, atoms) = first M.unions $ unzip xs
       mapM_ rmAtom atoms
       mapM_ putAtom $ (subst sigma <$> (pAtoms p))
    )
applyReaction (LocationD _ _ _) = return ()

{- Check whether a join pattern is matched by the atoms in the context -}
matchJoin :: (MonadJoin m, Functor m) => Join -> m (Maybe (M.Map String Expr, Atom))
matchJoin (VarJ var pats) = do
  mAtom <- find (chanIs var) <$> getAtoms
  return $ do atom@(MsgA _ es) <- mAtom
              sigma <- matchPatterns es
              return (sigma, atom)
   where chanIs v (MsgA v' _) = v == v'
         chanIs v _           = False
         matchPatterns es     = M.unions <$> (sequence $ zipWith matchPat pats es)

matchPat ::  Pat -> Expr -> Maybe (M.Map String Expr)
matchPat (VarP s) e = Just $ M.fromList [(s, e)]
matchPat (ZeroP) (ZeroE) = Just $ M.empty
matchPat (SuccP p) (SuccE e) = matchPat p e
matchPat _ _ = Nothing

heatAtom :: (MonadJoin m, Functor m) => Atom -> m ()
heatAtom InertA        = rmAtom InertA
heatAtom m@(MsgA _ _)  = return ()
heatAtom d@(DefA ds p) = do
  rmAtom d
  let ivs = (S.toList . S.unions $ map definedVars ds)
  sigma <- M.fromList . zipWith (\l r -> (l, VarE $ l ++ r)) ivs <$> (getFreshNames $ length ivs)
  mapM_ putDef (subst sigma <$> ds)
  mapM_ putAtom $ (pAtoms $ sigma `subst` p)

heatAtom m@(MatchA e ps) =
  let (pats, procs) = unzip ps
      procs' = zipWith (\proc -> maybe Nothing (\sigma -> Just $ subst sigma <$> (pAtoms proc)))
                       procs
                       (flip matchPat e <$> pats)
      firstProc = getFirst . mconcat $ First <$> procs'
  in case firstProc of
       Nothing -> error $ "Pattern match is not exhaustive"
       Just proc -> rmAtom m >> mapM_ putAtom proc

{-
 - At first we mark all the MsgP:s in the context, as well as the exported names.
 - Then we mark the defs that might be activated by the marked atoms
 - and mark all the MsgP:s, that are potentially produced by these
 - marked defs. Then we iterate, until no new defs or MsgP:s are marked.
 -}
garbageCollect :: (MonadJoin m, Functor m) => m ()
garbageCollect = do
  markedNames <- S.unions . map freeVars <$> getAtoms
  exportedNames <- getExportedNames
  liveDefs <- gc' (markedNames `S.union` exportedNames) []
  replaceDefs liveDefs
  -- Remove all exported names that aren't represented in any live defs
  setExportedNames $ exportedNames `S.intersection` (S.unions . map definedVars $ liveDefs)
  where
    defMatched :: Def -> S.Set String -> Bool
    defMatched (ReactionD js _) nms = and $ map (\(VarJ nmJ _) -> S.member nmJ nms) js
    defMatched (LocationD _ _ _) _ = True

    gc' :: (MonadJoin m, Functor m) => S.Set String -> [Def] -> m [Def]
    gc' markedNames defs = do
      mMarkedDefs <- mapM (\def ->
           if defMatched def markedNames
             then do rmDef def
                     return (Just def)
             else return Nothing) =<< getDefs
      let markedDefs = catMaybes mMarkedDefs
      -- add the set of produceable names to the marked atoms
      let produceableAtoms = S.unions $ map freeVars markedDefs
      if not $ produceableAtoms == S.empty
        then gc' (markedNames `S.union` produceableAtoms)
                 (defs `union` markedDefs)
        else return (defs `union` markedDefs)
