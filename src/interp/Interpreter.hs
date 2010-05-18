-- vim:set foldmethod=marker foldmarker=--{,--}:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interpreter( InterpConfig(..)
                  , MachineConfig(..)
                  , Context(..)
                  , initContext
                  , Event (..)
                  , EventLog
                  , Output (..)
                  , OutputLog
                  , execInterp) where

import Language
import JoinApi(ApiMap, Manipulator)

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

import Debug.Trace

instance Applicative (State Context) where
    pure = return
    (<*>) = ap

newtype JoinM a = J { runJoinM :: State Context a  }
    deriving (Monad, MonadState Context, Functor, Applicative)

--{ Interpreter Context

data Context = Context { cTime :: Integer -- ^ Abstract time representation
                       , cDefs :: [Def] -- ^ Active definitions
                       , cAtoms :: [Atom] -- ^ Active atoms
                       , cFreshNames :: [String] -- ^ Infinite stream of fresh names
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

--}

--{ Utility functions

getDefs ::  JoinM [Def]
getDefs = gets cDefs

rmDef :: Def -> JoinM ()
rmDef def = modify $ \s -> s { cDefs = delete def $ cDefs s }

putDef :: Def -> JoinM ()
putDef def = modify $ \s -> s { cDefs = def : cDefs s }

getAtoms :: JoinM [Atom]
getAtoms = gets cAtoms

rmAtom :: Atom -> JoinM ()
rmAtom atm = modify $ \s -> s { cAtoms = delete atm $ cAtoms s }

putAtom :: Atom -> JoinM ()
putAtom atm = modify $ \s -> s { cAtoms = atm : cAtoms s }

replaceAtoms :: [Atom] -> JoinM ()
replaceAtoms atoms = modify $ \s -> s {cAtoms = atoms}

replaceDefs :: [Def] -> JoinM ()
replaceDefs defs = modify $ \s -> s {cDefs = defs}

getFreshNames ::  Int -> JoinM [String]
getFreshNames n = do
  (fns, fns') <- gets (splitAt n . cFreshNames)
  modify $ \s -> s {cFreshNames = fns'}
  return fns

getExportedNames :: JoinM (S.Set String)
getExportedNames = gets cExportedNames

setExportedNames :: S.Set String -> JoinM ()
setExportedNames exports = modify $ \s -> s { cExportedNames = exports }

getStdGen :: JoinM R.StdGen
getStdGen = do
  (sg, sg') <- R.split <$> gets cStdGen
  modify $ \s -> s {cStdGen = sg'}
  return sg

getLocation :: JoinM String
getLocation = gets cLocation

isFailed :: JoinM Bool
isFailed = gets cFail

getTime :: JoinM Integer
getTime = gets cTime

initContext ::    [Def]    -- initial definitions
              -> [Atom]    -- initial atoms
              -> String    -- initial location name
              -> String    -- initial parent location name
              -> [String]  -- exported names
              -> R.StdGen  -- random seed
              -> Context

initContext ds as locName locParent exports stdGen = Context {
        cTime = 0
      , cDefs = ds
      , cAtoms = as
      , cFreshNames = freshNames
      , cStdGen = stdGen
      , cLocation = locName
      , cLocationParent = locParent
      , cFail = False
      , cExportedNames = S.fromList exports
      , cFailureConts = []
      }
    where freshNames = ["#" ++ locName ++ "#" ++ show i | i <- [1..]]

--}

-- The event log is a list of the external events that
-- influence the global state of the interpreter
type EventLog = [[Event]]
data Event = EvLinkUp   String String
           | EvLinkDown String String
           | EvSpecial  String [Pat] ([Expr] -> [Atom]) -- special atoms intended for reading from simulated hardware (e.g. sensors)


type OutputLog = [[Output]]
data Output = Message String String -- Machine Message
            | Debug   String String -- Machine Message

--{ Configuration data

data InterpConfig = IC {
    runGC :: Bool
  , gcInterval :: Integer
  , breakAt :: Maybe Integer
  , nondeterministic :: Bool
  , apiMap          :: ApiMap  -- ^ A map from atom names to functions. Used to enable IO in join programs.
  --, manipulators    :: [Manipulator]  -- ^ A list of atom-returning functions, that alter the state of the interpreter.
  , machineClasses  :: M.Map String [Atom]
  , initialMachines :: [MachineConfig]
  -- , comLinks :: M.Map String [(String, Double)]
}

type MachineConfig = (String , String)

--}

-- Executes a single step of the interpreter. If the context is in a failed state,
-- nothing happens.
execInterp :: InterpConfig -> Context -> Context
execInterp conf context
    | cFail context = context
    | otherwise     = execState (runJoinM $ interp conf) context

-- |Performs a single step of interpretation
interp :: InterpConfig -> JoinM ()
interp conf = do
  when (runGC conf) garbageCollect
  when (nondeterministic conf) scrambleContext
  (dss, ass) <- unzip <$> (mapM heatAtom =<< getAtoms)
  replaceAtoms $ concat ass
  mapM_ putDef $ concat dss
  mapM_ applyReaction =<< getDefs

scrambleContext :: JoinM ()
scrambleContext = do
  replaceDefs =<< scramble <$> getStdGen <*> getDefs
   where scramble stdGen xs =
            map snd $ sortBy (comparing fst) $ zip (R.randoms stdGen :: [Int]) xs

applyReaction :: Def -> JoinM ()
applyReaction d@(ReactionD js delay p) =
  sequence <$> mapM matchJoin js >>=
  maybe (return ())
        (\xs -> do let (sigma, atoms) = first M.unions $ unzip xs
                   t <- getTime
                   let reactionTime = max t $ delay + foldl max 0 (map getDelay atoms)
                   when (reactionTime <= t) $ do mapM_ rmAtom atoms
                                                 putAtom $ DelayA reactionTime (sigma `subst` p))
  where
    getDelay (DelayA d _) = d
    getDelay _ = 0

applyReaction (LocationD _ _ _) = return ()

{- Check whether a join pattern is matched by the atoms in the context -}
matchJoin :: Join -> JoinM (Maybe (M.Map String Expr, Atom))
matchJoin (VarJ var pats) = do
  atoms <- filter (chanIs var) <$> getAtoms
  let matches = map matchPatterns atoms
  return . getFirst . mconcat . map First $ matches
   where chanIs v (MsgA v' _)                   = v == v'
         chanIs v (DelayA d (Proc [MsgA v' _])) = v == v'
         chanIs v _                             = False
         matchPatterns atom@(DelayA d (Proc [msg])) = do
            (sigma, _) <- matchPatterns msg
            return (sigma, atom)
         matchPatterns atom@(MsgA _ es) = do
            sigma <- M.unions <$> zipWithM matchPat pats es
            return (sigma, atom)

-- | Match a pattern against an expression. Returns Nothing if the pattern
-- doesn't match, otherwise returns `Just m` where m maps variable names to
-- subexpressions.
matchPat ::  Pat -> Expr -> Maybe (M.Map String Expr)
matchPat (VarP s) e = Just $ M.fromList [(s, e)]
matchPat (IntP i1) (IntE i2)
  | i1 == i2  = Just M.empty
  | otherwise = Nothing
matchPat (ConP np ps) (ConE ne es)
  | np /= ne  = Nothing
  | otherwise = M.unions <$> (sequence $ zipWith matchPat ps es)
matchPat _ _ = Nothing


heatAtom :: Atom -> JoinM ([Def], [Atom])

heatAtom (DelayA d (Proc [DelayA d' p])) =
  heatAtom $ DelayA (d+d') p

heatAtom (DelayA d (Proc [a])) = do
  (ds, as) <- heatAtom a
  return (ds, [DelayA d . Proc $ as])

heatAtom (DelayA d (Proc as)) = do
  let as' = map (DelayA d . Proc . (:[])) as
  (dss, ass) <- unzip <$> mapM heatAtom as'
  return (concat dss, concat ass)

heatAtom InertA        = return ([], [])

heatAtom m@(MsgA _ _)  = return ([], [m])

heatAtom (DefA ds p) = do
  let ivs = (S.toList . S.unions $ map definedVars ds)
  sigma <- M.fromList . zipWith (\l r -> (l, VarE $ l ++ r)) ivs <$> (getFreshNames $ length ivs)
  return (subst sigma <$> ds, pAtoms $ sigma `subst` p)

heatAtom m@(MatchA e ps) =
  let (pats, procs) = unzip ps
      procs' = zipWith (\proc -> maybe Nothing (\sigma -> Just $ subst sigma <$> (pAtoms proc)))
                       procs
                       (flip matchPat e <$> pats)
      firstProc = getFirst . mconcat $ First <$> procs'
  in case firstProc of
       Nothing -> error $ "Pattern match is not exhaustive"
       Just proc -> return ([], proc)

{-
 - At first we mark all the MsgP:s in the context, as well as the exported names.
 - Then we mark the defs that might be activated by the marked atoms
 - and mark all the MsgP:s, that are potentially produced by these
 - marked defs. Then we iterate, until no new defs or MsgP:s are marked.
 -}
garbageCollect :: JoinM ()
garbageCollect = do
  markedNames <- S.unions . map freeVars <$> getAtoms
  exportedNames <- getExportedNames
  liveDefs <- gc' (markedNames `S.union` exportedNames) []
  replaceDefs liveDefs
  -- Remove all exported names that aren't represented in any live defs
  setExportedNames $ exportedNames `S.intersection` (S.unions . map definedVars $ liveDefs)
  where
    defMatched :: Def -> S.Set String -> Bool
    defMatched (ReactionD js delay _) nms = and $ map (\(VarJ nmJ _) -> S.member nmJ nms) js
    defMatched (LocationD _ _ _) _ = True

    gc' :: S.Set String -> [Def] -> JoinM [Def]
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
