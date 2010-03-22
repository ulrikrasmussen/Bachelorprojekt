{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--module Interpreter(runInterpreter, Context) where
module Interpreter where

import Language

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Arrow (first,second)

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

--instance Applicative (State Context) where
--    pure = return
--    (<*>) = ap

newtype JoinM a = J { runJoinM :: State Context a  }
    deriving (Monad, MonadState Context, Functor)

data Context = Context { cDefs :: [Def]
                       , cAtoms :: [Atom]
                       , cFreshNames :: [String]
                       , cLog :: [String]
                       }
 deriving (Eq)

instance Show Context where
  show context = "Defs:\n\t" ++ (concat . intersperse "\n\t" $ map show (cDefs context))
               ++ "\n\nAtoms:\n\t" ++ (concat . intersperse "\n\t" $ map show (cAtoms context))
               ++ "\n\nLog:\n\t" ++ (concat . reverse . intersperse "\n\t" $ (cLog context))

class (Monad m) => MonadJoin m where
  getDefs :: m [Def]
  rmDef :: Def -> m ()
  putDef :: Def -> m ()
  getAtoms :: m [Atom]
  rmAtom :: Atom -> m ()
  putAtom :: Atom -> m ()
  replaceDefs :: [Def] -> m ()
  debug :: String -> m ()
  cleanDebug :: m()

  getFreshNames :: Int -> m [String]

instance MonadJoin JoinM where
  getDefs = gets cDefs
  rmDef def = modify $ \s -> s { cDefs = delete def $ cDefs s }
  putDef def = modify $ \s -> s { cDefs = def : cDefs s }

  getAtoms = gets cAtoms
  rmAtom atm = modify $ \s -> s { cAtoms = delete atm $ cAtoms s }
  putAtom atm = modify $ \s -> s { cAtoms = atm : cAtoms s }

  replaceDefs defs = modify $ \s -> s {cDefs = defs}

  debug msg = modify $ \s -> s{cLog = msg : (cLog s)}
  cleanDebug = modify $ \s -> s{cLog = []}

  getFreshNames n = do
    (fns, fns') <- gets (splitAt n . cFreshNames)
    modify $ \s -> s {cFreshNames = fns'}
    return fns

initContext :: [Def] -> [Atom] -> Context
initContext ds as = Context ds as ['#' : show i | i <- [1..]] []

runInterpreter :: Proc -> Int -> Context
runInterpreter (Proc atms) n = execState (runJoinM $ interp n) (initContext [] atms)

interp :: (MonadJoin m, Functor m) => Int -> m ()
interp 0 = return ()
interp n = {-# SCC "interp" #-} do
  garbageCollect
  atoms <- getAtoms
  defs <- getDefs
  mapM heatAtom atoms
  mapM applyReaction defs
  atoms' <- getAtoms
  defs' <- getDefs
  if (n > 0) then interp $ n - 1
             else when (atoms /= atoms' || defs /= defs') $ interp n

applyReaction :: (MonadJoin m, Functor m) => Def -> m ()
applyReaction d@(ReactionD js p) =
  sequence <$> mapM matchJoin js >>=
  maybe (return ()) (\xs ->
    do let (sigma, atoms) = first M.unions $ unzip xs
       mapM_ rmAtom atoms
       mapM_ putAtom $ (subst sigma <$> (pAtoms p))
    )

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
 - At first we mark all the MsgP:s in the context.
 - Then we mark the defs that might be activated by the marked atoms
 - and mark all the MsgP:s, that are potentially produced by these
 - marked defs. Then we iterate, until no new defs or MsgP:s are marked.
 -}
garbageCollect :: (MonadJoin m, Functor m) => m ()
garbageCollect = {-# SCC "garbageCollect" #-} do
  cleanDebug
  --debug "running gc"
  markedNames <- S.unions . map freeVars <$> getAtoms
  --debug $ "markedNames: " ++ (show markedNames)
  liveDefs <- gc' markedNames []
  --debug $ "\n\tliveDefs: " ++ (show liveDefs)
  mapM putDef liveDefs
  replaceDefs liveDefs
  where
    defMatched :: Def -> S.Set String -> Bool
    defMatched (ReactionD js _) nms = and $ map (\(VarJ nmJ _) -> S.member nmJ nms) js

    gc' :: (MonadJoin m, Functor m) => S.Set String -> [Def] -> m [Def]
    gc' markedNames defs = do
      --debug $ "gc'( \n\t\t" ++ (show markedNames ) ++ "   , \n\t\t" ++ (show defs)
      markedDefs <- (mapM (\def -> if defMatched def markedNames
        then do
            rmDef def
            return(Just def)
        else return Nothing ) =<< getDefs)
      markedDefs <- return $ catMaybes markedDefs
      --debug $ "markedDefs: " ++ (show markedDefs)
      -- add the set of produceable names to the marked atoms
      let produceableAtoms = S.unions $ map freeVars markedDefs
      if not $ produceableAtoms == S.empty
        then gc' (markedNames `S.union` produceableAtoms)
                 (defs `union` markedDefs)
        else return (defs `union` markedDefs)
