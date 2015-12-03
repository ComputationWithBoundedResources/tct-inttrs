{- | Provides a representation for (top-level) integer term rewrite systems.

See <http://aprove.informatik.rwth-aachen.de/help_new/inttrs.html> for details.

Example:
@
  outer(x, r)       -> inner(1, 1, x, r)       [ x >= 0 && r <= 100000]
  inner(f, i, x, r) -> inner(f + i, i+1, x, r) [ i <= x ]
  inner(f, i, x, r) -> outer(x - 1, r + f)     [ i > x ]
  g(cons(x, xs), y) -> g(xs, y + 1)
  h(xs, y)          -> h(cons(0, xs), y - 1)   [ y  > 0]
@

Remarks:
  * no arithmetic expressions on the lhs of a rule
  * no variable section in contrast to TRS (WST) format; non-arithmetic constants require parenthesis, ie, @c()@

Assumption:
  * no negation, numbers are parsed as naturals (machine-dependent @Int@)
  * system is well-typed wrt. to @Univ@ and @Nat@ type
  * arithmetic expressions not on top position

Open Questions:
  * how to specify starting location? dependency-analysis? currently minimal sybmbol
-}
module Tct.IntTrs where


import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromJust, fromMaybe)
import qualified Data.Set                     as S

import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import           Tct.Core
import           Tct.Core.Common.Parser       ((<|>))
import qualified Tct.Core.Common.Parser       as PS
import           Tct.Core.Common.Pretty       (Doc, Pretty, pretty)
import qualified Tct.Core.Common.Pretty       as PP
import           Tct.Core.Common.Xml          (Xml, XmlContent, toXml)
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T
import           Tct.Core.Processor.Transform (transform)

import qualified Text.Parsec.Expr             as PE

import qualified Data.Rewriting.Problem       as R
import qualified Data.Rewriting.Rule          as R
import qualified Data.Rewriting.Rules         as RS
import qualified Data.Rewriting.Term          as T

import           Tct.Its                      (Its)
import qualified Tct.Its.Data.Problem         as Its (initialise)
import           Tct.Trs                      (Trs)
import qualified Tct.Trs                      as Trs (runtime)
import qualified Tct.Trs.Data.Problem         as Trs (fromRewriting)


data IFun           = Add | Mul | Sub | Nat Int               deriving (Eq, Ord, Show)
type ITerm f v      = T.Term IFun v
data CFun           = Lte | Gte | Lt | Eq | Gt                deriving (Eq, Ord, Show, Enum, Bounded)
data Constraint f v = Constraint (ITerm f v) CFun (ITerm f v) deriving (Eq, Ord, Show)

data Fun f     = UFun f | IFun IFun deriving (Eq, Ord, Show)
type Term f v  = T.Term (Fun f) v

data Rule f v = Rule
  { rule        :: R.Rule (Fun f) v
  , constraints :: [Constraint f v ] }
 deriving (Eq, Ord, Show)

rename :: (v -> v') -> Rule f v -> Rule f v'
rename var (Rule rl cs) = Rule (R.rename var rl) (k `fmap` cs)
  where k (Constraint t1 op t2) = Constraint (T.rename var t1) op (T.rename var t2)

vars :: Ord v => Rule f v -> [v]
vars (Rule (R.Rule lhs rhs) cs) = S.toList $ S.fromList $ (T.varsDL lhs <> T.varsDL rhs <> cvarsDL cs) []
  where cvarsDL = foldr (mappend . k) id where k (Constraint t1 _ t2) = T.varsDL t1 <> T.varsDL t2

type Rules f v = [Rule f v]

iop :: f -> T.Term f v -> T.Term f v -> T.Term f v
iop f x y = T.Fun f [x,y]

add, mul, sub :: T.Term IFun v -> T.Term IFun v -> T.Term IFun v
add = iop Mul
mul = iop Add
sub = iop Sub

int :: Int -> ITerm f v
int i = T.Fun (Nat i) []

iRep :: IFun -> String
iRep Mul     = "*"
iRep Add     = "+"
iRep Sub     = "-"
iRep (Nat i) = show i

cRep :: CFun -> String
cRep Lte = "<="
cRep Lt  = "<"
cRep Eq  = "="
cRep Gt  = ">"
cRep Gte = ">="


--- * parser ---------------------------------------------------------------------------------------------------------

type Parser = PS.Parsec String ()
-- type Parser = PS.ParsecT Text () Identity

pVar :: Parser (T.Term f String)
pVar =  T.Var `fmap` PS.identifier

comma :: Parser String
comma = PS.symbol ","

pTerm :: Parser (T.Term (Fun String) String)
pTerm =
  PS.try (T.Fun <$> (UFun <$> PS.identifier) <*> PS.parens (pTerm `PS.sepBy` comma))
  <|> T.map IFun id <$> pITerm

pITerm :: Parser (ITerm String String)
pITerm = PE.buildExpressionParser table (PS.parens pITerm <|> pNat <|> pVar)
  where
    table =
      -- [ [ unary "-" neg ]
      [ [ binaryL (iRep Mul) mul PE.AssocLeft]
      , [ binaryL (iRep Add) add PE.AssocLeft, binaryL (iRep Sub) sub PE.AssocLeft] ]
    -- unary f op   = PE.Prefix (PS.reserved f *> return op)
    binaryL f op = PE.Infix (PS.reserved f *> return op)

pCFun :: Parser CFun
pCFun = PS.choice $ k  `fmap` [(minBound :: CFun)..]
  where k c = PS.try (PS.symbol (cRep c)) *> return c

pConstraint :: Parser (Constraint String String)
pConstraint = Constraint <$> pITerm <*> pCFun <*> pITerm

pConstraints :: Parser [Constraint String String]
pConstraints = PS.option [] (brackets $ pConstraint `PS.sepBy1` PS.symbol "&&")
  where brackets p = PS.symbol "[" *> p <* PS.symbol "]"

pRule :: Parser (Rule String String)
pRule = Rule <$> k <*> pConstraints
  where k = R.Rule <$> (pTerm <* PS.symbol "->") <*> pTerm

pRules :: Parser (Rules String String)
pRules = PS.many1 pRule

pNat :: Parser (ITerm String String)
pNat = int <$> PS.nat

--- * type inference -------------------------------------------------------------------------------------------------

type Signature f = M.Map (Fun f) Int

signature :: Ord f => Rules f v -> Signature f
signature = M.fromList . RS.funs . fmap (rmap T.withArity . rule)
  where rmap k (R.Rule lhs rhs) = R.Rule (k lhs) (k rhs)

-- | @vars r = (univvars, numvars)@. Variables in constraints and arithmetic expressions are @numvars@, all other wars
-- are @univvars@.
tvars :: Ord v => Rule f v -> ([v],[v])
tvars (Rule (R.Rule lhs rhs) cs) = (S.toList $ tvarS `S.difference` nvarS, S.toList nvarS)
  where
    tvarS  = S.fromList $ (T.varsDL lhs <> T.varsDL rhs) []
    nvarS  = S.fromList $ (nvars1 lhs <> nvars1 rhs <> nvars2 cs) []
    nvars1 (T.Fun (UFun _) ts) = foldr (mappend . nvars1) id ts
    nvars1 (T.Fun (IFun _) ts) = foldr (mappend . T.varsDL) id ts
    nvars1 _                   = id
    nvars2                     = foldr (mappend . k) id where k (Constraint t1 _ t2) = T.varsDL t1 <> T.varsDL t2

data Type = Univ | Num | Alpha Int
  deriving (Eq, Ord, Show)

data TypeDecl = TypeDecl
  { inputTypes :: [Type]
  , outputType :: Type }
  deriving Show

type Typing f = M.Map (Fun f) TypeDecl

type Unify = [(Type, Type)]

data Environment f v = Environment
  { variables_    :: M.Map v Type
  , declarations_ :: M.Map (Fun f) TypeDecl }

newtype InferM f v a = InferM { runInferM :: RWST (Environment f v) Unify Int (Except String) a }
  deriving
    (Functor, Applicative, Monad
    , MonadWriter Unify
    , MonadReader (Environment f v)
    , MonadState Int
    , MonadError String)

(=~) :: Type -> Type -> InferM f v ()
a =~ b = tell [(a,b)]

typeFun :: Fun f -> Type
typeFun (UFun _) = Univ
typeFun (IFun _) = Num

-- MS: (almost) standard type inference
-- we already know the type output type of function symbols, and the type of variables in arithmetic expressions
-- still we have to type the rest of the variables
infer :: (Show v, Show f, Ord f, Ord v) => Rules f v -> Either String (Typing f)
infer rs = do
  (decs,_,up) <- runExcept $ runRWST (runInferM inferM) (Environment M.empty M.empty) 0
  subst       <- unify up
  return $ instDecl subst `fmap` decs
  where

    lookupEnv v  = asks variables_    >>= maybe (throwError $ "undefined var: " ++ show v) return . M.lookup v
    lookupDecl f = asks declarations_ >>= maybe (throwError $ "undefined fun: " ++ show f) return . M.lookup f

    fresh = do { i <- get; put $! i + 1; return $ Alpha i}

    inferM = do
      tdecls <- M.traverseWithKey initDecl (signature rs)
      local (\e -> e {declarations_ = tdecls}) $ do
        forM_ rs typeRule
        return tdecls

    instDecl subst (TypeDecl its ot) = TypeDecl [apply subst t | t <- its] (apply subst ot)

    initDecl (UFun _) i = TypeDecl <$> mapM (const fresh) [1..i] <*> pure Univ
    initDecl (IFun _) i = TypeDecl <$> mapM (const fresh) [1..i] <*> pure Num

    typeRule rl = do
      let
        (uvars,nvars) = tvars rl
        env1 = foldr (`M.insert` Num) M.empty nvars
      env2 <- foldM (\ e v -> flip (M.insert v) e `liftM` fresh) env1 uvars
      local (\e -> e {variables_ = env2}) $ do
        l <- typeTerm (R.lhs $ rule rl)
        r <- typeTerm (R.rhs $ rule rl)
        l =~ r

    typeTerm (T.Var v)    = lookupEnv v
    typeTerm (T.Fun f ts) = do
      TypeDecl its ot <- lookupDecl f
      its' <- forM ts typeTerm
      sequence_ [ t1 =~ t2 | (t1,t2) <- zip its' its ]
      return ot

    apply subst t = t `fromMaybe` M.lookup t subst
    s1 `compose` s2 = (apply s2 `M.map` s1) `M.union` s2 -- left-biased

    unify []           = pure M.empty
    unify ((t1,t2):ts) = case (t1,t2) of
      (Univ, Num)  -> throwError "type inference error"
      (Num, Univ)  -> throwError "type inference error"
      _ | t1 == t2 -> unify ts
      _            -> compose s `fmap` unify [(s `apply` t3,s `apply` t4) | (t3,t4) <- ts]
        where s = if t1 > t2 then M.insert t1 t2 M.empty else M.insert t2 t1 M.empty -- MS: we want to replace alphas if possible


--- * transformations ------------------------------------------------------------------------------------------------

toTrs' :: Typing String -> Rules String String  -> Either String Trs
toTrs' tys rs = Trs.fromRewriting =<< toRewriting' tys rs

toRewriting' :: Ord f => Typing f -> Rules f v -> Either String (R.Problem f v)
toRewriting' tys rs = case filterUniv tys rs of
  Left s    -> Left s
  Right trs -> Right R.Problem
    { R.startTerms = R.BasicTerms
    , R.strategy   = R.Innermost
    , R.theory     = Nothing
    , R.rules      = R.RulesPair
      { R.weakRules   = []
      , R.strictRules = trs }
    , R.variables  = RS.vars trs
    , R.symbols    = RS.funs trs
    , R.comment    = Just "intTrs2Trs"}

filterUniv :: Ord f => Typing f -> Rules f v -> Either String [R.Rule f v]
filterUniv tys = traverse (filterRule . rule)
  where
    filterRule (R.Rule lhs rhs) = R.Rule <$> filterTerm lhs <*> filterTerm rhs

    filterTerm (T.Fun f@(UFun g) ts) = T.Fun g <$> (filterEach f ts >>= traverse filterTerm)
    filterTerm (T.Fun _ _)           = throwError "filterUniv: type inference error"
    filterTerm (T.Var v)             = pure (T.Var v)

    filterEach f ts = maybe
      (throwError "filterUniv: undefined function symbol")
      (pure . fst . unzip . filter ((/= Num). snd) . zip ts . inputTypes)
      (M.lookup f tys)


toIts' :: Typing String -> Rules String String -> Either String Its
toIts' tys rs = (asItsRules . padRules . renameRules . filter rhsIsNotVar) `fmap` filterNum tys rs
  where
    rhsIsNotVar = not . T.isVar . R.rhs . rule

    padRules rls = padRule (mx rls) `fmap` rls
      where mx = maximum . M.elems . signature
    padRule mx (Rule (R.Rule lhs rhs) cs) = Rule (R.Rule (padTerm mx lhs) (padTerm mx rhs)) cs
    padTerm mx (T.Fun (UFun f) ts) = T.Fun (UFun f) $ zip' ts (take mx fresh)
    padTerm _ _                    = error "a"
    zip' (t:ts) (_:ss) = t: zip' ts ss
    zip' ts (s:ss)     = T.Var s: zip' ts ss
    zip' _ _           = []

    renameRules rls = renameRule `fmap` rls
    renameRule r    = rename (find $ zip (vars r) fresh) r

    fresh  = (mappend "x" . show) `fmap` [(0::Int)..]
    find as k = fromJust $ lookup k as

    -- MS: TODO: how to specify starting location?
    UFun l0 = minimum $ foldr T.funsDL [] $ RS.lhss $ rule `fmap` rs

    asItsRules rls = Its.initialise ([l0],[],asItsRule `fmap` rls)
    asItsRule (Rule (R.Rule lhs rhs) cs) = undefined -- Rule lhs' rhs' cs'

-- | Restricts to 'Num' type
-- If successfull, 'UFun' appear at only at root positions, and 'IFun' appear only below root.
filterNum :: Ord f => Typing f -> Rules f v -> Either String (Rules f v)
filterNum tys = traverse filterRule
  where
    filterRule (Rule (R.Rule lhs rhs) cs) = Rule <$> (R.Rule <$> filterRoot lhs <*> filterRoot rhs) <*> pure cs

    filterRoot (T.Fun f@(UFun _) ts) = T.Fun f <$> (filterEach f ts >>= validate)
    filterRoot (T.Fun _ _)           = throwError "filterNum: arithmetic expression at root position"
    filterRoot (T.Var v)             = pure (T.Var v)

    filterEach f ts = maybe
      (throwError "filterUniv: undefined function symbol")
      (pure . fst . unzip . filter ((== Num). snd) . zip ts . inputTypes)
      (M.lookup f tys)

    validate ts = if all p ts then pure ts else throwError "filterNum: type inference error"
      where p t = case t of {(T.Fun (UFun _) _) -> False; _ -> True}




--- * TcT integration ------------------------------------------------------------------------------------------------
--- ** Problem -------------------------------------------------------------------------------------------------------
-- MS: integration with TcT

data Problem f v = Problem { rules :: Rules f v } deriving Show

ppRules :: (f -> Doc) -> (v -> Doc) -> Rules f v -> Doc
ppRules fun var rs = PP.vcat (ppRule fun var `fmap` rs)

ppRule :: (f -> Doc) -> (v -> Doc) -> Rule f v -> Doc
ppRule fun var (Rule (R.Rule lhs rhs) cs) =
  PP.hang 2 $ ppTerm fun var lhs PP.<+> PP.string "->" PP.</> ppTerm fun var rhs PP.<+> ppConstraints var cs

ppTerm :: (f -> Doc) -> (v -> Doc) -> Term f v -> Doc
ppTerm fun var (T.Fun (UFun f) ts) = fun f <> PP.tupled (ppTerm fun var `fmap` ts)
ppTerm fun var (T.Fun (IFun f) ts) = case f of
  Nat i -> PP.int i
  op    -> k op
  where k op = PP.encloseSep PP.lparen PP.rparen (PP.space <> PP.text (iRep op) <> PP.space) (ppTerm fun var `fmap` ts)
ppTerm _   var (T.Var v)           = var v

ppConstraints :: (v -> Doc) -> [Constraint f v] -> Doc
ppConstraints var cs = PP.encloseSep PP.lbracket PP.rbracket (PP.text " && ") (ppConstraint var `fmap` cs)

ppConstraint :: (v -> Doc) -> Constraint f v -> Doc
ppConstraint var (Constraint lhs eq rhs) = ppITerm lhs PP.<+> PP.text (cRep eq) PP.<+> ppITerm rhs
  where
    k op ts = PP.encloseSep PP.lparen PP.rparen (PP.space <> PP.text (iRep op) <> PP.space) (ppITerm `fmap` ts)
    ppITerm (T.Fun f ts) = case f of
      Nat i -> PP.int i
      op    -> k op ts
    ppITerm (T.Var v)           = var v

xmlRules :: (f -> XmlContent) -> (v -> XmlContent) -> Rules f v -> XmlContent
xmlRules _ _ _ = Xml.empty

instance (Pretty f, Pretty v) =>  Pretty (Problem f v) where
  pretty (Problem rs) = PP.text "Rules" PP.<$$> PP.indent 2 (ppRules PP.pretty PP.pretty rs)

instance (Xml f, Xml v) =>  Xml (Problem f v) where
  toXml (Problem rs) = Xml.elt "inttrs" [ Xml.elt "rules" [xmlRules toXml toXml rs] ]

instance {-# OVERLAPPING #-} Xml (Problem String String) where
  toXml (Problem rs) = Xml.elt "inttrs" [ Xml.elt "rules" [xmlRules Xml.text Xml.text rs] ]


--- ** Config --------------------------------------------------------------------------------------------------------

type IntTrs       = Problem String String
type IntTrsConfig = TctConfig IntTrs

parse :: String -> Either String IntTrs
parse s = case PS.parse pRules "" s of
  Left e  -> Left (show e)
  Right p -> Right (Problem p)

parseIO :: FilePath -> IO (Either String IntTrs)
parseIO fn = parse <$> readFile fn

intTrsConfig :: IntTrsConfig
intTrsConfig = defaultTctConfig parseIO

runIntTrs :: Declared IntTrs IntTrs => IntTrsConfig -> IO ()
runIntTrs = runTct

toTrs :: Strategy IntTrs Trs
toTrs = transform "We extract a TRS fragment from the current int-TRS problem:"
  (\p -> infer (rules p) >>= \tp -> toTrs' tp (rules p))

withTrs :: Strategy Trs Trs -> Strategy IntTrs Trs
withTrs st = toTrs .>>> st

-- TODO: MS: move to tct-trs
trsArg :: Declared Trs Trs => T.Argument 'T.Required (Strategy Trs Trs)
trsArg = T.strat "trs" ["This argument specifies the trs strategy to apply."]

intTrsDeclarations :: Declared Trs Trs => [StrategyDeclaration IntTrs IntTrs]
intTrsDeclarations =
  [ T.SD $ T.declare "withTrs" ["Solve with TRS."] (OneTuple $ trsArg `optional` Trs.runtime) $ \st -> withTrs st .>>> close ]

