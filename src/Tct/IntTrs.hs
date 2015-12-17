{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  * there is a unique start location, ie., exists ONE rule with unique function symbol on lhs
-}
module Tct.IntTrs
  (
  -- * Transformation
  toTrs' , toIts' , infer
  , parseRules, parse, parseIO
  , prettyPrintRules
  , putTrs, putIts
  -- * Tct Integration
  , IntTrs, IntTrsConfig
  , rules
  , runIntTrs, intTrsConfig
  , toTrs, toIts, withTrs, withIts, withBoth, intTrsDeclarations
  ) where


import           Control.Applicative          ((<|>))
import           Control.Monad.Except
import           Control.Monad.RWS.Strict
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromJust, fromMaybe)
import qualified Data.Set                     as S
import           System.IO                    (hPutStrLn, stderr)

import qualified Tct.Common.Polynomial        as P
import qualified Tct.Common.Ring              as R
import           Tct.Core
import           Tct.Core.Common.Pretty       (Doc, Pretty, pretty)
import qualified Tct.Core.Common.Pretty       as PP
import           Tct.Core.Common.Xml          (Xml, XmlContent, toXml)
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T
import           Tct.Core.Processor.Transform (transform)

import qualified Text.Parsec                  as PS
import qualified Text.Parsec.Expr             as PE
import qualified Text.Parsec.Language         as PL
import qualified Text.Parsec.Token            as PT

import qualified Data.Rewriting.Problem       as R
import qualified Data.Rewriting.Rule          as R
import qualified Data.Rewriting.Rules         as RS
import qualified Data.Rewriting.Term          as T

-- TODO: MS: better export list for Its
import           Tct.Its                      (Its)
import qualified Tct.Its.Data.Problem         as Its (Its (..), initialise, domain)
import qualified Tct.Its.Data.Types           as Its (AAtom (..), ARule (..), ATerm (..), rules)
import qualified Tct.Its.Strategies           as Its (runtime)
import           Tct.Trs                      (Trs)
import qualified Tct.Trs                      as Trs (runtime)
import qualified Tct.Trs.Data.Problem         as Trs (fromRewriting)


data IFun           = Add | Mul | Sub | Nat Int               deriving (Eq, Ord, Show)
type ITerm f v      = T.Term IFun v
data CFun           = Lte | Lt | Eq | Gt | Gte                 deriving (Eq, Ord, Show, Enum, Bounded)
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

-- vars :: Ord v => Rule f v -> [v]
-- vars (Rule (R.Rule lhs rhs) cs) = S.toList $ S.fromList $ (T.varsDL lhs <> T.varsDL rhs <> cvarsDL cs) []
--   where cvarsDL = foldr (mappend . k) id where k (Constraint t1 _ t2) = T.varsDL t1 <> T.varsDL t2

varsL :: Ord v => Rule f v -> [v]
varsL (Rule (R.Rule lhs rhs) cs) = (T.varsDL lhs <> T.varsDL rhs <> cvarsDL cs) []
  where cvarsDL = foldr (mappend . k) id where k (Constraint t1 _ t2) = T.varsDL t1 <> T.varsDL t2

termToITerm :: ErrorM m => Term f v -> m (ITerm f v)
termToITerm (T.Fun (UFun _) _)  = throwError "termToIterm: not an iterm"
termToITerm (T.Fun (IFun f) ts) = T.Fun f <$> traverse termToITerm ts
termToITerm (T.Var v)           = pure (T.Var v)

type Rules f v = [Rule f v]

iop :: f -> T.Term f v -> T.Term f v -> T.Term f v
iop f x y = T.Fun f [x,y]

add, mul, sub :: T.Term IFun v -> T.Term IFun v -> T.Term IFun v
add = iop Add
mul = iop Mul
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

type ErrorM = MonadError String


--- * parser ---------------------------------------------------------------------------------------------------------

tok :: PT.TokenParser st
tok = PT.makeTokenParser
  PL.emptyDef
    { PT.commentStart    = "(*)"
    , PT.commentEnd      = "*)"
    , PT.nestedComments  = False
    , PT.identStart      = PS.letter
    , PT.identLetter     = PS.alphaNum <|> PS.oneOf "_'"
    , PT.reservedOpNames = cRep `fmap` [(minBound :: CFun)..]
    , PT.reservedNames   = []
    , PT.caseSensitive   = True }

pIdentifier, pComma :: Parser String
pIdentifier = PT.identifier tok
pComma      = PT.comma tok

pSymbol :: String -> Parser String
pSymbol = PT.symbol tok

pParens :: Parser a -> Parser a
pParens = PT.parens tok

pReservedOp :: String -> Parser ()
pReservedOp = PT.reservedOp tok

pNat :: Parser Int
pNat = fromIntegral <$> PT.natural tok

type Parser = PS.Parsec String ()
-- type Parser = PS.ParsecT Text () Identity

pVar :: Parser (T.Term f String)
pVar =  T.Var `fmap` pIdentifier

pTerm :: Parser (T.Term (Fun String) String)
pTerm =
  PS.try (T.Fun <$> (UFun <$> pIdentifier) <*> pParens (pTerm `PS.sepBy` pComma))
  <|> T.map IFun id <$> pITerm

pITerm :: Parser (ITerm String String)
pITerm = PE.buildExpressionParser table (pParens pITerm <|> (int <$> pNat) <|> pVar)
  where
    table =
      -- [ [ unary "-" neg ]
      [ [ binaryL (iRep Mul) mul PE.AssocLeft]
      , [ binaryL (iRep Add) add PE.AssocLeft, binaryL (iRep Sub) sub PE.AssocLeft] ]
    -- unary f op   = PE.Prefix (PS.reserved f *> return op)
    binaryL f op = PE.Infix (pSymbol f *> return op)

pCFun :: Parser CFun
pCFun = PS.choice $ k  `fmap` [(minBound :: CFun)..]
  where k c = PS.try (pReservedOp $ cRep c) *> return c

pConstraint :: Parser (Constraint String String)
pConstraint = Constraint <$> pITerm <*> pCFun <*> pITerm

pConstraints :: Parser [Constraint String String]
pConstraints = PS.option [] (brackets $ pConstraint `PS.sepBy1` pSymbol "&&")
  where brackets p = pSymbol "[" *> p <* pSymbol "]"

pRule :: Parser (Rule String String)
pRule = Rule <$> k <*> pConstraints
  where k = R.Rule <$> (pTerm <* pSymbol "->") <*> pTerm

pRules :: Parser (Rules String String)
pRules = PS.many1 pRule

-- | Parser for intTrs rules
parseRules :: Parser (Rules String String)
parseRules = pRules


--- * type inference -------------------------------------------------------------------------------------------------

type Signature f = M.Map (Fun f) Int

signature :: Ord f => Rules f v -> Signature f
signature = M.fromList . RS.funs . fmap (rmap T.withArity . rule)
  where rmap k (R.Rule lhs rhs) = R.Rule (k lhs) (k rhs)

fsignature :: Ord f => Rules f v -> Signature f
fsignature = M.filterWithKey (\k _ -> isUTerm k) . signature
  where isUTerm f = case f of {(UFun _) -> True; _ -> False}

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
        --  TODO: make this more explicit
        where s = if t1 > t2 then M.insert t1 t2 M.empty else M.insert t2 t1 M.empty -- MS: we want to replace alphas if possible


--- * transformations ------------------------------------------------------------------------------------------------

toTrs' :: Typing String -> Rules String String  -> Either String Trs
toTrs' tys rs = Trs.fromRewriting =<< toRewriting' tys rs

-- | Transforms the inttrs rules to a Trs. If successfull the problem is rendered to the standard output, otherwise
-- the error is rendered to standard error.
putTrs :: Rules String String -> IO ()
putTrs rs = case infer rs >>= flip toRewriting' rs of
  Left err  -> hPutStrLn stderr err
  Right trs -> putStrLn $ PP.display $ R.prettyWST pretty pretty trs

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


-- MS: assumes top-level rewriting; lhs and rhs is following form
-- f(aexp1,...,aexp2) -> g(aexp1,...,aexp2)
toIts' :: ErrorM m => Typing String -> Rules String String -> m Its
toIts' tys rs = do
  l0 <- findStart rs
  rs' <- filterNum tys rs
  asItsRules l0 . padRules . renameRules $ filter (not . rhsIsVar) rs'
  where
    rhsIsVar = T.isVar . R.rhs . rule

    asItsRules l0 rls = toItsRules rls >>= \rls' -> return $ Its.initialise ([l0],[],rls')

-- | Transforms the inttrs rules to a Its. If successfull the problem is rendered to the standard output, otherwise
-- the error is rendered to standard error.
putIts :: Rules String String -> IO ()
putIts rs = case infer rs >>= flip toIts' rs of
  Left err  -> hPutStrLn stderr err
  Right its -> putStrLn $ PP.display $ ppKoat its

-- TODO: MS: move to tct-its
ppKoat :: Its -> Doc
ppKoat its = PP.vcat
  [ PP.text "(GOAL COMPLEXITY)"
  , PP.text "(STARTTERM (FUNCTIONSYMBOL "<> PP.text (Its.fun $ Its.startterm_ its) <> PP.text "))"
  , PP.text "(VAR " <> PP.hsep (PP.text `fmap` Its.domain its) <> PP.text ")"
  , PP.text "(RULES "
  , PP.indent 2 $ PP.vcat (pp `fmap` Its.rules (Its.irules_ its))
  , PP.text ")" ]
  where
    pp (Its.Rule lhs rhss cs) =
      PP.pretty lhs
        <> PP.text " -> "
        <> PP.text "Com_" <> PP.int (length rhss) <> PP.tupled' rhss
        <> if null cs then PP.empty else PP.encloseSep PP.lbracket PP.rbracket (PP.text " /\\ ") (PP.pretty `fmap` cs)


renameRules :: Rules f String -> Rules f String
renameRules = fmap renameRule

renameRule :: Rule f String -> Rule f String
renameRule r = rename mapping r
  where
    mapping v = fromJust . M.lookup v . fst $ foldl k (M.empty, freshvars) (varsL r)
    k (m,i) v = if M.member v m then (m,i) else (M.insert v (head i) m, tail i)

freshvars :: [String]
freshvars  = (mappend "x" . show) `fmap` [(0::Int)..]

padRules :: Ord f => Rules f String -> Rules f String
padRules rls = padRule (mx rls) `fmap` rls
  where mx = maximum . M.elems . fsignature

padRule :: Int -> Rule f String -> Rule f String
padRule mx (Rule (R.Rule lhs rhs) cs) = Rule (R.Rule (padTerm mx lhs) (padTerm mx rhs)) cs

padTerm :: Int -> Term f String -> Term f String
padTerm mx (T.Fun (UFun f) ts) = T.Fun (UFun f) $ zip' ts (take mx freshvars)
  where
    zip' (s:ss) (_:rr) = s: zip' ss rr
    zip' []     rr     = T.Var `fmap` rr
    zip' _     []      = error "a"
padTerm _ _                    = error "a"

toItsRules :: (Ord v, ErrorM m) => Rules f v -> m [Its.ARule f v]
toItsRules = traverse toItsRule

toItsRule :: (Ord v, ErrorM m) => Rule f v -> m (Its.ARule f v)
toItsRule (Rule (R.Rule lhs rhs) cs) = Its.Rule <$> toItsTerm lhs <*> ((:[]) <$> toItsTerm rhs) <*> traverse toItsConstraint cs

toItsTerm :: (Ord v, ErrorM m) => Term f v -> m (Its.ATerm f v)
toItsTerm (T.Fun (UFun f) ts) = Its.Term f <$> traverse (termToITerm >=> itermToPoly) ts
toItsTerm _                   = throwError "toItsTerm: not a valid term"

itermToPoly :: (Ord v, ErrorM m) => ITerm f v -> m (P.Polynomial Int v)
itermToPoly (T.Fun (Nat n) [])       = pure $ P.constant n
itermToPoly (T.Fun f ts@(t1:t2:tss)) = case f of
  Add   -> R.bigAdd <$> traverse itermToPoly ts
  Mul   -> R.bigMul <$> traverse itermToPoly ts
  Sub   -> R.sub <$> itermToPoly t1 <*> (R.bigAdd `fmap` traverse itermToPoly (t2:tss))
  _     -> throwError "itermToPoly: not a valid term"
itermToPoly (T.Var v) = pure $ P.variable v
itermToPoly _ = throwError "itermToPoly: not a valid term"

toItsConstraint :: (Ord v, ErrorM m) => Constraint f v -> m (Its.AAtom v)
toItsConstraint (Constraint t1 cop t2) = case cop of
  Lte -> Its.Gte <$> itermToPoly t2 <*> itermToPoly t1
  Lt  -> Its.Gte <$> itermToPoly t2 <*> (R.add R.one <$> itermToPoly t1)
  Eq  -> Its.Eq  <$> itermToPoly t1 <*> itermToPoly t2
  Gt  -> Its.Gte <$> itermToPoly t1 <*> (R.add R.one <$> itermToPoly t2)
  Gte -> Its.Gte <$> itermToPoly t1 <*> itermToPoly t2

-- | Look for a single unique function symbol on the rhs.
findStart :: (ErrorM m, Ord f) => Rules f v -> m f
findStart rs = case S.toList $ roots R.lhs  `S.difference` roots R.rhs of
  [fun] -> pure fun
  _     -> throwError "Could not deduce a start symbol."
  where
    roots f            = foldr (k f) S.empty rs
    k f (Rule r _) acc = case f r of {T.Fun (UFun g) _ -> g `S.insert` acc; _ -> acc}

-- | Restricts to 'Num' type
-- If successfull, 'UFun' appears only at root positions, and 'IFun' appear only below root.
filterNum :: (ErrorM m, Ord f) => Typing f -> Rules f v -> m (Rules f v)
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

-- | Pretty printer for a list of rules.
prettyPrintRules :: (f -> Doc) -> (v -> Doc) -> Rules f v -> Doc
prettyPrintRules = ppRules

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

toIts :: Strategy IntTrs Its
toIts = transform "We extract a Its fragment from the current int-TRS problem:"
  (\p -> infer (rules p) >>= \tp -> toIts' tp (rules p))
  -- (\p -> infer (rules p) >>= \tp -> toIts' tp (rules p) >>= \its' -> trace (PP.display $ PP.pretty p) (trace (PP.display $ PP.pretty its') (return its')))

withIts :: Strategy Its Its -> Strategy IntTrs Its
withIts st = toIts .>>> st

withBoth :: Strategy Trs Trs -> Strategy Its Its -> Strategy IntTrs ()
withBoth st1 st2 = withProblem $ \p -> let tpM = infer (rules p) in fastest
  [ transform "a" (const $ tpM >>= \tp -> toTrs' tp (rules p)) .>>> st1 .>>> close
  , transform "a" (const $ tpM >>= \tp -> toIts' tp (rules p)) .>>> st2 .>>> close]

-- TODO: MS: move to tct-trs
trsArg :: Declared Trs Trs => T.Argument 'T.Required (Strategy Trs Trs)
trsArg = T.strat "trs" ["This argument specifies the trs strategy to apply."]

-- TODO: MS: move to tct-its
itsArg :: Declared Its Its => T.Argument 'T.Required (Strategy Its Its)
itsArg = T.strat "its" ["This argument specifies the trs strategy to apply."]

intTrsDeclarations :: (Declared Trs Trs, Declared Its Its) => [StrategyDeclaration IntTrs IntTrs]
intTrsDeclarations =
  [ T.SD $ T.declare "withTrs" ["Solve with TRS."]
      (OneTuple $ trsArg `optional` Trs.runtime)
      (\st -> withTrs st .>>> close)
  , T.SD $ T.declare "withIts" ["Solve with ITS."]
      (OneTuple $ itsArg `optional` Its.runtime)
      (\st -> withIts st .>>> close)
  , T.SD $ T.declare "withBoth" ["Solve with TRS and ITS."]
      (trsArg `optional` Trs.runtime, itsArg `optional` Its.runtime)
      (\st1 st2 -> withBoth st1 st2 .>>> close) ]

