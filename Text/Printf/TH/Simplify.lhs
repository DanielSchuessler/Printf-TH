
\begin{code}
module Text.Printf.TH.Simplify (simplify, Simplify) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Control.Monad (liftM)

class Simplify a where
    simplify :: a -> a

instance Simplify a => Simplify (Q a) where
    simplify = liftM simplify

instance Simplify a => Simplify [a] where
    simplify = map simplify

instance Simplify Exp where
    --simplify (VarE "GHC.Base:otherwise") = ConE ''True
    simplify (AppE e1 e2) = AppE (simplify e1) (simplify e2)

    simplify (InfixE me1 (VarE op) me2)
     = let (me1', me2') = (fmap simplify me1, fmap simplify me2)
       in f me1' op me2' 
          where 
              f (Just (LitE (IntegerL i1))) x (Just (LitE (IntegerL i2))) | x == '(>) =
                    if i1 > i2 then ConE 'True else ConE 'False
              f (Just (LitE (IntegerL i1))) x (Just (LitE (IntegerL i2))) | x ==  '(>=) =
                     if i1 >= i2 then ConE 'True
                                 else ConE 'False
              f (Just (LitE (IntegerL i1))) x ( Just (LitE (IntegerL i2))) | x ==  '(<) =
                     if i1 < i2 then ConE 'True
                                else ConE 'False
              f (Just (LitE (IntegerL i1))) x (Just (LitE (IntegerL i2))) | x == '(<=) =
                     if i1 <= i2 then ConE 'True
                                 else ConE 'False
              f (Just (LitE (IntegerL 0))) x (Just y) | x == '(+) = y
              f (Just y) x (Just (LitE (IntegerL 0))) | x == '(+) = y
              f (Just (LitE (IntegerL i1))) x (Just (LitE (IntegerL i2))) | x == '(+) =
                     LitE (IntegerL (i1 + i2))
              f (Just (LitE (IntegerL 0))) x (Just y) | x == '(-) = AppE (VarE 'negate) y
              f (Just y) x (Just (LitE (IntegerL 0)))  | x == '(-) = y
              f (Just (LitE (IntegerL i1))) x (Just (LitE (IntegerL i2))) | x == '(-) =
                     LitE (IntegerL (i1 - i2))
              f (Just (LitE (IntegerL 1))) x (Just y) | x == '(*) = y
              f (Just y) x (Just (LitE (IntegerL 1))) | x == '(*) = y
              f (Just (LitE (IntegerL i1))) x (Just (LitE (IntegerL i2))) | x == '(*) =
                     LitE (IntegerL (i1 * i2))
              f me1' _ me2' = InfixE me1' (VarE op) me2'
    simplify (CondE g t f) = let x = simplify g in
      f' x where f' (ConE y) | y == 'True = simplify t
                 f' (ConE y) | y == 'False = simplify f
                 f' g' = CondE g' (simplify t) (simplify f)
    simplify (LamE ps e) = LamE ps (simplify e)
    simplify (TupE es) = TupE (map simplify es)
    -- Should subst literals/vars?
    simplify (LetE ds e) = LetE (simplify ds) (simplify e)
    simplify (CaseE e ms) = CaseE (simplify e) (map simplify ms)
    simplify (DoE ss) = DoE (map simplify ss)
    simplify (CompE ss) = CompE (map simplify ss)
    simplify (ArithSeqE dd) = ArithSeqE (simplify dd)
    simplify (ListE es) = ListE (map simplify es)
    simplify (SigE e t) = SigE (simplify e) t
    simplify e = e

instance Simplify Dec where
    simplify (FunD f cs) = FunD f (map simplify cs)
    simplify (ValD p rhs ds) = ValD p (simplify rhs) (simplify ds)
    simplify (ClassD ctxt tycon [] tyvars ds) -- [] added correctly ? Marc
        = ClassD ctxt tycon [] tyvars (simplify ds)
    simplify (InstanceD ctxt typ ds) = InstanceD ctxt typ (simplify ds)
    simplify d = d

instance Simplify Body where
    simplify (NormalB e) = NormalB (simplify e)
    simplify (GuardedB ges)
     = GuardedB (map (\(e1, e2) -> (simplify e1, simplify e2)) ges)

instance Simplify Guard where
    simplify (NormalG e) = NormalG $ simplify e
    simplify (PatG l) = PatG $ map simplify l    

instance Simplify Range where
    simplify (FromR e) = FromR (simplify e)
    simplify (FromThenR e1 e2) = FromThenR (simplify e1) (simplify e2)
    simplify (FromToR e1 e2) = FromToR (simplify e1) (simplify e2)
    simplify (FromThenToR e1 e2 e3)
     = FromThenToR (simplify e1) (simplify e2) (simplify e3)

instance Simplify Stmt where
    simplify (BindS p e) = BindS p (simplify e)
    simplify (LetS ds) = LetS (simplify ds)
    simplify (NoBindS e) = NoBindS (simplify e)
    simplify (ParS _) = error "simplify[Statement]: ParS"

instance Simplify Match where
    simplify (Match p rhs ds) = Match p (simplify rhs) (simplify ds)

instance Simplify Clause where
    simplify (Clause ps rhs ds) = Clause ps (simplify rhs) (simplify ds)
\end{code}

