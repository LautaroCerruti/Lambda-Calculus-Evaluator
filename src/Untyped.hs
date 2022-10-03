module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion lterm = aux lterm []
                   where aux (LVar n) l = case elemIndex n l of
                                                Just i -> Bound i
                                                _ -> Free (Global n)
                         aux (App t1 t2) l = (aux t1 l) :@: (aux t2 l)
                         aux (Abs n t) l = Lam (aux t (n:l))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free n) (nEnv, _) = case lookup n nEnv of
                                Just v -> v
                                Nothing -> VNeutral (NFree n)
eval' (t1 :@: t2) e = vapp (eval' t1 e) (eval' t2 e)
eval' (Lam t) (nEnv, lEnv) = VLam (\x -> eval' t (nEnv, x:lEnv))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' v 0

quote' (VLam f) i = Lam (quote' (f (VNeutral (NFree (Quote i)))) (i+1))          
quote' (VNeutral (NFree (Quote num))) i = Bound (i - num - 1)
quote' (VNeutral (NFree (Global n))) _ = Free (Global n)
quote' (VNeutral (NApp neutral val)) i = (quote' (VNeutral neutral) i) :@: (quote' val i)
