{-|
Here we show how to transform Condition datatypes in the presence of their
very restrictive type parameter, which reflects the datatype's conjunctive
normal form: [[(Symbol, *)]]. Inner lists indicate disjunctions, and the outer
list indicates the conjuncts.

To transform these, we offer a substitution: any terminal condition (Eq, Lt, Gt)
of a particular type (Symbol, *) (to indicate the column against which the
condition applies) can be run through a function to produce any
ConditionDisjunction, and that disjunction will be merged into the conjunction.

It's because of the type parameter that we must do most of the work in
typeclasses.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

import GHC.TypeLits (Symbol)
import GHC.Exts (Constraint)
import Data.TypeNat.Nat
import Data.Proxy
import Data.Relational
import Unsafe.Coerce

type Column0 = '("foo", Int)
type Column1 = '("bar", Bool)

type Schema0 = '[ Column0 ]
type Schema1 = '[ Column0, Column1 ]

column0 :: Column Column0
column0 = column

column1 :: Column Column1
column1 = column

type family SubstituteD (substitution :: [(Symbol, *)]) (column :: (Symbol, *)) (disjunction :: [(Symbol, *)]) :: [(Symbol, *)] where
  SubstituteD this for '[] = '[]
  SubstituteD this for (for ': rest) = Append this (SubstituteD this for rest)
  SubstituteD this for (x ': rest) = x ': (SubstituteD this for rest)

type family SubstituteC (substitution :: [(Symbol, *)]) (column :: (Symbol, *)) (conjunction :: [[(Symbol, *)]]) :: [[(Symbol, *)]] where
  SubstituteC this for '[] = '[]
  SubstituteC this for (xs ': xss) = (SubstituteD this for xs) ': (SubstituteC this for xss)

class SubstituteDisjunction this for disjunction where
    substituteDisjunction
      :: (ConditionTerminal for -> ConditionDisjunction this)
      -> ConditionDisjunction disjunction
      -> ConditionDisjunction (SubstituteD this for disjunction)

instance SubstituteDisjunction this for '[] where
    substituteDisjunction _ = id

instance
    ( SubstituteDisjunction this for rest
    , AppendCondition ConditionDisjunction this (SubstituteD this for rest)
    )
    => SubstituteDisjunction this for (for ': rest)
  where
    substituteDisjunction f (OrCondition left right) =
        appendCondition (f left) (substituteDisjunction f right)

class SubstituteConjunction this for conjunction where
    substituteConjunction
      :: (ConditionTerminal for -> ConditionDisjunction this)
      -> ConditionConjunction conjunction
      -> ConditionConjunction (SubstituteC this for conjunction)

instance SubstituteConjunction this for '[] where
    substituteConjunction _ = id

instance
    ( SubstituteDisjunction this for x
    , SubstituteConjunction this for rest
    )
    => SubstituteConjunction this for (x ': rest)
  where
    substituteConjunction f (AndCondition left right) =
        AndCondition (substituteDisjunction f left) (substituteConjunction f right)

exampleCondition2 = column1 .==. True .||. false .&&. true

exampleCondition2' = substituteConjunction (\(x :: ConditionTerminal Column1) -> false) exampleCondition2
