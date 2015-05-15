{-|
Module      : Data.Relational.RelationalMapping
Description : Mapping to Relational types.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Relational.RelationalMapping (

    RelationalMapping(..)

  , makeSelect
  , makeInsert
  , makeUpdate
  , makeDelete

  , select
  , insert
  , delete
  , update

  , SelectConstraint
  , InsertConstraint
  , DeleteConstraint
  , UpdateConstraint

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import GHC.Exts (Constraint)
import Data.Bijection
import Data.Proxy
import Data.Relational
import Data.Relational.Universe
import Data.Relational.Interpreter

-- | Instances of this class can be pushed to / pulled from a relational
--   interpreter.
class ( Eq d
      , KnownSymbol (RelationalTableName d)
      , IsSubset (RelationalSchema d) (RelationalSchema d)
      , IsSubsetUnique (RelationalSchema d) (RelationalSchema d)
      )
      => RelationalMapping d
  where

  type RelationalTableName d :: Symbol
  type RelationalSchema d :: [(Symbol, *)]

  relationalSchema :: Proxy d -> Schema (RelationalSchema d)

  relationalTable :: Proxy d -> Table '(RelationalTableName d, RelationalSchema d)
  relationalTable proxy = Table Proxy (relationalSchema proxy)

  rowBijection :: d :<->: Row (RelationalSchema d)

makeSelect
  :: forall d condition .
     ( RelationalMapping d
     , IsSubset (Concat condition) (RelationalSchema d)
     )
   => Proxy d
   -> Condition condition
   -> Select '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) condition
makeSelect proxy condition =
    Select
      (relationalTable proxy)
      (fullProjection (relationalSchema proxy))
      condition

type family SelectConstraint datatype interpreter condition :: Constraint where
  SelectConstraint d i c = (
      RelationalMapping d
    , RelationalInterpreter i
    , Functor (InterpreterMonad i)
    , IsSubset (Concat c) (RelationalSchema d)
    , Every (InUniverse (Universe i)) (Snds (RelationalSchema d))
    , Every (InUniverse (Universe i)) (Snds (Concat c))
    , InterpreterSelectConstraint i (RelationalSchema d) (RelationalSchema d) c
    , ConvertToRow (Universe i) (RelationalSchema d)
    )

select
  :: SelectConstraint d t conditioned
  => Proxy d
  -> Proxy t
  -> Condition conditioned
  -> (InterpreterMonad t) [Maybe d]
select proxyD proxyI condition =
    let selectTerm = makeSelect proxyD condition
    in  (fmap . fmap . fmap) (biFrom rowBijection) (interpretSelect' proxyI selectTerm)
    -- ^ three fmaps, for the monad, the list, and the maybe.

makeInsert
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Insert '(RelationalTableName d, RelationalSchema d)
makeInsert d = Insert (relationalTable (Proxy :: Proxy d)) (biTo rowBijection d)

type family InsertConstraint datatype interpreter :: Constraint where
  InsertConstraint d i = (
      RelationalMapping d
    , RelationalInterpreter i
    , InterpreterInsertConstraint i (RelationalSchema d)
    , Every (InUniverse (Universe i)) (Snds (RelationalSchema d))
    )

insert
  :: InsertConstraint d interpreter
  => d
  -> Proxy interpreter
  -> (InterpreterMonad interpreter) ()
insert d proxyI =
    let insertTerm = makeInsert d
    in  interpretInsert proxyI insertTerm

makeDelete
  :: forall d c .
     ( RelationalMapping d
     , IsSubset (Concat c) (RelationalSchema d)
     )
  => Proxy d
  -> Condition c
  -> Delete '(RelationalTableName d, RelationalSchema d) c
makeDelete proxyD condition = Delete (relationalTable proxyD) (condition)

type family DeleteConstraint datatype interpreter condition :: Constraint where
  DeleteConstraint d i condition = (
      RelationalMapping d
    , RelationalInterpreter i
    , Every (InUniverse (Universe i)) (Snds (Concat (condition)))
    , InterpreterDeleteConstraint i (RelationalSchema d) (condition)
    , IsSubset (Concat condition) (RelationalSchema d)
    )

delete
  :: DeleteConstraint d interpreter c
  => Proxy interpreter
  -> Proxy d
  -> Condition c
  -> (InterpreterMonad interpreter) ()
delete proxyI proxyD condition =
    let deleteTerm = makeDelete proxyD condition
    in  interpretDelete proxyI deleteTerm

makeUpdate
  :: forall d c .
     ( RelationalMapping d
     , IsSubset (Concat c) (RelationalSchema d)
     )
  => d
  -> Condition c
  -> Update '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) c
makeUpdate d condition =
    Update
      (relationalTable proxyD)
      (fullProjection (relationalSchema proxyD))
      (condition)
      (biTo rowBijection d)
  where
    proxyD :: Proxy d
    proxyD = Proxy

type family UpdateConstraint datatype interpreter condition :: Constraint where
  UpdateConstraint d i condition = (
      RelationalMapping d
    , RelationalInterpreter i
    , Every (InUniverse (Universe i)) (Snds (RelationalSchema d))
    , Every (InUniverse (Universe i)) (Snds (Concat (condition)))
    , InterpreterUpdateConstraint i (RelationalSchema d) (RelationalSchema d) (condition)
    , IsSubset (Concat condition) (RelationalSchema d)
    )

update
  :: UpdateConstraint d interpreter c
  => Proxy interpreter
  -> d
  -> Condition c
  -> (InterpreterMonad interpreter) ()
update proxyI d condition =
    let updateTerm = makeUpdate d condition
    in  interpretUpdate proxyI updateTerm
