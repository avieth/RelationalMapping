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
  , update
  , delete

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Bijection
import Data.Proxy
import Data.Relational
import Data.Relational.Universe
import Data.Relational.Interpreter

-- | Instances of this class can be pushed to / pulled from a relational
--   interpreter.
class ( Eq d
      , KnownSymbol (RelationalTableName d)
      , IsSubset (Concat (CompleteCharacterization d)) (RelationalSchema d)
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

  -- Must be able to characterize a given message, so that we can isolate it
  -- for deletes and updates.
  type CompleteCharacterization d :: [[(Symbol, *)]]
  completeCharacterization :: d -> Condition (CompleteCharacterization d)

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

select
  :: ( IsSubset (Concat conditioned) (RelationalSchema d)
     , Every (Data.Relational.Universe.InUniverse (Universe t)) (Snds (RelationalSchema d))
     , Every (Data.Relational.Universe.InUniverse (Universe t)) (Snds (Concat conditioned))
     , InterpreterSelectConstraint t (RelationalSchema d) (RelationalSchema d) conditioned
     , RelationalMapping d
     , ConvertToRow (Universe t) (RelationalSchema d)
     , RelationalInterpreter t
     , Functor (InterpreterMonad t)
     )
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

insert
  :: ( Every (InUniverse (Universe interpreter)) (Snds (RelationalSchema d))
     , InterpreterInsertConstraint interpreter (RelationalSchema d)
     , RelationalMapping d
     , RelationalInterpreter interpreter
     )
  => d
  -> Proxy interpreter
  -> (InterpreterMonad interpreter) ()
insert d proxyI =
    let insertTerm = makeInsert d
    in  interpretInsert proxyI insertTerm

makeDelete
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Delete '(RelationalTableName d, RelationalSchema d) (CompleteCharacterization d)
makeDelete d = Delete (relationalTable (Proxy :: Proxy d)) (completeCharacterization d)

delete
  :: ( Every (InUniverse (Universe interpreter)) (Snds (Concat (CompleteCharacterization d)))
     , InterpreterDeleteConstraint interpreter (RelationalSchema d) (CompleteCharacterization d)
     , RelationalMapping d
     , RelationalInterpreter interpreter
     )
  => d
  -> Proxy interpreter
  -> (InterpreterMonad interpreter) ()
delete d proxyI =
    let deleteTerm = makeDelete d
    in  interpretDelete proxyI deleteTerm

makeUpdate
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Update '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) (CompleteCharacterization d)
makeUpdate d =
    Update
      (relationalTable proxy)
      (fullProjection (relationalSchema proxy))
      (completeCharacterization d)
      (biTo rowBijection d)
  where
    proxy :: Proxy d
    proxy = Proxy

update
  :: ( Every (InUniverse (Universe interpreter)) (Snds (RelationalSchema d))
     , Every (InUniverse (Universe interpreter)) (Snds (Concat (CompleteCharacterization d)))
     , InterpreterUpdateConstraint interpreter (RelationalSchema d) (RelationalSchema d) (CompleteCharacterization d)
     , RelationalMapping d
     , RelationalInterpreter interpreter
     )
  => d
  -> Proxy interpreter
  -> (InterpreterMonad interpreter) ()
update d proxyI =
    let updateTerm = makeUpdate d
    in  interpretUpdate proxyI updateTerm
