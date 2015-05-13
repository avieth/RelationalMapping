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

module Data.Relational.RelationalMapping (

    RelationalMapping(..)

  , select
  , insert
  , update
  , delete

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import Data.Bijection
import Data.Proxy
import Data.Relational

-- | Instances of this class can be pushed to / pulled from a relational
--   interpreter.
class ( KnownSymbol (RelationalTableName d)
      , Subset (Concat (CompleteCharacterization d)) (RelationalSchema d) ~ 'True
      , Subset (RelationalSchema d) (RelationalSchema d) ~ 'True
      , SubsetUnique (RelationalSchema d) (RelationalSchema d) ~ 'True
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

select
  :: forall d condition .
     ( RelationalMapping d
     , Subset (Concat condition) (RelationalSchema d) ~ 'True
     )
   => Proxy d
   -> Condition condition
   -> Select '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) condition
select proxy condition =
    Select
      (relationalTable proxy)
      (fullProjection (relationalSchema proxy))
      condition

insert
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Insert '(RelationalTableName d, RelationalSchema d)
insert d = Insert (relationalTable (Proxy :: Proxy d)) (biTo rowBijection d)

delete
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Delete '(RelationalTableName d, RelationalSchema d) (CompleteCharacterization d)
delete d = Delete (relationalTable (Proxy :: Proxy d)) (completeCharacterization d)

update
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Update '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) (CompleteCharacterization d)
update d =
    Update
      (relationalTable proxy)
      (fullProjection (relationalSchema proxy))
      (completeCharacterization d)
      (biTo rowBijection d)
  where
    proxy :: Proxy d
    proxy = Proxy
