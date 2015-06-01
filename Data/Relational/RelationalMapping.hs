{-|
Module      : Data.Relational.RelationalMapping
Description : Mapping to and from Relational types.
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
  , RelationalTable

  , toRow
  , fromRow

  , rmselect
  , rminsert
  , rmupdate
  , rmdelete

  ) where

import GHC.TypeLits (Symbol, KnownSymbol)
import GHC.Exts (Constraint)
import Data.Bijection
import Data.Proxy
import Data.Relational

-- | Instances of this class are associated with some relational table type.
--   The @rowBijection@ allows us to read and write to a table of this type.
class
    ( KnownSymbol (RelationalTableName d)
    , IsSubset (RelationalSchema d) (RelationalSchema d)
    , IsSubsetUnique (RelationalSchema d) (RelationalSchema d)
    )
    => RelationalMapping d
  where

  type RelationalTableName d :: Symbol
  type RelationalSchema d :: [(Symbol, *)]

  relationalSchema :: Proxy d -> Schema (RelationalSchema d)

  relationalTable :: Proxy d -> Table (RelationalTable d)
  relationalTable proxy = table (relationalSchema proxy)

  rowBijection :: d :<->: Row (RelationalSchema d)

type RelationalTable d = '(RelationalTableName d, RelationalSchema d)

toRow :: RelationalMapping d => d -> Row (RelationalSchema d)
toRow = biTo rowBijection

fromRow :: RelationalMapping d => Row (RelationalSchema d) -> d
fromRow = biFrom rowBijection

rmselect
  :: forall d condition .
     ( RelationalMapping d
     , IsSubset (Concat condition) (RelationalSchema d)
     )
  => Proxy d
  -> Condition condition
  -> Select '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) condition
rmselect proxy condition =
    Select
      (relationalTable proxy)
      (fullProjection (relationalSchema proxy))
      condition

rminsert
  :: forall d .
     ( RelationalMapping d
     )
  => d
  -> Insert '(RelationalTableName d, RelationalSchema d)
rminsert d = Insert (relationalTable (Proxy :: Proxy d)) (biTo rowBijection d)

rmupdate
  :: forall d c .
     ( RelationalMapping d
     , IsSubset (Concat c) (RelationalSchema d)
     )
  => d
  -> Condition c
  -> Update '(RelationalTableName d, RelationalSchema d) (RelationalSchema d) c
rmupdate d condition =
    Update
      (relationalTable proxyD)
      (fullProjection (relationalSchema proxyD))
      (condition)
      (biTo rowBijection d)
  where
    proxyD :: Proxy d
    proxyD = Proxy

rmdelete
  :: forall d c .
     ( RelationalMapping d
     , IsSubset (Concat c) (RelationalSchema d)
     )
  => Proxy d
  -> Condition c
  -> Delete '(RelationalTableName d, RelationalSchema d) c
rmdelete proxyD condition = Delete (relationalTable proxyD) (condition)
