module Main where

import Prelude (Unit, class Show, show, bind, ($))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(Nil), (:))

import Rad.Typeable
  ( class Typeable
  , typename
  , TypeRep(..)
  )

import Rad.GraphQL
  ( exec
  , Query(..)
  , Selection(..)
  , SelectionSet
  , FieldName
  )
import Bustle.GraphQL (schema)

q :: Query
q = Query
  $ Selection "hello"
  : SubSelection "post"
    ( Selection "id"
    : Selection "title"
    : SubSelection "author"
      ( Selection "id"
      : Selection "name"
      : Selection "avatar"
      : SubSelection "post1"
        ( Selection "id"
        : Selection "body"
        : Nil
        )
      : SubSelection "post2"
        ( Selection "id"
        : Selection "title"
        : Selection "body"
        : Nil
        )
      : Nil
      )
    : Nil
    )
  : SubSelection "other"
    ( Selection "id"
    : Selection "title"
    : Selection "body"
    : Nil
    )
  : Nil

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
  log (exec schema q)
