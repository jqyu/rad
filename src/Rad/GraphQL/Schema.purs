module Rad.GraphQL.Schema

  ( Schema(..)
  , TypeDict
  , defineSchema

  , TypeDef(..)
  , ResolveLeaf
  , FieldDict
  , defineType
  , defineObject

  , FieldDef(..)
  , defineField
  ) where

import Rad.Typeable
  ( class Typeable
  , typeof
  , TypeRep

  , TypeableExists
  , mkTypeableExists
  , runTypeableExists
  )

import Prelude (Unit, ($), map, show)

import Data.List (List, (:))
import Data.Tuple (Tuple(..))
import Data.StrMap (StrMap, fromList)


-- A Schema consists of a TypeDef dictionary, and a root type

type TypeDict = StrMap (TypeableExists TypeDef)

data Schema = Schema (TypeDef Unit) TypeDict

defineSchema :: TypeDef Unit -> List (TypeableExists TypeDef) -> Schema
defineSchema root types = Schema root (fromList types')

  where types' :: List (Tuple String (TypeableExists TypeDef))
        types' = map getTuple
               $ defineType typeDefInt
               : defineType typeDefString
               : types

        getTuple :: TypeableExists TypeDef -> Tuple String (TypeableExists TypeDef)
        getTuple t = Tuple (runTypeableExists typeDefName t) t

-- A TypeDef describes how to resolve a query relative to a given type instance

type ResolveLeaf a = a -> String
type FieldDict a = StrMap (TypeableExists (FieldDef a))

data TypeDef a = ObjectDef (TypeRep a) (FieldDict a)
               | ScalarDef (TypeRep a) (ResolveLeaf a)

typeDefName :: forall a. TypeDef a -> String
typeDefName (ObjectDef tr _) = show tr
typeDefName (ScalarDef tr _) = show tr

-- Common scalar definitions

typeDefInt :: TypeDef Int
typeDefInt = ScalarDef typeof show

typeDefString :: TypeDef String
typeDefString = ScalarDef typeof show

defineType :: forall a. (Typeable a) => TypeDef a -> TypeableExists TypeDef
defineType = mkTypeableExists

-- Constrained constructor that ensures all of our TypeDefs are of Typeable objects

defineObject :: forall a. (Typeable a) => List (TypeableExists (FieldDef a)) -> TypeDef a
defineObject defs = ObjectDef typeof (fromList defs')

  where defs' :: List (Tuple String (TypeableExists (FieldDef a)))
        defs' = map getTuple defs

        getTuple :: TypeableExists (FieldDef a) -> Tuple String (TypeableExists (FieldDef a))
        getTuple f = Tuple (runTypeableExists fieldName f) f

-- A FieldDef describes an individual field of an Object Type

data FieldDef a b = FieldDef String (TypeRep b) (a -> b)

fieldName :: forall a b. FieldDef a b -> String
fieldName (FieldDef name _ _) = name

-- Constrained constructor that ensures all of our FieldDefs are from Typeables to other Typeables

defineField :: forall a b. (Typeable a, Typeable b) => String -> (a -> b) -> TypeableExists (FieldDef a)
defineField name fn = mkTypeableExists def
  where def :: FieldDef a b
        def = FieldDef name typeof fn
