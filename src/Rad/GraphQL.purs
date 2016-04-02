module Rad.GraphQL
  ( Query(..)
  , Result
  , SelectionSet
  , Selection(..)
  , FieldName
  , exec
  ) where

import Prelude (unit, ($), (++), map, show)
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.StrMap (lookup)
import Unsafe.Coerce (unsafeCoerce)

import Rad.Typeable
  ( class Typeable
  , TypeRep
  , TypeableExists
  , runTypeableExists
  )

import Rad.GraphQL.Schema
  ( Schema(..)
  , TypeDict
  , TypeDef(..)
  , FieldDict
  , FieldDef(..)
  )

type SelectionSet = List Selection
type FieldName = String
type Result = String

data Selection = Selection    FieldName
               | SubSelection FieldName SelectionSet

newtype Query = Query SelectionSet

coerceType :: forall a. Maybe (TypeableExists TypeDef) -> Maybe (TypeDef a)
coerceType (Just def) = Just (runTypeableExists unsafeCoerce def)
coerceType Nothing = Nothing

lookupType :: forall a. (Typeable a) => TypeDict -> TypeRep a -> Maybe (TypeDef a)
lookupType dict a = coerceType $ lookup (show a) dict

lookupField :: forall a. (Typeable a) => FieldDict a -> FieldName -> Maybe (TypeableExists (FieldDef a))
lookupField dict n = lookup n dict

execField :: forall a. (Typeable a) => TypeDict -> FieldDict a -> a -> Selection -> Result
execField dict fdict ctx (Selection name) =  name ++ ":" ++ subresult

  where subresult :: Result
        subresult = case lookupField fdict name of
                         Just def -> runTypeableExists evalField def
                         Nothing  -> "[ERROR: Type Not Found]"

        evalField :: forall b. (Typeable b) => FieldDef a b -> Result
        evalField (FieldDef _ tr r) = case lookupType dict tr of
                                           Just (ObjectDef _ _) -> "[ERROR: Expected subselection on field \""++name++"\"]"
                                           Just (ScalarDef _ resolve) -> resolve $ r ctx
                                           Nothing -> "[ERROR: Return Type Not Found]"

execField dict fdict ctx (SubSelection name ss) = name ++ ":" ++ subresult

  where subresult :: Result
        subresult = case lookupField fdict name of
                         Just def -> runTypeableExists evalField def
                         Nothing -> "[ERROR: Type Not Found]"

        evalField :: forall b. (Typeable b) => FieldDef a b -> Result
        evalField (FieldDef _ tr r) = case lookupType dict tr of
                                           Just (ObjectDef _ fdict) -> execObj dict fdict (r ctx) ss
                                           Just (ScalarDef _ _) -> "[ERROR: Cannot evaluate subselection on field \""++name++"\"]"
                                           Nothing -> "[ERROR: Return Type Not Found]"


execField _ _ _ _ = "[ ERROR: if you see this message i've seriously fucked up the implementation ]"

commaJoin :: List String -> String
commaJoin Nil = ""
commaJoin (Cons s Nil) = s ++ "\n"
commaJoin (Cons s l) = s ++ ",\n" ++ commaJoin l

execObj :: forall a. (Typeable a) => TypeDict -> FieldDict a -> a -> SelectionSet -> Result
execObj dict fdict ctx sels = "{\n"
  ++ commaJoin (map (execField dict fdict ctx) sels)
  ++ "}"

exec :: Schema -> Query -> Result
exec (Schema (ObjectDef _ fdict) dict) (Query ss) = execObj dict fdict unit ss
exec _ _ = "[ ERROR: root query type must be an ObjectDef ]"

