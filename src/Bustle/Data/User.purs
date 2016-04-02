module Bustle.Data.User
  ( User(..)
  , id
  , name
  , avatar
  ) where

import Prelude (class Show, show, (++))

import Rad.Typeable
  ( class Typeable
  , typeof
  , TypeRep(..)
  )

-- DATA DEFINITIONS

data User = User Int String String

-- INSTANCES

instance showUser :: Show User where
  show (User id name avatar) = "USER:(" ++ (show id) ++ "," ++ name ++ "," ++ avatar ++ ")"

instance typeofPost :: Typeable User where
  typeof = TypeRep "User"

-- METHODS

id :: User -> Int 
id (User i _ _) = i

name :: User -> String
name (User _ n _) = n

avatar :: User -> String
avatar (User _ _ a) = a
