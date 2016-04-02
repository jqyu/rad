module Bustle.Data.Post
  ( Post(..)
  , id
  , title
  , body
  ) where

import Prelude (class Show, show, (++))

import Rad.Typeable
  ( class Typeable
  , typeof
  , TypeRep(..)
  )

-- DATA DEFINITIONS

data Post = Post Int String String

-- INSTANCES

instance showPost :: Show Post where
  show (Post id title body) = "POST:(" ++ (show id) ++ "," ++ title ++ "," ++ body ++ ")"

instance typeofPost :: Typeable Post where
  typeof = TypeRep "Post"

-- METHODS

id :: Post -> Int 
id (Post i _ _) = i

title :: Post -> String
title (Post _ t _) = t

body :: Post -> String
body (Post _ _ b) = b
