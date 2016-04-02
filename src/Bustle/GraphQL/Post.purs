module Bustle.GraphQL.Post
  ( typedef
  ) where

import Prelude (($))
import Data.List (List(..), (:))

import Rad.GraphQL.Schema
  ( TypeDef
  , defineObject
  , defineField
  )

import Bustle.Data.Post
  ( Post
  , id
  , title
  , body
  )

import Bustle.Data.User (User(..))

author :: Post -> User
author _ = User 998 "James" "selfie.jpg"

typedef :: TypeDef Post
typedef = defineObject
  $ defineField "id"     id
  : defineField "title"  title
  : defineField "body"   body
  : defineField "author" author
  : Nil
