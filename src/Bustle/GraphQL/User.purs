module Bustle.GraphQL.User
  ( typedef
  ) where

import Prelude (($))
import Data.List (List(..), (:))

import Rad.GraphQL.Schema
  ( TypeDef
  , defineObject
  , defineField
  )

import Bustle.Data.User
  ( User
  , id
  , name
  , avatar
  )

import Bustle.Data.Post (Post(..))

post :: Int -> User -> Post
post 1 _ = Post 1 "my fun post" "its really fun"
post _ _ = Post 2 "my other post" "not so fun"

typedef :: TypeDef User
typedef = defineObject
  $ defineField "id"     id
  : defineField "name"   name
  : defineField "avatar" avatar
  : defineField "post1"  (post 1)
  : defineField "post2"  (post 2)
  : Nil
