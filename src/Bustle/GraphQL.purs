module Bustle.GraphQL
  ( schema
  ) where

import Rad.GraphQL.Schema
  ( Schema
  , defineSchema
  , TypeDef
  , defineType
  , defineObject
  , defineField
  )

import Prelude (Unit, ($))
import Data.List (List(..), (:))

import Bustle.Data.Post (Post(..))
import Bustle.GraphQL.Post (typedef) as Post

import Bustle.Data.User (User(..))
import Bustle.GraphQL.User (typedef) as User

getPost :: Unit -> Post
getPost _ = Post 1 "my post" "its a fun post really"

getUser :: Unit -> User
getUser _ = User 998 "James" "selfie.jpg"

hello :: Unit -> String
hello _ = "world"

root :: TypeDef Unit
root = defineObject
  $ defineField "post"  getPost
  : defineField "other" getPost
  : defineField "user"  getUser
  : defineField "hello" hello
  : Nil

schema :: Schema
schema = defineSchema root 
  $ defineType Post.typedef
  : defineType User.typedef
  : Nil
