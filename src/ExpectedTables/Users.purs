module ExpectedTables.Users where

import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Puregres.Type (Column, Table(Table), makeColumn)

users :: Table
users = Table "public.users"

user_id :: Column Int
user_id = makeColumn users "user_id"

last_name :: Column (NullOrUndefined String)
last_name = makeColumn users "last_name"

email :: Column String
email = makeColumn users "email"

registered :: Column Boolean
registered = makeColumn users "registered"
