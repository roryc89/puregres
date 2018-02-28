module ExpectedTables.Items where

import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Puregres.Type (Column, Table(Table), makeColumn)

items :: Table
items = Table "public.items"

item_id :: Column Int
item_id = makeColumn items "item_id"

item_name :: Column String
item_name = makeColumn items "item_name"

item_description :: Column (NullOrUndefined String)
item_description = makeColumn items "item_description"
