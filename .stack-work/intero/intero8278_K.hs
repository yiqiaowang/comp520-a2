module SymbolTable
  ( newMap
  , hasKey
  , addSym
  , getSym
  , getInfo
  , removeJust
  , parseEntry
  , IdName
  , IdType
  , IdValue (..)
  , Entry (..)
  ) where

-- Functions to build and traverse a symbol table.
import AST (Type(..))
import Data.Map (Map, adjust, insert, lookup, member)
import qualified Data.Map as Map

-- Data Structure(s) to Hold Symbol table entries
type IdName = String

type IdType = Type

data IdValue
  = ValEmpty
  | ValStr String
  | ValFloat Float
  | ValInteger Integer
  deriving (Show, Eq)

-- This will be the type used in the map.
data Entry =
  Entry IdType
        IdValue
  deriving (Show, Eq)

-- Table will be keyed by the name of the symbol
-- Create a new empty table
newMap :: Map IdName Entry
newMap = Map.empty :: Map IdName Entry

-- Check if an idname is in the symbol table
hasKey :: IdName -> Map IdName Entry -> Bool
hasKey key map = Map.member key map

-- Add new entry to symbol table
addSym :: IdName -> Entry -> Map IdName Entry -> Map IdName Entry
addSym key entry map = Map.insert key entry map

-- Get an entry from the symbol table
getSym :: IdName -> Map IdName Entry -> Maybe Entry
getSym name map = Map.lookup name map


removeJust :: Maybe Entry -> Entry
removeJust (Just e) = e
removeJust Nothing =  error "Remove Just failed"


parseEntry :: Entry -> (IdType, IdValue)
parseEntry (Entry t v) = (t,v)


-- Extract the type and value information out of an entry
getInfo :: Maybe Entry -> Maybe (IdType, IdValue)
getInfo Nothing = Nothing
getInfo (Just (Entry t v)) = Just (t, v)
