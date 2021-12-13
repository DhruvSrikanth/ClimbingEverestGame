module Command where

import Text.Parsec hiding (parse, runParser, (<|>))
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Data.Char
import Data.List

import Item
import Direction

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Map
  | Exit
  deriving (Show, Eq)

type Conjunction = [Command]

-- Parser
parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

-- Try applying different parsers
(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

-- Grab any kind of whitespace
grabSpaces = many1 $ satisfy (`elem` " \r\t\n")

-- Parse item names
itemNameP :: Parser ItemName
itemNameP = (foldl (<|>) $ P.parserFail "") $ map (\item -> (P.optional grabSpaces >> P.string (show item) >> (P.eof <|> P.optional grabSpaces)) *> pure item) itemNames

-- lookP :: Parser Command
-- -- lookP = P.optional P.spaces >> P.string "look" >> P.string " " >> P.optional P.spaces *> pure Look
-- lookP = Look <$ (P.optional P.spaces *> P.string "look" *> P.optional P.spaces)

nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = P.many1 itemNameP

-- Previous version
-- nounPhrase :: Parser [ItemName]
-- nounPhrase = nounPhrase_stub

-- Split all item names mentioned to be read by the parser 
nounPhrase :: Parser [ItemName]
nounPhrase = P.sepBy1 itemNameP (P.optional grabSpaces *> P.string "," *> (P.eof <|> P.optional grabSpaces))

-- Extract the command to check the inventory of the player
inventoryP :: Parser Command
inventoryP = (P.optional grabSpaces >> P.string "inventory" >> (P.eof <|> P.optional grabSpaces)) *> pure Inventory

-- Extract the command to take an item
takeP :: Parser Command
takeP = (P.optional grabSpaces >> P.string "take" >> P.string " " >> (P.eof <|> P.optional grabSpaces)) *> nounPhrase >>= (pure . Take)

-- Extract the command to exit the game
exitP :: Parser Command
exitP = (P.optional grabSpaces >> (foldl (<|>) $ parserFail "") [P.string "exit", P.string "quit"] >> (P.eof <|> P.optional grabSpaces)) *> pure Exit 

-- Extract the command to drop an item
dropP :: Parser Command
dropP = (P.optional grabSpaces >> P.string "drop" >> P.string " " >> (P.eof <|> P.optional grabSpaces)) *> nounPhrase >>= (pure . Drop)

-- Extract the command to look around the room where the player is present
lookP :: Parser Command
lookP = P.optional grabSpaces >> P.string "look" >> (P.eof <|> P.optional grabSpaces) *> pure Look

-- Extract the command to look at the map with the player on it
mapP :: Parser Command
mapP = P.optional grabSpaces >> P.string "map" >> (P.eof <|> P.optional grabSpaces) *> pure Map

-- Extract the command to show a particular direction
directionP :: Parser Direction
directionP = (foldl (<|>) $ P.parserFail "") $ map (\direction -> (P.optional grabSpaces >> P.string (show direction) >> (P.eof <|> P.optional grabSpaces)) *> pure direction) [N,S,E,W]

-- Extract the command to move in a particular direction
moveP :: Parser Command
moveP = P.optional grabSpaces *> directionP <* (P.eof <|> P.optional grabSpaces) >>= (\direction -> pure (Move direction))

-- Extract any of the commands mentioned below
commandP :: Parser Command
commandP =  P.optional grabSpaces *> inventoryP <|> lookP <|> mapP <|> takeP <|> dropP <|> moveP <|> exitP <* (P.eof <|> P.optional grabSpaces)

-- Chain commands with the phrase and
conjunctionP :: Parser Conjunction
conjunctionP = P.sepBy1 (P.optional grabSpaces *> commandP) (P.optional grabSpaces *> P.string "and" *> P.optional grabSpaces)

-- Parse the players input
parseInput :: String -> Maybe Conjunction
parseInput input_string = case parse conjunctionP input_string of
                            Left _ -> Nothing
                            Right parse_result -> Just parse_result