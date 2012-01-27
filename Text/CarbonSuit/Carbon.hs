-- Only UTF-8 output.
module Text.CarbonSuit.Carbon where

import Text.Parsec

import Text.CarbonSuit.Types

displayCarbon :: Carbon -> String
displayCarbon (Carbon _ bs) = unlines (map displayBlock bs)

displayBlock :: Block -> String
displayBlock = d where
  d (Attribute k v)  = "(attr) " ++ k ++ ":" ++ v
  d (GeneralDot k v) = "(gdot) " ++ k ++ ":" ++ v
  d (Reference k v)  = "(refe) " ++ k ++ ":" ++ v
  d (Text ls)        = "(text)\n" ++ unlines ls
  d (Prompt ls)      = "(prom)\n" ++ unlines ls
  d (Null c)         = "(null) '" ++ [c] ++ "'"

----------------------------------------------------------------------
-- Main Carbon suit parsers
----------------------------------------------------------------------

data ParserState = PS
  deriving Show

initialPS :: ParserState
initialPS = PS

parseCarbon :: String -> Parsec String ParserState Carbon
parseCarbon fn = do
  -- Skip everything befor the first attribute.
  _ <- blanklines >> many (try normalParagraph)

  -- Parse the document as a list of blocks.
  manyTill block eof >>= return . Carbon fn

spaceChar :: Parsec String st Char
spaceChar = char ' ' <|> char '\t'

skipSpaces :: Parsec String st ()
skipSpaces = skipMany spaceChar

blankline :: Parsec String ParserState Char
blankline = skipSpaces >> newline

blanklines :: Parsec String ParserState String
blanklines = many $ try blankline

anyLine :: Parsec String ParserState String
anyLine = manyTill anyChar newline

normalParagraph :: Parsec String ParserState [String]
normalParagraph = do
  ls <- many1 (notFollowedBy (try attributeLine >> return ' ') >> anyLine)
  return ls

generalDotLine :: Parsec String ParserState (String,String)
generalDotLine = do
  p <- getPosition
  if sourceColumn p /= 1
    then parserFail "General dotted construct should begin in column 1."
    else do
      k <- many1 (noneOf " .")
      char '.' >> skipSpaces
      v <- manyTill anyChar newline
      return (k,v)

generalDotBlock :: Parsec String ParserState Block
generalDotBlock = do
  (k,v) <- generalDotLine
  _ <- blanklines
  return $ GeneralDot k v

-- A line with an attribute.
attributeLine :: Parsec String ParserState (String,String)
attributeLine = do
  p <- getPosition
  if sourceColumn p /= 1
    then parserFail "Attribute line should begin in column 1."
    else do
      k <- many1 (noneOf " \t\n:")
      char ':' >> skipSpaces
      v <- manyTill anyChar newline
      return (k,v)

attributeBlock :: Parsec String ParserState Block
attributeBlock = do
  (k,v) <- attributeLine
  _ <- blanklines
  return $ Attribute k v

referenceLine :: Parsec String ParserState (String,String)
referenceLine = do
  skipSpaces
  p <- getPosition
  if odd (sourceColumn p)
    then parserFail "Odd indentation : not a reference block."
    else do
      _ <- char '['
      k <- many1 (noneOf "]\n")
      string "]:" >> skipSpaces
      v <- manyTill anyChar newline
      return (k,v)

referenceBlock :: Parsec String ParserState Block
referenceBlock = do
  (k,v) <- referenceLine
  _ <- blanklines
  return $ Reference k v

textBlock :: Parsec String ParserState Block
textBlock = do
  skipSpaces
  p <- getPosition
  if odd (sourceColumn p)
    then parserFail "Odd indentation : not a text block."
    else do
      ls <- many1 (notFollowedBy stop >> skipSpaces >> anyLine)
      _ <- blanklines
      return $ Text ls

promptBlock :: Parsec String ParserState Block
promptBlock = do
  skipSpaces
  p <- getPosition
  if sourceColumn p < 5 || even (sourceColumn p)
    then parserFail "Not a prompt block."
    else do
      ls <- many1 (notFollowedBy (try stop) >> anyLine)
      _ <- blanklines
      return $ Prompt (map (drop 4) $ (replicate (sourceColumn p - 1) ' ' ++ head ls) : tail ls)

stop :: Parsec String ParserState Char
stop = try (blankline <|> (attributeLine >> return ' '))

block :: Parsec String ParserState Block
block = choice [
    try attributeBlock  <?> "attributeBlock"
  , try generalDotBlock <?> "generalDotBlock"
  , try referenceBlock  <?> "referenceBlock"
  , try textBlock       <?> "textBlock"
  , try promptBlock     <?> "promptBlock"
  , nullBlock           <?> "nullBlock"
  ] <?> "block"

nullBlock :: Parsec String ParserState Block
nullBlock = anyChar >>= return . Null

----------------------------------------------------------------------
-- Secondary Carbon suit parsers (e.g. to parse a Text block)
----------------------------------------------------------------------

strChar :: Parsec String st Char
strChar = noneOf "[`"

str :: Parsec String st Inline
str = many1 strChar >>= return . Str

cod :: Parsec String st Inline
cod = do
  _ <- char '`'
  k <- manyTill anyChar (char '`')
  skipSpaces
  return (Cod k)

ref :: Parsec String st Inline
ref = do
  _ <- char '['
  k <- manyTill anyChar (char ']')
  skipSpaces
  return (Ref k)

inline :: Parsec String st Inline
inline = choice [
    str <?> "str"
  , cod <?> "cod"
  , ref <?> "ref"
  ] <?> "inline"

tt :: String -> IO ()
tt s = do
  let result = runParser (many inline) initialPS "carbon-suit" s
  case result of
    Left err ->
      do putStrLn "Error :"
         print err
    Right a  -> putStrLn (show a)

