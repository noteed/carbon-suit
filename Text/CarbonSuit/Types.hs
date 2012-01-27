module Text.CarbonSuit.Types where

data Carbon = Carbon { filename :: String, blocks :: [Block] }
  deriving Show

data Block = Attribute String String
           | GeneralDot String String
           | Reference String String
           | Text [String]
           | Prompt [String]
           | Null Char
  deriving Show

data Inline = Str String
            | Cod String
            | Ref String
  deriving Show

isAttribute :: Block -> Bool
isAttribute (Attribute _ _) = True
isAttribute _ = False

isAttributeWithKey :: String -> Block -> Bool
isAttributeWithKey k1 (Attribute k2 _) = if k1 == k2 then True else False
isAttributeWithKey _ _ = False

isReference :: Block -> Bool
isReference (Reference _ _) = True
isReference _ = False

isReferenceWithKey :: String -> Block -> Bool
isReferenceWithKey k1 (Reference k2 _) = if k1 == k2 then True else False
isReferenceWithKey _ _ = False

attributeValue :: Block -> String
attributeValue (Attribute _ s) = s
attributeValue _ = error "attributeValue not applied to an Attribute block."

getAttributes :: String -> Carbon -> [String]
getAttributes k c = map attributeValue $
  filter (isAttributeWithKey k) (blocks c)

getAuthors :: Carbon -> [String]
getAuthors = getAttributes "author"

getTitles :: Carbon -> [String]
getTitles = getAttributes "title"

getDates :: Carbon -> [String]
getDates = getAttributes "date"

lookupRefs :: String -> Carbon -> [Block]
lookupRefs k c = filter (isReferenceWithKey k) (blocks c)

