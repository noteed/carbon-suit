module Text.CarbonSuit.XHtml where

import Text.CarbonSuit.Carbon
import Text.XHtml.Strict
import System.FilePath (dropExtension)
-- This is here because I parse Text into Inline's,
-- but it should be done before, something
-- like LightCarbon and FullCarbon.
import Text.Parsec

renderAsStandAloneXHtml :: Carbon -> String
renderAsStandAloneXHtml c = renderHtml $ -- or use showHtml
  header << (styleSheetLink "/my.css" +++ thetitle << tit)
  +++ body << (tag "a" ! [href "/"] << "Interlocked Features"
  +++ carbonToXHtml c +++ codeJavascript ga1 +++ codeJavascript ga2)

  where [tit] = getTitles c  -- TODO only one title handled for now
        ga1 = "var gaJsHost = ((\"https:\" == document.location.protocol)"
              ++" ? \"https://ssl.\" : \"http://www.\");\n"
              ++ "document.write(unescape(\"%3Cscript src='\" + gaJsHost "
              ++ "+ \"google-analytics.com/ga.js' "
              ++ "type='text/javascript'%3E%3C/script%3E\"));\n"
        ga2 = "try {\n"
              ++ "var pageTracker = _gat._getTracker(\"UA-7990941-1\");\n"
              ++ "pageTracker._trackPageview();\n"
              ++ "} catch(err) {}\n"

renderAsXHtml :: Carbon -> String
renderAsXHtml = renderHtmlFragment . carbonToXHtml

carbonToXHtml :: Carbon -> Html
carbonToXHtml c =
  h1 <<
  tag "a" ! [href $ "/entry/" ++ (dropExtension (filename c) ++ ".html")]
  << tit
  +++ thespan ! [theclass "author"] << aut
  +++ spaceHtml +++ spaceHtml +++ spaceHtml
  +++ thespan ! [theclass "date"] << dat
  +++ map (xBlock c) bs

  where [aut] = getAuthors c -- TODO only one author handled for now
        [tit] = getTitles c  -- idem here
        [dat] = getDates c   -- idem here
        bs = filter (\b -> not $ isAttribute b || isReference b) (blocks c)

xBlock :: Carbon -> Block -> Html

xBlock _ (Attribute k v) =
  thediv ! [theclass "attribute"]
  <<
  (thespan ! [theclass "key"] << k
  +++ thespan ! [theclass "value"] << v)

xBlock _ (Reference k v) =
  thediv ! [theclass "reference"]
  <<
  (thespan ! [theclass "key"] << k
  +++ thespan ! [theclass "value"] << v)

xBlock c (Text ls) = paragraph << (map x is)
  where Right is = runParser (many inline) ()
                   "carbon-suit(inline)" (concatMap (++ " ") ls)
        x (Str s) = primHtml (" " ++ s ++ " ")
        x (Cod s) = thespan ! [theclass "code"] << s
        x (Ref k) = tag "a" ! [href url] << k
          where url = case lookupRefs k c of
                       [Reference _ v] -> v
                       _ -> error $ "Can't find the reference " ++ k ++ " from " ++ show is

xBlock _ (Prompt ls) = (pre ! [theclass "prompt"]) (primHtml $ unlines ls)

xBlock _ _ = thespan ! [theclass "unimplemented"] << "unimplemented"

styleSheetLink :: String -> Html
styleSheetLink fn =
  thelink ! [rel "stylesheet", thetype "text/css", href fn] << noHtml

srcJavascript :: String -> Html
srcJavascript fn =
  tag "script" ! [thetype "text/javascript", src fn] << noHtml

codeJavascript :: String -> Html
codeJavascript code =
  tag "script" ! [thetype "text/javascript"] << (primHtml code)

