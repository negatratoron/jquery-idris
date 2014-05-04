 module JQuery

-- These methods match the JQuery API as closely as possible
-- http://api.jquery.com/



data JQSelector = JQSelectID String
                | JQSelectTag String

instance Show JQSelector where
  show (JQSelectID id) = "#" ++ id
  show (JQSelectTag tag) = tag


data ElementType = Body
                 | Canvas
                 | Div


instance Show ElementType where
  show Body = "body"
  show Canvas = "canvas"
  show Div = "div"


data Attribute = Height Int
               | Id String
               | Width Int

instance Show Attribute where
  show (Height px) = "height=\"" ++ show px ++ "px\""
  show (Id id) = "id=\"" ++ id ++ "\""
  show (Width px) = "width=\"" ++ show px ++ "px\""

data HTMLElement = MkElement ElementType (List Attribute) (Maybe HTMLElement)
                 | MkText String


-- methods that show the open tag and close tag for HTML elements
showOpenTag : HTMLElement -> String
showOpenTag (MkElement type attrs _) = "<" ++ typeString ++ attrsString ++ ">"
  where typeString = show type
        attrsString = foldl (\acc, attr => acc ++ " " ++ show attr) "" attrs
showOpenTag (MkText text) = text


showCloseTag : HTMLElement -> String
showCloseTag (MkElement type _ _) = "</" ++ show type ++ ">"
showCloseTag (MkText _) = ""


innerElement : HTMLElement -> Maybe HTMLElement
innerElement (MkElement _ _ (Just el)) = Just el
innerElement _ = Nothing


instance Show HTMLElement where
  show el = openTagString ++ innerElementString ++ closeTagString
    where openTagString = showOpenTag el
          closeTagString = showCloseTag el
          innerElementString = fromMaybe "" $ map show $ innerElement el


-- monad that encapsulates the usages of $ that select DOM elements
data JQuery = MkJQuery -- selects no elements
            | MkJQuerySelector JQSelector
            | MkJQueryElement HTMLElement


instance Show JQuery where
  show MkJQuery = ""
  show (MkJQuerySelector selector) = show selector
  show (MkJQueryElement element) = show element



-- (DOM) Manipulation
appendTo : JQuery -> JQuery -> IO JQuery
-- appendTo target source = mkForeign (
--   FFun "%1.appendTo(%0)" [FString, FString] FUnit
--   ) (show source) (show target)

appendTo target source = do
  mkForeign (
    FFun "$(%0).appendTo($(%1))" [FString, FString] FUnit
    ) (show source) (show target)
  return source
