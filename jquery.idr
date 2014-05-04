module JQuery

-- These methods match the JQuery API as closely as possible
-- http://api.jquery.com/



data JQSelector = JQSelectID String

instance Show JQSelector where
  show (JQSelectID id) = "#" ++ id


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
  show (Id id) = "id=\"" ++ show id ++ "\""
  show (Width px) = "width=\"" ++ show px ++ "px\""

data HTMLElement = MkElement ElementType (List Attribute) (Maybe HTMLElement)
                 | MkText String

instance Show HTMLElement where
  show (MkElement type attrs (Just el)) =
    "<" ++ typeString ++ attrsString ++ ">" ++
    childElementString ++
    "<" ++ typeString ++ "/>" where
      typeString = show type
      childElementString = show el
      attrsString = 


-- monad that encapsulates the usages of $ that select DOM elements
data JQuery a = MkJQuery a -- selects no elements
              | MkJQuerySelector a JQSelector
              | MkJQueryElement a JQElement
              | MkJQueryUnion a (List (JQuery ()))


instance Show (JQuery a) where
  show (MkJQuery _) = "$()"
  show (MkJQuerySelector _ selector) = "$(" ++ show (show selector) ++ ")"
  show (MkJQueryElement a element) = "$()"
  show (MkJQueryUnion _ (jsa :: jsbs)) = show jsa ++ ".add(" ++ show (MkJQueryUnion () jsbs) ++ ")"
  

unwrapJQ : JQuery a -> a
unwrapJQ (MkJQuery a) = a
unwrapJQ (MkJQuerySelector a _) = a
unwrapJQ (MkJQueryElement a _) = a
unwrapJQ (MkJQueryUnion a _) = a

clobberJQ : JQuery a -> JQuery ()
clobberJQ (MkJQuery _) = MkJQuery ()
clobberJQ (MkJQuerySelector _ s) = MkJQuerySelector () s
clobberJQ (MkJQueryElement _ e) = MkJQueryElement () e 
clobberJQ (MkJQueryUnion _ jqs) = MkJQueryUnion () jqs

mkJQUnion : JQuery a -> JQuery b -> JQuery a
mkJQUnion jqa jqb = MkJQueryUnion (unwrapJQ jqa) [(clobberJQ jqa), (clobberJQ jqb)]


instance Functor JQuery where
  map func (MkJQuery a) = MkJQuery (func a)
  map func (MkJQuerySelector a selector) = MkJQuerySelector (func a) selector
  map func (MkJQueryElement a e) = MkJQueryElement (func a) e
  map func (MkJQueryUnion a jqs) = MkJQueryUnion (func a) jqs


instance Applicative JQuery where
  pure = MkJQuery
  jqFunc <$> jq = map (unwrapJQ jqFunc) (mkJQUnion jq jqFunc)


instance Monad JQuery where
  jq >>= func = func (unwrapJQ jq)



-- (DOM) Manipulation
appendTo : JQuery a -> JQuery b -> IO ()
appendTo source target = mkForeign (
  FFun "$(%0).appendTo($(%1))" [FString, FString] FUnit
  ) (show source) (show target)
