module JQuery

-- These methods match the JQuery API as closely as possible
-- http://api.jquery.com/



data JQSelector = JQSelectID String


data ElementType = Body
                 | Canvas
                 | Div


data Attribute = Height Int
               | Id String
               | Width Int


-- monad that encapsulates the usages of $ that select DOM elements
data JQuery a = MkJQuery a -- selects no elements
              | MkJQuerySelector a JQSelector
              | MkJQueryElement a ElementType (List Attribute)
              | MkJQueryUnion a (List (JQuery ()))


unwrapJQ : JQuery a -> a
unwrapJQ (MkJQuery a) = a
unwrapJQ (MkJQuerySelector a _) = a
unwrapJQ (MkJQueryElement a _ _) = a
unwrapJQ (MkJQueryUnion a _) = a

clobberJQ : JQuery a -> JQuery ()
clobberJQ (MkJQuery _) = MkJQuery ()
clobberJQ (MkJQuerySelector _ s) = MkJQuerySelector () s
clobberJQ (MkJQueryElement _ et attrs) = MkJQueryElement () et attrs
clobberJQ (MkJQueryUnion _ jqs) = MkJQueryUnion () jqs

mkJQUnion : JQuery a -> JQuery b -> JQuery a
mkJQUnion jqa jqb = MkJQueryUnion (unwrapJQ jqa) [(clobberJQ jqa), (clobberJQ jqb)]


instance Functor JQuery where
  map func (MkJQuery a) = MkJQuery (func a)
  map func (MkJQuerySelector a selector) = MkJQuerySelector (func a) selector
  map func (MkJQueryElement a et attrs) = MkJQueryElement (func a) et attrs
  map func (MkJQueryUnion a jqs) = MkJQueryUnion (func a) jqs


instance Applicative JQuery where
  pure = MkJQuery
  jqFunc <$> jq = map (unwrapJQ jqFunc) (mkJQUnion jq jqFunc)


instance Monad JQuery where
  jq >>= func = func (unwrapJQ jq)



-- (DOM) Manipulation
appendTo : JQuery a -> String -> IO a
appendTo selector target = mkForeign (
  FFun "$(%0).appendTo(%1)" [FFunction FInt FInt, FInt] FInt
  ) selector target
