module JQuery

-- These methods match the JQuery API as closely as possible
-- http://api.jquery.com/


-- monad that encapsulates the usages of $ that select DOM elements
data JQuery a = JQ a -- selects no elements
              | JQSelector JQSelector a
              | JQElement String a -- TODO: define Element type

data JQSelector = JQSelectID String


select : Selector -> JQuery ()
select = JQSelector


instance Monad JQuery where
  return = JQuery





-- (DOM) Manipulation
appendTo : String -> String -> IO ()
appendTo selector target = mkForeign (
  FFun "$(%0).appendTo(%1)" [FFunction FInt FInt, FInt] FInt
  ) selector target
