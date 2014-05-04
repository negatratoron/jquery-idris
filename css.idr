module Css


data CssSimpleSelector = CssUniversalSelector
                       | CssTypeSelector String

instance Eq CssSimpleSelector where
  CssUniversalSelector == CssUniversalSelector = True
  (CssTypeSelector s) == (CssTypeSelector s) = True
  _ == _ = False


instance Show CssSimpleSelector where
  show CssUniversalSelector = "*"
  show (CssTypeSelector type) = type


data CssPseudoSelector = CssFirstChild
                       | CssLink
                       | CssVisited
                       | CssActive
                       | CssHover
                       | CssFocus
                       | CssLang String


instance Show CssPseudoSelector where
  show CssFirstChild = ":first-child"
  show CssLink = ":link"
  show CssVisited = ":visited"
  show CssActive = ":active"
  show CssHover = ":hover"
  show CssFocus = ":focus"
  show (CssLang lang) = ":lang(" ++ lang ++ ")"


data CssSelectorMod = CssClassMod String -- sounds like a video game
                    | CssIdMod String
                    | CssAttributeSetMod String
                    | CssAttributeEqualMod String String -- name string
                    | CssAttributeSpaceListContainsMod String String -- name string
                    | CssAttributeHyphenListContainsMod String String -- name string
                    | CssPseudoSelectorMod CssPseudoSelector


instance Show CssSelectorMod where
  show (CssClassMod classname) = "." ++ classname ++ ")"
  show (CssIdMod id) = "#" ++ id
  show (CssAttributeSetMod attr) = "[" ++ attr ++ "]"
  show (CssAttributeEqualMod attr match) =
    "[" ++ attr ++ "=\"" ++ match ++ "\"]"
  show (CssAttributeSpaceListContainsMod attr match) =
    "[" ++ attr ++ "~=\"" ++ match ++ "\"]"
  show (CssAttributeHyphenListContainsMod attr match) =
    "[" ++ attr ++ "|=\"" ++ match ++ "\"]"


data CssCombinator = CssDescendant
                   | CssChild
                   | CssAdjacentSibling


instance Show CssCombinator where
  show CssDescendant = " "
  show CssChild = " > "
  show CssAdjacentSibling = " + "


data CssSelector = MkCssSelector CssSimpleSelector (List CssSelectorMod)
                 | MkCssSelectorChain CssSelector CssSelector CssCombinator


instance Show CssSelector where
  show (MkCssSelector simpleSelector mods) =
    if (length mods) > 0 && (simpleSelector == CssUniversalSelector) then
      selectorMods
    else
      show simpleSelector ++ selectorMods
    where
      selectorMods = foldr (++) "" $ map show mods
  show (MkCssSelectorChain s1 s2 c) = show s1 ++ show c ++ show s2
