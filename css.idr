module Css


data CssSimpleSelector = CssUniversalSelector
                       | CssTypeSelector String


data CssPseudoSelector = CssFirstChild
                       | CssLink
                       | CssVisited
                       | CssActive
                       | CssHover
                       | CssFocus
                       | CssLang String

data CssSelectorMod = CssClassMod String -- sounds like a video game
                    | CssIdMod String
                    | CssAttributeSetMod String
                    | CssAttributeEqualMod String String -- name string
                    | CssAttributeSpaceListContainsMod String String -- name string
                    | CssAttributeHyphenListContainsMod String String -- name string
                    | CssPseudoSelectorMod CssPseudoSelector


data CssCombinator = CssDescendant
                   | CssChild
                   | CssAdjacentSibling

instance Show CssCombinator where
  show CssDescendant = " "
  show CssChild = " > "
  show CssAdjacentSibling = " + "


data CssSelector = MkCssSelector CssSimpleSelector (List CssSelectorMod)
                 | MkCssSelectorChain CssSelector CssSelector CssCombinator


