module Css

data CssCombinator = CssDescendant
                   | CssChild
                   | CssAdjacentSibling

instance Show CssCombinator where
  show CssDescendant = " "
  show CssChild = " > "
  show CssAdjacentSibling = " + "



data CssSimpleSelector = CssUniversalSelector
                       | CssTypeSelector String


data CssSelectorMod = CssClassSelectorMod String
                    | CssIdSelectorMod String
                    | CssAttributeSetSelector String
                    | CssAttributeEqualSelector String String -- name string
                    | CssAttributeContainsSelector String String -- name string
                    | CssPseudoSelectorMod CssPseudoSelector


data CssSelector = CssSelector CssSimpleSelector [CssSelectorMod]
                 | CssSelectorChain CssSelector CssSelector CssCombinator


