module CSS

data CSSCombinator = CSSDescendant
                   | CSSChild
                   | CSSAdjacentSibling

instance Show CSSCombinator where
  show CSSDescendant = " "
  show CSSChild = " > "
  show CSSAdjacentSibling = " + "





data CSSSimpleSelector = CSSUniversalSelector
                       | CSSTypeSelector String


data CSSSelectorMod = CSSClassSelectorMod String
                    | CSSIdSelectorMod String
                    | CSSAttributeSetSelector String
                    | CSSAttributeEqualSelector String String -- name string
                    | CSSAttributeContainsSelector String String -- name string
