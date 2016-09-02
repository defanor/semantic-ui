module Types where

data Inline = IText String
            | ILink String String
            deriving (Show, Eq, Read)

data Block = BParagraph [Inline]
           | BImage String
           | BSection String [Block]
           deriving (Show, Eq, Read)

data BlockState a = BSFew a [a]
                  | BSOne a
                  | BSRec a [BlockState a]
                  deriving (Show, Eq)

