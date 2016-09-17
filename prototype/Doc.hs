{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
import Types
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Text.Pandoc as P
import Text.Pandoc.MediaBag
import System.FilePath
import System.Environment
import Data.Default
import Data.Maybe
import Control.Monad.Identity
import Data.List

import Text.Parsec

type BParser = ParsecT [P.Block] ()
type IParser = ParsecT [P.Inline] ()

bSatisfy :: (Stream s m P.Block) => (P.Block -> Bool) -> ParsecT s u m P.Block
bSatisfy f = tokenPrim (\c -> show c)
             (\pos c _cs -> pos) -- todo: actually update it
             (\c -> if f c then Just c else Nothing)

iSatisfy :: (Stream s m P.Inline) => (P.Inline -> Bool) -> ParsecT s u m P.Inline
iSatisfy f = tokenPrim (\c -> show c)
             (\pos c _cs -> pos)
             (\c -> if f c then Just c else Nothing)

bAny :: (Stream s m P.Block) => ParsecT s u m P.Block
bAny = bSatisfy (const True)

iAny :: (Stream s m P.Inline) => ParsecT s u m P.Inline
iAny = iSatisfy (const True)

blockGT :: Int -> P.Block -> Bool
blockGT n (P.Header m _ _) = n < m
blockGT _ _ = True

block :: Monad m => BParser m P.Block -> BParser m Block
block pBlock = do
  b <- pBlock
  case b of
    P.Header i attr inl -> do
      children <- many (block $ bSatisfy (blockGT i))
      pure $ BSection (concatMap inlineText inl) children
    P.Para inl -> case parse (many inline) "paragraph" inl of
      Left err -> fail $ show err
      Right r -> pure $ BParagraph r
    P.CodeBlock attr str -> pure $ BCode Nothing str
    other -> pure $ BParagraph [IText $ "Not implemented: " ++ show other]

isStr :: P.Inline -> Bool
isStr (P.Str _) = True
isStr _ = False

isWhitespace :: P.Inline -> Bool
isWhitespace P.Space = True
isWhitespace P.SoftBreak = True
isWhitespace _ = False

whitespace :: Monad m => IParser m ()
whitespace = iSatisfy isWhitespace >> pure ()

iString :: Monad m => IParser m String
iString = do
  i <- iAny
  case i of
    P.Str str -> pure str
    _ -> fail "not a string"

iText :: Monad m => IParser m Inline
iText = (IText . intercalate " ") <$>
  (option () whitespace *>
   many1 (try $ iString <* option () whitespace))

inline :: Monad m => IParser m Inline
inline = choice [try iText, nonStrInline]

inlineText :: P.Inline -> String
inlineText (P.Str s) = s
inlineText P.Space = " "
inlineText other = "not implemented: " ++ show other

nonStrInline :: Monad m => IParser m Inline
nonStrInline = do
  i <- iAny
  case i of
    P.Str str -> fail "string in non-string inline"
    P.Link attr inl (uri, title) -> pure $ ILink (concatMap inlineText inl) uri
    P.Code attr str -> pure $ ICode Nothing str
    other -> pure $ IText $ "not implemented: " ++ show other

composeDoc :: P.Pandoc -> IO Block
composeDoc (P.Pandoc meta blocks) = do
  case parse (many (block bAny)) "input" blocks of
    Left err -> fail $ show err
    Right r -> pure $ BSection "doc" r

main :: IO ()
main = do
  a <- getArgs
  case a of
    [fn] -> case P.getReader $ tail $ takeExtension fn of
      Left err -> putStrLn err
      Right reader -> do
        doc <- case reader of
          P.StringReader f -> do
            r <- f def =<< readFile fn
            case r of
              Left err -> fail $ show err
              Right d -> composeDoc d
          P.ByteStringReader f -> do
            r <- f def =<< L8.readFile fn
            case r of
              Left err -> fail $ show err
              Right (d, b) -> composeDoc d
        L8.putStrLn $ printBlock doc
    _ -> putStrLn "Usage: doc <file name>"
