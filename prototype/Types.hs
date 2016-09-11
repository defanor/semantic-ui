{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.AttoLisp as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data Inline = IText String
            | ILink String String
            deriving (Show, Eq, Read)

data Block = BParagraph [Inline]
           | BImage String
           | BSection String [Block]
           deriving (Show, Eq, Read)

instance ToLisp Inline where
   toLisp (IText str) = L.toLisp str
   toLisp (ILink str target) = L.mkStruct "link" [L.toLisp str, L.toLisp target]

instance FromLisp Inline where
  parseLisp (String s) = pure $ IText $ T.unpack s
  parseLisp l = struct "link" ILink l

instance ToLisp Block where
  toLisp (BParagraph inl) = mkStruct "paragraph" $ Prelude.map toLisp inl
  toLisp (BImage src) = mkStruct "image" [toLisp src]
  toLisp (BSection t bs) = mkStruct "section" $ toLisp t : Prelude.map toLisp bs

instance FromLisp Block where
  parseLisp (List (Symbol "paragraph" : inl)) = BParagraph <$> mapM parseLisp inl
  parseLisp (List [Symbol "image", src]) = BImage <$> parseLisp src
  parseLisp (List (Symbol "section" : title : bs)) = BSection <$> parseLisp title <*> mapM parseLisp bs
  parseLisp other = fail $ "Failed to parse: " ++ show other


printBlock :: Block -> BL.ByteString
printBlock = encode

parseBlock :: BL.ByteString -> Either String Block
parseBlock x = (A.eitherResult $ A.parse lisp x) >>= parseEither parseLisp
