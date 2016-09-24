{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.AttoLisp as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

data Inline = IText String
            | ILink String String
            | ICode (Maybe String) String
            deriving (Show, Eq, Read)

data Block = BParagraph [Inline]
           | BImage String
           | BSection String [Block]
           | BCode (Maybe String) String
           deriving (Show, Eq, Read)

instance ToLisp Inline where
   toLisp (IText str) = L.toLisp str
   toLisp (ILink str target) = L.mkStruct "link" [L.toLisp str, L.toLisp target]
   toLisp (ICode Nothing code) = L.mkStruct "code" [L.toLisp code]
   toLisp (ICode (Just lang) code) = L.mkStruct "code" [L.toLisp lang, L.toLisp code]

instance FromLisp Inline where
  parseLisp (String s) = pure $ IText $ T.unpack s
  parseLisp (List [Symbol "code", code]) = ICode Nothing <$> parseLisp code
  parseLisp (List [Symbol "code", lang, code]) = ICode . Just <$> parseLisp lang <*> parseLisp code
  parseLisp l = struct "link" ILink l

instance ToLisp Block where
  toLisp (BParagraph inl) = mkStruct "paragraph" $ Prelude.map toLisp inl
  toLisp (BCode Nothing l) = mkStruct "code" [toLisp l]
  toLisp (BCode (Just lang) l) = mkStruct "code" $ [toLisp lang, toLisp l]
  toLisp (BImage src) = mkStruct "image" [toLisp src]
  toLisp (BSection t bs) = mkStruct "section" $ toLisp t : Prelude.map toLisp bs

instance FromLisp Block where
  parseLisp (List (Symbol "paragraph" : inl)) = BParagraph <$> mapM parseLisp inl
  parseLisp (List [Symbol "code", code]) = BCode Nothing <$> parseLisp code
  parseLisp (List [Symbol "code", lang, code]) = BCode . Just <$> parseLisp lang <*> parseLisp code
  parseLisp (List [Symbol "image", src]) = BImage <$> parseLisp src
  parseLisp (List (Symbol "section" : title : bs)) = BSection <$> parseLisp title <*> mapM parseLisp bs
  parseLisp other = fail $ "Failed to parse: " ++ show other

printInline :: Inline -> BL.ByteString
printInline = encode

printBlock :: Block -> BL.ByteString
printBlock = encode

parseBlock :: BL.ByteString -> Either String Block
parseBlock x = (A.eitherResult $ A.parse lisp x) >>= parseEither parseLisp
