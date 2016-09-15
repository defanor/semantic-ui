import Text.Feed.Types as FT
import Text.Feed.Import
import Text.Atom.Feed as TAF
import Text.Feed.Query
import Text.RSS.Syntax as RS
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.HTML.TagSoup
import Data.Maybe
import System.Environment

import Network.HTTP.Simple
import Network.HTTP.Client
import Control.Concurrent.Async
import System.Directory
import System.FilePath
import Control.Monad

import Types


main :: IO ()
main = do
  cacheDir <- getXdgDirectory XdgCache "feeds"
  createDirectoryIfMissing True cacheDir
  getArgs >>= mapConcurrently (getFeed cacheDir) >>= L8.putStrLn . printBlock . BSection "Feeds"

getFeed :: FilePath -> String -> IO Block
getFeed cd uri = do
  r <- httpLBS =<< parseRequest uri
  case parseFeedString (L8.unpack $ responseBody r) of
    Just f -> BSection (getFeedTitle f) <$> mapConcurrently (parseItem cd) (getFeedItems f)
    Nothing -> pure $ BSection "error" [BParagraph [IText "Failed to parse", ILink uri uri]]

parseItem :: FilePath -> Item -> IO Block
parseItem cd (AtomItem e) = do
  summary <- maybe (pure []) (extractBlocks cd) $ entrySummary e
  pure $ BSection (showTC $ entryTitle e) $ concat
    [[BParagraph [IText $ "Updated: " ++ entryUpdated e]],
     summary,
     [BParagraph (IText "URIs:" : map makeURI (entryLinks e))]]
parseItem cd (FT.RSSItem i) = BSection (maybe "No title" id (rssItemTitle i)) <$> do
  descr <- maybe (pure []) (extractBlocks' cd . parseTags) $ rssItemDescription i
  pure $ concat [maybe [] (\x -> [BParagraph [IText $ "Updated: " ++ x]]) (rssItemPubDate i),
                 descr,
                 maybe [] (\x -> [BParagraph [IText "URI: ", ILink x x]]) (rssItemLink i)]
parseItem _ _ = undefined


showTC :: TextContent -> String
showTC (TextString s) = s
showTC other = show other

makeURI :: Link -> Inline
makeURI l = ILink (maybe (linkHref l) id (linkTitle l)) (linkHref l)

-- that's awkward, but apparently the recent atom revision allows html
-- in summaries
extractBlocks :: FilePath -> TextContent -> IO [Block]
extractBlocks cd (HTMLString s) = extractBlocks' cd $ parseTags s
extractBlocks _ other = pure [BParagraph [IText $ showTC other]]

extractBlocks' :: FilePath -> [Tag String] -> IO [Block]
extractBlocks' _ [] = pure []
extractBlocks' cd (TagOpen "img" img : xs) =
  case lookup "src" img of
    Nothing -> pure []
    Just uri -> do
      let fn = cd </> (fromJust $ lookup "src" img)
          picture = Just $ BImage fn
          alt = lookup "alt" img >>= \a -> pure (BParagraph [IText a])
      exists <- doesFileExist fn
      unless exists $ do
        createDirectoryIfMissing True (takeDirectory fn)
        r <- httpLBS =<< parseRequest uri
        L8.writeFile fn $ responseBody r
      rest <- extractBlocks' cd xs
      pure $ catMaybes [picture, alt] ++ rest
extractBlocks' cd (TagText txt : xs) = (BParagraph [IText txt] :) <$> extractBlocks' cd xs
extractBlocks' cd (_:xs) = extractBlocks' cd xs
