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

import Types


main :: IO ()
main = getArgs >>= mapM getFeed >>= putStrLn . show . BSection "Feeds"

getFeed :: String -> IO Block
getFeed uri = do
  r <- httpLBS =<< parseRequest uri
  case parseFeedString (L8.unpack $ responseBody r) of
    Just f -> BSection (getFeedTitle f) <$> mapM parseItem (getFeedItems f)
    Nothing -> pure $ BSection "error" [BParagraph [IText "Failed to parse", ILink uri uri]]

parseItem :: Item -> IO Block
parseItem (AtomItem e) = do
  summary <- maybe (pure []) extractBlocks $ entrySummary e
  pure $ BSection (showTC $ entryTitle e) $ concat
    [[BParagraph [IText $ "Updated: " ++ entryUpdated e]],
     summary,
     [BParagraph (IText "URIs:" : map makeURI (entryLinks e))]]
parseItem (FT.RSSItem i) = BSection (maybe "No title" id (rssItemTitle i)) <$> do
  descr <- maybe (pure []) (extractBlocks' . parseTags) $ rssItemDescription i
  pure descr
parseItem x = undefined


showTC :: TextContent -> String
showTC (TextString s) = s
showTC other = show other

makeURI :: Link -> Inline
makeURI l = ILink (maybe (linkHref l) id (linkTitle l)) (linkHref l)

-- that's awkward, but apparently the recent atom revision allows html
-- in summaries
extractBlocks :: TextContent -> IO [Block]
extractBlocks (HTMLString s) = extractBlocks' $ parseTags s
extractBlocks other = pure [BParagraph [IText $ showTC other]]

extractBlocks' :: [Tag String] -> IO [Block]
extractBlocks' [] = pure []
extractBlocks' (TagOpen "img" img : xs) =
  case lookup "src" img of
    Nothing -> pure []
    Just uri -> do
      r <- httpLBS =<< parseRequest uri
      let fn = "/tmp/" ++ (map fileName $ fromJust $ lookup "src" img)
          picture = Just $ BImage fn
          alt = lookup "alt" img >>= \a -> pure (BParagraph [IText a])
      L8.writeFile fn $ responseBody r
      rest <- extractBlocks' xs
      pure $ catMaybes [picture, alt] ++ rest
  where fileName '/' = '_'
        fileName x = x
extractBlocks' (TagText txt : xs) = (BParagraph [IText txt] :) <$> extractBlocks' xs
extractBlocks' (_:xs) = extractBlocks' xs
