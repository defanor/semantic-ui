import Types
import Text.Feed.Types
import Text.Feed.Import
import Text.Atom.Feed as TAF
import Network.HTTP
import Text.HTML.TagSoup

import Data.Maybe
import qualified Data.ByteString as B
import Network.Stream
import Network.URI

main :: IO ()
main = do
  r <- simpleHTTP (getRequest "http://xkcd.com/atom.xml")
  b <- getResponseBody r
  d <- case parseFeedString b of
    Just (AtomFeed f) -> makeDoc f
    _ -> pure err
  putStrLn $ show d
  where
    err = (BSection "Feed reader"
           [BParagraph [IText "Failed to retrieve the feed"]])

showTC :: TextContent -> String
showTC (TextString s) = s
showTC other = show other

makeDoc :: TAF.Feed -> IO Block
makeDoc f = BSection (showTC $ feedTitle f) . concat <$> mapM makeSection (feedEntries f)

makeSection :: TAF.Entry -> IO [Block]
makeSection e = case entrySummary e of
  Nothing -> pure []
  Just s -> do
    summary <- extractBlocks s
    pure $ [BSection (showTC $ entryTitle e) $ summary ++
            [BSection "URIs" [BParagraph $ map makeURI (entryLinks e)]]]

makeURI :: Link -> Inline
makeURI l = ILink (maybe (linkHref l) id (linkTitle l)) (linkHref l)

-- that's awkward, but apparently the recent atom revision allows html
-- in summaries
extractBlocks :: TextContent -> IO [Block]
extractBlocks (HTMLString s) = extractBlocks' $ parseTags s
extractBlocks other = pure [BParagraph [IText $ showTC other]]

extractBlocks' :: [Tag String] -> IO [Block]
extractBlocks' (TagOpen "img" img : xs) =
  case lookup "src" img >>= parseURI of
    Nothing -> pure []
    Just uri -> do
      r <- simpleHTTP (Request uri GET [] B.empty) :: IO (Result (Response B.ByteString))
      b <- getResponseBody r
      let fn = "/tmp/" ++ (drop 28 $ fromJust $ lookup "src" img)
      B.writeFile fn b
      let picture = Just $ BImage fn
      let alt = lookup "alt" img >>= \a -> pure (BParagraph [IText a])
      -- writeFile fn b
      pure $ catMaybes [picture, alt]
extractBlocks' _ = pure []
