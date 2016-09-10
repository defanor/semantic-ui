import Types
import Text.Feed.Types
import Text.Feed.Import
import Text.Atom.Feed as TAF
import Network.Download
import Text.HTML.TagSoup

import Data.Maybe
import qualified Data.ByteString as B

main :: IO ()
main = do
  r <- openURIString "http://xkcd.com/atom.xml"
  d <- case r of
    Right b -> case parseFeedString b of
      Just (AtomFeed f) -> makeDoc f
      _ -> pure $ err "Failed to parse the feed"
    Left e -> pure $ err $ "Failed to retrieve the feed: " ++ e
  putStrLn $ show d
  where
    err x = (BSection "Feed reader"
           [BParagraph [IText x]])

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
  case lookup "src" img of
    Nothing -> pure []
    Just uri -> do
      r <- openURI uri
      case r of
        Right b -> do
          let fn = "/tmp/" ++ (map fileName $ fromJust $ lookup "src" img)
          B.writeFile fn b
          let picture = Just $ BImage fn
          let alt = lookup "alt" img >>= \a -> pure (BParagraph [IText a])
          pure $ catMaybes [picture, alt]
        _ -> pure []
  where fileName '/' = '_'
        fileName x = x
extractBlocks' _ = pure []
