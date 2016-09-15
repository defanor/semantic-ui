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
import Control.Applicative
import Data.List
import Data.Time.Clock

import Types


main :: IO ()
main = do
  cacheDir <- getXdgDirectory XdgCache "feeds"
  createDirectoryIfMissing True cacheDir
  entries <- getArgs >>= mapConcurrently (getFeed cacheDir)
  L8.putStrLn $ printBlock $ BSection "Entries" $
    map snd $ reverse $  sortOn fst $ catMaybes $ concat entries

getFeed :: FilePath -> String -> IO [Maybe (UTCTime, Block)]
getFeed cd uri = do
  r <- httpLBS =<< parseRequest uri
  case parseFeedString (L8.unpack $ responseBody r) of
    Just f -> mapConcurrently (parseItem cd) (getFeedItems f)
    Nothing -> pure []

parseItem :: FilePath -> Item -> IO (Maybe (UTCTime, Block))
parseItem cd i = do
  case (getItemPublishDate i, getItemSummary i <|> getItemDescription i) of
    (Just (Just t), Just sum) -> do
      summary <- extractBlocks' cd $ parseTags sum
      let blocks = concat
            [[BParagraph [IText $ "Date: " ++ show t]],
             summary,
             maybe [] (\uri -> [BParagraph [IText "URI: ", ILink uri uri]]) (getItemLink i)]
      pure $ Just (t, BSection (maybe "No title" id $ getItemTitle i) blocks)
    _ -> pure Nothing


showTC :: TextContent -> String
showTC (TextString s) = s
showTC other = show other

makeURI :: Link -> Inline
makeURI l = ILink (maybe (linkHref l) id (linkTitle l)) (linkHref l)

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
