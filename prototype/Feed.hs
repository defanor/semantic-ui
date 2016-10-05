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
import Network.URI

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
    Just f -> mapConcurrently (parseItem uri cd) (getFeedItems f)
    Nothing -> pure []

parseItem :: String -> FilePath -> Item -> IO (Maybe (UTCTime, Block))
parseItem uri cd i = do
  case (getItemPublishDate i, getItemSummary i <|> getItemDescription i) of
    (Just (Just t), Just sum) -> do
      summary <- extractBlocks uri cd $ parseTags sum
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

imageURI :: String -> [Attribute String] -> Maybe String
imageURI uri img = do
  src <- lookup "src" img
  if isRelativeReference src
    then do
    src' <- parseRelativeReference src
    uri' <- parseAbsoluteURI uri
    pure . show $ src' `relativeTo` uri'
    else pure src

extractBlocks :: String -> FilePath -> [Tag String] -> IO [Block]
extractBlocks _ _ [] = pure []
extractBlocks uri cd (TagOpen "img" img : xs) =
  case imageURI uri img of
    Just absURI -> do
      let fn = cd </> absURI
          picture = Just $ BImage fn
          alt = lookup "alt" img >>= \a -> pure (BParagraph [IText a])
      exists <- doesFileExist fn
      unless exists $ do
        createDirectoryIfMissing True (takeDirectory fn)
        r <- httpLBS =<< parseRequest absURI
        L8.writeFile fn $ responseBody r
      rest <- extractBlocks uri cd xs
      pure $ catMaybes [picture, alt] ++ rest
extractBlocks uri cd (TagText txt : xs) = (BParagraph [IText txt] :) <$> extractBlocks uri cd xs
extractBlocks uri cd (_:xs) = extractBlocks uri cd xs
