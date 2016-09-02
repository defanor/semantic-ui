module BlockState where

import SDL

import Types


activeCoord :: BlockState Bool -> BlockState [Rectangle Int] -> Maybe [Rectangle Int]
activeCoord bsb bsc = case findBS (== True) bsb of
  [path] -> getBS bsc path
  _ -> Nothing
  

navigate :: Int -> BlockState Bool -> BlockState Bool
navigate i bs = case findBS (== True) bs of
  [path] -> let nxt = either id id (navigateB i bs path) in
    maybe bs id $ do
      deactivated <- setBS bs path False
      setBS deactivated nxt True
  _ -> case bs of
    BSRec a blocks -> BSRec True blocks

navigate' :: Int -> BlockState Bool -> BlockState Bool
navigate' i bs = case findBS (== True) bs of
  [path] -> let nxt = (if i < 0 then prevD else nextD) bs path in
    maybe bs id $ do
      deactivated <- setBS bs path False
      setBS deactivated nxt True
  _ -> case bs of
    BSRec a blocks -> BSRec True blocks

nextD :: BlockState a -> [Int] -> [Int]
nextD bs p = case getBS bs (p ++ [0]) of
  Just x -> p ++ [0] -- down
  Nothing -> case navigateB 1 bs p of
    Right x -> x -- side
    Left x -> upAndNext bs p

prevD :: BlockState a -> [Int] -> [Int]
prevD bs p = case reverse p of
  (0:xs) -> reverse xs -- up
  (n:xs) -> downToLast bs $ reverse $ (n - 1) : xs
  [] -> downToLast bs p


downToLast :: BlockState a -> [Int] -> [Int]
downToLast bs p = case neighbourCount bs (p ++ [0]) of
  0 -> if length p > 0 then take (length p - 1) p else p
  n -> downToLast bs $ p ++ [n - 1]

upAndNext :: BlockState a -> [Int] -> [Int]
upAndNext bs p = case reverse p of
  (_:x:xs) -> let path = reverse (x + 1 : xs) in
    case getBS bs path of
      Just x -> path
      Nothing -> upAndNext bs path
  other -> []


navigateB :: Int -> BlockState a -> [Int] -> Either [Int] [Int]
navigateB i bs p = case reverse p of
  (x:xs) -> let path = reverse $ x + i : xs in
    maybe (Left $ reverse $ (if i < 0 then neighbourCount bs p - 1 else 0) : xs)
                   (const (Right path))
                   $ getBS bs path
  [] -> Left []

neighbourCount :: BlockState a -> [Int] -> Int
neighbourCount bs [] = 1
neighbourCount bs p = case getBS' bs (take (length p - 1) p) of
          Just (BSRec _ b) -> length b
          Just (BSFew _ b) -> length b
          Nothing -> 0
          _ -> 1
    
getBS :: BlockState a -> [Int] -> Maybe a
getBS bs p = do
  b <- getBS' bs p
  pure $ case b of
           BSOne s -> s
           BSFew s _ -> s
           BSRec s _ -> s

getBS' :: BlockState a -> [Int] -> Maybe (BlockState a)
getBS' x [] = Just x
getBS' (BSRec _ bs) (x:xs) =
  if length bs > x && x >= 0 then getBS' (bs !! x) xs else Nothing
getBS' (BSFew _ is) [n] = -- a hack, should be harmless for now
  if length is > n && n >= 0 then Just (BSOne $ is !! n) else Nothing
getBS' _ _ = Nothing

setBS :: BlockState a -> [Int] -> a -> Maybe (BlockState a)
setBS (BSRec s blocks) [] x = Just $ BSRec x blocks
setBS (BSOne s) [] x = Just $ BSOne x
setBS (BSFew s is) [] x = Just $ BSFew x is
setBS (BSFew s is@(_:_)) [n] x = Just $ BSFew s (take n is ++ x : drop (n + 1) is)
setBS (BSRec ms bs@(_:_)) (x:xs) s = do
  let before = take x bs
      after = drop (x + 1) bs
  inner <- if length bs > x then setBS (bs !! x) xs s else Nothing
  pure $ BSRec ms (before ++ (inner:after))
setBS _ _ _ = Nothing

findBS :: (a -> Bool) -> BlockState a -> [[Int]]
findBS pred bs = findBS' [] pred bs

findBS' :: [Int] -> (a -> Bool) -> BlockState a -> [[Int]]
findBS' path pred (BSOne s) = if pred s then [path] else []
findBS' path pred (BSFew s is) =
  let cur = if pred s then [path] else []
      others = zipWith (\b pos -> if pred b then [path ++ [pos]] else []) is [0..]
  in concat $ cur : others
findBS' path pred (BSRec s bs) =
  let cur = if pred s then [path] else []
      others = zipWith (\b pos -> findBS' (path ++ [pos]) pred b) bs [0..]
  in concat $ cur : others
