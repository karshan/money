module HTML where

type Attributes = [(JSString, JSString)]
createElt :: (MonadIO m, IsNode n, MonadReader Document m) => JSString -> Attributes -> [n] -> m Element
createElt tag attrs children = do
    doc <- ask
    e <- fromMaybe (error $ "createElement returned null for " ++ show tag) <$>
        createElement doc (Just tag)
    mapM_ (uncurry (setAttribute e)) attrs
    mapM_ (appendChild e . Just) children
    return e

div_ :: (MonadIO m, IsNode n, MonadReader Document m) => Attributes -> [n] -> m Element
div_ = createElt "div"

text :: (MonadIO m, MonadReader Document m) => JSString -> m Text
text t = do
    doc <- ask
    fromMaybe (error $ "createTextNode returned null for " ++ show t) <$>
        createTextNode doc t


