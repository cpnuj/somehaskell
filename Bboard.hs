{-# language OverloadedStrings #-}

module Main (main) where

import Data.Time.Clock
import Data.Text (Text)

import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Lucid as H

import qualified Web.Twain as Twain
import Network.Wai.Handler.Warp (run, Port)

main :: IO ()
main = runServer 3000

runServer :: Port -> IO ()
runServer port = do
    app <- mkApp
    putStrLn $ unwords
        [ "Bulletin board running at"
        , "http://localhost:" <> show port
        , "(ctrl-c to quit)"
        ]
    run port app

mkApp :: IO Twain.Application
mkApp = do
    dummyPosts <- mkDummyPosts
    appState <- STM.newTVarIO AppState { appNextId = 1, appPosts = dummyPosts }
    foldr (<$>)
        (pure $ Twain.notFound $ Twain.send $ Twain.text "Error: not found")
        (routes appState)

routes :: STM.TVar AppState -> [Twain.Middleware]
routes appState =
    [ Twain.get "/" $ do
        posts <- liftIO $ appPosts <$> STM.readTVarIO appState
        Twain.send $ displayAllPosts posts

    , Twain.get "/post/:id" $ do
        pid <- Twain.param "id"
        posts <- liftIO $ appPosts <$> STM.readTVarIO appState
        Twain.send $ displayPost pid posts

    , Twain.get "/new" $
        Twain.send handleGetNewPost

    , Twain.post "/new" $ do
        title   <- Twain.param "title"
        author  <- Twain.param "author"
        content <- Twain.param "content"
        time    <- liftIO getCurrentTime
        let post = Post
                    { pTime    = time
                    , pTitle   = title
                    , pAuthor  = author
                    , pContent = content
                    }
        resp <- liftIO $ handleNewPost post appState
        Twain.send resp

    , Twain.post "/post/:id/delete" $ do
        pid <- Twain.param "id"
        res <- liftIO $ handleDeletePost pid appState
        Twain.send res
    ]

data Post = Post
    { pTime    :: UTCTime
    , pAuthor  :: Text
    , pTitle   :: Text
    , pContent :: Text
    }

type Posts = M.Map Integer Post

ppPost :: Post -> Text
ppPost post =
    let header = T.unwords
            [ "[" <> T.pack (show (pTime post)) <> "]"
            , pTitle post
            , "by"
            , pAuthor post
            ]
        seperator =
            T.replicate (T.length header) "-"
    in
        T.unlines [ seperator, header, seperator, pContent post, seperator ]

mkDummyPosts :: IO Posts
mkDummyPosts = do
    c <- getCurrentTime
    return $ M.singleton 0 Post
        { pTime    = c
        , pTitle   = "Dummy Title"
        , pAuthor  = "Dummy Author"
        , pContent = "bla bla bla..."
        }

displayAllPosts :: Posts -> Twain.Response
displayAllPosts =
    Twain.html . H.renderBS . template "Bulletin board - posts" . allPostsHtml

displayPost :: Integer -> Posts -> Twain.Response
displayPost pid posts = case M.lookup pid posts of
    Just post ->
        Twain.html . H.renderBS . template "Bulletin board - posts" $
            postHtml pid post
    Nothing   ->
        Twain.raw
            Twain.status404
            [("Content-Type", "text/plain; charset=utf-8")]
            "404 Not found."

data AppState = AppState
    { appNextId :: Integer
    , appPosts  :: Posts
    }

handleDeletePost :: Integer -> STM.TVar AppState -> IO Twain.Response
handleDeletePost pid appState = do
    found <- deletePost pid appState
    return $
        if found
        then Twain.redirect302 "/"
        else
            Twain.raw
                Twain.status404
                [("Content-Type", "text/html; charset=utf-8")]
                "404 Not Found."

deletePost :: Integer -> STM.TVar AppState -> IO Bool 
deletePost pid appStateTVar = STM.atomically $ do
    appState <- STM.readTVar appStateTVar
    let posts = appPosts appState
    case M.lookup pid posts of
        Just _  -> do
            STM.writeTVar
                appStateTVar
                appState { appPosts = M.delete pid posts }
            return True
        Nothing -> return False

type Html = H.Html ()

template :: T.Text -> Html -> Html
template title content = H.doctypehtml_ $ do
    H.head_ $ do
        H.meta_ [ H.charset_ "utf-8" ]
        H.title_ (H.toHtml title)
        H.link_ [ H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/style.css"  ]
    H.body_ $ do
        H.div_ [ H.class_ "main" ] $ do
            H.h1_ [ H.class_ "logo" ] $
                H.a_ [H.href_ "/"] "Bulletin Board"
            content

allPostsHtml :: Posts -> Html
allPostsHtml posts = do
    H.p_ [ H.class_ "new-button" ] $
        H.a_ [H.href_ "/new"] "New Post"
    mapM_ (uncurry postHtml) $ reverse $ M.toList posts

postHtml :: Integer -> Post -> Html
postHtml pid post = do
    H.div_ [ H.class_ "post" ] $ do
        H.div_ [ H.class_ "post-header" ] $ do
            H.h2_ [ H.class_ "post-title" ] $
                H.a_
                    [H.href_ ("/post/" <> T.pack (show pid))]
                    (H.toHtml $ pTitle post)

            H.span_ $ do
                H.p_ [ H.class_ "post-time" ] $
                    H.toHtml (T.pack (show (pTime post)))
                H.p_ [ H.class_ "post-author" ] $
                    H.toHtml (pAuthor post)

        H.div_ [H.class_ "post-content"] $ do
          H.toHtml (pContent post)

newPostHtml :: Html
newPostHtml = do
  H.form_
    [ H.method_ "post"
    , H.action_ "/new"
    , H.class_ "new-post"
    ]
    ( do
      H.p_ $ H.input_ [H.type_ "text", H.name_ "title", H.placeholder_ "Title..."]
      H.p_ $ H.input_ [H.type_ "text", H.name_ "author", H.placeholder_ "Author..."]
      H.p_ $ H.textarea_ [H.name_ "content", H.placeholder_ "Content..."] ""
      H.p_ $ H.input_ [H.type_ "submit", H.value_ "Submit", H.class_ "submit-button"]
    )

handleGetNewPost :: Twain.Response
handleGetNewPost =
    Twain.html . H.renderBS . template "Bulletin board - posts" $ newPostHtml

newPost :: Post -> AppState -> AppState
newPost post app =
    let pid = appNextId app in
    AppState
    { appNextId = pid + 1
    , appPosts  = M.insert pid post (appPosts app)
    }

handleNewPost :: Post -> STM.TVar AppState -> IO Twain.Response
handleNewPost post appstate = STM.atomically $ do
    STM.modifyTVar appstate (newPost post)
    return $ Twain.redirect303 "/"
