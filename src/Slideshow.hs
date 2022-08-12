--{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Slideshow where

import Arc.Util
import Arc.Widgets.Code (CodeBlock (..), codeBlock)
import Control.Lens ((%~), (&), (.~))
import Control.Monad (void)
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Default
import Data.Proxy
import Data.Text (Text)
import Reflex.Dom hiding (list)
import Reflex.Dom.Builder.Class

data SlideData t m = SlideData
    { slideNumber :: Int
    , slideTitle :: Maybe Text
    , slideSubtitle :: Maybe Text
    , slideSublitle :: Maybe Text
    , slideClassName :: Text
    , slideStartTime :: Int
    , slideDuration :: Int
    , slideContent :: (DomBuilder t m, PostBuild t m) => m ()
    , slideCounter :: Reflex t => Dynamic t Int
    , slideActive :: Reflex t => Dynamic t Bool
    }

instance Default (SlideData t m) where
    def =
        SlideData
            { slideNumber = 0
            , slideTitle = Nothing
            , slideSubtitle = Nothing
            , slideSublitle = Nothing
            , slideClassName = "slide"
            , slideStartTime = 1
            , slideDuration = 1
            , slideContent = blank
            , slideCounter = constDyn 0
            , slideActive = constDyn False
            }

data SlideshowData t m = SlideshowData
    { slideshowTitle :: Maybe Text
    , slideshowSubtitle :: Maybe Text
    , slideshowAuthor :: Maybe Text
    , slideshowSections :: [SlideshowSection]
    , slideshowSlides :: [SlideData t m]
    , slideshowDuration :: Int
    , slideshowCounter :: Reflex t => Dynamic t Int
    , slideshowActiveSlide :: Reflex t => Dynamic t Int
    }

data SlideshowSection = SlideshowSection
    { sectionName :: Text
    , sectionPosition :: Int
    }

instance Default (SlideshowData t m) where
    def =
        SlideshowData
            { slideshowTitle = Nothing
            , slideshowSubtitle = Nothing
            , slideshowAuthor = Nothing
            , slideshowSections = []
            , slideshowSlides = []
            , slideshowDuration = 0
            , slideshowCounter = constDyn 0
            , slideshowActiveSlide = constDyn 0
            }

type Slideshow t m = StateT (SlideshowData t m) m

setSlideshowTitle :: Monad m => Text -> Slideshow t m ()
setSlideshowTitle t = modify $ \s -> s{slideshowTitle = Just t}

setSlideshowSubtitle :: Monad m => Text -> Slideshow t m ()
setSlideshowSubtitle t = modify $ \s -> s{slideshowSubtitle = Just t}

update :: (DomBuilder t m, PostBuild t m) => Dynamic t a -> (a -> m b) -> m (Event t b)
update d f = dyn $ f <$> d

constrain :: Int -> Int -> Int -> Int
constrain a b = max a . min b

slideshow :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Slideshow t m () -> m ()
slideshow s = mdo
    let left = keyup ArrowLeft e
    let right = keyup ArrowRight e
    let space = keyup Space e
    let click = domEvent Click e
    let rightClick = domEvent Contextmenu e
    let mouseMove = domEvent Mousemove e
    let cfg =
            (def :: ElementConfig EventResult t (DomBuilderSpace m))
                & elementConfig_initialAttributes .~ ("tabindex" =: "0")
                & elementConfig_modifyAttributes .~ ((\c -> "data-active-slide" =: Just (tshow c)) <$> updated slideCounter)
                & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Contextmenu (const preventDefault)
    (e, slideCounter) <- element "main" cfg $ mdo
        c <- foldDyn ((constrain 1 slideshowDuration .) . (+)) 1 (leftmost [1 <$ leftmost [right, space, click], (-1) <$ leftmost [left, rightClick]])
        (a, SlideshowData{..}) <- runStateT s $ def{slideshowCounter = c}
        let activeSlide =
                leftmost $
                    ( \SlideData{..} ->
                        push
                            ( \a ->
                                return $ if a then Just slideNumber else Nothing
                            )
                            $ updated slideActive
                    )
                        <$> slideshowSlides
        slideCounter <- holdDyn 1 activeSlide
        el "time" $ do
            display c
            text " / "
            text $ tshow slideshowDuration
        return slideCounter
    blank

type Slide t m = StateT (SlideData t m) (Slideshow t m)

slide :: forall t m. (DomBuilder t m, PostBuild t m) => Slide t m () -> Slideshow t m ()
slide s = do
    modify $ \ssd -> ssd{slideshowDuration = slideshowDuration ssd + 1}
    SlideshowData{..} <- get
    (a, sd@SlideData{..}) <-
        runStateT s $
            def
                { slideNumber = length slideshowSlides
                , slideStartTime = slideshowDuration
                , slideCounter = slideshowCounter
                }
    let slideActive :: Dynamic t Bool
        slideActive = do
            c <- slideshowCounter
            return $ c >= slideStartTime && c < slideStartTime + slideDuration
        className :: Dynamic t Text
        className = (\a -> slideClassName <> " " <> if a then "active" else "inactive") <$> slideActive
        slideAttrs =
            mconcat
                <$> sequenceA
                    [ ("class" =:) <$> className
                    , return $ "data-slide-number" =: tshow slideNumber
                    , return $ "data-from" =: tshow slideStartTime
                    , return $ "data-to" =: tshow (slideStartTime + slideDuration)
                    ]
    lift $
        elDynAttr "section" slideAttrs $ do
            maybe blank (el "h2" . text) slideTitle
            maybe blank (elClass "div" "subtitle" . text) slideSubtitle
            slideContent
    modify $ \st ->
        st
            { slideshowDuration = slideshowDuration + slideDuration - 1
            , slideshowSlides = slideshowSlides ++ [sd{slideActive = slideActive}]
            }

titleSlide :: forall t m. (DomBuilder t m, PostBuild t m) => Slideshow t m ()
titleSlide = do
    SlideshowData{..} <- get
    slide $ do
        setSlideClassName "slide title-slide"
        setSlideContent $
            appendContent $ do
                maybe blank (elClass "h1" "title" . text) slideshowTitle
                maybe blank (elClass "p" "subtitle" . text) slideshowSubtitle

section :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> Slideshow t m ()
section name = do
    modify $ \sd ->
        sd
            { slideshowSections =
                slideshowSections sd
                    ++ [ SlideshowSection{sectionName = name, sectionPosition = slideshowDuration sd}
                       ]
            }
    slide $ do
        setSlideClassName "slide section-slide"
        setSlideContent $ appendContent $ elClass "h2" "title section-title" $ text name

setSlideTitle :: Monad m => Text -> Slide t m ()
setSlideTitle t = modify $ \s -> s{slideTitle = Just t}

setSlideSubtitle :: Monad m => Text -> Slide t m ()
setSlideSubtitle t = modify $ \s -> s{slideSubtitle = Just t}

setSlideClassName :: Monad m => Text -> Slide t m ()
setSlideClassName t = modify $ \s -> s{slideClassName = t}

type SlideContent t m = StateT (m ()) (Slide t m)

setSlideContent :: (DomBuilder t m, PostBuild t m) => SlideContent t m () -> Slide t m ()
setSlideContent c = modify (\s -> s{slideContent = blank}) >> appendSlideContent c

appendSlideContent :: (DomBuilder t m, PostBuild t m) => SlideContent t m () -> Slide t m ()
appendSlideContent c = do
    SlideData{..} <- get
    (a, sc) <- runStateT c slideContent
    modify $ \s -> do
        s{slideContent = sc}

appendContent :: Monad m => m () -> SlideContent t m ()
appendContent = modify . flip (>>)

pause :: Monad m => SlideContent t m ()
pause = lift $ modify $ \s -> s{slideDuration = slideDuration s + 1}

unpause :: Monad m => SlideContent t m ()
unpause = lift $ modify $ \s -> s{slideDuration = slideDuration s - 1}

timedEl :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> m () -> SlideContent t m ()
timedEl elementTag = timedElClass elementTag ""

timedElClass :: forall t m a. (DomBuilder t m, PostBuild t m) => Text -> Text -> m () -> SlideContent t m ()
timedElClass elementTag elementClass child = do
    pause
    SlideData{..} <- lift get
    let className :: Dynamic t Text
        className = (\c -> elVisible c <> " " <> elementClass) <$> slideCounter
        showTime :: Int
        showTime = slideStartTime + slideDuration - 1
        elVisible :: Int -> Text
        elVisible c
            | c >= showTime = "visible"
            | otherwise = "invisible"
    let el = elDynAttr elementTag ((\cn c -> "class" =: cn <> "data-from" =: tshow showTime) <$> className <*> slideCounter) child
    appendContent el

type List t m = SlideContent t m ()

li :: forall t m. (DomBuilder t m, PostBuild t m) => m () -> List t m
li = timedEl "li"

liClass :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> m () -> List t m
liClass = timedElClass "li"

li' :: forall t m. (DomBuilder t m, PostBuild t m) => SlideContent t m () -> List t m
li' m = do
    (a, st) <- lift $ runStateT m blank
    appendContent $ el "li" st

list :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> List t m -> SlideContent t m ()
list tag m = do
    (a, st) <- lift $ runStateT m blank
    appendContent $ el tag st

listClass :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> Text -> List t m -> SlideContent t m ()
listClass tag className m = do
    (a, st) <- lift $ runStateT m blank
    appendContent $ elClass tag className st

unorderedList :: forall t m. (DomBuilder t m, PostBuild t m) => List t m -> SlideContent t m ()
unorderedList = list "ul"

unorderedListClass :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> List t m -> SlideContent t m ()
unorderedListClass = listClass "ul"

orderedList :: forall t m. (DomBuilder t m, PostBuild t m) => List t m -> SlideContent t m ()
orderedList = list "ol"

code :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> Text -> SlideContent t m ()
code = (appendContent .) . code'

code' :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> Text -> m ()
code' lang content =
    codeBlock $
        def
            { codeBlockLanguage = Just lang
            , codeBlockContent = content
            }
