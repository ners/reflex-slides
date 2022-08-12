module Style where

import Arc.Clay.Util
import Clay
import Clay.Stylesheet

mainStyle :: Css
mainStyle = do
    html <> body ? do
        margin nil nil nil nil
        padding nil nil nil nil
    main_ ? do
        fontSize (px 64)
        lineHeight (em 1.5)
        fontWeight $ weight 300
        width (vw 100)
        height (vh 100)
        userSelect none
        overflowX hidden
        overflowY hidden
        ".playing" & cursor cursorNone
    time ? do
        position absolute
        right (em 1.5)
        bottom (em 0)
        opacity 0.3
        fontSize (pct 50)
    ".slide" ? do
        width (vw 100)
        height (vh 100)
        overflow auto
        padding2 (em 1) (em 2)
        boxSizing borderBox
        display flex
        flexDirection column
    ".inactive" & do
        display none
    ".visible" <> ".invisible" ? do
        transition "opacity" (ms 100) easeInOut (sec 0)
    ".invisible" ? do
        opacity 0
