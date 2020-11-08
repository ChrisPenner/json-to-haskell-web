-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso hiding (defaultOptions)
import Miso.String
import Data.Aeson hiding (defaultOptions)
import JsonToHaskell (jsonToHaskell, defaultOptions)

-- | Type synonym for an application model
type Model = (MisoString, MisoString)

-- | Sum type for application events
data Action
  =
  NoOp
  | Update MisoString
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = ("", "")                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    -- logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (Update newSrc) _ =
    case eitherDecode (fromMisoString newSrc) of
        Left err -> noEff (newSrc, ms err)
        Right value -> noEff (newSrc, ms $ jsonToHaskell defaultOptions value)
updateModel NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel (src, out) =
    div_ []
         [ h1_ [class_ "title"] []
         , h2_ [class_ "links"]
               [ text "JSON to Haskell"
               , a_ [ href_ "https://github.com/ChrisPenner/json-to-haskell"
                    ]
                    [text "Github"]
               , text " | "
               , a_ [ href_ "https://github.com/ChrisPenner/json-to-haskell"
                    ]
                    [text "CLI version (Hackage)"]
               , text " | "
               , a_ [ href_ "https://hackage.haskell.org/package/json-to-haskell"
                    ]
                    [text "Chris's Blog"]
               , text " | "
               , a_ [ href_ "https://twitter.com/chrislpenner"
                    ]
                    [text "Chris's Twitter"]
               ]
         , div_ [class_ "container"]
                [ div_ [class_ "input"]
                       [ h2_ [] [text "Paste JSON Here"]
                       , textarea_ [value_ src, onInput Update]
                                   []
                       ]
                , div_ [class_ "output"]
                       [ h2_ [] [text "Copy Haskell Here"]
                       , textarea_ [value_ out] []
                       ]
                ]
         ]
