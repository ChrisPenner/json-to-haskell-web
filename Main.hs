-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso hiding (defaultOptions)
import Miso.String
import Data.Aeson hiding (defaultOptions)
import JsonToHaskell (jsonToHaskell, simpleOptions, Options(..))

-- | Type synonym for an application model
data Model = Model
    { input :: MisoString
    , output :: MisoString
    , strict :: Bool
    , includeHeader :: Bool
    , includeInstances :: Bool
    , prefixRecordFields :: Bool
    } deriving (Show, Eq, Ord)

-- | Sum type for application events
data Action
  =
  NoOp
  | Update MisoString
  | Toggle Toggles
  deriving (Show, Eq)

data Toggles =
    Strict
      | IncludeHeader
      | IncludeInstances
      | PrefixRecordFields
    deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Model { input=""
                   , output=""
                   , strict=False
                   , includeHeader=True
                   , includeInstances=True
                   , prefixRecordFields=True
                   }                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    -- logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (Update newSrc) model = noEff $ rerender model{input=newSrc}
updateModel (Toggle t) model =
    noEff $ case t of
        Strict -> rerender model{strict=not $ strict model}
        IncludeHeader -> rerender model{includeHeader=not $ includeHeader model}
        IncludeInstances -> rerender model{includeInstances=not $ includeInstances model}
        PrefixRecordFields -> rerender model{prefixRecordFields=not $ prefixRecordFields model}
updateModel NoOp m = noEff m

rerender :: Model -> Model
rerender model =
    let opts = simpleOptions 
                { _tabStop = 2
                -- , _numberType = UseDoubles
                -- , _textType = UseText
                -- , _mapType = UseMap
                -- , _listType = UseList
                , _includeHeader = includeHeader model
                , _includeInstances = includeInstances model
                , _strictData = strict model
                , _prefixRecordFields = prefixRecordFields model
                }
     in case eitherDecode (fromMisoString (input model)) of
        Left err -> model{output=ms err}
        Right value -> model{output=ms $ jsonToHaskell opts value}

checkBox :: MisoString -> Bool -> Toggles -> View Action
checkBox name val toggle =
    label_ [] [ input_ [type_ "checkbox", checked_ val, onClick (Toggle toggle)], text name]

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model =
    div_ []
         [ h1_ [class_ "title"] [ text "JSON to Haskell" ]
         , h2_ [class_ "links"]
               [ a_ [ href_ "https://github.com/ChrisPenner/json-to-haskell"
                    ]
                    [text "Github"]
               , text " | "
               , a_ [ href_ "https://hackage.haskell.org/package/json-to-haskell"
                    ]
                    [text "CLI version (Hackage)"]
               , text " | "
               , a_ [ href_ "https://chrispenner.ca"
                    ]
                    [text "Chris's Blog"]
               , text " | "
               , a_ [ href_ "https://twitter.com/chrislpenner"
                    ]
                    [text "Chris's Twitter"]
               ]
         , div_ [class_ "settings"] [ checkBox "Include Module Header" (includeHeader model) IncludeHeader
                                    , checkBox "Include JSON Instances" (includeInstances model) IncludeInstances
                                    , checkBox "Strict Data" (strict model) Strict
                                    ]
         , div_ [class_ "container"]
                [ div_ [class_ "input"]
                       [ h2_ [] [text "Paste JSON Here"]
                       , textarea_ [value_ (input model), onInput Update]
                                   []
                       ]
                , div_ [class_ "output"]
                       [ h2_ [] [text "Copy Haskell Here"]
                       , textarea_ [value_ (output model)] []
                       ]
                ]
         ]
