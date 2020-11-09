-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso hiding (defaultOptions)
import Miso.String
import Data.Aeson hiding (defaultOptions)
import JsonToHaskell

-- | Type synonym for an application model
data Model = Model
    { input :: MisoString
    , output :: MisoString
    , strict :: Bool
    , includeHeader :: Bool
    , includeInstances :: Bool
    , prefixRecordFields :: Bool
    , textType :: TextType
    , listType :: ListType
    , numberType :: NumberType
    } deriving (Show, Eq)

-- | Sum type for application events
data Action
  =
  Rerender
  | Update MisoString
  | Toggle Toggles
  | Select Selects
  deriving (Show, Eq)

data Toggles =
    Strict
      | IncludeHeader
      | IncludeInstances
      | PrefixRecordFields
    deriving (Show, Eq)

data Selects =
    NType MisoString
    | MType MisoString
    | LType MisoString
    | TType MisoString
    deriving (Eq, Show, Ord)

showStringType :: TextType -> MisoString
showStringType = \case
  UseText -> "Text"
  UseString -> "String"
  UseByteString -> "ByteString"

showListType :: ListType -> MisoString
showListType = \case
  UseList -> "List"
  UseVector -> "Vector"

showNumberType :: NumberType -> MisoString
showNumberType = \case
  UseSmartFloats -> "Smart Floats"
  UseSmartDoubles -> "Smart Doubles"
  UseFloats -> "Floats"
  UseDoubles -> "Doubles"
  UseScientific -> "Scientific"


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Rerender -- initial action to be executed on application load
    model  = Model { input="{ \"company\": \n { \"employees\": \n    [ {\"name\": \"Jon\", \"age\": 32} \n    , {\"name\": \"Alice\", \"age\": 27} \n    ] \n , \"star_rating\": 4.7 \n } \n}"
                   , output=""
                   , strict=False
                   , includeHeader=True
                   , includeInstances=True
                   , prefixRecordFields=True
                   , textType=UseText
                   , listType=UseList
                   , numberType=UseSmartDoubles
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
updateModel (Select s) model =
    noEff . rerender $ case s of
        TType "Text" -> model{textType=UseText}
        TType "ByteString" -> model{textType=UseByteString}
        TType "String" -> model{textType=UseString}
        LType "List" -> model{listType=UseList}
        LType "Vector" -> model{listType=UseVector}
        NType "Smart Floats" -> model{numberType=UseSmartFloats}
        NType "Smart Doubles" -> model{numberType=UseSmartDoubles}
        NType "Floats" -> model{numberType=UseFloats}
        NType "Doubles" -> model{numberType=UseDoubles}
        NType "Scientific" -> model{numberType=UseScientific}
        _ -> model
updateModel (Toggle t) model =
    noEff . rerender $ case t of
        Strict -> model{strict=not $ strict model}
        IncludeHeader -> model{includeHeader=not $ includeHeader model}
        IncludeInstances -> model{includeInstances=not $ includeInstances model}
        PrefixRecordFields -> model{prefixRecordFields=not $ prefixRecordFields model}
updateModel Rerender m = noEff $ rerender m

rerender :: Model -> Model
rerender model =
    let opts = simpleOptions
                { _tabStop = 2
                , _numberType = numberType model
                , _textType = textType model
                -- , _mapType = UseMap
                , _listType = listType model
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

select :: MisoString
       -> MisoString
       -> [MisoString]
       -> (MisoString -> Selects)
       -> View Action
select name val strs buildSelect =
    label_ []
           [ text name
           , select_ [onInput (Select . buildSelect)]
                     (fmap buildOpt strs)
           ]
  where
    buildOpt s =
        option_ [selected_ (val == s), value_ s] [text s]


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model =
    div_ [class_ "main"]
         [ h1_ [class_ "title"] [text "JSON to Haskell"]
         , h2_ [class_ "install"] [textarea_ [disabled_ True, value_ "stack install json-to-haskell"] []]
         , h2_ [class_ "links"]
               [ a_ [ href_ "https://github.com/ChrisPenner/json-to-haskell"
                    , target_ "_blank"
                    ]
                    [text "Github"]
               , text " | "
               , a_ [ href_ "https://hackage.haskell.org/package/json-to-haskell"
                    , target_ "_blank"
                    ]
                    [text "CLI version (Hackage)"]
               , text " | "
               , a_ [ href_ "https://chrispenner.ca"
                    , target_ "_blank"
                    ]
                    [text "Chris's Blog"]
               , text " | "
               , a_ [ href_ "https://twitter.com/chrislpenner"
                    , target_ "_blank"
                    ]
                    [text "Chris's Twitter"]
               ]
         , div_ [class_ "settings"]
                [ div_ [class_ "checkboxes"]
                       [ checkBox "Include Module Header"
                                  (includeHeader model)
                                  IncludeHeader
                       , checkBox "Include JSON Instances"
                                  (includeInstances model)
                                  IncludeInstances
                       , checkBox "Strict Data"
                                  (strict model)
                                  Strict
                       ]
                , div_ [class_ "selects"]
                       [ select "String Type: "
                                (showStringType (textType model))
                                ["Text", "String", "ByteString"]
                                TType
                       , select "List Type: "
                                (showListType (listType model))
                                ["List", "Vector"]
                                LType
                       , select "Number Type: "
                                (showNumberType (numberType model))
                                [ "Smart Floats"
                                , "Smart Doubles"
                                , "Floats"
                                , "Doubles"
                                , "Scientific"
                                ]
                                NType
                       ]
                ]
         , div_ [class_ "container"]
                [ div_ [class_ "input"]
                       [ h2_ [] [text "Paste JSON"]
                       , textarea_ [ value_ (input model)
                                   , onInput Update
                                   ]
                                   []
                       ]
                , div_ [class_ "output"]
                       [ h2_ [] [text "Copy Haskell"]
                       , textarea_ [ disabled_ True
                                   , value_ (output model)
                                   ]
                                   []
                       ]
                ]
         ]
