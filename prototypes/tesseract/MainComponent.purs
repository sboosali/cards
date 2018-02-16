module MainComponent where

import Prelude

import CSS as CSS
import Card as C
import DOM.HTML.Indexed as DI
import Data.Array as A
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.String as S
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

cardColorToStyles :: Array C.Color -> { bodyColor :: CSS.CSS, textBoxColor :: CSS.CSS }
cardColorToStyles [] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 198 208 209,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 223 228 224
}
cardColorToStyles [C.White] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 212 207 188,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 225 226 218
}
cardColorToStyles [C.Blue] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 108 156 194,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 166 193 202
}
cardColorToStyles [C.Black] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 42 45 43,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 172 160 146
}
cardColorToStyles [C.Red] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 207 113 85,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 217 180 154
}
cardColorToStyles [C.Green] = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 118 140 104,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 177 182 159
}
cardColorToStyles _ = {
  bodyColor: CSS.backgroundColor $ CSS.rgb 192 165 96,
  textBoxColor: CSS.backgroundColor $ CSS.rgb 246 234 212
}

divWithClass :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
divWithClass className = HH.div [ HP.class_ $ H.ClassName className ]

renderCard :: forall p r i.
  Array (HH.IProp DI.HTMLdiv i) -> C.Card -> HH.HTML p i
renderCard props (C.Card card) =
  HH.div
    (props <> [ HC.style $ (cardColorToStyles card.colors).bodyColor ])
    [
      divWithClass "card-body"
        [
          HH.div
            [
              HP.class_ $ H.ClassName "name-bar",
              HC.style $ (cardColorToStyles card.colors).textBoxColor
            ]
            [
              divWithClass "card-name" [ HH.text card.name ],
              divWithClass "spacer" [],
              divWithClass "card-cost" [ HH.text cardCost ]
            ],
          divWithClass "art"
            [HH.div
              [HP.class_ $ H.ClassName "art-image",
               HC.style $ CSS.backgroundImage $ CSS.url artUrl
              ]
              []
            ],
          HH.div
            [
              HP.class_ $ H.ClassName "type-bar",
              HC.style $ (cardColorToStyles card.colors).textBoxColor
            ]
            [
              divWithClass "card-type" [ HH.text cardType ],
              divWithClass "spacer" [],
              divWithClass "edition-symbol"
                [ 
                  divWithClass "expansion" [ HH.text expansion ],
                  divWithClass "rarity" [ HH.text rarity ]
                ]
            ],
          HH.div
            [
              HP.class_ $ H.ClassName "card-text",
              HC.style $ (cardColorToStyles card.colors).textBoxColor
            ]
            [
              divWithClass "oracle-text" [ HH.text oracleText ],
              divWithClass "spacer" [],
              divWithClass "flavor-text" [ HH.text flavorText ]
            ],
          pt
        ]
    ]
  where
    cardCost = fromMaybe "" card.manaCost
    types = S.joinWith " " card.types
    cardType = if A.null card.subtypes
      then types
      else types <> " — " <> S.joinWith " " card.subtypes
    expansion = "ISD"
    artUrl = "https://magiccards.info/scans/en/"
      <> S.toLower expansion
      <> "/" <> card.number
      <> ".jpg"
    rarity = case card.rarity of
      C.Common -> "(C)"
      C.Uncommon -> "(U)"
      C.Rare -> "(R)"
      C.MythicRare -> "(M)"
      C.BasicLand -> "(L)"
    oracleText = fromMaybe "" card.text
    flavorText = fromMaybe "" card.flavor
    pt = case card of
      { power: Just p, toughness: Just t } ->
        HH.div
          [
            HP.class_ $ H.ClassName "pt",
            HC.style $ (cardColorToStyles card.colors).textBoxColor
          ]
          [ HH.text $ p <> "/" <> t ]
      { loyalty: Just l } ->
        HH.div
          [
            HP.class_ $ H.ClassName "pt",
            HC.style $ (cardColorToStyles card.colors).textBoxColor
          ]
          [ HH.text $ show l ]
      _ -> HH.div_ []

type State = {
  pack :: Array C.Card
, deck :: Array C.Card
}

data Query a
  = PickCard C.Card a
  | ReceiveCards (Array C.Card) a

data Message
  = PassedCards (Array C.Card)
  | EndOfPack

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Unit -> State
  initialState = const { pack: [], deck: [] }

  render :: State -> H.ComponentHTML Query
  render state =
    divWithClass "draft-container"
      [
        divWithClass "pack-pane"
          [ HH.text "Pack",
            HH.div_ $ map (\card ->
              renderCard [
                HP.class_ $ H.ClassName "card card-clickable",
                HE.onClick $ HE.input_ (PickCard card)
              ] card)
              state.pack
          ],
        divWithClass "deck-pane"
          [ HH.text "Deck",
            HH.div_ $ map (\card ->
              renderCard [ HP.class_ $ H.ClassName "card" ] card)
              state.deck
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (PickCard (C.Card card) next) = do
    st <- H.get
    let packWithoutCard = A.filter (\(C.Card c) -> c.id /= card.id) st.pack
        newDeck = st.deck `A.snoc` C.Card card
    H.modify (_ { pack = [], deck = newDeck })
    H.raise $ PassedCards packWithoutCard
    pure next
  eval (ReceiveCards cards next) = do
    H.modify (_ { pack = cards })
    when (A.null cards) $ H.raise EndOfPack
    pure next

