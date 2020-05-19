module Example where

import Prelude
import Bulma.Common as B
import Bulma.Components.Dropdown as BD
import Bulma.Elements.Button as BB
import Bulma.Elements.Elements as BE
import Bulma.Modifiers.Modifiers as BM
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Halogen as H
import Halogen.HTML (IProp, a, button, div, i, span, span_, text)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

items :: Array String
items = [ "Foo", "Bar", "Baz" ]

type State
  = { selected :: Maybe String
    , dropOpen :: Boolean
    }

initialState :: forall a. a -> State
initialState =
  const
    { selected: Nothing
    , dropOpen: false
    }

data Action
  = DropClick
  | DropSelect String
  | WindowClick

{-
Curious how window click would work for two dropdows.
If one dropdown is open, would clicking in another one close the first?
Multiple dropdowns open simultaneously is not a critical issue.
-}
handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  DropClick -> H.modify_ \s -> s { dropOpen = not s.dropOpen }
  DropSelect str -> H.modify_ \s -> s { selected = Just str, dropOpen = false }
  WindowClick -> H.modify_ \s -> s { dropOpen = false }

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

mkDropItem :: forall a. String -> Boolean -> HH.HTML a Action
mkDropItem str active =
  let
    activeCss
      | active = [ BD.isState BD.Active ]
      | otherwise = []
  in
    a
      [ classes $ snoc activeCss BD.dropdownItem
      , HE.onClick \_ -> Just $ DropSelect str
      ]
      [ text str ]

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    ddtext
      | Just str <- state.selected = str
      | otherwise = "Choose..."

    ddactive
      | state.dropOpen = [ BD.isState BD.Active ]
      | otherwise = []

    dditems = map (\item -> mkDropItem item $ Just item == state.selected) items
  in
    HH.div
      [ classes $ snoc ddactive BD.dropdown ]
      [ div [ class_ BD.dropdownTrigger ]
          [ button
              [ class_ BB.button
              , HE.onClick \_ -> Just DropClick
              ]
              [ span_ [ text ddtext ]
              , span [ classes [ BE.icon, BM.isSize B.Small ] ]
                  [ i [ css_ "fas fa-angle-down" ] [] ]
              ]
          ]
      , div [ class_ BD.dropdownMenu ]
          [ div [ class_ BD.dropdownContent ] dditems ]
      ]

-- Helper-functions for converting class names from Bulma to Halogen
convertClassName :: B.ClassName -> H.ClassName
convertClassName bc = wrap $ unwrap bc

class_ :: forall r i. B.ClassName -> IProp ( class ∷ String | r ) i
class_ bc = HP.class_ $ convertClassName bc

classes :: forall r i. Array (B.ClassName) -> IProp ( class ∷ String | r ) i
classes bcs = HP.classes $ map convertClassName bcs

-- For just writing plain css strings
css_ :: forall r i. String -> IProp ( class ∷ String | r ) i
css_ str = HP.class_ $ wrap str
