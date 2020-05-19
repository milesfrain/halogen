module Example where

import Prelude

import Bulma.Common as B
import Bulma.Components.Dropdown as BD
import Bulma.Elements.Button as BB
import Bulma.Elements.Elements as BE
import Bulma.Modifiers.Modifiers as BM
import Data.Array (snoc)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML (IProp, a, button, div, i, span, span_, text)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (Hook, HookM, UseEffect)
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource as ES
import Web.Event.Event (EventType(..), stopPropagation)
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as ME

newtype UseWindowClick hooks
  = UseWindowClick (UseEffect hooks)

derive instance newtypeUseWindowClick :: Newtype (UseWindowClick hooks) _

useWindowClick :: forall m. MonadAff m => HookM m Unit -> Hook m UseWindowClick Unit
useWindowClick handler =
  Hooks.wrap Hooks.do

    Hooks.useLifecycleEffect do
      window <- liftEffect HTML.window

      _ <- Hooks.subscribe do
        ES.eventListenerEventSource
          (EventType "click")
          (Window.toEventTarget window)
          (const $ Just handler)

      pure $ Just $ pure unit

    Hooks.pure unit

items :: Array String
items = [ "Foo", "Bar", "Baz" ]

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  Hooks.component \_ _ -> Hooks.do

    select /\ selectId <- Hooks.useState (Nothing :: Maybe String)
    open /\ openId <- Hooks.useState false

    -- Close dropdown when clicked in window outside of button
    useWindowClick $ Hooks.modify_ openId (const false)

    let
      ddtext
        | Just str <- select = str
        | otherwise = "Choose..."

      ddactive
        | open = [ BD.isState BD.Active ]
        | otherwise = []

      dditems = map (\item -> mkDropItem item $ Just item == select) items

      mkDropItem str active =
        let
          activeCss
            | active = [ BD.isState BD.Active ]
            | otherwise = []
        in
          a
            [ classes $ snoc activeCss BD.dropdownItem
            -- select and close menu
            , onClickNoPropagation do
                Hooks.modify_ selectId (const $ Just str)
                Hooks.modify_ openId (const false)
            ]
            [ text str ]

    Hooks.pure do
      HH.div
        [ classes $ snoc ddactive BD.dropdown ]
        [ div [ class_ BD.dropdownTrigger ]
            [ button
                [ class_ BB.button
                -- open or close menu
                , onClickNoPropagation $ Hooks.modify_ openId not
                ]
                [ span_ [ text ddtext ]
                , span [ classes [ BE.icon, BM.isSize B.Small ] ]
                    [ i [ css_ "fas fa-angle-down" ] [] ]
                ]
            ]
        , div [ class_ BD.dropdownMenu ]
            [ div [ class_ BD.dropdownContent ] dditems ]
        ]


-- Not sure if a more general type signature is possible
--onClickNoPropagation :: forall r i. i -> IProp ( onClick :: ME.MouseEvent | r ) i
onClickNoPropagation :: forall r m. MonadAff m => HookM m Unit -> IProp ( onClick :: ME.MouseEvent | r ) (HookM m Unit)
onClickNoPropagation act =
  HE.onClick (\evt -> Just $ do
      -- stopPropagation is necessary so button click not also interpreted as window click.
      liftEffect $ stopPropagation $ ME.toEvent evt
      act)


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
