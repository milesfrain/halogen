module Main where

import Prelude
import Effect (Effect)
import Example as Example
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MyRouting (logRoute, logRouteTests)

--main :: Effect (Effect Unit)
main = logRoute

--main :: Effect Unit
--main = logRouteTests
{-
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Example.component unit body
-}
