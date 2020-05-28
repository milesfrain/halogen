module MyRouting where

import Prelude
import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Routing (match)
import Routing.Hash (matches)
import Routing.Match (Match, int, lit, str)

type PostId
  = Int

data MyRoute
  = PostIndex
  | Post PostId
  | PostEdit PostId
  | PostBrowse String String

derive instance genericMyRoute :: Generic MyRoute _

instance showMyRoute :: Show MyRoute where
  show = genericShow

myRoute :: Match MyRoute
myRoute =
  oneOf
    [ PostEdit <$> (int <* lit "edit")
    , Post <$> int
    , PostBrowse <$> str <*> str
    , pure PostIndex
    ]

matchMyRoute :: String -> Either String MyRoute
matchMyRoute = match myRoute

logRouteTests :: Effect Unit
logRouteTests = do
  logShow $ matchMyRoute "8/edit" -- PostEdit
  logShow $ matchMyRoute "8" -- Post
  logShow $ matchMyRoute "foo/bar" -- PostBrowse
  logShow $ matchMyRoute "" -- PostIndex

logRoute :: Effect (Effect Unit)
logRoute = do
  matches myRoute \_ newRoute -> case newRoute of
    PostIndex -> logShow newRoute
    Post postId -> logShow newRoute
    PostEdit postId -> logShow newRoute
    PostBrowse year month -> logShow newRoute
