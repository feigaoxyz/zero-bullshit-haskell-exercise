{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runServer
  )
where

import           Control.Arrow                  ( (>>>) )
import           Data.Function                  ( (&) )
import           Debug.Trace                    ( traceShowId )
import           GHC.Generics                   ( Generic )
import qualified Data.Aeson                    as Aeson
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.List                     as List

import qualified Zero.Server                   as Server

-- Ex01
helloHandler :: Server.Request -> Server.Response
helloHandler _ = Server.stringResponse "hello"

-- Ex02
echoHandler :: Server.Request -> Server.Response
echoHandler = Server.requestBody >>> Server.stringResponse

-- Ex03
caseHandler :: Server.Request -> Server.Response
caseHandler req =
  (case req & Server.requestBody of
      "1" -> "one"
      "2" -> "two"
      "3" -> "three"
      _   -> "google"
    )
    & Server.stringResponse

-- Ex04
stringManipulationHandler :: Server.Request -> Server.Response
stringManipulationHandler req =
  let body   = Server.requestBody req
      term   = "I'm positive"
      len    = length term
      (h, r) = splitAt len body
      resp   = if h == term then "I think" ++ r else body
  in  Server.stringResponse resp

data State = State { mkCounter :: Int
, mkSwitch :: Bool
}

-- Ex05
switchHandler :: State -> Server.Request -> (State, Server.Response)
switchHandler state _ =
  let s = mkSwitch state
  in  ( state { mkSwitch = not s }
      , Server.stringResponse (if s then "On" else "Off")
      )

-- Ex06
currentCountHandler, increaseHandler
  :: State -> Server.Request -> (State, Server.Response)
currentCountHandler s _ =
  (s, Server.stringResponse ("Current count: " ++ show (mkCounter s)))
increaseHandler state _ =
  (state { mkCounter = mkCounter state + 1 }, Server.stringResponse "")

-- Ex07, Ex08
data Item = Item { model :: String , quantity :: Int }
  deriving (Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

newtype Cart = Cart (Map String Int)
  deriving (Eq, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Cart where
  toJSON (Cart cart) =
    let items = cart & Map.toList & List.sortOn snd & List.reverse
    in Aeson.toJSON items

insertItemToCart :: Item -> Cart -> Cart
insertItemToCart Item { model = model, quantity = quantity } (Cart cart) =
  let alterFun v = case v of
        Nothing -> Just quantity
        Just v  -> if v + quantity == 0 then Nothing else Just (v + quantity)
      newCart = Map.alter alterFun model cart
  in  Cart newCart

cartGetHandler, cartPostHandler
  :: Cart -> Server.Request -> (Cart, Server.Response)
cartGetHandler cart req = (cart, Server.jsonResponse cart)
cartPostHandler cart req =
  let (result, newCart) = case req & Server.requestBody & Server.decodeJson of
        Left  err  -> ("error", cart)
        Right item -> ("ok", insertItemToCart item cart)
  in  (newCart, Server.stringResponse result)


runServer :: IO ()
runServer =
  let initState = State 0 False
  in
    Server.startServer
      [ Server.simpleHandler Server.GET "/hello" helloHandler
      , Server.simpleHandler Server.POST "/echo" echoHandler
      , Server.simpleHandler Server.POST "/case" caseHandler
      , Server.simpleHandler Server.POST
                             "/string-manipulation"
                             stringManipulationHandler
      , Server.handlersWithState
        initState
        [ Server.statefulHandler Server.POST "/onoff-switch" switchHandler
        , Server.statefulHandler Server.GET "/current-count" currentCountHandler
        , Server.statefulHandler Server.POST "/increase" increaseHandler
        ]
      , Server.handlersWithState
        (Cart Map.empty)
        [ Server.statefulHandler Server.GET "/cart" cartGetHandler
        , Server.statefulHandler Server.POST "/cart" cartPostHandler
        ]
      ]
