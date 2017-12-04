{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Generics.Product
import Data.Proxy
import Data.Text.Lazy
import Data.Void
import GHC.Generics
import GHC.TypeLits
import Web.Scotty

-- GRowToList from kcsongor
type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]

type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- My Route and Handler type definition
data GetRequest
data PostRequest

data Route method req res (url :: Symbol) = Route

type GetRoute = Route GetRequest Void
type PostRoute = Route PostRequest

-- go through the records pairwise and register each handler
class RegisterRoutes
    routes
    (routesL :: [(Symbol, *)])
    handlers
    (handlersL :: [(Symbol, *)])
    m
  where
    registerRoutesImpl :: forall
       . Monad m
      => routes
      -> Proxy routesL
      -> handlers
      -> Proxy handlersL
      -> m ()

instance RegisterRoutes routes '[] handlers '[] m
  where
    registerRoutesImpl _ _ _ _ = pure ()

instance
  ( RegisterRoutes routes routesTail handlers handlersTail m
  , HasField' name routes route
  , HasField' name handlers handler
  , RegisterHandler route handler m
  ) => RegisterRoutes
         routes
         ('(name, route) ': routesTail)
         handlers
         ('(name, handler) ': handlersTail)
         m
  where
    registerRoutesImpl routes _ handlers _ = do
        registerHandlerImpl route handler
        registerRoutesImpl
          routes
          (Proxy :: Proxy routesTail)
          handlers
          (Proxy :: Proxy handlersTail)
        pure ()
      where
        route = getField @name routes
        handler = getField @name handlers

registerRoutes :: forall routes routesL handlers handlersL m
   . Monad m
  => Generic routes
  => Generic handlers
  => routesL ~ GRowToList (Rep routes)
  => handlersL ~ GRowToList (Rep handlers)
  => RegisterRoutes
       routes
       routesL
       handlers
       handlersL
       m
  => routes
  -> handlers
  -> m ()
registerRoutes routes handlers =
  registerRoutesImpl
    routes
    (Proxy :: Proxy routesL)
    handlers
    (Proxy :: Proxy handlersL)

-- register each handler, to the route method and concrete monad used
class RegisterHandler route handler m
  where
    registerHandlerImpl :: route -> handler -> m ()

instance
  ( KnownSymbol url
  , Show res
  ) => RegisterHandler
         (Route GetRequest Void res url)
         (IO res)
         ScottyM
  where
    registerHandlerImpl _ handler =
        get (capture path) $ do
          res <- liftAndCatchIO handler
          text . pack $ show res
      where
        path = symbolVal (Proxy :: Proxy url)

data MyRoutes = MyRoutes
  { home :: GetRoute Bool "/"
  , hello :: GetRoute Int "/hello"
  , bye :: GetRoute String "/bye"
  } deriving (Generic)

myRoutes :: MyRoutes
myRoutes = MyRoutes
  { home = Route
  , hello = Route
  , bye = Route
  }

data MyHandlers = MyHandlers
  { home :: IO Bool
  , hello :: IO Int
  , bye :: IO String
  } deriving (Generic)

myHandlers :: MyHandlers
myHandlers = MyHandlers
  { home = pure True
  , hello = pure 1
  , bye = pure "bye"
  }

main :: IO ()
main = scotty 3001 $ do
  registerRoutes myRoutes myHandlers
