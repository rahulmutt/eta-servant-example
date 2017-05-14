module Servlet.Jetty (runJetty) where

import Java
import qualified Network.Wai as Wai
import Network.Wai.Servlet
import GHC.Base (unsafeCoerce#)

data {-# CLASS "org.eclipse.jetty.servlet.ServletHolder" #-}
  ServletHolder = ServletHolder (Object# ServletHolder)
  deriving Class

data {-# CLASS "javax.servlet.Servlet" #-}
  Servlet = Servlet (Object# Servlet)
  deriving Class

data {-# CLASS "org.eclipse.jetty.servlet.ServletContextHandler" #-}
  ServletContextHandler = ServletContextHandler (Object# ServletContextHandler)
  deriving Class

data {-# CLASS "org.eclipse.jetty.server.Server" #-}
  Server = Server (Object# Server)
  deriving Class

type instance Inherits Server = '[Object, HandlerContainer]

data {-# CLASS "org.eclipse.jetty.server.HandlerContainer" #-}
  HandlerContainer = HandlerContainer (Object# HandlerContainer)
  deriving Class

type instance Inherits HandlerContainer = '[LifeCycle]

data {-# CLASS "org.eclipse.jetty.util.component.LifeCycle" #-}
  LifeCycle = LifeCycle (Object# LifeCycle)
  deriving Class

foreign import java unsafe "@new" newServletContextHandler
  :: (b <: HandlerContainer) => b -> String -> Java a ServletContextHandler

foreign import java unsafe addServlet
  :: ServletHolder -> String -> Java ServletContextHandler ()

foreign import java unsafe "@new" newServletHolder :: Servlet -> Java a ServletHolder

foreign import java unsafe "@new" newServer :: Int -> Java a Server

foreign import java unsafe "@interface" start :: (a <: LifeCycle) => Java a ()

unsafeForceCast :: (Class a, Class b) => a -> b
unsafeForceCast a = obj (unsafeCoerce# (unobj a))

runJetty :: Int -> Wai.Application -> IO ()
runJetty port app = java $ do
  server <- newServer port
  holder <- newServletHolder $ unsafeForceCast (makeServlet app)
  handler <- newServletContextHandler server "/"
  handler <.> addServlet holder "/"
  server <.> start


