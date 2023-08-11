{-# OPTIONS_GHC -XPackageImports #-}

import "gloss" Graphics.Gloss
import "JuicyPixels" Graphics.Gloss.Juicy
import "http-client" Network.HTTP.Client
import "http-client-tls" Network.HTTP.Client.TLS
import "random" System.Random
import "gloss" Graphics.Gloss.Interface.Pure.Game
import "gloss" Graphics.Gloss.Data.Picture
import "JuicyPixels" Codec.Picture (DynamicImage, decodeImage)
import "JuicyPixels" Codec.Picture.Types (Image(..), PixelRGBA8(..))
import qualified "bytestring" Data.ByteString.Lazy as ByteStringLazy
import qualified "bytestring" Data.ByteString.Lazy.Char8 as ByteStringLazyChar8



-- Define game constants
minTunnelWidth :: Float
minTunnelWidth = 400

maxTunnelWidth :: Float
maxTunnelWidth = 800

minHeight :: Float
minHeight = 10

maxHeight :: Float
maxHeight = 100

obstacleWidth :: Float
obstacleWidth = 65

obstacleHeight :: Float
obstacleHeight = 135

moveSpeed :: Float
moveSpeed = 7

gravity :: Float
gravity = 0.35

helicopterWidth :: Float
helicopterWidth = 100

helicopterHeight :: Float
helicopterHeight = 60

-- Game state
data GameState = GameState
    { helicopterPos :: Point
    , helicopterVelocity :: Float
    , spacePressed :: Bool
    , tunnels :: [Tunnel]
    , obstacles :: [Obstacle]
    }

data Tunnel = Tunnel
    { tunnelX :: Float
    , tunnelWidth :: Float
    , tunnelStart :: Float
    , tunnelEnd :: Float
    }

data Obstacle = Obstacle
    { obstacleX :: Float
    , obstacleY :: Float
    }

initialState :: StdGen -> Picture -> GameState
initialState gen logo = GameState
    { helicopterPos = (200, 100)
    , helicopterVelocity = 0
    , spacePressed = False
    , tunnels = [Tunnel 0 canvasWidth 50 50, Tunnel canvasWidth randTunnelWidth 50 randTunnelHeight]
    , obstacles = [Obstacle canvasWidth (canvasHeight / 2), Obstacle (canvasWidth * 2) (canvasHeight / 2)]
    , drawLogo = logo
    }
    where
        (randTunnelWidth, newGen) = randomR (minTunnelWidth, maxTunnelWidth) gen
        (randTunnelHeight, _) = randomR (minHeight, maxHeight) newGen


-- Utility functions
clamp :: (Ord a) => a -> a -> a -> a
clamp x a b = max a (min b x)

randInt :: Float -> Float -> Float
randInt a b = a + (b - a) * fromIntegral (fst (randomR (0, 1000) (mkStdGen 0))) / 1000

-- Update game state
updateGameState :: Float -> GameState -> GameState
updateGameState dt game =
    let game' =
            if spacePressed game
                then game { helicopterVelocity = -0.7 }
                else game { helicopterVelocity = 0 }
        newHelicopterVelocity = helicopterVelocity game' + gravity
        newHelicopterY = clamp (snd (helicopterPos game') + newHelicopterVelocity) 0 (canvasHeight - helicopterHeight)
        newHelicopterPos = (fst (helicopterPos game'), newHelicopterY)
        newTunnels = updateTunnels dt (tunnels game')
        newObstacles = updateObstacles dt (obstacles game')
    in game' { helicopterPos = newHelicopterPos, tunnels = newTunnels, obstacles = newObstacles }

updateTunnels :: Float -> [Tunnel] -> [Tunnel]
updateTunnels dt tunnels = map updateTunnel tunnels
    where
        updateTunnel tunnel =
            let newX = tunnelX tunnel - moveSpeed * dt
                newTunnel =
                    if tunnelIndex == length tunnels - 1 && newX + tunnelWidth tunnel <= canvasWidth
                        then Tunnel (newX + tunnelWidth tunnel) randTunnelWidth (tunnelEnd tunnel) randTunnelHeight
                        else tunnel { tunnelX = newX }
                (randTunnelWidth, newGen) = randomR (minTunnelWidth, maxTunnelWidth) (mkStdGen 0)
                (randTunnelHeight, _) = randomR (minHeight, maxHeight) newGen
            in newTunnel
        tunnelIndex = length tunnels - 1

updateObstacles :: Float -> [Obstacle] -> [Obstacle]
updateObstacles dt obstacles = map updateObstacle obstacles
    where
        updateObstacle obstacle =
            let newX = obstacleX obstacle - moveSpeed * dt
                newObstacle =
                    if obstacleIndex == length obstacles - 1 && newX + obstacleWidth <= canvasWidth
                        then Obstacle (canvasWidth * 2) (randInt (maxHeight + 50) (canvasHeight - obstacleHeight - maxHeight - 50))
                        else obstacle { obstacleX = newX }
            in newObstacle
        obstacleIndex = length obstacles - 1

-- Handle key events
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) game = game { spacePressed = True }
handleEvent (EventKey (SpecialKey KeySpace) Up _ _) game = game { spacePressed = False }
handleEvent _ game = game

-- Draw the game
render :: GameState -> Picture
render game =
    pictures
        [ drawBackground
        , drawHelicopter game
        , drawTunnels (tunnels game)
        , drawObstacles (obstacles game)
        , drawLogo
        ]

drawBackground :: Picture
drawBackground = color black $ rectangleSolid canvasWidth canvasHeight

drawHelicopter :: GameState -> Picture
drawHelicopter game =
    let (x, y) = helicopterPos game
    in translate x y $ color white $ rectangleSolid helicopterWidth helicopterHeight

drawTunnels :: [Tunnel] -> Picture
drawTunnels tunnels = pictures $ map drawTunnel tunnels
    where
        drawTunnel tunnel =
            let x = tunnelX tunnel
                start = tunnelStart tunnel
                end = tunnelEnd tunnel
                topWall = translate x 0 $ rectangleSolid (tunnelWidth tunnel) start
                bottomWall = translate x (canvasHeight - end) $ rectangleSolid (tunnelWidth tunnel) (end + 450)
            in color green $ pictures [topWall, bottomWall]

drawObstacles :: [Obstacle] -> Picture
drawObstacles obstacles = pictures $ map drawObstacle obstacles
    where
        drawObstacle obstacle =
            let (x, y) = (obstacleX obstacle, obstacleY obstacle)
            in color green $ rectangleSolid obstacleWidth obstacleHeight

drawLogo :: Picture
drawLogo = logoPicture  -- Replace `logoPicture` with the actual drawing code for your logo

logoPicture :: Picture
logoPicture = undefined  -- Add the drawing code for your logo here

-- Game configuration
canvasWidth :: Float
canvasWidth = 800

canvasHeight :: Float
canvasHeight = 550

-- Game settings
window :: Display
window = InWindow "Basic Helicopter Game" (round canvasWidth, round canvasHeight) (10, 10)

fps :: Int
fps = 60

-- Game entry point
main :: IO ()
main = do
    gen <- getStdGen
    logoImage <- retrieveLogoImage
    case logoImage of
        Right dynamicImage -> do
            let initialState' = initialState gen
                logoPicture = scale 0.2 0.2 $ Pictures [Image (dynamicImageRGBA8 dynamicImage)]
                initialState = initialState' { drawLogo = logoPicture }
            play window white fps initialState render handleEvent updateGameState
        Left err -> putStrLn $ "Failed to load the logo image: " ++ err


retrieveLogoImage :: IO (Either String DynamicImage)
retrieveLogoImage = do
    manager <- newTlsManager
    request <- parseRequest "https://ipfs.io/ipfs/QmYouSUeLudR5e5TK4sbLV6bvh4v9HEVgGKcCnb2LbButV"
    response <- httpLbs request manager
    let imageData = ByteStringLazy.toStrict $ responseBody response
    return $ decodeImage imageData
