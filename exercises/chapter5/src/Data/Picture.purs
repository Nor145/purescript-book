module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math as Math

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

-- 5.5

-- 1.
factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2.
binomial :: Number -> Number -> Number
binomial n k | k > n = 0
binomial n 0 = 1
binomial n k = binomial (n-1) (k-1) + binomial (n-1) k

-- 5.9

-- 1.
sameCity :: Person -> Person -> Boolean
sameCity { address: { city: x }} { address: { city: y }}
  | x == y    = true
  | otherwise = false

-- 3.
fromSingleton :: forall a. a -> Array a -> a
fromSingleton def [x] = x
fromSingleton def _ = def

-- 5.14

-- 1.
origin :: Point
origin = Point { x: 0.0, y: 0.0 }

circle :: Shape
circle = Circle origin 10.0

-- 2.
scaleAndCentre :: Shape -> Shape
scaleAndCentre (Circle _ r) = Circle origin (r * 2.0)
scaleAndCentre (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
scaleAndCentre (Line (Point start) (Point end)) = Line transformStart transformEnd where
  xdiff           = start.x - end.x
  ydiff           = start.y - end.y
  transformStart  = Point { x: -xdiff, y: -ydiff }
  transformEnd    = Point { x: xdiff, y: ydiff }
scaleAndCentre (Text _ str) = Text origin str

-- 3.
extractString :: Shape -> Maybe String
extractString (Text _ str) = Just str
extractString _ = Nothing

-- 5.17

-- 1.
area :: Shape -> Number
area (Circle _ r) = pi * (pow r 2.0)
area (Rectangle _ w h) = w * h
area _ = 0.0

-- 2.


