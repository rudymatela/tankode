module Tankode.Obstacles
  ( corners
  , rounded
  , old
  , centerCircle
  , centerDiamond
  , centerSquare
  )
where

import Data.Tuple (swap)
import Tankode.Data

old :: Rational -> Rational -> [Obstacle]
old w h | h > w = map (map swap) $ old h w
old w h =
  [ [ (0, 0)
    , (1, 0)
    , (0, 1)
    ]
  , [ (w,   h)
    , (w-1, h)
    , (w,   h-1)
    ]
  , [ (h/2,   h-1)
    , (h/2+1, h)
    , (h/2-1, h)
    ]
  , [ (w-h/2, 1)
    , (w-h/2-1, 0)
    , (w-h/2+1, 0)
    ]
  , [ (w/2 - 1,   h/2 - 1/2)
    , (w/2 - 1/2, h/2 - 1)
    , (w/2 + 1,   h/2 + 1/2)
    , (w/2 + 1/2, h/2 + 1)
    ]
  ]

corners :: Rational -> Rational -> [Obstacle]
corners w h | h > w = map (map swap) $ corners h w
corners w h =
  [ [ (0, 0)
    , (1, 0)
    , (0, 1)
    ]
  , [ (w,   h)
    , (w-1, h)
    , (w,   h-1)
    ]
  , [ (0, h)
    , (0, h-1)
    , (1, h)
    ]
  , [ (w, 0)
    , (w, 1)
    , (w-1, 0)
    ]
  ]

rounded :: Rational -> Rational -> [Obstacle]
rounded w h | h > w = map (map swap) $ corners h w
rounded w h =
  [ [ (0, 0)
    , (2, 0)
    , (0, 1)
    ]
  , [ (0, 0)
    , (1, 0)
    , (0, 2)
    ]
  , [ (w,   h)
    , (w-2, h)
    , (w,   h-1)
    ]
  , [ (w,   h)
    , (w-1, h)
    , (w,   h-2)
    ]
  , [ (0, h)
    , (0, h-2)
    , (1, h)
    ]
  , [ (0, h)
    , (0, h-1)
    , (2, h)
    ]
  , [ (w, 0)
    , (w, 2)
    , (w-1, 0)
    ]
  , [ (w, 0)
    , (w, 1)
    , (w-2, 0)
    ]
  ]

centerCircle :: Rational -> Rational -> [Obstacle]
centerCircle w h =
  [ [ (w/2 - 1,   h/2 - 1/2)
    , (w/2 - 1/2, h/2 - 1)
    , (w/2 + 1/2, h/2 - 1)
    , (w/2 + 1,   h/2 - 1/2)
    , (w/2 + 1,   h/2 + 1/2)
    , (w/2 + 1/2, h/2 + 1)
    , (w/2 - 1/2, h/2 + 1)
    , (w/2 - 1,   h/2 + 1/2)
    ]
  ]

centerDiamond :: Rational -> Rational -> [Obstacle]
centerDiamond w h =
  [ [ (w/2 - 1, h/2)
    , (w/2, h/2 - 1)
    , (w/2 + 1, h/2)
    , (w/2, h/2 + 1)
    ]
  ]

centerSquare :: Rational -> Rational -> [Obstacle]
centerSquare w h =
  [ [ (w/2 - 1/2, h/2 - 1/2)
    , (w/2 + 1/2, h/2 - 1/2)
    , (w/2 + 1/2, h/2 + 1/2)
    , (w/2 - 1/2, h/2 + 1/2)
    ]
  ]
