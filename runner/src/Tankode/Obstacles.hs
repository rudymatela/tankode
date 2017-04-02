-- |
-- Module      : Tankode.Obstacles
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Obstacle templates so users don't need to provide explicit coordinates every
-- time.
module Tankode.Obstacles
  ( corners
  , rounded
  , rounded3
  , old
  , centerCircle
  , centerDiamond
  , centerSquare
  , ex
  , ox
  , bowtie

  , makeFieldWith
  )
where

import Data.Tuple (swap)
import Tankode.Data

makeFieldWith :: [Obstacle] -> Rational -> Rational -> Field
makeFieldWith os = updateObstacles (++ os) .: makeField where (.:) = (.) . (.)

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

rounded3 :: Rational -> Rational -> [Obstacle]
rounded3 w h | h > w = map (map swap) $ corners h w
rounded3 w h =
  [ [ (0, 0)
    , (3, 0)
    , (0, 1)
    ]
  , [ (0, 0)
    , (2, 0)
    , (0, 2)
    ]
  , [ (0, 0)
    , (1, 0)
    , (0, 3)
    ]
  , [ (w,   h)
    , (w-3, h)
    , (w,   h-1)
    ]
  , [ (w,   h)
    , (w-2, h)
    , (w,   h-2)
    ]
  , [ (w,   h)
    , (w-1, h)
    , (w,   h-3)
    ]
  , [ (0, h)
    , (0, h-3)
    , (1, h)
    ]
  , [ (0, h)
    , (0, h-2)
    , (2, h)
    ]
  , [ (0, h)
    , (0, h-1)
    , (3, h)
    ]
  , [ (w, 0)
    , (w, 3)
    , (w-1, 0)
    ]
  , [ (w, 0)
    , (w, 2)
    , (w-2, 0)
    ]
  , [ (w, 0)
    , (w, 1)
    , (w-3, 0)
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

ex :: Rational -> Rational -> [Obstacle]
ex w h | h > w = map (map swap) $ ex h w
ex w h =
  [ [ (w/2 + 1, 0)
    , (w/2    , 1)
    , (w/2 - 1, 0)
    ]
  , [ (w/2 - 1, h)
    , (w/2    , h-1)
    , (w/2 + 1, h)
    ]
  , [ (w/2 - h/2 + 1, h/2)
    , (0            ,     w/2 + 1)
    , (0            , h - w/2 - 1)
    ]
  , [ (w/2 + h/2 - 1, h/2)
    , (w            ,     w/2 + 1)
    , (w            , h - w/2 - 1)
    ]
  ]

bowtie :: Rational -> Rational -> [Obstacle]
bowtie w h | h > w = map (map swap) $ bowtie h w
bowtie w h =
  [ [ (1, h/2)
    , (0, h/2 + 1)
    , (0, h/2 - 1)
    ]
  , [ (w    , h/2 - 1)
    , (w    , h/2 + 1)
    , (w - 1, h/2)
    ]
  , [ (    h/2 + 1, h - 2)
    , (w - h/2 - 1, h - 2)
    , (w - h/2 + 1, h)
    , (    h/2 - 1, h)
    ]
  , [ (    h/2 - 1, 0)
    , (w - h/2 + 1, 0)
    , (w - h/2 - 1, 2)
    , (    h/2 + 1, 2)
    ]
  ]

ox :: Rational -> Rational -> [Obstacle]
ox w h | h > w = map (map swap) $ ox h w
ox w h =
  [ [ (0, h/2 + 1)
    , (0, h/2 - 1)
    , (1, h/2)
    ]
  , [ (w,   h/2 - 1)
    , (w,   h/2 + 1)
    , (w-1, h/2)
    ]
  , [ (0, 0)
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
  , [ (w/2 - 5/4, h/2 - 1)
    , (w/2 - 1/2, h/2 - 1)
    , (w/2 + 5/4, h/2 + 3/4)
    , (w/2 + 5/4, h/2 + 1)
    , (w/2 + 1/2, h/2 + 1)
    , (w/2 - 5/4, h/2 - 3/4)
    ]
  ]
