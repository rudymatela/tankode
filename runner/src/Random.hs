-- |
-- Module      : Random
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Additional instances of 'Random' for 'System.Random' from Haskell's random
-- package.  A pull request has been made to include those in the official
-- random package:
--
-- https://github.com/haskell/random/pull/43
--
-- Until it is merged and a new version is released this module will exist
-- here.
module Random (module System.Random) where

import System.Random

instance (Random a, Random b) => Random (a,b) where
  randomR ((lox,loy),(hix,hiy)) rng = let (x,rng')  = randomR (lox,hix) rng
                                          (y,rng'') = randomR (loy,hiy) rng'
                                       in ((x,y),rng'')
  random rng = let (x,rng')  = random rng
                   (y,rng'') = random rng'
                in ((x,y),rng'')

instance (Random a, Random b, Random c) => Random (a,b,c) where
  randomR ((lx,ly,lz),(hx,hy,hz)) rng =
    case randomR (((lx,ly),lz),((hx,hy),hz)) rng of
      (((x,y),z),rng') -> ((x,y,z),rng')
  random rng =
    case random rng of
      (((x,y),z),rng') -> ((x,y,z),rng')

instance (Random a, Random b, Random c, Random d) => Random (a,b,c,d) where
  randomR ((lx,ly,lz,lw),(hx,hy,hz,hw)) rng =
    case randomR (((lx,ly),lz,lw),((hx,hy),hz,hw)) rng of
      (((x,y),z,w),rng') -> ((x,y,z,w),rng')
  random rng =
    case random rng of
      (((x,y),z,w),rng') -> ((x,y,z,w),rng')

instance (Random a, Random b, Random c, Random d, Random e) => Random (a,b,c,d,e) where
  randomR ((lx,ly,lz,lw,lv),(hx,hy,hz,hw,hv)) rng =
    case randomR (((lx,ly),lz,lw,lv),((hx,hy),hz,hw,hv)) rng of
      (((x,y),z,w,v),rng') -> ((x,y,z,w,v),rng')
  random rng =
    case random rng of
      (((x,y),z,w,v),rng') -> ((x,y,z,w,v),rng')

instance (Random a, Random b, Random c, Random d, Random e, Random f) => Random (a,b,c,d,e,f) where
  randomR ((lx,ly,lz,lw,lv,lu),(hx,hy,hz,hw,hv,hu)) rng =
    case randomR (((lx,ly),lz,lw,lv,lu),((hx,hy),hz,hw,hv,hu)) rng of
      (((x,y),z,w,v,u),rng') -> ((x,y,z,w,v,u),rng')
  random rng =
    case random rng of
      (((x,y),z,w,v,u),rng') -> ((x,y,z,w,v,u),rng')
