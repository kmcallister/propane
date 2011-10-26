import Propane
import Data.Fixed ( divMod' )

wave t th = rotate th (unbal . cos . (+t) . fst)

combine xs = wrap . sum . sequence xs where
    wrap n = case divMod' n 1 of
        (k, v) -> if odd k then 1-v else v

ani t = fmap cGray . scale (1/64) . combine $ map (wave t) (spaced 7 0 pi)

main = saveAnimation "out.ani" 32 (Size 400 400) (speed (2*pi) ani)
