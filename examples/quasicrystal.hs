import Propane
import Data.Fixed ( divMod' )

wave th = rotate th (unbal . cos . fst)

combine xs = wrap . sum . sequence xs where
    wrap n = case divMod' n 1 of
        (k, v) -> if odd k then 1-v else v

im = fmap cGray . scale (1/64) . combine $ map wave (spaced 7 0 pi)

main = saveImage "out.png" (Size 400 400) im
