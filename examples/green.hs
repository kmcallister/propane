import Propane
import Data.Colour.Names

main = saveImage "out.png" (Size 400 400) (const (opaque green))
