import Propane

main = saveImage "out.png" (Size 400 400) im where
    im (x,y) = cRGB (unbal x) (unbal y) 0
