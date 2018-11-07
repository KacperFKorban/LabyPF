roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b- d) / e, (-b + d)/ e)
    where d = sqrt $ b * b - 4 * a * c
          e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x / l, y / l)
    where l = sqrt $ x ^ 2 + y ^ 2

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x / l, y / l, z / l)
    where l = sqrt $ x ^ 2 + sqrt (y ^ 2 + z ^ 2)

triangleArea :: Double -> Double -> Double -> Double
triangleArea x y z = sqrt $ p * (p - x) * (p - y) * (p - z)
    where p = (x + y + z) / 2