-- import Prelude hiding (cos, sin, tan, acos, asin, atan, atan2)
-- import qualified Prelude as P
-- import Graphics.Gloss
-- import Graphics.Gloss.Raster.Field
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}


--  @-threaded -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3@


-- @ data LL a = BXYZ { size  :: {v: Int | v > 0 }
--                      , elems :: {v: [a] | (len v) = size }
--                      }
--   @


-- @ newtype Latitude = Latitude { lat :: {x:Double | -90 < x && x <= 90}} @
-- newtype Latitude = Latitude Double

-- @ data Longitude = Long {long:Double | -10 < long && long <= 90} @
-- data Longitude = Long Double

-- instance Show Longitude where
--       show _ = "hi"

-- @ type Latitude = {x:Double | -90 < x && x <= 90} @
-- type Latitude = Double

-- @ type Longitude = {x:Double | -90 < x && x <= 90} @
-- type Longitude = Double

-- @ type Score = {x:Double | -1 <= x && x <= 1} @
-- type Score = Double

-- @ type Weight = {x:Double | 0 <= x && x <= 1} @
-- type Weight = Double

-- @ type Coordinate = (Latitude, Longitude) @
-- type Coordinate = (Latitude, Longitude)

-- @ null_island :: Coordinate @
-- null_island :: Coordinate
-- null_island = (0, 0)

-- @ type Map = Coordinate -> Score @
-- type Map = Coordinate -> Score

-- @ empty_map :: Map @
-- empty_map :: Map
-- empty_map = \coord -> 0

-- @ type Layer = (Map, Weight) @
-- type Layer = (Map, Weight)

-- @ measure weight_sum :: [(((Double, Double) -> Double), Double)] -> Double @
-- weight_sum :: [Layer] -> Double
-- weight_sum [] = 0
-- weight_sum ((_, w):xs) = w + weight_sum xs

-- @ type LayerSet = {ls:[Layer] | weight_sum ls /= 99} @
-- type LayerSet = [Layer]

-- @ lay :: Layer @
-- lay :: Layer
-- lay = (empty_map, 1)

-- @ l1 :: LayerSet @
-- l1 :: LayerSet
-- l1 = [lay]

-- combine_layers :: LayerSet -> Map
-- combine_layers [] coord = 0
-- combine_layers (l:ls) coord =

-- -- @ combine_layers :: {ls:[Layers] | ls /= []} -> Map @
-- combine_layers :: [Layer] -> Map
-- combine_layers ls = map (/ (weight_sum ls)) ls



deg_to_rad, rad_to_deg :: (RealFloat a) => a -> a
deg_to_rad = (* (pi / 180.0))
rad_to_deg = (* (180.0 / pi))
-- sin = P.sin . deg_to_rad
-- cos = P.cos . deg_to_rad
-- tan = P.tan . deg_to_rad
-- acos = rad_to_deg . P.acos
-- asin = rad_to_deg . P.asin
-- atan = rad_to_deg . P.atan

-- atan2 :: (RealFloat a) => a -> a -> a
-- atan2 x y = rad_to_deg $ P.atan2 x y

hypot :: (RealFloat a) => a -> a -> a
hypot x y = sqrt $ x^2 + y^2

-- normalizes an angle to the range [-pi, pi]; adapted from Graphics.Gloss.Geometry.Angle
normalize_angle :: Double -> Double
normalize_angle a = a - 2 * pi * floor' ((a + pi) / (2 * pi)) where
      floor' x = fromIntegral (floor x :: Int)




-------


data Sphere = Sphere { radius :: Double
                     }

-- oblate spheroid implied, equatorial radius <= polar radius
data Spheroid = Spheroid { equatorial_radius :: Double
                         , polar_radius      :: Double
                         }

flattening :: Spheroid -> Double
flattening (Spheroid a b) = (a - b) / a

eccentricity :: Spheroid -> Double
eccentricity (Spheroid a b) = sqrt $ 1 - (b^2) / (a^2)

surface_area :: Spheroid -> Double
surface_area s@(Spheroid a b) = 2 * pi * a^2 * (1 + ((1 - e^2) / e) * atanh e) where
      e = eccentricity s

mean_radius_spheroid_approximation :: Spheroid -> Sphere
mean_radius_spheroid_approximation (Spheroid a b) = Sphere $ (2*a + b) / 3

-- sphere with same surface area as given spheroid
authalic_spheroid_approximation :: Spheroid -> Sphere
authalic_spheroid_approximation = Sphere . sqrt . (/(4*pi)) . surface_area

reference_ellipsoid :: Double -> Double -> Spheroid
reference_ellipsoid a inv_f = Spheroid a (a - a/inv_f)

wgs_84 :: Spheroid
wgs_84 = reference_ellipsoid 6378137.0 298.257223563

-- measured in radians in range [-pi/2, pi/2]
data Latitude = Latitude Double

instance Show Latitude where
      show (Latitude lat) = show (abs lat_deg) ++ if lat_deg > 0 then "° N" else "° S" where
            lat_deg = rad_to_deg lat

lat :: Double -> Latitude
lat l = Latitude $ normalize_angle l where
      n = normalize_angle l

equator :: Latitude
equator = lat 0

-- measured in radians in range [-pi, pi]
data Longitude = Longitude Double

instance Show Longitude where
      show (Longitude long) = show (abs long_deg) ++ if long_deg > 0 then "° E" else "° W" where
            long_deg = rad_to_deg long

long :: Double -> Longitude
long l = Longitude $ normalize_angle l

prime_meridian :: Longitude
prime_meridian = long 0


data Coordinate = Coordinate { latitude  :: Latitude
                             , longitude :: Longitude
                             }

instance Show Coordinate where
      show (Coordinate lat long) = show lat ++ ", " ++ show long


antipode :: Coordinate -> Coordinate
antipode (Coordinate (Latitude φ) (Longitude λ)) = (-φ, if λ > 0 then λ - 180 else λ + 180)


-- the position on a map
data Position = Position { easting  :: Double
                         , northing :: Double}

type Projection = (Coordinate -> Position, Position -> Maybe Coordinate)

data Projection = Projection { project   :: Coordinate -> Position
                             , unproject :: Position -> Maybe Coordinate
                             }


-- null_island, north_pole, south_pole :: Coordinate
-- null_island = Coordinate equator prime_meridian
-- north_pole = Coordinate (Latitude pi) prime_meridian

-- type Equatorial_Radius = Double
-- type Polar_Radius = Double
-- type Spheroid = (Equatorial_Radius, Polar_Radius)


-- wgs_84 :: Spheroid
-- wgs_84 = (6378137.0, 6356752.314245)

-- flattening :: Spheroid -> Double
-- flattening (a, b) = (a - b) / a

-- mean_radius :: Spheroid -> Double
-- mean_radius (a, b) = (2*a + b) / 3


-- type Latitude = Double

-- equator :: Latitude
-- equator = 0


-- type Longitude = Double

-- prime_meridian :: Longitude
-- prime_meridian = 0


-- type Coordinate = (Latitude, Longitude)

-- null_island, north_pole, south_pole, eastern_hemisphere_center, western_hemisphere_center :: Coordinate
-- null_island = (0, 0)
-- north_pole = (90, 0)
-- south_pole = (-90, 0)
-- eastern_hemisphere_center = (0, 90)
-- western_hemisphere_center = (0, -90)

-- antipode :: Coordinate -> Coordinate
-- antipode (φ, λ) = (-φ, if λ > 0 then λ - 180 else λ + 180)

-- valid_coordinate :: Coordinate -> Bool
-- valid_coordinate (φ, λ) = φ >= -90 && φ <= 90 && λ >= -180 && λ <= 180

-- type Place = Spheroid -> Coordinate

-- kaaba :: Place
-- kaaba wgs_84 = (21.422510, 39.826168)


-- type Distance = Double
-- type DistanceFunction = Coordinate -> Coordinate -> Distance

-- -- approximation using a sphere model of earth -- error < 1%
-- distance_spherical :: Spheroid -> DistanceFunction
-- distance_spherical spheroid (φ₁, λ₁) (φ₂, λ₂) = r * δσ where
--       δλ = abs $ λ₁ - λ₂
--       x = hypot (cos φ₂ * sin δλ) (cos φ₁ * sin φ₂ - sin φ₁ * cos φ₂ * cos δλ)
--       y = sin φ₁ * sin φ₂ + cos φ₁ * cos φ₂ * cos δλ
--       δσ = deg_to_rad $ atan2 x y
--       r = mean_radius spheroid


-- type Easting = Double
-- type Northing = Double
-- type Position = (Easting, Northing)

-- origin :: Position
-- origin = (0, 0)


-- type Projection = Position -> Coordinate

-- plate_carrée :: Projection
-- plate_carrée (x, y) = (y, x)

-- orthographic_spherical :: Spheroid -> Coordinate -> Projection
-- orthographic_spherical spheroid center@(φ₀, λ₀) pos@(x, y) = if (pos == origin) then center else (φ, λ) where
--       ρ = hypot x y
--       r = mean_radius spheroid
--       c = asin $ ρ / r
--       φ = asin $ cos c * sin φ₀ + (y * sin c * cos φ₀) / ρ
--       λ = λ₀ + atan2 (x * sin c) (ρ * cos c * cos φ₀ - y * sin c * sin φ₀)


-- type Spatial data_type = Coordinate -> data_type
-- type Map data_type = Position -> data_type

-- make_map :: Projection -> Spatial a -> Map (Maybe a)
-- make_map proj spatial_data pos = if valid_coordinate coord then Just (spatial_data coord) else Nothing where
--       coord = proj pos

-- -----


-- type Score = Double
-- type Weight = Double

-- uniform_spatial :: Score -> Spatial Score
-- uniform_spatial s = \coord -> s

-- positivity, neutrality, negativity :: Spatial Score
-- positivity = uniform_spatial 1
-- neutrality = uniform_spatial 0
-- negativity = uniform_spatial (-1)


-- type Layer = (Spatial Score, Weight)

-- -- sum of weights must be 1
-- combine_layers :: [Layer] -> Spatial Score
-- combine_layers [] = neutrality
-- combine_layers ((s, w):ls) = \coord -> (s coord) * w + (combine_layers ls coord)


-- weight_distance :: DistanceFunction -> Coordinate -> (Distance -> Weight) -> Spatial Score -> Spatial Score
-- weight_distance dist_func from weight_func base to = score where
--       dist = dist_func from to
--       score = (weight_func dist) * (base to)

-- scale_map :: Double -> Map a -> Map a

-- scale_map s m (x, y) = m (x / s, y / s)


-- -------------------------------------------------

-- test_dist_f = (distance_spherical wgs_84)
-- test_dist_importance = (\d -> (2000000/(d+2000000)))
-- test_kaaba_importance = weight_distance test_dist_f (kaaba wgs_84) test_dist_importance positivity
-- test_np_disimportance = weight_distance test_dist_f south_pole     test_dist_importance negativity
-- test_all_locality = combine_layers [(test_kaaba_importance, 0.5), (test_np_disimportance, 0.5)]

-- -- test_map = scale_map 0.00558 $ make_map plate_carrée test_all_locality

-- test_map = scale_map 0.00000008 $ make_map (orthographic_spherical wgs_84 null_island) test_all_locality

-- -- test_map :: Map (Maybe Score)
-- -- test_map = scale_map 0.0053 $ make_map plate_carrée (weight_distance d north_pole d_f (weight_distance d (kaaba wgs_84) d_f positivity)) where
-- --       d = (distance_approximation wgs_84)
-- --       d_f = (\d -> (8000000/(d+8000000)))


-- -------------------------------------------------



-- color_spectrum :: [(Float, Color)] -> Float -> Color
-- color_spectrum ((l, lc):rest@((u, uc):_)) x
--       | x > u = color_spectrum rest x
--       | otherwise = mixColors (u - x) (x - l) lc uc

-- cool_grey, cool_amber, cool_blue, cool_purple :: Color
-- cool_grey   = rgbI 75   75  75
-- cool_amber  = rgbI 141 111  71
-- cool_blue   = rgbI 48   77 193
-- cool_purple = rgbI 255  22 231

-- bw_color_spectrum, cool_color_spectrum :: Float -> Color
-- bw_color_spectrum = color_spectrum [(0, black), (1, white)]
-- cool_color_spectrum = color_spectrum [(0, cool_grey), (0.5, cool_amber), (0.8, cool_blue), (1, cool_purple)]


-- type ColorField = (Float, Float) -> Color

-- plot_map :: (Map (Maybe Score)) -> ColorField
-- plot_map m (x, y) = case (m (realToFrac x, realToFrac y)) of
--       Nothing  -> black
--       (Just s) -> cool_color_spectrum $ realToFrac ((s + 1) / 2)


-- w, h :: Int
-- w = 1900
-- h = 1900

-- main
--  = display
--         (InWindow
--             "Livability Index"
--             (w, h)
--             (0, 0))
--       black
--       picture

-- picture = makePicture w h 1 1 (plot_map test_map)


main = putStrLn "Hello World"
