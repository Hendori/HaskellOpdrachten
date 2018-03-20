------------------------------------------------------------------
-- Functioneel programmeren
-- Opdracht 1: Functionele figuren (versie 1.3)
--
-- Student:Hendri Eleveld 
-- Nummer:851988862
--
------------------------------------------------------------------
module Figuur where

import Data.Char
import System.IO

type Figuur a = Pos -> a

type Pos = (Double, Double) -- (x, y)

schaakbord :: Figuur Bool
schaakbord (x, y) = even (round x) == even (round y)

------------------------------------------------------------------
-- 1. ASCII rendering

type Dimensie = (Int, Int) -- (breedte, hoogte)

coordinaten :: Dimensie -> [[(Int, Int)]]
coordinaten (x,y) = ([[(a,b)|a<-[0..x-1]]|b<-[0..y-1]])

render :: Dimensie -> Figuur a -> [[a]] 
render d f = [[f(naarPositie d (x,y))|(x,y) <- xs] | xs <- coordinaten d] 

naarPositie :: Dimensie -> (Int, Int) -> Pos
naarPositie d (x, y) = (fromIntegral x*2/b-1, 1-fromIntegral y*2/h)
  where
    b  = fromIntegral (fst d-1)
    h  = fromIntegral (snd d-1)

boolChar :: Bool -> Char
boolChar a | a = '@'
           | not a = '.'

verander :: (a -> b) -> Figuur a -> Figuur b
verander = (.)  

toon :: Figuur Bool -> IO ()
toon = putStrLn . unlines . render (32, 20) . verander boolChar
------------------------------------------------------------------
-- 2. Basisvormen

cirkel :: Double -> Figuur Bool
cirkel r (x,y) = (x^2+y^2)<=r^2 

vierkant :: Double -> Figuur Bool
vierkant l (x,y) = 2*abs(x)<=l && 2*abs(y)<=l 

driehoek :: Figuur Bool
driehoek (x,y) = (y<=(2*x+1)) && (y<=(-2*x+1)) && y>=(-1) 

------------------------------------------------------------------
-- 3. Transformaties

transform :: (Pos -> Pos) -> Figuur a -> Figuur a
transform f fig = fig . f --werkt dit? 

verschuif :: (Double, Double) -> Figuur a -> Figuur a
verschuif (x,y) fig(x0,y0) = fig(x0-x,y0-y) 

schaal :: Double -> Double -> Figuur a -> Figuur a
schaal x y fig(x0,y0) = fig(x0/x,y0/y)

infixr 7 #
infixr 7 //

(#) :: (Double, Double) -> Figuur a -> Figuur a -- verschuiven
(#) = verschuif 

(//) :: Double -> Figuur a -> Figuur a -- schalen
(//) x = schaal x x

------------------------------------------------------------------
-- 4. Transformaties met poolcoordinaten

type Polar = (Double, Double) -- (afstand, hoek)

toPolar :: Pos -> Polar
toPolar (x, y) = (sqrt (x^2 + y^2), hoek) 
  where hoek  | x == 0     = pi/2 * signum y
              | x < 0      = atan (y/x) + pi
              | otherwise  = atan (y/x)

fromPolar :: Polar -> Pos
fromPolar (r, h) = (cos h*r, sin h*r)

transPolar :: (Polar -> Polar) -> Figuur a -> Figuur a
transPolar = undefined 

roteer :: Double -> Figuur a -> Figuur a
roteer u f(x,y) = f(fromPolar(r,a-u))
    where 
        r = fst(toPolar(x,y))
        a = snd(toPolar(x,y))

krul :: Double -> Figuur a -> Figuur a
krul d f(x,y) = f(fromPolar(r,a-d*r))
    where 
        r = fst(toPolar(x,y))
        a = snd(toPolar(x,y))

------------------------------------------------------------------
-- 5. Composities

compositie :: (a -> b -> c) -> Figuur a -> Figuur b -> Figuur c
compositie f fa fb (x,y) =undefined -- f (fa(x,y) fb(x,y)) 

(<+>) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<+>) fa fb (x,y) = (||) (fa(x,y)) (fb(x,y))

(<&>) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<&>) fa fb (x,y) = fa(x,y) && fb(x,y)

(<->) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<->) fa fb (x,y) = fa(x,y) && not(fb(x,y))  

ring :: Double -> Double -> Figuur Bool
ring a b = cirkel b <-> cirkel a 

box :: Double -> Double -> Figuur Bool
box a b = vierkant b <-> vierkant a

------------------------------------------------------------------
-- 6. Kleuren

type Kleur = (Double, Double, Double, Double) -- (rood, groen, blauw, alpha)

alpha :: Kleur -> Double
alpha (_, _, _, a) = a

roodp :: Kleur -> Double
roodp (a, _, _, _) = a
groenp :: Kleur -> Double
groenp (_, a, _, _) = a
blauwp :: Kleur -> Double
blauwp (_, _, a, _) = a

rood, groen, blauw, zwart, wit, leeg :: Kleur
rood   = (1,0,0,1)
groen  = (0,1,0,1)
blauw  = (0,0,1,1)
zwart  = (0,0,0,1)
wit    = (1,1,1,1)
leeg   = (0,0,0,0) -- volledig doorzichtig

veranderKleur :: (Double -> Double) -> Kleur -> Kleur
veranderKleur f c = ((f(roodp c)), (f(groenp c)), (f(blauwp c)), (f(alpha c)))

transparant :: Double -> Kleur -> Kleur
transparant u c = veranderKleur (*u) c 

zipKleur :: (Double -> Double -> Double) -> Kleur -> Kleur -> Kleur
zipKleur f c1 c2 = (f(roodp c1) (roodp c2), f(groenp c1) (groenp c2), f(blauwp c1) (blauwp c2), f(alpha c1) (alpha c2)) 

-- Ik heb in mixkleur ook zwart mee laten nemen, omdat in de voorbeeldaanroep de alpha waarde 1.0 moet zijn
mixKleur :: Double -> Kleur -> Kleur -> Kleur
mixKleur x c1 c2 = zipKleur max (veranderKleur (*x) c1) (zipKleur max (veranderKleur (*(1-x)) c2) (veranderKleur (*1) zwart))

------------------------------------------------------------------
-- 7. PPM rendering

headerPPM :: Dimensie -> String
headerPPM (x,y) = "P6 " ++ (show x) ++ " " ++ (show y) ++ " 255\n" 

--deze is specifiek bedoeld om van een element van een kleurtuple een char te maken
dTC :: Double -> Char
dTC x | round (255*x) > 0 =  chr(round(255*x))
      | otherwise = ' ' 

kleurPPM :: Kleur -> String
kleurPPM c = [dTC(roodp c), dTC(groenp c), dTC(blauwp c), dTC(alpha c)]

maakPPM :: Dimensie -> Figuur Kleur -> String
maakPPM d f = undefined  

schrijf :: FilePath -> Figuur Kleur -> IO ()
schrijf file = writeBinaryFile file . maakPPM (300, 300)

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile file s = do
  h <- openBinaryFile file WriteMode
  hPutStr h s
  hClose h
  
kleur :: Kleur -> Figuur Bool -> Figuur Kleur
kleur = kleurMet . const

kleurMet :: Figuur Kleur -> Figuur Bool -> Figuur Kleur
kleurMet = compositie (\k b -> if b then k else leeg)

------------------------------------------------------------------
-- 8. Kleuren composities

over :: Kleur -> Kleur -> Kleur
over = undefined

(<>) :: Figuur Kleur -> Figuur Kleur -> Figuur Kleur
(<>) = undefined

stapel :: [Figuur Kleur] -> Figuur Kleur
stapel = undefined

------------------------------------------------------------------
-- 9. Gradienten

gradient :: Kleur -> Kleur -> Figuur Kleur
gradient = undefined

gradientCirkel :: Kleur -> Kleur -> Figuur Kleur
gradientCirkel = undefined

------------------------------------------------------------------
-- 10. Voorbeelden

fig1, fig2, fig3, fig4 :: Figuur Kleur
fig1 = kleur (transparant 0.6 rood) (cirkel 0.9) 
       <> kleur wit (0.4 // schaakbord)
fig2 = kleur (transparant 0.6 (mixKleur 0.5 blauw wit)) ((0.4,0.4) # vierkant 1) 
       <> kleur rood (cirkel 0.5)
fig3 = stapel [ kleur (transparant 0.8 rood) ((x/2,-0.3) # ring 0.3 0.45) 
              | x <- [-1..1] ]
       <> kleur wit ((0.4 // schaakbord) <&> driehoek)
fig4 = kleurMet ((0.2,0.2) # 1.2 // gradientCirkel zwart wit) (cirkel 0.8)

type Serie = Double -> Figuur Kleur

serie :: Double -> Serie -> Figuur Kleur
serie n f = stapel (map f [0..n])

spiraal :: Serie -> Serie
spiraal g t = verschuif (fromPolar (t/10, t*0.8)) (g t)

eindvb :: Figuur Kleur
eindvb = figs <> bollen <> grid
  where
    paars   =  transparant 0.9 (mixKleur 0.5 rood blauw)
    lgrijs  =  mixKleur 0.3 zwart wit
    bol     =  kleurMet ((0.3,0.3) # 1.2 // gradientCirkel paars rood) (cirkel 1)
    bollen  =  (-0.1, -0.1) # serie 9 (spiraal (\t -> (0.05 + t/40) // bol))
    grid    =  kleurMet achter (0.3 // krul 0.1 schaakbord)
    achter  =  roteer (pi/6) (gradient zwart lgrijs)
    figs    =  (-0.5,0.5) # kleurMet grad (0.15 // foldr1 (<+>) lijst)
    lijst   =  [  (3,-0.5)  # schaal 1.1 0.9 (ring 0.6 1)
               ,  (2,2)     # ring 0.4 1
               ,  (-2,-2)   # roteer (pi/4) (box 0.6 1.5)
               ,  (0,2)     # cirkel 1 <-> ((0.5,0) # cirkel 1)
               ,  roteer (-pi/4) (driehoek <&> cirkel 1) 
               ]
    grad    =  gradient (transparant 0.85 groen) (transparant 0.85 blauw)
