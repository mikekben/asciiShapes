-- Haskell library to construct ASCII art
-- (c) 2020-2021 Benjamin Mikek (see LICENSE for details)


{-# LANGUAGE LambdaCase #-}
import GHC.Num

specialChar = '#'
emptyChar = ' '


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

mmap :: (a -> b) -> [a] -> [b]
mmap _ [] = []
mmap f (x:xs) = f x : mmap f xs


longer :: [a] -> [a] -> [a]
longer f s = if (length f) >= (length s) then f
                                         else s


makeList :: Natural -> a -> [a]
makeList 0 _ = []
makeList n x = x : makeList (n-1) x



transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)





iota :: Natural -> [Natural]
iota 0 = []
iota n = n : iota (n-1)

pairCon :: (a,b) -> ([a],[b]) -> ([a],[b])
pairCon (f,s) ([],[]) = ([f],[s])
pairCon (f,s) ([],ls) = ([f],s:ls)
pairCon (f,s) (ls,[]) = (f:ls,[s])
pairCon (f,s) (ls1,ls2) = (f:ls1,s:ls2)

scaleDouble :: Natural -> Natural -> Natural -> Natural
scaleDouble w h x = round ((fromIntegral x / fromIntegral h) * fromIntegral w)

roundOdd :: Double -> Natural
roundOdd n = case (odd (round n),n < (fromIntegral (round n))) of (True,_) -> round n
                                                                  (False,True) -> (round n)-1
                                                                  (False,False) -> (round n)+1

divideRound :: Natural -> Double -> Natural
divideRound n d = tn (round ((fromIntegral n)/d))

tn :: Int -> Natural
tn = naturalFromInteger . toInteger

data BesidePosition = BTop | BMiddle | BBottom
data AbovePosition = ALeft | AMiddle | ARight


data Figure =   Rectangle Natural Natural           |
                RightTriangle Natural Natural       |
                CenterTriangle Natural              |
                Circle Natural                      |
                Line Natural Natural                |
                Text String Natural                 |
                Invisible Figure                    |
                FlipV Figure                        |
                FlipH Figure                        |
                Beside Figure Figure BesidePosition |
                Above Figure Figure AbovePosition   |
                CutH Figure Double                  



makeSquare :: Natural -> Figure
makeSquare n = Rectangle n n

makeTrapezoid :: Natural -> Natural -> Natural -> Figure
makeTrapezoid h t b = if (b >= t) then Beside (Beside (FlipH (RightTriangle (divideRound (b-t) 2.0) h)) (Rectangle t h) BMiddle) (RightTriangle (divideRound (b-t) 2.0) h) BMiddle
                                  else FlipV (makeTrapezoid h b t)

makeParallelogram :: Natural -> Natural -> Natural -> Figure
makeParallelogram h t b = Beside (Beside (FlipH (RightTriangle (divideRound (b-t) 2.0) h)) (Rectangle t h) BMiddle) (FlipV (RightTriangle (divideRound (b-t) 2.0) h)) BMiddle



isEmpty :: Figure -> Bool
isEmpty (Rectangle w h) = w==0 || h==0
isEmpty (RightTriangle w h) = w==0 || h==0
isEmpty (CenterTriangle n) = n==0
isEmpty(Circle r) = r==0
isEmpty(Line x y) = False
isEmpty (FlipV f) = isEmpty f
isEmpty(FlipH f) = isEmpty f
isEmpty(Beside f s _) = isEmpty f && isEmpty s
isEmpty(Above f s _) = isEmpty f && isEmpty s
isEmpty(CutH f p) = isEmpty f || p == 1
isEmpty(Invisible f) = isEmpty f
isEmpty(Text str _) = str == ""



fillerRow :: Figure -> String
fillerRow (Rectangle w h) = makeList w emptyChar
fillerRow (RightTriangle w h) = makeList w emptyChar
fillerRow (CenterTriangle n) = makeList n emptyChar
fillerRow (Circle r) = makeList ((2*r)+1) emptyChar
fillerRow (Line x y) = makeList x emptyChar
fillerRow (Invisible f) = fillerRow f
fillerRow (FlipV fig) = fillerRow fig
fillerRow (FlipH fig) = fillerRow fig
fillerRow (Beside f s _) = fillerRow f ++ fillerRow s
fillerRow (Above f s _) = longer (fillerRow f) (fillerRow s)
fillerRow (CutH f p) = fillerRow f
fillerRow (Text str size) = makeList (((tn . length) str) * (divideRound size 2.0)) emptyChar




circleRow :: Natural -> Natural -> String
circleRow 0 _ = []
circleRow r h =  let dr = fromIntegral r
                     dh = fromIntegral h
                     m = roundOdd (2.0 * sqrt (dr^2 - (dr-dh)^2))
                     lHalf = round ((((2.0*dr)+1.0)-(fromIntegral m)) / 2.0)
                     rHalf = ((2*r)+1) - m - lHalf
                     in  (makeList lHalf emptyChar) ++ (makeList m specialChar) ++ (makeList rHalf emptyChar)


forceHeight :: (Figure,Figure) -> BesidePosition -> ([String],[String])
forceHeight (f,s) BTop =    let fList = figureStrings f
                                sList = figureStrings s
                                fLength = tn (length fList)
                                sLength = tn (length sList)
                            in if fLength > sLength then (fList,sList++(makeList (fLength - sLength) (fillerRow s)))
                                                    else (fList ++ (makeList (sLength-fLength) (fillerRow f)),sList)
forceHeight (f,s) BMiddle = let fList = figureStrings f
                                sList = figureStrings s
                                fLength = tn (length fList)
                                sLength = tn (length sList)
                                half = tn (round  (abs (((fromIntegral fLength) - (fromIntegral sLength))/2.0)))
                            in if fLength > sLength then (fList,(makeList half (fillerRow s))++sList++(makeList (fLength - sLength - half) (fillerRow s)))
                                                    else ((makeList half (fillerRow f))++fList ++ (makeList (sLength - fLength - half) (fillerRow f)),sList)
forceHeight (f,s) BBottom = let fList = figureStrings f
                                sList = figureStrings s
                                fLength = tn (length fList)
                                sLength = tn (length sList)
                            in if fLength > sLength then (fList,(makeList (fLength - sLength) (fillerRow s))++sList)
                                                    else ((makeList (sLength-fLength) (fillerRow f))++fList,sList)


forceWidth :: (Figure,Figure) -> AbovePosition -> ([String],[String])
forceWidth (f,s) ALeft =    let fList = figureStrings f
                                sList = figureStrings s
                                fWidth = tn (length (fillerRow f))
                                sWidth = tn (length (fillerRow s))
                            in if fWidth > sWidth   then (fList,map (\x -> x ++ (makeList (fWidth-sWidth) emptyChar)) sList)
                                                    else (map (\x -> x ++ (makeList (sWidth-fWidth) emptyChar)) fList,sList)
forceWidth (f,s) AMiddle =  let fList = figureStrings f
                                sList = figureStrings s
                                fWidth = tn (length (fillerRow f))
                                sWidth = tn (length (fillerRow s))
                                half = tn (round  (abs (((fromIntegral fWidth) - (fromIntegral sWidth))/2.0)))
                            in if fWidth > sWidth   then (fList,map (\x -> (makeList half emptyChar) ++ x ++ (makeList (fWidth-sWidth-half) emptyChar)) sList)
                                                    else (map (\x -> (makeList half emptyChar) ++ x ++ (makeList (sWidth-fWidth-half) emptyChar)) fList,sList)
forceWidth (f,s) ARight =   let fList = figureStrings f
                                sList = figureStrings s
                                fWidth = tn (length (fillerRow f))
                                sWidth = tn (length (fillerRow s))
                            in if fWidth > sWidth   then (fList,map (\x -> (makeList (fWidth-sWidth) emptyChar) ++ x) sList)
                                                    else (map (\x -> (makeList (sWidth-fWidth) emptyChar) ++ x) fList,sList)



lineRec :: [String] -> Natural -> Natural -> Natural -> Natural -> [String]
lineRec soFar used 0 perRow width = soFar
lineRec soFar used rowsLeft perRow width =  lineRec (soFar ++ [(makeList used emptyChar) ++ (makeList perRow specialChar) ++ (makeList (width - perRow - used) emptyChar)]) (used + perRow) (rowsLeft - 1) perRow width


textWidthConstant = 1.5

character :: Char -> Natural -> Figure

character ' ' size =    (Invisible (Rectangle (divideRound size textWidthConstant) size))
character '!' size =    let width = divideRound size textWidthConstant
                            ten = max 1 (divideRound size 10.0)
                            circSize = 1 + (ten * 2)
                            triSize = divideRound size 2.0
                            trap = FlipV (makeTrapezoid triSize ten width)
                            spacer = (Invisible (Rectangle ten (size - triSize - circSize)))
                            circ = Circle ten
                        in  (Above (Above trap spacer AMiddle) circ AMiddle)

-- Missing: F, J, M, P, Q, V, W, X, Y, Z


character 'A' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            ninetyP = width - fiveP - fiveP
                        in  (Beside (Above (Beside (Invisible (Rectangle fiveP fiveP)) (Rectangle ninetyP fiveP) BMiddle) (Beside (Rectangle fiveP (size - fiveP)) (Rectangle ninetyP fiveP) BMiddle) ALeft) (Rectangle fiveP (size - fiveP)) BBottom)
character 'B' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            ninetyP = width - fiveP - fiveP
                            lhalf = divideRound (size - fiveP - fiveP - fiveP) 2.0
                            uhalf = size - fiveP - fiveP - fiveP - lhalf
                            inv = Invisible (Rectangle fiveP fiveP)
                            beam = (Beside (Rectangle ninetyP fiveP) inv BMiddle)
                        in  (Beside (Rectangle fiveP size) (Above (Above (Above (Above beam (Rectangle fiveP uhalf) ARight) beam ALeft) (Rectangle fiveP lhalf) ARight) beam ARight) BMiddle)
character 'C' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            nfp = width - fiveP
                            beam = (Beside (Invisible (Rectangle fiveP fiveP)) (Rectangle nfp fiveP) BMiddle)
                        in  (Above (Above  beam (Rectangle fiveP (size-fiveP-fiveP)) ALeft) beam ARight)


character 'D' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            twenty = max 1 (divideRound size 4.0)
                            sixty = size - twenty - twenty
                            diag = Line twenty twenty
                            tb = (Rectangle (width - twenty - fiveP) fiveP)
                        in  (Beside (Rectangle fiveP size)
                                    (Beside (Above (Above tb (Invisible (Rectangle (width - twenty - fiveP) (size - fiveP - fiveP))) AMiddle) tb AMiddle) 
                                            (Above (Above diag (Rectangle fiveP sixty) ARight) (FlipH diag) ARight) 
                                            BMiddle)
                                    BMiddle)
character 'E' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            ht = size - fiveP - fiveP
                        in  (Above (Above (Rectangle width fiveP) (Beside (Rectangle fiveP ht) (Rectangle (width - fiveP) fiveP) BMiddle) ARight) (Rectangle width fiveP) ARight)
character 'G' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            inv = Invisible (Rectangle fiveP fiveP)
                            lhalf = divideRound (size - fiveP - fiveP - fiveP) 2.0
                            uhalf = size - fiveP - fiveP - fiveP - lhalf
                        in  (Above (Beside inv (Rectangle (width-fiveP) fiveP) BMiddle) (Beside (Above (Rectangle fiveP (size - fiveP - fiveP)) inv AMiddle) (Above (Above (Rectangle (divideRound width 2.0) fiveP) (Rectangle fiveP lhalf) ARight) (Rectangle (width - fiveP) fiveP) ARight) BBottom) ALeft)
character 'H' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            ninetyP = width - fiveP - fiveP
                        in  (Beside (Beside (Rectangle fiveP size) (Rectangle ninetyP fiveP) BMiddle) (Rectangle fiveP size) BMiddle)
character 'I' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            ninetyP = size - fiveP - fiveP
                        in  (Above (Above (Rectangle width fiveP) (Rectangle fiveP ninetyP) AMiddle) (Rectangle width fiveP) AMiddle)

character 'K' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            bhalf = divideRound size 2.0
                            uhalf = size - bhalf
                            bottom = (Beside (Rectangle fiveP bhalf) (Line (width - fiveP) bhalf) BMiddle)
                            top = (Beside (Rectangle fiveP uhalf) (FlipV (Line (width - fiveP) uhalf)) BMiddle)
                        in  (Above top bottom AMiddle)


character 'L' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                        in  (Above (Rectangle fiveP (size-fiveP)) (Rectangle width fiveP) ALeft)
character 'N' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                        in  (Beside (Beside (Rectangle fiveP size) (Line (width - fiveP - fiveP) size) BMiddle) (Rectangle fiveP size) BMiddle)
character 'O' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            inv = Invisible (Rectangle fiveP fiveP)
                            side = (Above inv (Rectangle fiveP (size - fiveP - fiveP)) AMiddle)
                            bottom = (Beside (Beside inv (Rectangle (width - fiveP - fiveP) fiveP) BMiddle) inv BMiddle)
                        in  (Above (Beside (Beside side (Rectangle (width - fiveP - fiveP) fiveP) BTop) side BTop) bottom AMiddle)
character 'R' size =    let width = divideRound size textWidthConstant
                            half = divideRound size 2.0
                            fiveP = max 1 (divideRound size 20.0)
                            tenP = fiveP + fiveP
                        in  (Above (Beside (Above (Above (Rectangle (width - fiveP) fiveP) (Rectangle fiveP (half - tenP)) ALeft) (Rectangle (width - fiveP) fiveP) ARight) (Rectangle fiveP (half-tenP)) BMiddle) (Beside (Rectangle fiveP (size-half)) (Line (width-1) (size-half)) BTop) ALeft)
character 'S' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            inv = Invisible (Rectangle fiveP fiveP)
                            nfp = width - fiveP
                            lhalf = divideRound (size - fiveP - fiveP - fiveP) 2.0
                            uhalf = size - fiveP - fiveP - fiveP - lhalf
                            middle = (Beside (Beside inv (Rectangle (width - fiveP - fiveP) fiveP) BMiddle) inv BMiddle)
                            top = (Beside inv (Rectangle nfp fiveP) BMiddle)
                        in  (Above (Above (Above (Above top (Rectangle fiveP uhalf) ALeft) middle ALeft) (Rectangle fiveP lhalf) ARight) (FlipH top) ARight)
character 'T' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                        in  (Above (Rectangle width fiveP) (Rectangle fiveP (size-fiveP)) AMiddle)
character 'U' size =    let width = divideRound size textWidthConstant
                            fiveP = max 1 (divideRound size 20.0)
                            twenty = max 1 (divideRound size 5.0)
                            diag = (Line twenty twenty)
                            bottom = (Beside (Beside diag (Rectangle (width - twenty - twenty) fiveP) BBottom) (FlipV diag) BBottom)
                            top = (Beside (Beside (Rectangle fiveP (size - twenty)) (Invisible (Rectangle (width - fiveP - fiveP) (size-twenty))) BMiddle) (Rectangle fiveP (size - twenty)) BMiddle)
                        in (Above top bottom AMiddle)

           
                     
-- Should respect invariant (isEmpty f) == (figureStrings f == [])

figureStrings :: Figure -> [String]
 -- Rectangle --
figureStrings (Rectangle 0 _) = []
figureStrings (Rectangle _ 0) = []
figureStrings (Rectangle w h) = makeList h (makeList w specialChar)
 -- RightTriangle --
figureStrings (RightTriangle 0 _) = []
figureStrings (RightTriangle _ 0) = []
figureStrings (RightTriangle w h) = map (\x -> (makeList (scaleDouble w h x) specialChar) ++ (makeList (w - (scaleDouble w h x))) emptyChar) (reverse (iota h))
 -- CenterTriangle --
figureStrings (CenterTriangle n) = if odd n then map (\x -> (makeList (quotNatural (n-((2*x)-1)) 2) emptyChar)++(makeList ((2*x)-1) specialChar)++(makeList (quotNatural (n-((2*x)-1)) 2) emptyChar)) (reverse (iota ((quotNatural n 2)+1)))
                                            else (map (\x -> (makeList (quotNatural (n-(2*x)) 2) emptyChar)++(makeList (2*x) specialChar)++(makeList (quotNatural (n-(2*x)) 2) emptyChar)) (reverse (iota (quotNatural n 2))))
 -- Circle --
figureStrings (Circle r) = map (\x -> circleRow r x) (map (\x -> x-1) (reverse (iota ((2*r)+1))))
 -- Line -- (NOT DONE!)
figureStrings (Line x y) =  if x >= y then lineRec [] 0 y (divideRound x (fromIntegral y)) x
                                      else transpose (figureStrings (Line y x))


figureStrings (Text str height) = let nothing = (Rectangle 0 0)
                                      buffer = (Invisible (Rectangle (max 1 (divideRound height 10.0)) height))
                                  in  figureStrings(foldl (\x y -> (Beside x (Beside buffer y BMiddle) BMiddle)) nothing (map (\x -> character x height) str))


-- Invisible --
figureStrings (Invisible f) = map (\x -> (makeList (tn (length x)) emptyChar)) (figureStrings f)
 -- FlipV --
figureStrings (FlipV fig) = reverse (figureStrings fig)
 -- FlipH --
figureStrings (FlipH fig) = map reverse (figureStrings fig)
 -- Beside --
figureStrings (Beside f s pos) = let (firstLst,secondLst) = forceHeight (f,s) pos
                                 in  zipWith (++) firstLst secondLst
 -- Above --
figureStrings (Above f s pos) = let (firstLst,secondLst) = forceWidth (f,s) pos
                                in firstLst ++ secondLst
 -- Cut --
figureStrings (CutH f p) =  let base = figureStrings f
                                toDrop = round (p * (fromIntegral (length base)))
                            in  drop toDrop base












 -- Example figures --

 -- A snowman with a hat
snowman :: Figure
snowman = Above (Above (makeSquare 5) (Rectangle 9 1) AMiddle)  (Above (Circle 4) (Above (Circle 6) (Circle 8) AMiddle) AMiddle) AMiddle

-- A ''dog'' :(
dog :: Figure
dog = Beside (FlipV (RightTriangle 2 2)) (Beside (Beside (Above (Rectangle 13 5) (Rectangle 1 4) ALeft) (Rectangle 1 9) BBottom) (Circle 2) BTop) BTop

-- A heart
heart :: Figure
heart = Above (Beside (FlipV (CutH (Circle 10) 0.51)) (FlipV (CutH (Circle 10) 0.51)) BMiddle) (FlipV (CenterTriangle 38)) AMiddle

goodLuck :: Figure
goodLuck = Text "GOOD LUCK!" 15

-- A Sierpinski triangle
sierpinski :: Natural -> Natural -> Figure
sierpinski n 1 = CenterTriangle n
sierpinski n depth = Above (sierpinski n (depth - 1)) (Beside (sierpinski n (depth - 1)) (sierpinski n (depth - 1)) BMiddle) AMiddle






draw :: Figure -> IO ()
draw fig
    | isEmpty fig = return ()
    | otherwise = let ker = (\case 
                                [] -> return ()
                                (x:xs) -> do    putStrLn x
                                                ker xs)
                  in ker (figureStrings fig)

drawWide :: Figure -> IO ()
drawWide fig
    | isEmpty fig = return ()
    | otherwise = let ker = (\case 
                                [] -> return ()
                                (x:xs) -> do    putStrLn (foldl (++) "" (map (\case 
                                                                ' ' -> "  "
                                                                '#' -> "##")
                                                            x))
                                                ker xs)
                  in ker (figureStrings fig)