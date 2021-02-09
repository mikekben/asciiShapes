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

tn :: Int -> Natural
tn = naturalFromInteger . toInteger

data BesidePosition = BTop | BMiddle | BBottom
data AbovePosition = ALeft | AMiddle | ARight


data Figure = Rectangle Natural Natural| RightTriangle Natural Natural | CenterTriangle Natural | Circle Natural | Line Natural Natural | FlipV Figure | FlipH Figure | Beside Figure Figure BesidePosition | Above Figure Figure AbovePosition


makeSquare :: Natural -> Figure
makeSquare n = Rectangle n n



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



fillerRow :: Figure -> String
fillerRow (Rectangle w h) = makeList w emptyChar
fillerRow (RightTriangle w h) = makeList w emptyChar
fillerRow (CenterTriangle n) = makeList n emptyChar
fillerRow (Circle r) = makeList ((2*r)+1) emptyChar
fillerRow (Line x y) = makeList (x+1) emptyChar
fillerRow (FlipV fig) = fillerRow fig
fillerRow (FlipH fig) = fillerRow fig
fillerRow (Beside f s _) = fillerRow f ++ fillerRow s
fillerRow (Above f s _) = longer (fillerRow f) (fillerRow s)


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


lineRec :: [String] -> Natural -> Natural -> Natural -> [String]
lineRec soFar used x y =    if (used>=x) then soFar
                            else    let unmaxed = (round ((fromIntegral x)/(fromIntegral y)))
                                        run = max unmaxed 1
                                    in lineRec (soFar++[((makeList used emptyChar)++(makeList run specialChar)++(makeList (x-used-run) emptyChar))]) (used+unmaxed) x y



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
 -- Line --
figureStrings (Line x y) = lineRec [] 0 (x+1) (y+1)
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





 -- Example figures --

 -- A snowman with a hat
snowman :: Figure
snowman = Above (Above (makeSquare 5) (Rectangle 9 1) AMiddle)  (Above (Circle 4) (Above (Circle 6) (Circle 8) AMiddle) AMiddle) AMiddle

-- A ''dog'' :(
dog :: Figure
dog = Beside (FlipV (RightTriangle 2 2)) (Beside (Beside (Above (Rectangle 13 5) (Rectangle 1 4) ALeft) (Rectangle 1 9) BBottom) (Circle 2) BTop) BTop

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