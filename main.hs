-- MC 346
-- Haskell Project 
-- Group:
--  Ruy Castilho Barrichelo RA 177012
--  Vitor Kaoru Aoki        RA ------

import Data.Char
import Data.Map

main = do
    -- getstartFinishInput $ getBusLineInput $ 
    travelTimes <- getTravelTimeInput []
    busLines <- getBusLineInput []
    endPoints <- getEndPointsInput

    putChar '\n'
    putStrLn "Travel times:"
    print travelTimes
    putStrLn "\nBus Lines:"
    print busLines
    putStrLn "\nEnd Points:"
    print endPoints

getTravelTimeInput travelTimes = do
        travelTimeInput <- getLine 
        if Prelude.null travelTimeInput  
            then return travelTimes 
            else do  
                getTravelTimeInput ((castFourthToFloat $ tuplify4 $ words travelTimeInput):travelTimes)

getBusLineInput busLines= do
    busLineInput <- getLine
    if Prelude.null busLineInput  
        then return busLines
        else do  
            getBusLineInput ((castSecondToFloat $ tuplify2 $ words busLineInput):busLines)

getEndPointsInput = do
    endPointsInput <- getLine
    return $ tuplify2 $ words endPointsInput

castSecondToFloat (a, b) = (a, read b :: Float)
castFourthToFloat (a, b, c, d) = (a, b, c, read d :: Float)

tuplify2 [x, y] = (x,y)
tuplify4 [w, x, y, z] = (w, x, y, z)
