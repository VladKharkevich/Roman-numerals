duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string


get_IV_OrEmptyString:: Int -> String
get_IV_OrEmptyString number = 
    if number `mod` 10 == 4
    then "IV"
    else ""


get_IX_OrEmptyString:: Int -> String
get_IX_OrEmptyString number = 
    if number `mod` 10 == 9
    then "IX"
    else ""


get_XL_OrEmptyString:: Int -> String
get_XL_OrEmptyString number = 
    if (number `mod` 100) `div` 10 == 4
    then "XL"
    else ""


get_XC_OrEmptyString:: Int -> String
get_XC_OrEmptyString number = 
    if (number `mod` 100) `div` 10 == 9
    then "XC"
    else ""


get_CD_OrEmptyString:: Int -> String
get_CD_OrEmptyString number = 
    if (number `mod` 1000) `div` 100 == 4
    then "CD"
    else ""

get_CM_OrEmptyString:: Int -> String
get_CM_OrEmptyString number = 
    if (number `mod` 1000) `div` 100 == 9
    then "CM"
    else ""


get_I_OrEmptyString:: Int -> String
get_I_OrEmptyString number = duplicate "I" ((number `mod` 5) `mod` 4)

get_V_OrEmptyString:: Int -> String
get_V_OrEmptyString number =  
    if number `mod` 10 >= 5 && number `mod` 10 < 9
    then "V"
    else ""

get_X_OrEmptyString:: Int -> String
get_X_OrEmptyString number = duplicate "X" (((number `mod` 50) `div` 10) `mod` 4)

get_L_OrEmptyString:: Int -> String
get_L_OrEmptyString number =  
    if number `mod` 100 >= 50 && number `mod` 100 < 90
    then "L"
    else ""

get_C_OrEmptyString:: Int -> String
get_C_OrEmptyString number = duplicate "C" (((number `mod` 500) `div` 100) `mod` 4)

get_D_OrEmptyString:: Int -> String
get_D_OrEmptyString number =  
    if number `mod` 1000 >= 500 && number `mod` 1000 < 900
    then "D"
    else ""

get_M_OrEmptyString:: Int -> String
get_M_OrEmptyString number = duplicate "M" (number `div` 1000)


rim:: Int -> String
rim number = 
    get_M_OrEmptyString number ++  
    get_CM_OrEmptyString number ++ 
    get_D_OrEmptyString number ++ 
    get_CD_OrEmptyString number ++
    get_C_OrEmptyString number ++ 
    get_XC_OrEmptyString number ++
    get_L_OrEmptyString number ++ 
    get_XL_OrEmptyString number ++
    get_X_OrEmptyString number ++ 
    get_IX_OrEmptyString number ++
    get_V_OrEmptyString number ++ 
    get_IV_OrEmptyString number ++ 
    get_I_OrEmptyString number


main = do
    putStrLn "Enter value for your number: "
    input <- getLine
    let x = (read input :: Int)
    if x >= 0
    then print (rim x)
    else putStrLn "It's negative number!"