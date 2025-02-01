module HelloWorld (main) where
    
fac :: Int -> Int
fac 0 = 1
fac n=n*fac (n-1)

main :: IO ()
main = print ("Hello World: " ++ show(fac 5))
