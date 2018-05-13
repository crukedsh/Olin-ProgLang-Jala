# Operators

## Monadic operators
 - Negative `-`
    ```
    JALA> - 1 2
    [ _1 _2 ]
    ``` 
    
 - ShapeOf `$`
     ```
    JALA> m=. 3 4 $ 1 2 3
    JALA> m
    [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
    JALA> $m
    [ 3 4 ]
     ``` 
 
 - Tally `#`
    ```
    JALA> # 1 2
    2
    JALA> m=. 3 4 $ 1 2 3
    JALA> m
    [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
    JALA> # m
    3
    ``` 
    
 - Sum `+/`
   ```
   JALA> +/ 1 2
   3
   JALA> m=. 3 4 $ 1 2 3
   JALA> m
   [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
   JALA> +/ m
   [ 6 6 6 6 ]
   ``` 
   
 - Not `-.`
   ```
   JALA> -. 1 0 1
   [ 0 1 0 ]
   ```
   
 - Ravel `,`
   ```
   JALA> m=. 3 4 $ 1 2 3
   JALA> m
   [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
   JALA> ,m
   [ 1 2 3 1 2 3 1 2 3 1 2 3 ]
   ```
   
 - Transpose `|:`
   ```
   JALA> m=. 3 4 $ 1 2 3
   JALA> m
   [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
   JALA> |:m
   [ [ 1 2 3 ] [ 2 3 1 ] [ 3 1 2 ] [ 1 2 3 ] ]   
   ```
   
## Dyadic operators
 - Plus `+`
    ```
    JALA> m=. 3 4 $ 1 2 3
    JALA> m
    [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
    JALA> n=. 1
    JALA> m+n
    [ [ 2 3 4 2 ] [ 3 4 2 3 ] [ 4 2 3 4 ] ]   
    ```
    
 - Minus `-`
    ```
    JALA> m=. 3 4 $ 1 2 3
    JALA> m
    [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
    JALA> n=. 1
    JALA> m-n
    [ [ 0 1 2 0 ] [ 1 2 0 1 ] [ 2 0 1 2 ] ]  
    ```
    
 - Times `*`
    ```
    JALA> m=. 3 4 $ 1 2 3
    JALA> m
    [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
    JALA> n=. 2
    JALA> m*n
    [ [ 2 4 6 2 ] [ 4 6 2 4 ] [ 6 2 4 6 ] ] 
    ```
    
 - Divide `%`
    ```
    JALA> m=. 3 4 $ 2 4 6
    JALA> m
    [ [ 2 4 6 2 ] [ 4 6 2 4 ] [ 6 2 4 6 ] ] 
    JALA> n=. 2
    JALA> m%n
    [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ] 
    ```
    
 - Shape `$`
    ```
    JALA> m=. 3 4 $ 2 4 6
    JALA> m
    [ [ 2 4 6 2 ] [ 4 6 2 4 ] [ 6 2 4 6 ] ] 
    ```
    
 - And `+.`
    ```
    JALA> m=. 3 4 $ 1 0 1
    JALA> m
    [ [ 1 0 1 1 ] [ 0 1 1 0 ] [ 1 1 0 1 ] ]
    JALA> n=. 3 $ 1 0 
    JALA> n
    [ 1 0 1 ]
    JALA> m +. n
    [ [ 1 1 1 1 ] [ 0 1 1 0 ] [ 1 1 1 1 ] ]
    ```
 - Or `*.`
    ```
    JALA> m=. 3 4 $ 1 0 1
    JALA> m
    [ [ 1 0 1 1 ] [ 0 1 1 0 ] [ 1 1 0 1 ] ]
    JALA> n=. 3 $ 1 0 
    JALA> n
    [ 1 0 1 ]
    JALA> m *. n
    [ [ 1 0 1 1 ] [ 0 0 0 0 ] [ 1 1 0 1 ] ]
    ```
    
 - Append `,`
   ```
   JALA> m=. 3 4 $ 1 2 3
   JALA> m
   [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] ]
   JALA> n=. 4 $ 0 1
   JALA> n
   [ 0 1 0 1 ]
   JALA> m,n
   [ [ 1 2 3 1 ] [ 2 3 1 2 ] [ 3 1 2 3 ] [ 0 1 0 1 ] ]
   ```