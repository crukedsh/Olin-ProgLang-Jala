# Syntax
Let's learn jala through examples
```
JALA> 3 * 4 - 5         NB. right to left
_3
JALA> 3 +- 4 5          NB. dyadic bounds monatic
[ _1 _2 ]
JALA> m=.2 3 $ 4 5 6    NB. assignment
JALA> $ 2 3 $ 4 5 6
[ 2 3 ]
JALA> (+ +/) 2 3        NB. monatic hook
[ 7 8 ]                 NB. + +/ 2 3   
JALA> 5 (% +/) 2 3      NB. dyadic hook
1                       NB. 5 % +/ 2 3                        
JALA> (+/ % #) 1 2 3    NB. monatic fork: average
2                       NB. (+/ 1 2 3) % (# 1 2 3)
JALA> 5 (+ * -) 4       NB. dyadic fork: squared difference
9                       NB. (5 + 4) * (5 - 4)
JALA> (# @ $) m         NB. combinator: rank
2                       NB. # $ m
JALA> ($ @ |:)m         NB. combinator: shape of transpose 
[ 3 2 ]
JALA> m =. 2 3 $ 0 1
JALA> m
[ [ 0 1 0 ] [ 1 0 1 ] ]
JALA> n=. 0 1
JALA> (m*.-.n)+.((-.m)*.n)  NB. XOR
[ [ 0 1 ] [ 1 0 ] ]
```