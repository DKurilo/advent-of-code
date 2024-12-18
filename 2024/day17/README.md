# day17
program in input
```
bst rA
bxl 2
cdv rB
bxc
bxl 3
out rB
adv 3
jnz 0
```
a bit more readable
```
start:  b = (a `mod` 8) `xor` 2
        c = a `div` (2^rB)
        b = (b `xor` cB) `xor` 3
        print b `mod` 8
        a = a `div` 8
        if a /= 0 go to start
```
