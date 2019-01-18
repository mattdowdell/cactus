# Grammar

## Basic Terms
```bnf
hex digit = dec digit | "a" | "b" | "c" | "d" | "e" | "f" | "A" | "B" | "C" |
            "D" | "E" | "F"
dec digit = oct digit | "8" | "9"
oct digit = bin digit | "2" | "3" | "4" | "5" | "6" | "7"
bin digit = "0" | "1"


letter    = uppercase | lowercase
uppercase = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" |
            "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" |
            "W" | "X" | "Y" | "Z"
lowercase>= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" |
            "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" |
            "w" | "x" | "y" | "z"
```

## LLL
```
ident = ( letter | "_" ), { letter | "_" | dec digit }


```
