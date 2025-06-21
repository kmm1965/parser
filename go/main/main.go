package main

import (
    "fmt"
    c "calculator"
)

func calc(s string){
    fmt.Println(s, "=", c.Calculate(s).Get().Key)
}

func main(){
    calc("72 - 7 - (1 - 2) * 3")                   // 68.0
    calc("-7")                                     // 7.0
    calc("-2^2")                                   // 4.0
    calc(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)")  // -8.889
    calc("3^(1+1)^3")                              // 6561.0
    calc("sin(1+1)")                               // 0.9092974268256816
    calc("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")       // -0.36340857314265324
    calc("sqr(2 + 3)")                             // 25.0
    calc("sin(-PI/4)")                             // -0.7071067811865475
    calc(" E ^ PI")                                // 23.140692632779267
    calc(" PI ^ E")                                // 22.45915771836104
}
