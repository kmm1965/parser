package main

import (
    "fmt"
    c "calculator"
)

func main(){
    fmt.Println(c.Calculate("72 - 7 - (1 - 2) * 3").Get().String())                   // 68.0
    fmt.Println(c.Calculate("-7").Get().String())                                     // 7.0
    fmt.Println(c.Calculate("-2^2").Get().String())                                   // 4.0
    fmt.Println(c.Calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)").Get().String())  // -8.889
    fmt.Println(c.Calculate("3^(1+1)^3").Get().String())                              // 6561.0
    fmt.Println(c.Calculate("sin(1+1)").Get().String())                               // 0.9092974268256816
    fmt.Println(c.Calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )").Get().String())       // -0.36340857314265324
    fmt.Println(c.Calculate("sqr(2 + 3)").Get().String())                             // 25.0
    fmt.Println(c.Calculate("sin(-PI/4)").Get().String())                             // -0.7071067811865475
    fmt.Println(c.Calculate(" E ^ PI").Get().String())                                // 23.140692632779267
    fmt.Println(c.Calculate(" PI ^ E").Get().String())                                // 22.45915771836104
}
