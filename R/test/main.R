source("../src/calculator.R")

get_val <- function(m){
    (m %>% from_just())[[1]]
}

print(get_val(calculate(" 7.21e-1 - 7.3 - (1.5 - 2.2) * (-3.3)"))) # -8.889
print(get_val(calculate("sin ( 2_SQRTPI * sqr ( 2 ) - 1 )")))      # -0.3634085731426532
print(get_val(calculate("sqr(sin(2)) + sqr(cos(1 + 1))")))         # 1.0
print(get_val(calculate("3 ^ 2 ^ 3")))                             # 6561.0
print(get_val(calculate(" E ^ PI ")))                              # 23.140692632779267
print(get_val(calculate(" PI ^ E ")))                              # 22.45915771836104

