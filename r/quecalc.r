quecalc <-
function (stg) 
{
    print("Warning from qcalc: QUE rating equation is approximate")
    a1 <- 5.99432
    b1 <- -36.5344
    c1 <- 56.6576
    a2 <- -0.2925
    b2 <- 1.2087
    c2 <- 0
    ifelse(stg > 0.33, a1 + b1 * stg + c1 * stg^2, a2 + b2 * 
        stg + c2 * stg^2)
}
