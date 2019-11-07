arfcalc <-
function (stg) 
{
    print("Warning from qcalc: ARF rating equation is invalid after HY99")
    a <- -0.2927
    b <- 1.4174
    c <- 81.5627
    c * stg^(b + a * log(stg))
}
