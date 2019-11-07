albcalc <-
function (stg) 
{
    ifelse(stg >= 0.4, 6.337 * (stg - 0.4)^2.367, 0)
}
