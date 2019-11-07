stgcalc <-
function (q0, lower, upper, stn, hy) 
{
    qcalc2 <- function(stg, stn, hy, q) {
        qcalc(stn, stg, hy) - q
    }
    options(show.error.messages = FALSE)
    result <- try(uniroot(f = qcalc2, interval = c(lower, upper), 
        tol = 2e-04, stn = stn, hy = hy, q = q0)$root)
    options(show.error.messages = TRUE)
    if (inherits(result, "try-error")) 
        return(NA)
    else return(result)
}
