finney2 <-
function (m, z) 
{
    apply(cbind(m, z), 1, function(row) {
        df <- row[1]
        bt <- (row[2] * (df^2))/(2 * (df + 1))
        p <- 1:100
        series <- bt/((df/2 + p - 1) * p)
        terms <- cumprod(series)
        if (abs(terms[length(terms)] > 1e-07)) 
            print("Finney's gm did not converge")
        return(1 + sum(terms))
    })
}
