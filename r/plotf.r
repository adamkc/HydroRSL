plotf <-
function (panel) 
{
    with(panel, {
        pars <- as.numeric(pars)
        xgrid <- seq(0.1, max(c(pars[3], 5), na.rm = TRUE), length = 50)
        dgrid <- df(xgrid, pars[1], pars[2])
        plot(xgrid, dgrid, type = "l", col = "blue", lwd = 3)
        if (!is.na(pars[3])) {
            lines(rep(pars[3], 2), c(0, 0.95 * max(dgrid)), lty = 2, 
                col = "red")
            text(pars[3], max(dgrid), as.character(pars[3]), 
                col = "red")
        }
    })
    panel
}
