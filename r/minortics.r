minortics <-
function (side) 
{
    if (side%%2 == 0) 
        axp <- par()$yaxp
    else axp <- par()$xaxp
    axmin <- axp[1]
    axmax <- axp[2]
    nints <- axp[3]
    unit <- (axmax - axmin)/nints
    axmax <- axmax + unit
    axmin <- axmin - unit
    nints <- nints + 2
    type <- round(unit/10^floor(log10(unit) + .Machine$double.eps))
    tinytics <- seq(axmin, axmax, length = 10 * nints + 1)
    if (type == 1 || type == 2) 
        midtics <- seq(axmin, axmax, length = 2 * nints + 1)
    else if (type == 5) 
        midtics <- seq(axmin, axmax, length = 5 * nints + 1)
    else return("Unexpected tick interval.  No minor ticks plotted.")
    axis(side, at = midtics, label = F, tck = -0.01)
    axis(side, at = tinytics, label = F, tck = -0.005)
}
