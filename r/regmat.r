regmat <-
function (formula, xpro = 0.78, ypro = 0.92, ...) 
{
    splom(formula, pscales = 0, panel = function(x, y) {
        panel.xyplot(x, y)
        panel.lmline(x, y)
        fit <- lm(y ~ x)
        r2 <- round(summary(fit)$r.squared, 3)
        s <- round(summary(fit)$sigma, 3)
        expr1 <- substitute(paste(r^2, "=", r2), list(r2 = r2))
        expr2 <- substitute(paste("s =", s), list(s = s))
        cpl <- current.panel.limits()
        x1 <- xpro * cpl$xlim[1] + (1 - xpro) * cpl$xlim[2]
        y1 <- ypro * cpl$ylim[2] + (1 - ypro) * cpl$ylim[1]
        y2 <- (ypro - 0.1) * cpl$ylim[2] + (1.1 - ypro) * cpl$ylim[1]
        panel.text(x1, y1, expr1, cex = 0.75)
        panel.text(x1, y2, expr2, cex = 0.75)
    }, ...)
}
