aic.loess <-
function (model) 
{
    call <- model$call
    formula <- call$formula
    span <- model$pars$span
    degree <- model$pars$degree
    if (length(formula) != 3) 
        stop("This only works for simple y ~ x models")
    y <- eval(formula[[2]])
    x <- eval(formula[[3]])
    aicc <- crit.loess(span, x, y, degree)
    return(model$n * (aicc - 1))
}
