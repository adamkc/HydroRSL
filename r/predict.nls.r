predict.nls <-
function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
    interval = c("none", "confidence", "prediction"), level = 0.95, 
    ...) 
{
    if (missing(newdata)) 
        return(as.vector(fitted(object)))
    object$m$predict(newdata)
}
