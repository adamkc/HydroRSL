listfuncs <-
function (env = .GlobalEnv, data.class = "function", ...) 
{
    objnames <- objects(env)
    dclass <- sapply(objnames, function(x) data.class(eval(as.name(x))))
    objnames[dclass == data.class]
}
