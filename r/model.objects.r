model.objects <-
function () 
{
    objnames <- objects(envir = .GlobalEnv)
    objtypes <- sapply(objnames, function(x) data.class(eval(as.name(x))))
    lists <- objnames[objtypes == "list"]
    firstcomp <- sapply(lists, function(x) names(eval(as.name(x)))[1])
    lists[firstcomp == "yhat"]
}
