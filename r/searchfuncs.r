searchfuncs <-
function (string, names) 
{
    if (missing(names)) 
        names <- listfuncs()
    funcstrings <- sapply(names, function(x) paste(deparse(eval(as.name(x))), 
        collapse = " "))
    names[grep(string, funcstrings)]
}
