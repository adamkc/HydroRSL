zfill <-
function (x, n) 
{
    nc <- nchar(x)
    zeros <- paste(rep(0, n), collapse = "")
    paste(substring(zeros, nc + 1, n), substring(x, 1, nc), sep = "")
}
