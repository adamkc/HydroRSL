which.na <-
function (x) 
{
    seq(along = x)[is.na(x)]
}
