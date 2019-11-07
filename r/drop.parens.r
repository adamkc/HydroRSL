drop.parens <-
function (x) 
{
    x1 <- sub("[(]", "", x)
    sub("[)]", "", x1)
}
