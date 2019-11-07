allmatches <-
function (x, y) 
{
    (1:length(y))[!is.na(match(y, x))]
}
