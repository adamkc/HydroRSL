endpoints <-
function (object, all = F) 
{
    ssc <- object$predssc
    c(start = ssc[1], end = last.val(ssc))
}
