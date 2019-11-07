mystamp <-
function (cex = 0.7, string = date()) 
{
    mtext(string, side = 1, line = -1, cex = cex, outer = T, 
        adj = 1)
}
