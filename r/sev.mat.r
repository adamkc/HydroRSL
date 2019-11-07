sev.mat <-
function (matrix, a, b, c) 
{
    sevmat <- matrix
    ssc <- as.numeric(dimnames(matrix)[[2]])
    for (i in 1:length(ssc)) {
        sevmat[, i] <- sev(ssc[i], matrix[, i], a, b, c)
    }
    sevmat
}
