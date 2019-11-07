find.factors <-
function (data) 
{
    modes <- sapply(data, data.class)
    factor.names <- names(modes)[modes == "factor"]
    if (length(factor.names) > 0) {
        cat("Error: character data found in", paste(factor.names, 
            collapse = ", "), "\n\n")
        for (fac in factor.names) {
            list <- find.chardata(data[, fac])
            names(list) <- paste(fac, names(list), sep = ".")
            print(list)
        }
    }
    return()
}
