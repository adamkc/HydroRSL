select.flofile <-
function (stn, allfiles) 
{
    regex <- paste(stn, "[0-9][0-9][.]flo", sep = "")
    flofiles <- grep(regex, allfiles, value = T, ignore = T)
    hy <- as.numeric(substring(flofiles, 4, 5))
    flofiles <- flofiles[order(yy2year(hy))]
    cat("\nChoose a FLO file:\n\n")
    n <- length(flofiles)
    if (n == 0) {
        print("No flo files found in station directory")
        return()
    }
    for (i in 1:n) {
        cat(paste(flofiles[i], "->", i, "\n", sep = " "))
    }
    while (1) {
        answer <- readline("\nEnter file number: <CR> = Quit) ")
        if (answer == "") 
            return()
        answer <- as.integer(answer)
        if (!is.na(answer) && answer %in% 1:n) 
            break
    }
    cat("\n")
    flofiles[answer]
}
