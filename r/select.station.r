select.station <-
function (path) 
{
    if (!file.exists(path)) 
        stop(paste(path, "not found.\n", "To change the location of station directories do: fix(floplot.gui)\n", 
            "The default TTS home location is specified at line 1 of floplot.gui."))
    folders <- list.files(path)
    stationdirs <- folders[nchar(folders) == 3]
    n <- length(stationdirs)
    if (n == 0) 
        stop("No station directories found\n")
    cat("Station directories are:")
    for (i in 1:n) {
        if ((i%%16) == 1) 
            cat("\n")
        cat(stationdirs[i], " ")
    }
    while (1) {
        cat("\n")
        stn <- readline("Enter station name (<CR> = Quit): ")
        stn <- tolower(stn)
        if (stn == "") 
            return(NULL)
        if (stn %in% tolower(stationdirs)) 
            break
        else cat("Station directory not found under ", path, 
            "\n")
    }
    extravars <- character(0)
    minstg <- NA
    if (!file.exists("labels.txt")) {
        cat("No labels.txt found in ", getwd(), "\n")
        cat("You will not be able to plot temperature or rainfall\n")
    }
    else {
        labels <- scan("labels.txt", what = "")
        data <- strsplit(labels, ",")
        stations <- tolower(sapply(data, function(x) x[2]))
        if (stn %in% stations) {
            names(data) <- stations
            selected.data <- data[[stn]]
            array.id <- as.numeric(selected.data[1])
            if (array.id == 0) 
                extravars <- selected.data[-(1:12)]
            else extravars <- selected.data[-(1:14)]
            minstg <- as.numeric(selected.data[4])
        }
    }
    cat("\n")
    list(stn = stn, extravars = extravars, minstg = minstg)
}
