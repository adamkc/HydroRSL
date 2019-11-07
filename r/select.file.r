select.file <-
function (pathname = getTTSenv("LOGGERHOME")) 
{
    cat("\nWelcome to TTS RAWPLOT...\n")
    if (file.exists(pathname)) {
        files <- list.files(path = pathname, pattern = "[dD][aA][tT]$", 
            all.files = FALSE, full.names = FALSE)
        if (length(files) == 0) 
            stop(paste("No data files found in", pathname))
    }
    else if (pathname == "") {
        stop("Please set the data logger Home Dir from the TTS menu\n")
    }
    else {
        stop(paste("Data repository", pathname, "not found\n", 
            "Please reset the data logger Home Dir from the TTS menu\n"))
    }
    cat(paste("\nThe following files, found within the ", pathname, 
        " directory,\nare available for plotting:\n\n", sep = ""))
    for (i in seq(along = files)) {
        cat(paste(files[i], "->", i, "\n", sep = " "))
    }
    answer <- readline("\nFile selection? (<CR> = Quit): ")
    if (answer == "") 
        return("")
    file <- paste(pathname, files[as.integer(answer)], sep = "/")
    cat("\n")
    file
}
