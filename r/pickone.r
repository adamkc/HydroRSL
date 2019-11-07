pickone <-
function (values, whatitis = "value") 
{
    while (TRUE) {
        cat("Choose a", whatitis, "from the following list:\n")
        for (i in seq(along = values)) {
            cat(i, ":", values[i], "\n")
        }
        choice <- readline("Enter the number corresponding to your choice: ")
        if (choice %in% seq(along = values)) 
            break
    }
    as.numeric(choice)
}
