tts.gui <-
function () 
{
    while (1) {
        choice <- tolower(readline("\nPlot raw or appended files? Enter 'r' or 'a': "))
        if (choice %in% c("r", "a")) 
            break
    }
    if (choice == "r") 
        rawplot.gui()
    else floplot.gui()
}
