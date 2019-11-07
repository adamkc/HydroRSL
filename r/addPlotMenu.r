addPlotMenu <-
function () 
{
    winMenuAdd("TTS Plots")
    winMenuAddItem("TTS Plots", "Set Logger Home Dir (for raw data plots)", 
        "setLoggerHome()")
    winMenuAddItem("TTS Plots", "Set TTS Home Dir (for appended data plots)", 
        "setTTSHome()")
    winMenuAddItem("TTS Plots", "Plot Raw Data", "rawplot.gui()")
    winMenuAddItem("TTS Plots", "Plot Appended Data", "floplot.gui()")
}
