addttsMenu <-
function () 
{
    winMenuAdd("TTS")
    winMenuAddItem("TTS", "Set Logger Home Directory", "setLoggerHome()")
    winMenuAddItem("TTS", "Set TTS Home Directory", "setTTSHome()")
    winMenuAddItem("TTS", "Plot Raw Data", "rawplot.gui()")
    winMenuAddItem("TTS", "Plot Appended Data", "floplot.gui()")
    winMenuAddItem("TTS", "Read data and merge", "readAndMerge.gui()")
    winMenuAddItem("TTS", "Create interstorm data", "drop.storms.gui()")
    winMenuAddItem("TTS", "Identify sediment samples for exclusion", 
        "reselData.gui()")
    winMenuAddItem("TTS", "Scatterplot", "scatterPlot.gui()")
    winMenuAddItem("TTS", "Create a regression model for SSC using turbidity or flow", 
        "modelssc.gui()")
    winMenuAddItem("TTS", "Create a composite model for SSC using turbidity or flow", 
        "compmodel.gui()")
    winMenuAddItem("TTS", "Create a model for SSC using time interpolation", 
        "lineartime.gui()")
    winMenuAddItem("TTS", "Plot one or more contiguous models", 
        "ttsplot.gui()")
    winMenuAddItem("TTS", "Stats and total for several models", 
        "total.gui()")
    winMenuAddItem("TTS", "Save results as SSC time series", 
        "tts.ssc.gui()")
}
