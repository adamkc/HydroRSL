reselData.simple <-
function (type) 
{
    reselectSamples <- function(panel) {
        with(panel, {
            stn <- tolower(pars[1])
            hy4 <- pars[2]
            hy2 <- substring(hy4, 3, 4)
            setTTSenv(c("STN", "HY"), c(stn, hy4))
            ttshome <- getTTSenv("TTSHOME")
            objname <- paste(stn, hy2, ".sed", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                stop(paste(objname, "not found"))
            seddata <- eval(as.name(objname))
            seddata <- reselect(seddata, "turb", "ssc", type, 
                objname)
            assign(objname, seddata, envir = .GlobalEnv)
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control(paste(type, "samples"), type = type)
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    res1 <- rp.button(panel, action = reselectSamples, title = "OK", 
        pos = "left", quit = TRUE)
    rp.button(panel, action = nothing, title = "Cancel", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}
