readAndMerge.gui <-
function () 
{
    readMerge <- function(panel) {
        with(panel, {
            stn <- tolower(pars[1])
            hy4 <- pars[2]
            if (nchar(hy4) < 3) {
                if (nchar(hy4) == 2) 
                  hy4 <- paste("20", hy4, sep = "")
                if (nchar(hy4) == 1) 
                  hy4 <- paste("200", hy4, sep = "")
            }
            hy2 <- substring(hy4, 3, 4)
            setTTSenv(c("STN", "HY"), c(stn, hy4))
            ttshome <- getTTSenv("TTSHOME")
            cmd <- paste("read.flo(\"", stn, "\",", hy2, ")\n", 
                "read.lab(\"", stn, "\",", hy2, ")\n", "merge.flo(\"", 
                stn, "\",", hy2, ")\n", "mismatches(\"", stn, 
                "\",", hy2, ")\n", sep = "")
            saveCommand(stn, hy2, save = savecmd, cmd = cmd)
            flodata <- read.flo(stn, hy2, ttshome)
            if (is.data.frame(flodata)) {
                objname <- paste(stn, hy2, ".flo", sep = "")
                assign(objname, flodata, envir = .GlobalEnv)
                print(paste("Data imported to object", objname))
            }
            labdata <- read.lab(stn, hy2, ttshome)
            if (is.data.frame(labdata)) {
                objname <- paste(stn, hy2, ".lab", sep = "")
                assign(objname, labdata, envir = .GlobalEnv)
                print(paste("Data imported to object", objname))
            }
            seddata <- merge.flo(stn, hy2)
            objname <- paste(stn, hy2, ".sed", sep = "")
            assign(objname, seddata, envir = .GlobalEnv)
            print(paste("Flo and lab data merged to object", 
                objname))
            print(mismatches(stn, hy2))
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control("Read and Merge Data")
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save command to file", 
        action = nothing)
    res1 <- rp.button(panel, action = readMerge, title = "OK", 
        pos = "left", quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}
