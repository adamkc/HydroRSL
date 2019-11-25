drop.storms.gui <-
function () 
{
    drop <- function(panel) {
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
            floyy.flo <- paste("flo", hy2, ".flo", sep = "")
            trbyy.flo <- paste("trb", hy2, ".flo", sep = "")
            floyy.sed <- paste("flo", hy2, ".sed", sep = "")
            trbyy.sed <- paste("trb", hy2, ".sed", sep = "")
            rhs1 <- paste("drop.storms(\"", stn, "\",", hy2, 
                ",source=\"", datesource, "\")", sep = "")
            rhs2 <- paste("interstorm.flo[is.na(interstorm.flo$turb),]", 
                sep = "")
            rhs3 <- paste("interstorm.flo[!is.na(interstorm.flo$turb),]", 
                sep = "")
            rhs4 <- paste(stn, hy2, ".sed", sep = "")
            cmd1 <- paste("interstorm.flo", rhs1, sep = " <- ")
            cmd2 <- paste(floyy.flo, rhs2, sep = " <- ")
            cmd3 <- paste(trbyy.flo, rhs3, sep = " <- ")
            cmd4 <- paste(floyy.sed, rhs4, sep = " <- ")
            cmd5 <- paste(trbyy.sed, rhs4, sep = " <- ")
            interstorm.flo <- eval(parse(text = rhs1))
            nmissing <- sum(is.na(interstorm.flo$turb))
            if (nmissing > 0) 
                allcmds <- paste(cmd1, cmd2, cmd3, cmd4, cmd5, 
                  sep = "\n")
            else {
                allcmds <- paste(cmd1, cmd3, cmd5, sep = "\n")
                cat("No missing interstorm turbidity data. Flow-based estimates will be unnecessary.\n")
            }
            saveCommand(stn, hy2, save = savecmd, cmd = allcmds)
            res3 <- eval(parse(text = rhs3))
            res4 <- eval(parse(text = rhs4))
            assign(trbyy.flo, res3, envir = .GlobalEnv)
            assign(trbyy.sed, res4, envir = .GlobalEnv)
            if (nmissing > 0) {
                res2 <- eval(parse(text = rhs2))
                assign(floyy.flo, res2, envir = .GlobalEnv)
                assign(floyy.sed, res4, envir = .GlobalEnv)
            }
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control("Create inter-storm data sets")
    env <- getTTSenv()
    init.stn <- getTTSenv("STN")
    init.hy <- getTTSenv("HY")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    rp.radiogroup(panel, datesource, c("filesystem", "workspace"), 
        title = "Source of storm dates", initval = "workspace", 
        action = nothing)
    rp.checkbox(panel, savecmd, initval = TRUE, label = "Save commands to file", 
        action = nothing)
    res1 <- rp.button(panel, action = drop, title = "OK", pos = "left", 
        quit = FALSE)
    rp.button(panel, action = nothing, title = "Done", pos = "right", 
        quit = TRUE)
    rp.do(panel, nothing)
}
