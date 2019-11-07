check.interstorm <-
function (stn, hy, interstorm, surrogate, checkflo = T, checksed = T) 
{
    if (interstorm) {
        if (surrogate == "turbidity") 
            sta <- "trb"
        else sta <- "flo"
        if (checksed) {
            objname <- paste(sta, hy, ".sed", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                cat("ERROR:", objname, "not found\n")
            sedobj <- eval(as.name(objname))
            if (attr(sedobj, "stn") != stn) 
                cat("ERROR: The interstorm sed data is from station", 
                  attr(sedobj, "stn"), ", not", stn, "\n")
        }
        if (checkflo) {
            objname <- paste(sta, hy, ".flo", sep = "")
            if (!exists(objname, env = .GlobalEnv)) 
                cat("ERROR:", objname, "not found\n")
            floobj <- eval(as.name(objname))
            if (attr(floobj, "stn") != stn) 
                cat("ERROR: The interstorm flo data is from station", 
                  attr(sedobj, "stn"), ", not", stn, "\n")
        }
    }
    else sta <- stn
    return(sta)
}
