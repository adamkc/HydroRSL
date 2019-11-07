getTTSenv <-
function (varname) 
{
    if (!file.exists("TTSenvironment.txt")) 
        return(data.frame(row.names = c("TTSHOME", "LOGGERHOME"), 
            value = c(NA, NA), stringsAsFactors = F))
    data <- read.table("TTSenvironment.txt", sep = "=", row.names = 1, 
        as.is = T, header = F, col.names = c("name", "value"))
    if (missing(varname)) 
        data
    else data[varname, "value"]
}
