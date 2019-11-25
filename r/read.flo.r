#' read.flo
#'
#' @param stn 
#' @param hy 
#' @param ttshome 
#' @param fileLoc 
#'
#' @return
#' @export
#'
#' @examples
read.flo <-
function (stn, hy, ttshome = getTTSenv("TTSHOME"),fileLoc=NULL) 
{
    if(!is.null(fileLoc)){
        abs.path <- fileLoc
        stn <- substr(basename(fileLoc),start = 1,stop = 3)
        hy <-  substr(basename(fileLoc),start = 4,stop = 5)
    } else {
        file <- paste(stn, zfill(hy, 2), ".flo", sep = "")
        abs.path <- paste(ttshome, stn, file, sep = "/")
    }
    columns <- c("year", "mo", "dy", "time", "dump", "bottle", 
        "codes", "rawstg", "stg", "stgcode", "q", "rawturb", 
        "turb", "turbcode")
    if (!file.exists(abs.path)) {
        print(paste(abs.path, "does not exist"))
        return(NULL)
    }
    print(paste("Reading file", abs.path))
    data <- try(read.table(abs.path, strip.white = T, sep = ",")[, 
        1:length(columns)])
    if (inherits(data, "try-error")) 
        return(data)
    names(data) <- columns
    data$chr <- make.chr(data$year, data$mo, data$dy, data$time)
    find.factors(data[, c("chr", "dump", "bottle", "stg", "stgcode", 
        "q", "turb", "turbcode")])
    data <- data[, c("chr", "dump", "bottle", "codes", "stg", 
        "stgcode", "q", "turb", "turbcode")]
    attr(data, "stn") <- stn
    data
}
