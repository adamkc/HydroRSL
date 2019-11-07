read.raw <-
function (filepath) 
{
    filetype <- scan(filepath, what = "", n = 1)
    if (filetype %in% c("TOACI1", "TOA5")) 
        data <- read.crbasic(filepath)
    else data <- read.mixed(filepath)
}
