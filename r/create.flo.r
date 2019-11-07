create.flo <-
function (data, filepath, extras, write = T) 
{
    year <- as.numeric(as.character(years(data$chr)))
    mo <- zfill(as.numeric(months(data$chr)), 2)
    dy <- zfill(as.numeric(days(data$chr)), 2)
    mt <- zfill(msm2mt(round(1440 * (as.numeric(data$chr)%%1))), 
        4)
    dump <- 1
    bot <- 0
    code <- "BX"
    stg <- 0
    corstg <- 0
    stgcode <- 1
    q <- data$q
    turb <- data$turb
    corturb <- data$corturb
    df <- data.frame(year, mo, dy, mt, dmp = 1, bot = 0, code = "BX", 
        rawstg = data$q, corstg = data$q, stgcode = 1, q = data$q, 
        rawturb = data$turb, turb = data$turb, turbcode = -1)
    if (!missing(extras)) {
        for (var in extras) df[, var] <- data[, var]
    }
    if (write) 
        write.table(df, filepath, quote = FALSE, row.names = F, 
            col.names = F, sep = ",")
    else return(df)
}
