var.check <-
function (tclvar) 
{
    attach(tclvar, warn = F)
    on.exit(detach(tclvar))
    tflag <- T
    tclvalue(sd) <- gsub("-", "/", tclvalue(sd))
    tclvalue(ed) <- gsub("-", "/", tclvalue(ed))
    sd <- try(dates(tclvalue(sd)))
    ed <- try(dates(tclvalue(ed)))
    if (inherits(sd, "try-error")) {
        winDialog(type = c("ok"), "ERROR: Start date is not in the proper format (mm/dd/yy).")
        tflag <- F
    }
    if (inherits(ed, "try-error")) {
        winDialog(type = c("ok"), "ERROR: End date is not in the proper format (mm/dd/yy).")
        tflag <- F
    }
    st <- as.integer(tclvalue(st))
    et <- as.integer(tclvalue(et))
    if (is.integer(st) & !is.na(st)) {
        if (st < 0 | st > 2400) {
            winDialog(type = c("ok"), "ERROR: Start time is required to be between 0 and 2400.")
            tflag <- F
        }
    }
    else {
        winDialog(type = c("ok"), "ERROR: Start time must be an integer value.")
        tflag <- F
    }
    if (is.integer(et) & !is.na(et)) {
        if (et < 0 | et > 2400) {
            winDialog(type = c("ok"), "ERROR: End time is required to be between 0 and 2400.")
            tflag <- F
        }
    }
    else {
        winDialog(type = c("ok"), "ERROR: End time must be an integer value.")
        tflag <- F
    }
    if (tflag != F) {
        schron <- sd + mt2msm(st)/1440
        echron <- ed + mt2msm(et)/1440
        if (schron > echron) {
            winDialog(type = c("ok"), "ERROR: Start date-time is greater than end date-time.")
            tflag <- F
        }
    }
    min1 <- as.numeric(tclvalue(min1))
    max1 <- as.numeric(tclvalue(max1))
    if (is.na(min1)) {
        winDialog(type = c("ok"), "ERROR: The left axis minimum stage must be an numeric value.")
        tflag <- F
    }
    if (is.na(max1)) {
        winDialog(type = c("ok"), "ERROR: The left axis maximum stage must be an numeric value.")
        tflag <- F
    }
    if (!is.na(min1) & !is.na(max1)) {
        if (min1 > max1) {
            winDialog(type = c("ok"), "ERROR: The left axis minimum stage is\ngreater than the left axis maximum stage.")
            tflag <- F
        }
    }
    if (tclvalue(pright) == 1) {
        min2 <- as.numeric(tclvalue(min2))
        max2 <- as.numeric(tclvalue(max2))
        if (is.na(min2)) {
            winDialog(type = c("ok"), "ERROR: The right axis minimum stage must be an numeric value.")
            tflag <- F
        }
        if (is.na(max2)) {
            winDialog(type = c("ok"), "ERROR: The right axis maximum stage must be an numeric value.")
            tflag <- F
        }
        if (!is.na(min2) & !is.na(max2)) {
            if (min2 > max2) {
                winDialog(type = c("ok"), "ERROR: The right axis minimum stage is\ngreater than the right axis maximum stage.")
                tflag <- F
            }
        }
    }
    tflag
}
