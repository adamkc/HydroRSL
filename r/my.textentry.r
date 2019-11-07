my.textentry <-
function (panel, var, action = I, labels = NA, names = labels, 
    title = NA, initval = NA, parent = window, pos = NULL, signal = "<KeyRelease>", 
    ...) 
{
    ischar <- is.character(panel)
    if (ischar) {
        panelname <- panel
        panel <- .geval(panel)
    }
    else {
        panelname <- panel$intname
        panelreturn <- deparse(substitute(panel))
        .gassign(panel, panelname)
    }
    pos = .newpos(pos, ...)
    if (.checklayout(pos)) {
        varname <- deparse(substitute(var))
        if ((varname %in% names(panel)) && (all(is.na(initval)))) {
            initval <- .geval(panelname, "$", varname)
        }
        if ((length(initval) == 1) && (is.na(initval))) {
            if ((length(labels) == 1) && (is.na(labels))) {
                nboxes <- 1
                if (is.na(title)) 
                  title <- varname
                labels <- varname
            }
            else {
                nboxes <- length(labels)
                if (is.na(title) & (nboxes == 1)) 
                  title <- labels
            }
            initval <- rep(NA, nboxes)
        }
        else {
            nboxes <- length(initval)
            if ((length(labels) == 1) && (is.na(labels))) 
                if (nboxes != 1) {
                  labels <- paste(varname, 1:nboxes, sep = "")
                }
                else {
                  labels <- varname
                }
            else if (length(labels) != nboxes) 
                stop("lengths of labels and initval do not match.")
        }
        if ((nboxes == 1) & (!is.na(title))) 
            labels <- title
        .geval(panelname, "$", varname, " <- vector(length=", 
            nboxes, ")")
        if (nboxes > 1) 
            if (is.na(title)) 
                title <- varname
        if ((!is.list(pos)) || (is.null(pos$grid))) {
            gd = panel$window
        }
        else {
            gd = .geval(panelname, "$", pos$grid)
        }
        if (nboxes > 1) {
            frame <- tkwidget(gd, "labelframe", text = title, 
                padx = 2, pady = 2)
        }
        else {
            frame <- tkframe(gd)
        }
        if ((!is.list(pos)) || ((is.null(pos$row)) && (is.null(pos$column)))) {
            .rp.layout(frame, pos)
        }
        else {
            if (is.null(pos$sticky)) {
                pos$sticky <- "w"
            }
            if (is.null(pos$rowspan)) {
                pos$rowspan = 1
            }
            if (is.null(pos$columnspan)) {
                pos$columnspan = 1
            }
            tkgrid(frame, row = pos$row, column = pos$column, 
                sticky = pos$sticky, `in` = gd, rowspan = pos$rowspan, 
                columnspan = pos$columnspan)
        }
        for (i in 1:nboxes) {
            if (is.na(initval[i])) 
                initval[i] <- "NA"
            inittclvalue <- .rp.initialise(panelname, paste(varname, 
                i, sep = ""), initval[i])
            tclvariable <- .geval(panelname, "$", varname, i, 
                ".tcl <- tclVar(", deparse(inittclvalue), ")")
            if (is.numeric(inittclvalue)) {
                .geval(panelname, "$", varname, "[", i, "] <- deparse(", 
                  inittclvalue, ")")
            }
            else {
                .geval(panelname, "$", varname, "[", i, "] <- '", 
                  inittclvalue, "'")
            }
            if (!any(is.na(names))) {
                .geval("names(", panelname, "$", varname, ")[", 
                  i, "] <- '", names[i], "'")
            }
            f <- function() {
                for (i in 1:nboxes) {
                  .geval(panelname, "$", varname, "[", i, "] <- tclvalue(", 
                    panelname, "$", varname, i, ".tcl)")
                  if (!any(is.na(names))) {
                    .geval("names(", panelname, "$", varname, 
                      ")[", i, "] <- '", names[i], "'")
                  }
                }
                panel <- action(.geval(panelname))
                if (!is.null(panel$intname)) {
                  .gassign(panel, panelname)
                }
                else {
                  stop("The panel was not passed back from the action function.")
                }
            }
            if (!any(is.na(names))) {
                .geval("names(", panelname, "$", varname, ")[", 
                  i, "] <- '", names[i], "'")
            }
            label <- tklabel(frame, text = labels[i], height = "1")
            if ((!is.list(pos)) || ((is.null(pos$width)) && (is.null(pos$height)))) {
                entry <- tkentry(frame, textvariable = tclvariable)
            }
            else {
                entry <- tkentry(frame, textvariable = tclvariable, 
                  width = pos$width)
            }
            tkgrid(label, entry)
            tkgrid.configure(label, sticky = "w")
            tkgrid.configure(entry, sticky = "e")
            tkbind(entry, signal, f)
        }
    }
    if (ischar) 
        invisible(panelname)
    else assign(panelreturn, .geval(panelname), envir = parent.frame())
}
