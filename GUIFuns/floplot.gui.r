floplot.gui <-
function (path = getTTSenv("TTSHOME")) 
{
    require(tcltk) || stop("tcltk support is absent")
    options(warn = -1)
    require(chron) || stop("chron support is absent")
    options(warn = 0)
    cat("\n\nWelcome to TTS FLO PLOT...\n")
    while (1) {
        stn.info <- select.station(path)
        if (is.null(stn.info)) 
            return(invisible())
        stn <- stn.info$stn
        if (stn == "") 
            break
        filepath <- paste(path, stn, sep = "/")
        files <- list.files(filepath)
        flofile <- select.flofile(stn, files)
        if (is.null(flofile)) 
            next
        flopath <- paste(filepath, flofile, sep = "/")
        flodata <- read.flofile(flopath, stn.info$extravars)
        attr(flodata, "stn") <- stn
        vars <- names(flodata)
        plot <- function() {
            if (tclvalue(choice) != "none") {
                tflag <- var.check(tclvar)
                if (tflag == T) {
                  schron <- chron(tclvalue(sd), mt2msm(as.numeric(tclvalue(st)))/1440)
                  echron <- chron(tclvalue(ed), mt2msm(as.numeric(tclvalue(et)))/1440)
                  if (tclvalue(choice) == "stage") 
                    flodata$left <- flodata$stg
                  if (tclvalue(choice) == "discharge") 
                    flodata$left <- flodata$q
                  if (tclvalue(choice) == "turbidity") 
                    flodata$left <- flodata$turb
                  if (tclvalue(choice) == "stage-turbidity") {
                    flodata$left <- flodata$stg
                    flodata$right <- flodata$turb
                  }
                  if (tclvalue(choice) == "discharge-turbidity") {
                    flodata$left <- flodata$q
                    flodata$right <- flodata$turb
                  }
                  if (tclvalue(choice) == "rainfall") 
                    flodata$left <- flodata$rain
                  if (tclvalue(choice) == "discharge-rainfall") {
                    flodata$left <- flodata$q
                    flodata$right <- flodata$rain
                  }
                  if (tclvalue(choice) == "water temperature") 
                    flodata$left <- flodata$wtemp
                  if (tclvalue(choice) == "air temperature") 
                    flodata$left <- flodata$atemp
                  if (tclvalue(choice) == "water-air temperature") {
                    flodata$left <- flodata$wtemp
                    flodata$right <- flodata$atemp
                  }
                  if (tclvalue(choice) == "discharge-water temperature") {
                    flodata$left <- flodata$q
                    flodata$right <- flodata$wtemp
                  }
                  if (tclvalue(choice) == "throughfall") 
                    flodata$left <- flodata$lc1
                  if (tclvalue(choice) == "wind speed") 
                    flodata$left <- flodata$wind
                  flodata <- as.data.frame(flodata)
                  flodata <- flodata[flodata$chron >= schron & 
                    flodata$chron <= echron, ]
                  tts.rawplot(flodata, tclvar, schron, echron, 
                    stn.info$minstg)
                  tkraise(gui)
                }
            }
            else {
                winDialog(type = c("ok"), "ERROR: You must first choose the type of data you wish to\nplot from the Data Type pull down menu.")
            }
        }
        def.time <- function() {
            if (tclvalue(sd) == "m/d/y") {
                tclvalue(sd) <- format(dates(flodata$chron[1]))
                tclvalue(st) <- flodata$time[1]
            }
            else {
                check.sd <- try(dates(tclvalue(sd)))
                if (inherits(check.sd, "try-error")) {
                  tclvalue(sd) <- format(dates(flodata$chron[1]))
                  tclvalue(st) <- flodata$time[1]
                  winDialog(type = c("ok"), "ERROR: Defaulted to flodatas start date\ndue to an incorrect date field (mm/dd/yy).")
                }
            }
            if (tclvalue(ed) == "m/d/y") {
                tclvalue(ed) <- format(dates(flodata$chron[length(flodata$chron)]))
                tclvalue(et) <- flodata$time[length(flodata$time)]
            }
            else {
                check.ed <- try(dates(tclvalue(ed)))
                if (inherits(check.ed, "try-error")) {
                  tclvalue(ed) <- format(dates(flodata$chron[length(flodata$chron)]))
                  tclvalue(et) <- flodata$time[length(flodata$time)]
                  winDialog(type = c("ok"), "ERROR: Defaulted to flodatas end date\ndue to an incorrect date field (mm/dd/yy).")
                }
            }
        }
        cmd1 <- function() {
            time <- def.time()
            tclvalue(choice) <- "stage"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$stg, na.rm = T)
            tclvalue(max1) <- max(flodata$stg, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd1q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd2 <- function() {
            time <- def.time()
            tclvalue(choice) <- "turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$turb, na.rm = T)
            tclvalue(max1) <- max(flodata$turb, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd3 <- function() {
            time <- def.time()
            tclvalue(choice) <- "stage-turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$stg, na.rm = T)
            tclvalue(max1) <- max(flodata$stg, na.rm = T)
            tclvalue(min2) <- min(flodata$turb, na.rm = T)
            tclvalue(max2) <- max(flodata$turb, na.rm = T)
        }
        cmd3q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge-turbidity"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- min(flodata$turb, na.rm = T)
            tclvalue(max2) <- max(flodata$turb, na.rm = T)
        }
        cmd4 <- function() {
            time <- def.time()
            tclvalue(choice) <- "rainfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- 0
            tclvalue(max1) <- sum(flodata$rain, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd4q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge-rainfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- 0
            tclvalue(max2) <- sum(flodata$rain, na.rm = T)
        }
        cmd5 <- function() {
            time <- def.time()
            tclvalue(choice) <- "water temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$wtemp, na.rm = T)
            tclvalue(max1) <- max(flodata$wtemp, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd5q <- function() {
            time <- def.time()
            tclvalue(choice) <- "discharge-water temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$q, na.rm = T)
            tclvalue(max1) <- max(flodata$q, na.rm = T)
            tclvalue(min2) <- min(flodata$wtemp, na.rm = T)
            tclvalue(max2) <- max(flodata$wtemp, na.rm = T)
        }
        cmd6 <- function() {
            time <- def.time()
            tclvalue(choice) <- "air temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$atemp, na.rm = T)
            tclvalue(max1) <- max(flodata$atemp, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd7 <- function() {
            time <- def.time()
            tclvalue(choice) <- "water-air temperature"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 1
            tclvalue(min1) <- min(flodata$wtemp, na.rm = T)
            tclvalue(max1) <- max(flodata$wtemp, na.rm = T)
            tclvalue(min2) <- min(flodata$atemp, na.rm = T)
            tclvalue(max2) <- max(flodata$atemp, na.rm = T)
        }
        cmd8 <- function() {
            time <- def.time()
            tclvalue(choice) <- "throughfall"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$lc1, na.rm = T)
            tclvalue(max1) <- max(flodata$lc7, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        cmd9 <- function() {
            time <- def.time()
            tclvalue(choice) <- "wind speed"
            tclvalue(pleft) <- 1
            tclvalue(pright) <- 0
            tclvalue(min1) <- min(flodata$wind, na.rm = T)
            tclvalue(max1) <- max(flodata$wind, na.rm = T)
            tclvalue(min2) <- "NA"
            tclvalue(max2) <- "NA"
        }
        gui <- tktoplevel()
        tkwm.title(gui, "TTS DATA PLOT")
        labpath <- tklabel(gui, text = "Path name:")
        pathname <- tclVar(flopath)
        entpath <- tkentry(gui, textvariable = as.character(pathname), 
            justify = "center", background = "grey", state = "disabled")
        tkpack(labpath, anchor = "w")
        tkpack(entpath, fill = "both", padx = 2)
        tkpack(mb <- tkmenubutton(gui, text = "Data Type", underline = "0", 
            relief = "raised"), fill = "both", pady = 4, padx = 2)
        mb.menu <- tkmenu(mb)
        tkconfigure(mb, menu = paste(tcl("winfo", "parent", mb.menu), 
            ".", tcl("winfo", "name", mb.menu), sep = ""))
        if (any(vars == "stg")) 
            tkadd(mb.menu, "command", label = "stage", command = cmd1)
        if (any(vars == "q")) 
            tkadd(mb.menu, "command", label = "discharge", command = cmd1q)
        if (any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "turbidity", command = cmd2)
        if (any(vars == "stg") & any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "stage-turbidity", 
                command = cmd3)
        if (any(vars == "q") & any(vars == "turb")) 
            tkadd(mb.menu, "command", label = "discharge-turbidity", 
                command = cmd3q)
        if (any(vars == "rain")) 
            tkadd(mb.menu, "command", label = "rainfall", command = cmd4)
        if (any(vars == "q") & any(vars == "rain")) 
            tkadd(mb.menu, "command", label = "discharge-rainfall", 
                command = cmd4q)
        if (any(vars == "wtemp")) 
            tkadd(mb.menu, "command", label = "water temperature", 
                command = cmd5)
        if (any(vars == "q") & any(vars == "wtemp")) 
            tkadd(mb.menu, "command", label = "discharge-water temperature", 
                command = cmd5q)
        if (any(vars == "atemp")) 
            tkadd(mb.menu, "command", label = "air temperature", 
                command = cmd6)
        if (any(vars == "wtemp") & any(vars == "atemp")) 
            tkadd(mb.menu, "command", label = "water-air temperature", 
                command = cmd7)
        if (any(vars == "lc1") | any(vars == "lc2") | any(vars == 
            "lc3") | any(vars == "lc4") | any(vars == "lc5") | 
            any(vars == "lc6") | any(vars == "lc7")) 
            tkadd(mb.menu, "command", label = "throughfall", 
                command = cmd8)
        if (any(vars == "wind")) 
            tkadd(mb.menu, "command", label = "wind speed", command = cmd9)
        choice <- tclVar("none")
        entchoice <- tkentry(gui, textvariable = as.character(choice), 
            justify = "center", background = "grey", state = "disabled")
        tkpack(entchoice, fill = "both", padx = 2)
        big.frm <- tkframe(gui)
        datl.frm <- tkframe(big.frm)
        datr.frm <- tkframe(big.frm)
        labsd <- tklabel(datl.frm, text = "Start Date")
        sd <- tclVar("m/d/y")
        entsd <- tkentry(datl.frm, textvariable = as.character(sd), 
            width = 10, justify = "center", background = "white")
        tkpack(labsd, entsd)
        labst <- tklabel(datr.frm, text = "Start Time")
        st <- tclVar(0)
        entst <- tkentry(datr.frm, textvariable = as.character(st), 
            width = 10, justify = "center", background = "white")
        tkpack(labst, entst)
        labed <- tklabel(datl.frm, text = "End Date")
        ed <- tclVar("m/d/y")
        ented <- tkentry(datl.frm, textvariable = as.character(ed), 
            width = 10, justify = "center", background = "white")
        tkpack(labed, ented)
        labet <- tklabel(datr.frm, text = "End Time")
        et <- tclVar(2400)
        entet <- tkentry(datr.frm, textvariable = as.character(et), 
            width = 10, justify = "center", background = "white")
        tkpack(labet, entet)
        tkpack(datl.frm, datr.frm, side = "left")
        tkpack(big.frm, pady = 2, padx = 2)
        pleft <- tclVar(0)
        tkpack(tkcheckbutton(gui, text = "Left Axis", variable = as.character(pleft), 
            state = "disabled"), anchor = "w", padx = 1)
        min1.frm <- tkframe(gui)
        labmin1 <- tklabel(min1.frm, text = "axis min", anchor = "w")
        min1 <- tclVar("NA")
        entmin1 <- tkentry(min1.frm, textvariable = as.character(min1), 
            width = 6, justify = "center", background = "white")
        tkpack(entmin1, labmin1, side = "left")
        tkpack(min1.frm, anchor = "w", padx = 2)
        max1.frm <- tkframe(gui)
        labmax1 <- tklabel(max1.frm, text = "axis max", anchor = "w")
        max1 <- tclVar("NA")
        entmax1 <- tkentry(max1.frm, textvariable = as.character(max1), 
            width = 6, justify = "center", background = "white")
        tkpack(entmax1, labmax1, side = "left")
        tkpack(max1.frm, anchor = "w", padx = 2)
        pright <- tclVar(0)
        tkpack(tkcheckbutton(gui, text = "Right Axis", variable = as.character(pright), 
            state = "disabled"), anchor = "w", padx = 1)
        min2.frm <- tkframe(gui)
        labmin2 <- tklabel(min2.frm, text = "axis min")
        min2 <- tclVar("NA")
        entmin2 <- tkentry(min2.frm, textvariable = as.character(min2), 
            width = 6, justify = "center", background = "white")
        tkpack(entmin2, labmin2, side = "left")
        tkpack(min2.frm, anchor = "w", padx = 2)
        max2.frm <- tkframe(gui)
        labmax2 <- tklabel(max2.frm, text = "axis max")
        max2 <- tclVar("NA")
        entmax2 <- tkentry(max2.frm, textvariable = as.character(max2), 
            width = 6, justify = "center", background = "white")
        tkpack(entmax2, labmax2, side = "left")
        tkpack(max2.frm, anchor = "w", padx = 2)
        tclvar <- list(choice = choice, sd = sd, ed = ed, st = st, 
            et = et, min1 = min1, max1 = max1, min2 = min2, max2 = max2, 
            pright = pright, pleft = pleft)
        done <- tclVar(0)
        but.frm <- tkframe(gui)
        plot.but <- tkbutton(but.frm, text = "PLOT", command = plot)
        quit.but <- tkbutton(but.frm, text = "QUIT", command = function() tclvalue(done) <- 1)
        tkpack(plot.but, quit.but, fill = "both")
        tkpack(but.frm, fill = "both", pady = 4, padx = 2)
        tkraise(gui)
        tkbind(gui, "<Destroy>", function() tclvalue(done) <- 2)
        tkwait.variable(as.character(done))
        if (tclvalue(done) == "2") {
            print("GUI aborted")
            return(invisible())
        }
        if (!is.null(dev.list())) {
            dev.off()
        }
        tkdestroy(gui)
    }
}
