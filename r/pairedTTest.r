pairedTTest <-
function () 
{
    initializeDialog(title = gettextRcmdr("Paired t-Test"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title = gettextRcmdr("First variable (pick one)"))
    yBox <- variableListBox(top, .numeric, title = gettextRcmdr("Second variable (pick one)"))
    onOK <- function() {
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall = pairedTTest, message = gettextRcmdr("You must select two variables."))
            return()
        }
        if (x == y) {
            errorCondition(recall = pairedTTest, message = gettextRcmdr("Variables must be different."))
            return()
        }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x, 
            ", ", .activeDataSet, "$", y, ", alternative='", 
            alternative, "', conf.level=", level, ", paired=TRUE)", 
            sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "t.test")
    radioButtons(top, name = "alternative", buttons = c("twosided", 
        "less", "greater"), values = c("two.sided", "less", "greater"), 
        labels = gettextRcmdr(c("Two-sided", "Difference < 0", 
            "Difference > 0")), title = gettextRcmdr("Alternative Hypothesis"))
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- tkentry(confidenceFrame, width = "6", 
        textvariable = confidenceLevel)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky = "nw")
    tkgrid(tklabel(confidenceFrame, text = gettextRcmdr("Confidence Level"), 
        fg = "blue"))
    tkgrid(confidenceField, sticky = "w")
    tkgrid(alternativeFrame, confidenceFrame, sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix(rows = 3, columns = 2)
}
