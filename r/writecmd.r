writecmd <-
function (file = "", funcname, arglist, result = "", cmd = "") 
{
    if (cmd == "") {
        tmp <- call(funcname, arglist)[2]
        tmp1 <- substring(tmp, 6, nchar(tmp) - 1)
        tmp2 <- gsub("[ ]", "", tmp1)
        cmd <- paste(funcname, "(", tmp2, ")", sep = "")
        if (result != "") 
            cmd <- paste(result, " <- ", cmd, sep = "")
    }
    if (file != "") {
        cat("Command(s) submitted AND written to", file, ":\n")
        write(cmd, file = "")
    }
    else cat("Command(s) submitted to R:\n")
    write(cmd, file = file, append = T)
}
