padstring <-
function (string, len, char = " ", right = T, chop = T) 
{
    length <- nchar(string)
    if (right) 
        chopstring <- substring(string, 1, len)
    else chopstring <- substring(string, length - len + 1, length)
    if (chop) {
        string <- chopstring
    }
    diff <- len - nchar(chopstring)
    addstring <- sapply(diff, function(n) paste(rep(char, n), 
        collapse = ""))
    if (right) 
        paste(string, addstring, sep = "")
    else paste(addstring, string, sep = "")
}
