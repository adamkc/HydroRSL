panelselectStnYear <-
function () 
{
    updatevars <- function(panel) {
        with(panel, {
            stn <<- pars[1]
            hy <<- pars[2]
        })
        panel
    }
    nothing <- function(panel) panel
    panel <- rp.control()
    init.stn <- ifelse(exists("stn", env = .GlobalEnv), stn, 
        "")
    init.hy <- ifelse(exists("hy", env = .GlobalEnv), hy, "")
    my.textentry(panel, pars, action = nothing, labels = c("Station", 
        "Water year"), title = "Select station and year", initval = c(init.stn, 
        init.hy))
    rp.button(panel, action = updatevars, title = "OK", quit = TRUE)
    rp.button(panel, action = nothing, title = "Cancel", quit = TRUE)
    rp.do(panel, updatevars)
}
