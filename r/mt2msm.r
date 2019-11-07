mt2msm <-
function (time) 
{
    if (data.class(time) == "character") 
        time <- as.numeric(time)
    min <- time%%100
    hour <- time%/%100
    60 * hour + min
}
