msm2mt <-
function (msm) 
{
    hr <- msm%/%60
    mn <- msm%%60
    round(100 * hr + mn)
}
