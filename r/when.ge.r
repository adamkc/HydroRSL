when.ge <-
function (tts, lim = 0) 
{
    if (tts$xlog) {
        ssc <- tts$coef[1] + tts$coef[2] * log(tts$turb)
    }
    else {
        ssc <- tts$coef[1] + tts$coef[2] * tts$turb
    }
    range(tts$chr[ssc >= lim])
}
