density.draw <-
function (panel) 
{
    plot(density(panel$x, bw = panel$h))
    panel
}
