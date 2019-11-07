build.power.table <-
function (power, n1, n2, prop.n2, alpha, theta1, theta2, disp1, 
    disp2, expand.args, one.sample, compute.what) 
{
    if (!is.null(prop.n2) && any(prop.n2 <= 0)) {
        stop("prop.n2 must be greater than 0")
    }
    if (missing(disp1)) {
        disp1 <- NULL
        disp2 <- NULL
    }
    if (!(one.sample || compute.what == "sample.size")) {
        expand.n <- length(n1) != length(n2)
        if (expand.args && expand.n) {
            if (is.null(n1)) {
                n.table <- expand.grid(n1 = as.numeric(NA), n2 = n2, 
                  prop.n2 = prop.n2)
                n.table$n1 <- n.table$n2/n.table$prop.n2
            }
            else if (is.null(n2)) {
                n.table <- expand.grid(n1 = n1, n2 = as.numeric(NA), 
                  prop.n2 = prop.n2)
                n.table$n2 <- n.table$n1 * n.table$prop.n2
            }
            else {
                n.table <- expand.grid(n1 = n1, n2 = n2, prop.n2 = as.numeric(NA))
                n.table$prop.n2 <- n.table$n2/n.table$n1
            }
            prop.n2 <- seq(nrow(n.table))
            n1 <- n2 <- as.numeric(NA)
        }
        else {
            if (is.null(n1)) {
                n1 <- n2/prop.n2
            }
            else if (is.null(n2)) {
                n2 <- prop.n2 * n1
            }
            else {
                prop.n2 <- n2/n1
            }
            n.table <- as.data.frame(list(n1 = n1, n2 = n2, prop.n2 = prop.n2))
            if (expand.args) {
                prop.n2 <- seq(nrow(n.table))
                n1 <- n2 <- as.numeric(NA)
            }
        }
    }
    arg.list <- list(theta1 = theta1, disp1 = disp1, theta2 = theta2, 
        disp2 = disp2, delta = as.numeric(NA), alpha = alpha, 
        power = power, n1 = n1, n2 = n2, prop.n2 = prop.n2)
    arg.list <- arg.list[sapply(arg.list, length) > 0]
    if (expand.args) {
        power.table <- expand.grid(arg.list)
        if (!(one.sample || compute.what == "sample.size")) {
            n.table <- n.table[power.table$prop.n2, ]
            power.table$prop.n2 <- n.table$prop.n2
            power.table$n1 <- n.table$n1
            power.table$n2 <- n.table$n2
        }
    }
    else {
        power.table <- as.data.frame(arg.list)
    }
    if (compute.what != "delta") {
        power.table$delta <- power.table$theta2 - power.table$theta1
    }
    class(power.table) <- c("power.table", "data.frame")
    return(power.table)
}
