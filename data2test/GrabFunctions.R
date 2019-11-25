#One file:
newEnv <- new.env()
load("start/RevisedSedLoads.rdata", newEnv)
dump(c(lsf.str(newEnv)),file="normalCodeFile.R",
     envir=newEnv,control="useSource")


#One file per function
newEnv <- new.env()
load("start/RevisedSedLoads.rdata", newEnv)

funs <- lsf.str(newEnv)
for (i in seq_along(funs)){
  filename <- file.path("r",paste0(funs[i],".r"))
  dump(c(funs[i]),file=filename,
       envir=newEnv,control="useSource")
}
