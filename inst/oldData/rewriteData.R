# Transfer .dat examples to .rda data
tnFiles <- c("tn260_1981_2010.dat", "tn260_GL_2050.dat", "tn260_WH_2050.dat")
txFiles <- c("tx260_1981_2010.dat", "tx260_GL_2050.dat", "tx260_WH_2050.dat")

library(stringr)

rewriteData <- function(fileName, variable) {
  baseName <- str_split(fileName, "\\.")[[1]][1]

  assign(baseName, fread(paste0("inst/oldData/",fileName)))
  setnames(get(baseName), c("date", variable))
  get(baseName)[, date := as.Date(paste(date), format="%Y%m%d")]

  save(list=baseName, file = paste0("data/", baseName, ".rda"))
}

for (i in 1 : 3) {
  fileName <- tnFiles[i]
  rewriteData(fileName, "tn")
}

for (i in 1 : 3) {
  fileName <- txFiles[i]
  rewriteData(fileName, "tx")
}
