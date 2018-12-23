library(bio3d)
library(nabor)
library(dplyr)
library(plyr)
library(foreach)
library(doParallel)
library(boot)
library(rgl)

detectCores() ### Cuantos hay
cores <- makeCluster(5)
registerDoParallel(cores) ###Cuantos van a trabajar
getDoParWorkers() ###Cuantos estan trabajando
stopCluster(cores)
