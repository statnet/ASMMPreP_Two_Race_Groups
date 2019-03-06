
library("methods")
library("EpiModelHPC")
library("mardham2")

args <- commandArgs(trailingOnly = TRUE)
simno <- args[1]
jobno <- args[2]
fsimno <- paste(simno, jobno, sep = ".")
print(fsimno)

load("est/nwstats.rda")

#40% coverage with 2.1 diparity equal adherence averaged over atn113.

param <- param.adol(nwstats = st, prep.elig.model = "adol.riskhist.older", 
                    prep.start = 1040,
                    prep.coverage.b=.258 , prep.coverage.w=.542 ,
                    heat.multiplier.b = 10,
                    heat.multiplier.w = 1,
                    ai.scale.b = 2.63,
                    ai.scale.w = 2.63,
                    prepSpell = TRUE)

init <- init.adol(nwstats = st)
control <- control.adol(simno = fsimno, nsteps = 52 * 40,
                        nsims = 16, ncores = 16, resim.int = 4,
                        save.int = 100, verbose.int = 10,
                        save.network = FALSE, save.other = NULL)

netsim_hpc("est/fit.rda", param, init, control, compress = "gzip")
