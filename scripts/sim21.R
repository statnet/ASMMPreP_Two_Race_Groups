
library("methods")
library("EpiModelHPC")
library("mardham2")

args <- commandArgs(trailingOnly = TRUE)
simno <- args[1]
jobno <- args[2]
fsimno <- paste(simno, jobno, sep = ".")
print(fsimno)

load("est/nwstats.rda")

#20% coverage with 2.1 diparity .

param <- param.adol(nwstats = st, prep.elig.model = "adol.riskhist.older", 
                    prep.start = 1040,
                    prep.coverage.b=.129 , prep.coverage.w=.271 ,
                    prep.class.prob.b = c(.3085, .1871, .1871, .3173),
                    prep.class.prob.w = c(.1085, .1881, .1881, .5153),
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
