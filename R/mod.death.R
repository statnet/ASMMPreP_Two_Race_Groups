
#' @title Death Module
#'
#' @description Module function for simulting both general and disease-related
#'              deaths among population members.
#'
#' @inheritParams aging.mard
#'
#' @details
#' Deaths are divided into two categories: general deaths, for which demographic
#' data on age-specific mortality rates applies; and disease-related diseases,
#' for which the rate of death is a function of progression to end-stage AIDS.
#' Which nodes have died is determined stochastically for general deaths using
#' draws from a binomial distribution, and deterministically for disease-related
#' deaths after nodes have reach a maximum viral load value set in the
#' \code{vl.fatal} parameter.
#'
#' @return
#' This function returns the updated \code{dat} object accounting for deaths.
#' The deaths are deactivated from the main and casual networks, as those are in
#' \code{networkDynamic} class objects; dead nodes are not deleted from the
#' instant network until the \code{\link{simnet.mard}} module for bookkeeping
#' purposes.
#'
#' @keywords module
#' @export
#'
deaths.mard <- function(dat, at) {

  ## General deaths
  age <- floor(dat$attr$age)
  race <- dat$attr$race
  active <- dat$attr$active

  alive.B <- which(active == 1 & race == "B")
  age.B <- age[alive.B]
  death.B.prob <- dat$param$asmr.B[age.B]
  deaths.B <- alive.B[rbinom(length(death.B.prob), 1, death.B.prob) == 1]

  alive.W <- which(active == 1 & race == "W")
  age.W <- age[alive.W]
  death.W.prob <- dat$param$asmr.W[age.W]
  deaths.W <- alive.W[rbinom(length(death.W.prob), 1, death.W.prob) == 1]

  dth.gen <- c(deaths.B, deaths.W)


  ## Disease deaths
  dth.dis <- which(dat$attr$active == 1 &
                   dat$attr$stage == "D" &
                   dat$attr$vl >= dat$param$vl.fatal)

  dth.all <- NULL
  dth.all <- unique(c(dth.gen, dth.dis))


  
  if (length(dth.all) > 0) {

    dat$attr$active[dth.all] <- 0
    dat$attr$depart.time[dth.all] <- at
   
 

    for (i in 1:2) {
      dat$nw[[i]] <- deactivate.vertices(dat$nw[[i]], onset = at, terminus = Inf,
                                         v = dth.all, deactivate.edges = TRUE)
    }
  }

   

  # Risk history adolecent (remove the dead)
  dth.all<-sort(dth.all)
  for (i in 1:length(dth.all)){
  dat$riskhist <- subset(dat$riskhist,dat$riskhist[,"V1"] != dat$attr$uid[dth.all[i]])
  }

  
  ## Summary Output
  dat$epi$dth.gen[at] <- length(dth.gen)
  dat$epi$dth.gen.B[at] <- length(deaths.B)
  dat$epi$dth.gen.W[at] <- length(deaths.W)
  dat$epi$dth.dis[at] <- length(dth.dis)
  dat$epi$dth.dis.B[at] <- sum(race[dth.dis] == "B")
  dat$epi$dth.dis.W[at] <- sum(race[dth.dis] == "W")

  return(dat)
}
