% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/params.R
\name{param.adol}
\alias{param.adol}
\title{Epidemic Model Parameters for MARDHAM Models}
\usage{
param.adol(nwstats, race.method = 2, last.neg.test.B.int = 2900,
  last.neg.test.W.int = 2900, mean.test.B.int = 2900,
  mean.test.W.int = 2900, testing.pattern = "memoryless",
  test.window.int = 21, tt.traj.B.prob = c(0.077, 0, 0.356, 0.567),
  tt.traj.W.prob = c(0.052, 0, 0.331, 0.617), tx.init.B.prob = 0.092,
  tx.init.W.prob = 0.127, tx.halt.B.prob = 0.0102,
  tx.halt.W.prob = 0.0071, tx.reinit.B.prob = 0.00066,
  tx.reinit.W.prob = 0.00291, max.time.off.tx.full.int = 520 * 7,
  max.time.on.tx.part.int = 52 * 15 * 7, max.time.off.tx.part.int = 520 * 7,
  vl.acute.rise.int = 45, vl.acute.peak = 6.886, vl.acute.fall.int = 45,
  vl.set.point = 4.5, vl.aids.onset.int = 520 * 7, vl.aids.int = 52 * 2 *
  7, vl.fatal = 7, vl.full.supp = 1.5, vl.part.supp = 3.5,
  full.supp.down.slope = 0.25, full.supp.up.slope = 0.25,
  part.supp.down.slope = 0.25, part.supp.up.slope = 0.25,
  b.B.rate = 0.004262/7, b.W.rate = 0.004262/7, b.method = "fixed",
  URAI.prob = 0.0082 * 1.09, UIAI.prob = 0.0031 * 1.09, acute.rr = 6,
  circ.rr = 0.4, condom.rr = 0.25, disc.outset.main.B.prob = 0,
  disc.outset.main.W.prob = 0, disc.at.diag.main.B.prob = 1,
  disc.at.diag.main.W.prob = 1, disc.post.diag.main.B.prob = 0,
  disc.post.diag.main.W.prob = 0, disc.outset.pers.B.prob = 0,
  disc.outset.pers.W.prob = 0, disc.at.diag.pers.B.prob = 1,
  disc.at.diag.pers.W.prob = 1, disc.post.diag.pers.B.prob = 0,
  disc.post.diag.pers.W.prob = 0, disc.inst.B.prob = 0,
  disc.inst.W.prob = 0, circ.B.prob = 0.874, circ.W.prob = 0.918,
  ccr5.B.prob = c(0, 0.034), ccr5.W.prob = c(0.021, 0.176),
  ccr5.heteroz.rr = 0.3, num.inst.ai.classes = 1,
  base.ai.main.BB.rate = 0.0384, base.ai.main.BW.rate = 0.0384,
  base.ai.main.WW.rate = 0.0384, base.ai.pers.BB.rate = 0,
  base.ai.pers.BW.rate = 0, base.ai.pers.WW.rate = 0, ai.scale.b = 3.6,
  ai.scale.w = 3.6, cond.main.BB.prob = 0.54, cond.main.BW.prob = 0.54,
  cond.main.WW.prob = 0.54, cond.pers.always.prob = 0,
  cond.pers.BB.prob = 0, cond.pers.BW.prob = 0, cond.pers.WW.prob = 0,
  cond.inst.always.prob = 0, cond.inst.BB.prob = 0, cond.inst.BW.prob = 0,
  cond.inst.WW.prob = 0, cond.always.prob.corr = 0, cond.rr.BB = 1,
  cond.rr.BW = 1, cond.rr.WW = 1, cond.diag.main.beta = -0.67,
  cond.discl.main.beta = -0.85, cond.diag.pers.beta = 0,
  cond.discl.pers.beta = 0, cond.diag.inst.beta = 0,
  cond.discl.inst.beta = 0, vv.iev.BB.prob = 0.42, vv.iev.BW.prob = 0.56,
  vv.iev.WW.prob = 0.49, prep.start = 1, prep.elig.model = "none",
  prep.efficacy = 0.92, prep.class.prob.b = c(0.2085, 0.2438, 0.1314,
  0.4163), prep.class.prob.w = c(0.2085, 0.2438, 0.1314, 0.4163),
  prep.class.effect = c(0, 0.31, 0.81, 0.95), prep.coverage.b = 0.4,
  prep.coverage.w = 0.4, prep.cov.method = "curr", prep.cov.rate.b = 1,
  prep.cov.rate.w = 1, prep.rcomp = 1, prep.tst.int = 90,
  prep.risk.int = 182, prep.risk.reassess = FALSE, prep.delay.b = 26,
  prep.delay.w = 26, prepSpell = FALSE, prepDrop.b = 0.014337,
  prepDrop.w = 0.014337, prep.uaicount.thresh.b = 10,
  prep.uaicount.thresh.w = 10, heat.prob.contact.b = matrix(c(0.0022789,
  0.0045579, 0.0068368, 0.0091157, 0.0113947, 0.0136736, 0.0045579, 0.0091157,
  0.0136736, 0.0182315, 0.0227893, 0.0273472, 0.0068368, 0.0136736, 0.0205104,
  0.0273472, 0.034184, 0.0410208, 0.010597, 0.0211941, 0.0317911, 0.0423881,
  0.0529852, 0.0635822, 0.0333294, 0.0666588, 0.0999881, 0.1333175, 0.1666469,
  0.1999763), nrow = 5, ncol = 6, byrow = TRUE),
  heat.prob.contact.w = matrix(c(0.0022789, 0.0045579, 0.0068368, 0.0091157,
  0.0113947, 0.0136736, 0.0045579, 0.0091157, 0.0136736, 0.0182315, 0.0227893,
  0.0273472, 0.0068368, 0.0136736, 0.0205104, 0.0273472, 0.034184, 0.0410208,
  0.010597, 0.0211941, 0.0317911, 0.0423881, 0.0529852, 0.0635822, 0.0333294,
  0.0666588, 0.0999881, 0.1333175, 0.1666469, 0.1999763), nrow = 5, ncol = 6,
  byrow = TRUE), infect.prob = 0.01, prop.asmm.b_b = 0.9,
  prop.asmm.b_w = 0.1, prop.asmm.w_w = 0.9, prop.asmm.w_b = 0.1,
  cond.heat = 0.5, prev.yasmm.b = 0.26, prev.yasmm.w = 0.03,
  heat.trans.b = ((prop.asmm.b_b * cond.heat * prev.yasmm.b * infect.prob) +
  (prop.asmm.b_w * cond.heat * prev.yasmm.w * infect.prob)),
  heat.trans.w = ((prop.asmm.w_w * cond.heat * prev.yasmm.w * infect.prob) +
  (prop.asmm.w_b * cond.heat * prev.yasmm.b * infect.prob)),
  heat.multiplier.b = 8.16, heat.multiplier.w = 8.16, ...)
}
\arguments{
\item{nwstats}{Target statistics for the network model. An object of class
\code{nwstats} output from \code{\link{calc_nwstats.adol}}.}

\item{race.method}{Number of races in the model, with options of 1 or 2. If
1, then race-specific parameters will be averaged.}

\item{last.neg.test.B.int}{Time range in days for last negative test for
black men.}

\item{last.neg.test.W.int}{Time range in days for last negative test for
white men.}

\item{mean.test.B.int}{Mean intertest interval in days for black MSM who test.}

\item{mean.test.W.int}{Mean intertest interval in days for white MSM who test.}

\item{testing.pattern}{Method for HIV testing, with options \code{"memoryless"}
for constant hazard without regard to time since previous test, or
\code{"interval"} deterministic fixed intervals.}

\item{test.window.int}{Length of the HIV test window period in days.}

\item{tt.traj.B.prob}{Proportion of black MSM who enter one of four
testing/treatment trajectories: never test or treat, test and never
initiate treatment, test and treated with partial viral suppression,
and test and treated with full suppression.}

\item{tt.traj.W.prob}{Proportion of white MSM who enter into the four
testing/treatment trajectories, as defined above.}

\item{tx.init.B.prob}{Probability per time step that a black MSM who has
tested positive will initiate treatment.}

\item{tx.init.W.prob}{Probability per time step that a white MSM who has
tested positive will initiate treatment.}

\item{tx.halt.B.prob}{Probability per time step that a black MSM who is
currently on treatment will halt treatment.}

\item{tx.halt.W.prob}{Probability per time step that a white MSM who is
currently on treatment will halt treatment.}

\item{tx.reinit.B.prob}{Probability per time step that a black MSM who is
not currently on treatment but who has been in the past will
re-initiate treatment.}

\item{tx.reinit.W.prob}{Probability per time step that a white MSM who is
not currently on treatment but who has been in the past will
re-initiate treatment.}

\item{max.time.off.tx.full.int}{Number of days off treatment for a full
suppressor before onset of AIDS, including time before diagnosis.}

\item{max.time.on.tx.part.int}{Number of days on treatment for a
partial suppressor beofre onset of AIDS.}

\item{max.time.off.tx.part.int}{Nnumber of days off treatment for a
partial suppressor before onset of AIDS, including time before
diagnosis.}

\item{vl.acute.rise.int}{Number of days to peak viremia during acute
infection.}

\item{vl.acute.peak}{Peak viral load (in log10 units) at the height of acute
infection.}

\item{vl.acute.fall.int}{Number of days from peak viremia to set-point
viral load during the acute infection period.}

\item{vl.set.point}{Set point viral load (in log10 units).}

\item{vl.aids.onset.int}{Number of days to AIDS for a treatment-naive
patient.}

\item{vl.aids.int}{Duration of AIDS stage infection in days.}

\item{vl.fatal}{Viral load in AIDS at which death occurs.}

\item{vl.full.supp}{Log10 viral load at full suppression on ART.}

\item{vl.part.supp}{Log10 viral load at partial suppression on ART.}

\item{full.supp.down.slope}{For full suppressors, number of log10 units that
viral load falls per time step from treatment initiation or re-initiation
until the level in \code{vl.full.supp}.}

\item{full.supp.up.slope}{For full suppressors, number of log10 units that
viral load rises per time step from treatment halting until expected
value.}

\item{part.supp.down.slope}{For partial suppressors, number of log10 units
that viral load falls per time step from treatment initiation or
re-initiation until the level in \code{vl.part.supp}.}

\item{part.supp.up.slope}{For partial suppressors, number of log10 units that
viral load rises per time step from treatment halting until expected value.}

\item{b.B.rate}{Rate at which black MSM enter the population.}

\item{b.W.rate}{Rate at which white MSM enter the population.}

\item{b.method}{Method for calculating the number of expected births at each
time step, with \code{"fixed"} based on the number of persons at the
initial time step and \code{"varying"} based on the current time step.
The former method is compatible with \code{mardham1}.}

\item{URAI.prob}{Probability of transmission for a man having unprotected
receptive anal intercourse with an infected man at set point viral
load.}

\item{UIAI.prob}{Probability of transmission for an uncircumcised man having
unprotected insertive anal intercourse with an infected man at set
point viral load.}

\item{acute.rr}{Relative risk of infection (compared to that predicted by
elevated viral load) when positive partner is in the acute stage.}

\item{circ.rr}{Relative risk of infection from insertive anal sex when the
negative insertive partner is circumcised.}

\item{condom.rr}{Relative risk of infection from anal sex when a condom is
used.}

\item{disc.outset.main.B.prob}{Probability that an HIV-infected black MSM will
disclose his status at the start of a main partnership.}

\item{disc.outset.main.W.prob}{Probability that an HIV-infected white MSM will
disclose his status at the start of a main partnership.}

\item{disc.at.diag.main.B.prob}{Probability that a black MSM already in a main
partnership will disclose at the time of diagnosis.}

\item{disc.at.diag.main.W.prob}{Probability that a white MSM already in a main
partnership will disclose at the time of diagnosis.}

\item{disc.post.diag.main.B.prob}{Probability that an HIV-infected black MSM
in a main partnership will disclose his status, assuming he didn't
at the start of the partnership or at diagnosis.}

\item{disc.post.diag.main.W.prob}{Probability that an HIV-infected white MSM
in a main partnership will disclose his status, assuming he didn't
at the start of the partnership or at diagnosis.}

\item{disc.outset.pers.B.prob}{Probability that an HIV-infected black MSM will
disclose his status at the start of a casual partnership.}

\item{disc.outset.pers.W.prob}{Probability that an HIV-infected white MSM will
disclose his status at the start of a casual partnership.}

\item{disc.at.diag.pers.B.prob}{Probability that a black MSM already in a
casual partnership will disclose at the time of diagnosis.}

\item{disc.at.diag.pers.W.prob}{Probability that a white MSM already in a
casual partnership will disclose at the time of diagnosis.}

\item{disc.post.diag.pers.B.prob}{Probability that an HIV-infected black MSM
in a casual partnership will disclose his status, assuming he
didn't at the start of the partnership or at diagnosis.}

\item{disc.post.diag.pers.W.prob}{Probability that an HIV-infected white MSM
in a casual partnership will disclose his status, assuming he
didn't at the start of the partnership or at diagnosis.}

\item{disc.inst.B.prob}{Probability that an HIV-infected black MSM will
disclose his status to a one-off partner.}

\item{disc.inst.W.prob}{Probability that an HIV-infected white MSM will
disclose his status to a one-off partner.}

\item{circ.B.prob}{Probablity that a black new arrival in the population
will be circumcised.}

\item{circ.W.prob}{Probablity that a white new arrival in the population
will be circumcised.}

\item{ccr5.B.prob}{Vector of length two of frequencies of the Delta 32
mutation (homozygous and heterozygous, respectively) in the CCR5 gene
among black MSM.}

\item{ccr5.W.prob}{Vector of length two of frequencies of the Delta 32
mutation (homozygous and heterozygous, respectively) in the CCR5 gene
among white MSM.}

\item{ccr5.heteroz.rr}{Relative risk of infection for men who are heterozygous
in the CCR5 mutation.}

\item{num.inst.ai.classes}{Number of quantiles into which men should be
divided in determining their levels of one-off anal intercourse.}

\item{base.ai.main.BB.rate}{Expected coital frequency in black-black main
partnerships (acts per day).}

\item{base.ai.main.BW.rate}{Expected coital frequency in black-white main
partnerships (acts per day).}

\item{base.ai.main.WW.rate}{Expected coital frequency in white-white main
partnerships (acts per day).}

\item{base.ai.pers.BB.rate}{Expected coital frequency in black-black casual
partnerships (acts per day).}

\item{base.ai.pers.BW.rate}{Expected coital frequency in black-white casual
partnerships (acts per day).}

\item{base.ai.pers.WW.rate}{Expected coital frequency in white-white casual
partnerships (acts per day).}

\item{cond.main.BB.prob}{Probability of condom use in a black-black main
partnership.}

\item{cond.main.BW.prob}{Probability of condom use in a black-white main
partnership.}

\item{cond.main.WW.prob}{Probability of condom use in a white-white main
partnership.}

\item{cond.pers.always.prob}{Fraction of men in casual partnerships who always
use condoms in those partnerships.}

\item{cond.pers.BB.prob}{Of men who are not consistent condom users, per-act
probability of condom use in a black-black casual partnerships.}

\item{cond.pers.BW.prob}{Of men who are not consistent condom users, per-act
probability of condom use in a black-white casual partnerships.}

\item{cond.pers.WW.prob}{Of men who are not consistent condom users, per-act
probability of condom use in a white-white casual partnerships.}

\item{cond.inst.always.prob}{Fraction of men in instant partnerships who always
use condoms in those partnerships.}

\item{cond.inst.BB.prob}{Of men who are not consistent condom users, per-act
probability of condom use in a black-black one-off partnerships.}

\item{cond.inst.BW.prob}{Of men who are not consistent condom users, per-act
probability of condom use in a black-white one-off partnerships.}

\item{cond.inst.WW.prob}{Of men who are not consistent condom users, per-act
probability of condom use in a white-white one-off partnerships.}

\item{cond.always.prob.corr}{Correlation coefficient for probability of always
using condoms in both casual and one-off}

\item{cond.rr.BB}{Condom probability scaler for black-black partnerships for
model calibration purposes.}

\item{cond.rr.BW}{Condom probability scaler for black-white partnerships for
model calibration purposes.}

\item{cond.rr.WW}{Condom probability scaler for white-white partnerships for
model calibration purposes.}

\item{cond.diag.main.beta}{Beta multiplier for the log odds of using a
condom in a main partnership if the HIV-infected man has been
diagnosed.}

\item{cond.discl.main.beta}{Beta multiplier for the log odds of using a
condom in a main partnership if the HIV-infected man has disclosed.}

\item{cond.diag.pers.beta}{Beta multiplier for the log odds of using a
condom in a casual partnership if the HIV-infected man has been
diagnosed.}

\item{cond.discl.pers.beta}{Beta multiplier for the log odds of using a
condom in a casual partnership if the HIV-infected man has disclosed
his status.}

\item{cond.diag.inst.beta}{Beta multiplier for the log odds of using a
condom in a one-off partnership if the HIV-infected man has been
diagnosed.}

\item{cond.discl.inst.beta}{Beta multiplier for the log odds of using a
condom in a one-off partnership if the HIV-infected man has disclosed
his status.}

\item{vv.iev.BB.prob}{Probability that in a black-black partnership of
two versatile men, they will engage in intra-event versatility
("flipping") given that they're having AI.}

\item{vv.iev.BW.prob}{Probability that in a black-white partnership of
two versatile men, they will engage in intra-event versatility
("flipping") given that they're having AI.}

\item{vv.iev.WW.prob}{Probability that in a white-white partnership of
two versatile men, they will engage in intra-event versatility
("flipping") given that they're having AI.}

\item{prep.start}{Time step at which the PrEP intervention should start.}

\item{prep.elig.model}{Modeling approach for determining who is eligible for
PrEP. Current options are limited to: \code{none, adol.entry, adol.debuted, 
adol.AI, adol.entry.older, adol.debuted.older, adol.AI.older,
adol.entry.time, adol.debuted.time, adol.AI.time, adol.entry.older.time,
adol.debuted.older.time, adol.AI.older.time, adol.riskhist,
adol.riskhist.older, adol.riskhist.time, adol.riskhist.older.time}
for all persons who have never been on PrEP or those who have cycled off
see: prepSpell if \code{TRUE} and are disease-susceptible.}

\item{prep.efficacy}{The per-contact efficacy of PrEP to prevent infection if
used (parameter not currently used).}

\item{prep.class.effect}{The functional effectiveness of PrEP conditional on
PrEP class.}

\item{prep.cov.method}{The method for calculating PrEP coverage, with options
of \code{"curr"} to base the numerator on the number of people currently
on PrEP and \code{"ever"} to base it on the number of people ever on
PrEP.}

\item{prep.rcomp}{The relative change in rate of UAI across all partnership
types given current PrEP use, where 1 is no risk compensation.}

\item{prep.tst.int}{Testing interval for those who are actively on PrEP. This
overrides the mean testing interval parameters.}

\item{prep.risk.int}{Time window for assessment of risk eligibility for PrEP
in days.}

\item{prep.risk.reassess}{If \code{TRUE}, reassess eligibility for PrEP at
each testing visit.}

\item{prepSpell}{If \code{TRUE}, there is a constant hazard from dropping PrEP.}

\item{infect.prob}{the average (none role specific) probability of transmission given discordant UAI.}

\item{prop.asmm.b_b}{the proportion of black asmm contacts in the heat bath that are to black YASMM}

\item{prop.asmm.b_w}{the proportion of black asmm contacts in the heat bath that are to white YASMM}

\item{prop.asmm.w_w}{the proportion of white asmm contacts in the heat bath that are to white YASMM}

\item{prop.asmm.w_b}{the proportion of white asmm contacts in the heat bath that are to black YASMM}

\item{cond.heat}{the rate of condom use with YASMM}

\item{prev.yasmm.b}{prevalnce among black YASMM}

\item{prev.yasmm.w}{prevalnce among white YASMM}

\item{...}{Additional arguments passed to the function.}

\item{ai.scale}{General relative scaler for all act rates for model
calibration.}

\item{prep.class.prob}{The frequency of being in a low, medium, or high class
of adherence to PrEP.}

\item{prep.coverage}{The proportion of the eligible population who are start
PrEP once they become eligible.}

\item{prep.cov.rate}{The rate at which persons initiate PrEP conditional on
their eligibility, with 1 equal to instant start.}

\item{prep.delay}{The number of timesteps between becomeing eligible for PreP and starting PrEP.}

\item{PrepDrop}{The hazard for dropping PrEP given prepSpell==TRUE.}

\item{prep.uaicount.thresh}{The number of UAI one must have to initiate prep.}

\item{heat.prob.trans}{a matrix of dimensions length (riskg) by length(age) of the age and riskgroup 
specific probability of aquiring HIV from a node class not in the simulation (heat bath)  
This parameter includes both the probability of contact and transmission.}

\item{heat.prob.contact}{a matrix of dimensions length (riskg) by length(age) of the age and riskgroup 
specific probability of AI with a node class not in the simulation (heat bath).}

\item{heat.trans}{the probability of transmision in the heatbath given contact.}

\item{heat.multiplier}{is an atomic mutiplier to uniformly increase or devrease the intensity 
of the heatbath across all age and risk group catagories.}
}
\value{
A list object of class \code{param.adol}, which can be passed to
EpiModel function \code{netsim}.
}
\description{
Sets the epidemic parameters for stochastic network models
             simulated with \code{\link{netsim}} for MARDHAM.
}
