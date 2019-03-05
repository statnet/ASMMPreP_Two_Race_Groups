# Potential Impact of HIV Preexposure Prophylaxis Among Black and White Adolescent Sexual Minority Males.

This repository contains the source code to reproduce the analysis described in our HIV transmission model. In this model, we examine the impact of race specific PrEP uptake and adherence on HIV disparities between WHite and BLack adolecent sexaul minority males (ASMM).

This model is written and runs in the R programming language. To utilize the model, it is first necessary to install EpiModel, the epidemic modeling software, and MARDHAM, the extension package for modeling the HIV transmission dynamics among the ASMM examined here.

Running the scripts from this paper requires access to a high performance computing environment. We suggest using a unix cluster, although a high powered Windows or Mac cluster may work. If these encounter errors, we suggest moving to a high powered unix cluster.

Within R:
install.packages("EpiModel")

- install devtools if necessary, install.packages("devtools")

devtools::install_github("statnet/EpiModelHPC")
devtools::install_github("statnet/Mardham")

## Citation
# Hamilton DT, Goodreau SM, Jenness SM, Sullivan PS, Wang LY, Dunville RL, Barrios LC, Rosenberg ES. Potential Impact of HIV Preexposure Prophylaxis Among Black and White Adolescent Sexual Minority Males. Am J Public Health. 2018 Nov;108(S4):S284-S291. doi: 10.2105/AJPH.2018.304471.


OBJECTIVES:
To assess the potential impact of preexposure prophylaxis (PrEP) on the HIV epidemic among Black and White adolescent sexual minority males (ASMM).

METHODS:
We used a network model and race-specific data from recent trials to simulate HIV transmission among a population of Black and White 13- to 18-year-old ASMM over 20 years. We estimated the number of infections prevented (impact) and the number needed to treat to prevent an infection (efficiency) under multiple coverage and adherence scenarios.

RESULTS:
At modeled coverage and adherence, PrEP could avert 3% to 20% of infections among Black ASMM and 8% to 51% among White ASMM. A larger number, but smaller percentage, of infections were prevented in Black ASMM in all scenarios examined. PrEP was more efficient among Black ASMM (number needed to treat to avert an infection = 25-32) compared with White ASMM (146-237).

CONCLUSIONS:
PrEP can reduce HIV incidence among both Black and White ASMM but is far more efficient for Black ASMM because of higher incidence. Public Health Implications. Black ASMM communities suffer disproportionate HIV burden; despite imperfect adherence, PrEP programs could prevent HIV efficiently in these communities.

PMID: 30383415 PMCID: PMC6215365 DOI: 10.2105/AJPH.2018.304471
