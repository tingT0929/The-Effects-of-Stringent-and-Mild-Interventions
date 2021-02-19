# The-Effects-of-Stringent-and-Mild-Interventions
The effects of stringent and mild interventions for Coronavirus Pandemic

## 1.  Data
### 1) Abstract

- The daily numbers of the positive individuals of COVID-19 for Wenzhou, Shanghai, 
and the counties of the United States were collected. 

- The epidemic data was from the date when the positive individuals were firstly reported to one month later, 
for Wenzhou and Shanghai from the official websites of the health commission of local municipal cities. The total of 504 positive individuals from January 21, 2020,
to February 21, 2020, for Wenzhou and the total of 333 positive individuals from January 20, 2020, to February 20, 2020, for Shanghai were collected.

- Meanwhile, the daily positive individuals of COVID-19 for each infected county of the United States from March 1, 2020, to March 31, 2020, were compiled from Johns Hopkins University. 

- The covariate data including demographics information (residential population, area, the percentage of the population aged over 65), and the latitude of Wenzhou and Shanghai were collected from the 2019 statistical yearbook of the Zhejiang Provincial and Shanghai Municipal Bureau of Statistics. For the geographical characteristic, the average daily temperature for a month starting with January 21, 2020, for Wenzhou, and January 20, 2020, for Shanghai were collected from the website of Weather Underground, which was supported by International Business Machines Corporation (IBM). As for the economic condition, GDP per capita statistics nationwide in China were obtained from the latest statistical yearbooks. Correspondingly, the populations, areas, latitudes, GDP per capita of the counties of the United States were collected from the United States Census Bureau (USCB), while the average daily temperature of the United States' counties were available from National Centers for Environmental Information offered by National Oceanic and Atmospheric Administration (NOAA). 

### 2) Availability
The data to reproduce our results are available.

### 3) Data dictionary
The data incorporate 4 `.csv` files and a `raw data` folder.
- The names of regions ("ctname"), the percentage of the popuatlion aged over 65 ("prop.elder"), population density size ("density"), latitude ("Latitude"), GDP per capita ("pergdp"), temperature ("Meantem2020"), daily positive individuals of COVID-19 ("comfirmed"), and the selected principal components ("pc1", "pc2","pc3") are included in the `wzdat1014.csv` file. 
- Also, the same variables are included in the `shdat1014.csv` file. 
- The names of regions ("ctname"), latitude ("Latitude"), population ("Population") and the trajectories of COVID-19 from the day 1 to day 29 are included in the `latdat1014.csv` file. 
- The names of regions ("ctname"), the percentage of the popuatlion aged over 65 ("prop.elder"), population density size ("density"), latitude ("Latitude"), GDP per capita ("pergdp"), temperature ("Meantem2020"), daily positive individuals of COVID-19 ("comfirmed") for all infected areas are included in the `usedat1014.csv` file. 
- The `raw data` folder contain: a) the weather of all counties in the US, crawled from the website: [WEATHER UNDERGROUND](https://www.wunderground.com/history). b) `counties_location.dbf` contains the longitudes and latitudes of all counties.

The covariate information of the United States counties is available from [the United States Census Bureau (USCB)](https://www.census.gov/). The daily positive individuals and deaths of COVID-19 for each infected county of the United States from March 1, 2020, to March 31, 2020, are from Johns Hopkins University ([COVID-19 Dashboard](https://coronavirus.jhu.edu/map.html)). And the data related to COVID-19 of Wenzhou and Shanghai are collected from the official websites of the local health commission.

### 4) Permissions
The data were orignially collected by the authors.

----
## 2. Code
### 1) Abstract
We employed two classical casual effect models (synthetic control method) and (regression discontinuity) to identify the casual effects of interventions and to statistically measure the casual effects. After the casual effects of interventions were identified, we applied our proposed model (Susceptible-Infectious-self-Healing-Positive: SIHP) to simulate the stringent and the mild interventions delayed few days and the absence of both individual intervention and all interventions. The Bayesian framework and the technique of Markov Chain Monte Carlo (MCMC) were used in the SIHP model to obtain estimates of parameters.

### 2) Reporducibility
- The candidate counties of Wenzhou to construct a "synthetic Wenzhou" were selected by running `Spatial_clustering.R`. 
- The evaluation of treatment effects of both the stringent and mild interventions implemented in Wenzhou and Shanghai using SCM procedure by running `synth.R`.
- The SCM results of Table 1, Tables A3 and A4 were created by running `Table_1_A3_A4.R`. 
- The figure about the plots of SCM procedures (including placebo tests) for Wenzhou and Shanghai was generated by running `figure2.R`. (Before running this code, `synth.R` needs to be run first.) 
- The RD results of Table 2 and Table A5 were produced by running `RD_tables.R`. 
- The figure about the cutpff-point for Wenzhou and Shanghai was created by running `RD_fig.R`.
- The functions used in the state space SIHP model in the paper were given in `Epidemic_model.R`.
- The parameters estimation of state space SIHP models for Wenzhou and Shanghai were obtained by running `WZ.R` and `SH.R`.
- The figure about the effective reproduction number for different periods was generated  by runing `R_0_plot.R`.
- The simulation of the trajectories of COVID-19 by the proposed SIHP model was generated by running `Sim_epi_plot.R` (main-text) and `Sim_epi_plot_abroad.R` (appendix).
