# The-Effects-of-Stringent-and-Mild-Interventions
The effects of stringent and mild interventions for Coronavirus Pandemic

## 1.  Data
### 1) Abstract

The daily numbers of the positive individuals of COVID-19 for Wenzhou, Shanghai, 
and the counties of the United States were collected. The epidemic data was from the date when the positive individuals were firstly reported to one month later, 
for Wenzhou and Shanghai from the official websites of the health commission of local municipal cities. The total of 504 positive individuals from January 21, 2020,
to February 21, 2020, for Wenzhou and the total of 333 positive individuals from January 20, 2020, to February 20, 2020, for Shanghai were collected.
Meanwhile, the daily positive individuals of COVID-19 for each infected county of the United States from March 1, 2020, to March 31, 2020, were compiled from Johns Hopkins University. The covariate data including demographics information (residential population, area, the percentage of the population aged over 65), and the latitude of Wenzhou and Shanghai were collected from the 2019 statistical yearbook of the Zhejiang Provincial and Shanghai Municipal Bureau of Statistics. For the geographical characteristic, the average daily temperature for a month starting with January 21, 2020, for Wenzhou, and January 20, 2020, for Shanghai were collected from the website of Weather Underground, which was supported by International Business Machines Corporation (IBM). As for the economy condition, GDP per capita statistics nationwide in China were obtained from the latest statistical yearbooks. Correspondingly, the populations, areas, latitudes, GDP per capita of the counties of the United States were collected from the United States Census Bureau (USCB), while the average daily temperature of the United States' counties were available from National Centers for Environmental Information offered by National Oceanic and Atmospheric Administration (NOAA). 

### 2) Availability
The data to reproduce our results are available.

### 3) Data dictionary
The data incorporte 2 `.csv` files.
- The names of cities ("ctname"), the percentage of the popuatlion aged over 65 ("prop.elder"), population density size ("density"), latitude ("Latitude"), GDP per capita ("pergdp"), temperature ("Meantem2020"), daily positive individuals of COVID-19 (“comfirmed”), and the selected principal components ("pc1", "pc2","pc3") are included in the `wzdat1014.csv` file. 
- Also, the same variables are included in the `shdat1014.csv` file. 

### 4) Permissions
The data were orignially collected by the authors.

----
## 2. Code
### 1) Abstract
We employed two classical casual effect models (synthetic control method) and (regression discontinuity) to identify the casual effects of interventions and to statistically measure the causual effects. After the causual effects of interventions were identified, we applied our proposed model (Susceptible-Infectious-self-Healing-Positive: SIHP) to simulate the stringent and the mild interventions delayed few days and the absence of both individual intervention and all interventions. The Bayesian framework and the technique of Markov Chain Monte Carlo (MCMC) used in the SIHR model to obtain estimates of parameters.

### 2) Reporducibility
- The evaluation of treatment effects of both the stringent and mild interventions implemented in Wenzhou and Shanghai by running `synth.R`.
- The figure about the plots of SCM procedures for Wenzhou and Shanghai by running `figure2.R`. 
- The SCM results of Table 1, Tables S3 and S4 were created by running `Table 1&S3&S4.R`. 
- The candidate counties of Wenzhou to construct a "synthetic Wenzhou" were selected by running `Spatial_clustering.R`. 
- The RD results of Table 2 and Table S5 were produced by running `RD_tables.R`.   


- The estimation of Susceptible (S), Infectious (I), Hospitalized(H), and Removed (R) individuals for the dynamic system (A1) in the paper by runing `Model.R`.
- The simulation of delay effects by runing `Simulation.R`.


