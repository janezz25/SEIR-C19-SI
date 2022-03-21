# Model SEIR-C19-SI

This is a source code of the model presented at https://apps.lusy.fri.uni-lj.si/~janezz/analize/opis_modela_SEIR_C19SI.html (in Slovene).

The base model is the extended model SEIR with additional compartments for hospital and ICU admissions, divided into two separate submodels for modeling vaccinated and unvaccinated populations. Five base models construct the final extended model, where additional mixing of populations is enabled.

The model was and still is operatively used in daily forecasts and simulations of the covid epidemic in Slovenia. Additional information about the epidemiological modeling with the model can be found at  https://apps.lusy.fri.uni-lj.si/ (in Slovene).


### Usage


We need firstly to run the model with all parameters and data to produce forecast projections.

```
setwd("path_to_working_directory")
source("simulations_basic_v01.R")

```

The results are stored in file **epimod_latest.csv**.


Visualizations of the results are produced with the Rmarkdown file **results_simulations_basic_01.Rmd**. Open the file in Rstudio and compile it with Knit. The final visualizations are produced in a _HTML_ version of the file. 

The vizualizations include:

- daily forecasts,
- cumulative projections,
- weekly incidences,
- vaccination rates. 



### Note

This example is not calibrated to the actual epidemiological data in Slovenia. For the actual forecasts and simulations of the Covid19 in Slovenia please go to  https://apps.lusy.fri.uni-lj.si/~janezz/reports/long-term/zadnje_projekcije.html 


