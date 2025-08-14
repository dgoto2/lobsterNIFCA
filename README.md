### Northumberland (England) lobster stock assessment
This repository contains code used for the 2023 stock assessment work (data processing and assessment model fitting using Stock Synthesis, SS3) for the Northumberland stock of European lobster (Homarus gammarus).

#### File list

R (folder)

`1.observer_data.entrycheck_wales.R`

`2.observer_dataprocessing_wales.R`

`3.landings_data.processing_wales.R`

`4.cefas.lt.comp_dataprocessing_wales.R` 

`5.compute_cpue_wales.R`

`6.compute_observer.size.comp_wales.R`

`7.standardize_cpue_wales.R`

`8.ss_model_fitting_diagnostics.R`


#### Description

R: This folder contains R files for data processing and model-fitting

`1.observer_data.entrycheck_wales.R`: a script for assessing the structure and type of raw input data files

`2.observer_dataprocessing_wales.R`: a script for preprocessing the observer sampling data

`3.landings_data.processing_wales.R`: a script for preprocessing the iFISH landings data

`4.cefas.lt.comp_dataprocessing_wales.R`: a script for preprocessing the Cefas port sampling length composition data

`5.compute_cpue_wales.R`: a script for computing catch rate from the observer sampling data

`6.compute_observer.size.comp_wales.R`: a script for computing proportional length frequency distributions from the observer sampling data

`7.standardize_cpue_wales.R`: a script for standardizing observer CPUE using vector autoregressive spatio-temporal (VAST) models 

`8.ss_model_fitting_diagnostics.R`: a script for running Stock Synthesis model-fitting and model diagnostics

