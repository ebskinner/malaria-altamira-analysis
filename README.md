# malaria-altamira-analysis

Malaria transmission in the Brazilian Amazon
This repository contains code to reproduce the analysis in the paper "Resurgence of malaria in protected and rural areas after successful control program in a Brazilian Amazon municipality" by Skinner and Angeli Dutra et al.

Contents
01_extract_environ_vars.js: Google Earth Engine script to extract environmental variables

02_process_environ_data.R: Clean and merge environmental data with case data

03_morans_i.R: Calculate Moran's I for spatial autocorrelation

04_analysis.R: Main analysis script with statistical models


Data Availability
The raw malaria case data used in this analysis is not publicly available due to its sensitive nature and privacy concerns. Researchers interested in replicating or building upon this analysis should contact Brazilian Epidemiological Surveillance System for Malaria (SIVEP-Malária) directly.

All environmental and geographic data used in the analysis is publicly available from the sources cited in the Methods section of the associated paper. The Google Earth Engine script used to extract these variables is provided in the code/ directory.
