Salinization and variation in host susceptibility to disease can alter the role of parasites in consumer-mediated nutrient cycling

This file describes the six datasets and two R scripts used to run the analysis for “Salinization and variation in host susceptibility to disease can alter the role of parasites in consumer-mediated nutrient cycling”. R version 4.4.1 was used to analyze all the data. 

Authors: Zachary Gajewski1, Isabela Velasquez1, Mary Campbell1, Isabelle Relyea2, Bryon Tuthill1, Jimmy Sustachek3, Hilary A. Dugan3, Grace Wilkinson3, Jessica Hua1

1. Department of Forest and Wildlife Ecology, University of Wisconsin-Madison, Madison, Wisconsin, 53706
2. Department of Ecology and Evolutionary Biology, University of Michigan, Ann Arbor, Michigan, 48109
3. Center for Limnology, University of Wisconsin-Madison, Madison, Wisconsin, 53706

Corresponding Author: Zachary Gajewski, gajewskizach@gmail.com


R v4.4.1 Script Files
ExcretionDataCleaning.R
This file uses Excretion2023_Raw.csv, Excretion2024_Raw.csv, Excretion2024_QPCR_Raw.csv and TTD2024_Raw.csv. This R script combines and cleans these data files to create Excretion2023.csv and Excretion2024.csv, which are used in Anaylsis.R.

Analysis.R
This file uses Excretion2023.csv and Excretion2024.csv. This R script runs all the statistics described in the manuscript and outputs figures used in the manuscript.

Data Files

Excretion2024_QPCR_Raw.csv 
File Description: This data file contains the qPCR results from the 2024 excretion assay. Each row contains data from an individual tadpole.

Column Description:
ind: A unique tadpole individual number.
pop: This column indicates which population/egg mass each individual tadpole came from. 
nacl: This column has the concentration that the tadpole was exposed to during the excretion assay in g/L. 
FV3: This column indicates if the individual tadpole was exposed to FV3 (RV) or served as a control (CTRL) in the excretion assay.
mass_g: Tadpole mass in grams of the individual tadpoles at the end of the excretion assay.
Stage: The Gosner stage of the tadpoles at the end of the excretion assay.
SVL_mm: Tadpole snout vent length is given here in mm.
ng/ul: The DNA concentration reading from a Nanodrop in ng/ul. This is used to calculate the tadpole viral load.
SQ1: Samples were run in duplicates in qPCR and this column shows the starting quantity for replicate 1.
SQ2: The starting quantity reading from the qPCR duplicate. 
Notes: Any notes about the qPCR or tadpole measurements are made in this column.

TTD2024_Raw.csv
File Description: This data file contains the time to death (TTD) assay from 2024. Unlike 2023, the same tadpoles were not used for both the TTD and excretion assay. Each row contains an individual tadpole’s data.

Column Description:
Pop: This column contains the population/egg mass that the tadpoles were from.
Death_hr: This column has the hour that the tadpole died in during the TTD
Survival: In this column there is a 1 if the tadpole died during the TTD or a 0 if the tadpole survived.
Rep: This column contains the replicated number for each tadpole. It is a column of repeated 1:10 for FV3 treatments and 1:5 for controls.
FV3: This column indicates if the tadpole was a control (CTRL) or exposed to FV3 (RV) during the TTD.

Excretion2023_Raw.csv
File Description: This data file contains the individual tadpole nutrient excretions from the 2023 excretion assay. Each row represents an individual tadpole. Nutrient excretion rates in this csv file have not been corrected yet or transferred into rates used in the publication.

Column Description:
ind: A unique tadpole individual number.
Population: This column indicates which population each individual tadpole came from.
FV3: This column indicates if the individual tadpole was exposed to FV3 (Y) or served as a control (N) in the excretion assay and time to death assay.
TTD_Hr: This column contains which hour the tadpole died during the time to death assay.
SVL_mm: Tadpole snout vent length is given here in mm.
TTD_Mass_g: Tadpole mass in grams of the individual tadpoles at the end of the time to death assay.
Stage: The Gosner stage of the tadpoles at the end of the time to death assay.
Nucleic_Acid_ng_ul: The DNA concentration reading from a Nanodrop in ng/ul. This is used to calculate the tadpole viral load.
SQ1: Samples were run in duplicates in qPCR and this column shows the starting quantity for replicate 1.
SQ2: The starting quantity reading from the qPCR duplicate. 
NH4_ugL: This column contains NH4 (ug/L) results from the tadpole excretion assay.
SRP_ugL: This column contains SRP, soluble reactive phosphorus, (ug/L) results from the tadpole excretion assay.
Avg Mass Extra Individuals (g): Average tadpole mass in grams from five individual per population sacrificed after the nutrient excretion but during the TTD.
Notes: A column where any notes or observations about the tadpole or sample are.

Excretion2024_Raw.csv
File Description: This data file contains the individual tadpole nutrient excretions from the 2024 excretion assay. Each row represents an individual tadpole. Nutrient excretion rates in this csv file have not been corrected yet or transferred into rates used in the publication.

Column Description:
nacl: This column contains the NaCl concentration (mg/L) that the tadpoles were exposed to during the excretion assay. This column ranges from 0 to 1000 mg/L. 
NH4_ugL: This column contains NH4 (ug/L) results from the tadpole excretion assay.
SRP_ugL: This column contains SRP, soluble reactive phosphorus, (ug/L) results from the tadpole excretion assay.
mass_g: Tadpole mass in grams of the individual tadpoles.
ind: A unique tadpole individual number.
pop: This column indicates which population/egg mass each individual tadpole came from.
FV3: This column indicates if the individual tadpole was exposed to FV3 (RV) or served as a control (CTRL) in the excretion assay.
SVL_mm: Tadpole snout vent length is given here in mm.

Excretion2023.csv
File Description: This data file contains the individual tadpole nutrient excretions from the 2023 excretion assay. This data is currently in long format and each tadpoles has three rows for NH4, SRP, and NP Ratio. This file is an output from ExcretionDataCleaning.R.

Column Description:
population: This column indicates which population each tadpole came from. 
ind: A unique tadpole individual number.
Population: This column indicates which population each individual tadpole came from. This column changed all tadpoles that were not exposed to FV3 to a control group to run a cox regression with a single control group instead of individual control groups.
FV3: This column indicates if the individual tadpole was exposed to FV3 (Y) or served as a control (N) in the excretion assay and time to death assay.
TTD_Hr: This column contains which hour the tadpole died during the time to death assay.
SVL_mm: Tadpole snout vent length is given here in mm.
TTD_Mass_g: Tadpole mass in grams of the individual tadpoles at the end of the time to death assay.
Stage: The Gosner stage of the tadpoles at the end of the time to death assay.
Nucleic_Acid_ng_ul: The DNA concentration reading from a Nanodrop in ng/ul. This is used to calculate the tadpole viral load.
SQ1: Samples were run in duplicates in qPCR and this column shows the starting quantity for replicate 1.
SQ2: The starting quantity reading from the qPCR duplicate. 
NH4_ugL: This column contains NH4 (ug/L) results from the tadpole excretion assay.
SRP_ugL: This column contains SRP, soluble reactive phosphorus, (ug/L) results from the tadpole excretion assay.
Avg Mass Extra Individuals (g): Average tadpole mass in grams from five individual per population sacrificed after the nutrient excretion but during the TTD.
NH4_umol_L: This column is the output from converting the NH4_ugL column from ug of NH4 to umol/L of nitrogen. 
NH4_umol: This column is the output from converting NH4 umol/L to umol, which was done by times by the sample size taken (80ml).
SRP_umol_L: This column is the output from converting the SRP_ugL column from ug of SRP to umol/L of phosphorus.
SPR_umol: This column is the output from converting NH4 umol/L to umol, which was done by times by the sample size taken (80ml).
meanSQ: This column shows the average qPCR SQ value for each tadpole. This was used in the viral load calculation.
viralload: This column has the viral load for the individual tadpole. It was calculated with (meanSQ * 200)/ Nucleic_Acid_ng_ul
death: This column indicates if the tadpole died (1) during the time to death assay or survived (0)
HazardRatio: This column has the hazard ratio for each tadpole based on the tadpole’s population, which was calculated using a cox regression.
nutrient: This column indicates which nutrient is shown in the value column for that row.
Value: This column shows the corrected nutrient value for NH4, SRP, and N:P Ratio, which was found by correcting for excretion assay time (2 hours) and average tadpole mass.
Scalenutrient: This column is a scaled version of the value column and is used in the nutrient analysis in the manuscript.

Excretion2024.csv
File Description: This data file contains the individual tadpole nutrient excretions from the 2024 excretion assay. This data is currently in long format and each tadpoles has three rows for NH4, SRP, and NP Ratio. This file is an output from ExcretionDataCleaning.R.

Column Description:
pop: This column indicates which population/eggmass each individual tadpole came from.
FV3: This column indicates if the individual tadpole was exposed to FV3 (RV) or served as a control (CTRL) in the excretion assay.
ind: A unique tadpole individual number.
nacl: This column contains the NaCl concentration (mg/L) that the tadpoles were exposed to during the excretion assay. This column ranges from 0 to 1000 mg/L. 
mass_g: Tadpole mass in grams of the individual tadpoles at the end of the excretion assay.
Svl: snout vent length in mm of the individual tadpole at the end of the excretion assay.
Stage: The Gosner stage of the tadpoles at the end of the time to death assay.
viralload: This column has the viral load for the individual tadpole. It was calculated with (meanSQ * 200)/ Nucleic_Acid_ng_ul
nutrient: This column indicates which nutrient is shown in the value column for that row.
Value: This column shows the corrected nutrient value for NH4, SRP, and N:P Ratio, which was found by correcting for excretion assay time (2 hours) and average tadpole mass.
HazardRatio: This column has the hazard ratio for each tadpole based on the tadpole’s population, which was calculated using a cox regression.
Scalenutrient: This column is a scaled version of the value column and is used in the nutrient analysis in the manuscript.


<img width="470" height="646" alt="image" src="https://github.com/user-attachments/assets/793fbb7f-4895-4218-997c-efc501589475" />
