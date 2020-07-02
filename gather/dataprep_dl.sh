#!/bin/zsh

# Cleanup (1)
#rm -f ./raw_data1.json ./raw_data2.json ./raw_data3.json ./raw_data4.json ./raw_data5.json ./raw_data6.json ./raw_data7.json ./covid19-in-india.zip ./AgeGroupDetails.csv ./HospitalBedsIndia.csv ./ICMRTestingLabs.csv ./IndividualDetails.csv ./StatewiseTestingDetails.csv ./covid_19_india.csv ./population_india_census2011.csv

# DL from Kaggle
kaggle datasets download -d sudalairajkumar/covid19-in-india
unzip ./covid19-in-india.zip

# Cleanup (2)
#rm -f ./covid19-in-india.zip ./AgeGroupDetails.csv ./HospitalBedsIndia.csv ./ICMRTestingLabs.csv ./covid_19_india.csv ./population_india_census2011.csv

# DL from Covid-2019-India
aria2c https://api.covid19india.org/raw_data1.json
aria2c https://api.covid19india.org/raw_data2.json
aria2c https://api.covid19india.org/raw_data3.json
aria2c https://api.covid19india.org/raw_data4.json
aria2c https://api.covid19india.org/raw_data5.json
aria2c https://api.covid19india.org/raw_data6.json
aria2c https://api.covid19india.org/raw_data7.json

# Preprocess from Covid-2019-India
Rscript ./cindia_preproc.R

# Cleanup (3)
#rm -f ./raw_data1.json ./raw_data2.json ./raw_data3.json ./raw_data4.json ./raw_data5.json ./raw_data6.json ./raw_data7.json

# Preprocess from Kaggle
Rscript ./kaggle_preproc.R
Rscript ./statek.R

# Cleanup (4)
#rm -f ./IndividualDetails.csv ./StatewiseTestingDetails.csv

# Data merge (1)
Rscript ./merge.R

# Cleanup (5)
#rm -f ./individual_kaggle.Rda ./individual_scraped.Rda

# Data merge (2)
Rscript join.R

# Cleanup (6)
#rm -f ./individual.Rda ./statewise.Rda

# Data subsetting
Rscript additional_dataprep.R

# Data merge (3)
Rscript final_merge.R

# Cleanup (7)
#rm -f ./d_s_data.Rda ./date_state_info.Rda

# Restructure folders
#rm -R -f ./clean_dataframes
#rm -f ../ANALYSIS/final_dataframe.Rda
mkdir ./clean_dataframes
mv ./*.Rda ./clean_dataframes/
cp -f ./clean_dataframes/final_dataframe.Rda ../ANALYSIS/
cp -f ./clean_dataframes/state_info.Rda ./

# Final cleanup
#rm -f ./individual_kaggle.Rda ./individual_scraped.Rda ./individual.Rda ./statewise.Rda ./d_s_data.Rda ./date_state_info.Rda
