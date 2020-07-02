# LIBRARIES

library(rjson)
library(dplyr)

################################################################################

# HOUSEHOLD TASKS

setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")

################################################################################

# INGEST DATA

j_i_1 <- jsonlite::read_json("raw_data1.json", simplifyVector = TRUE)$raw_data
j_i_2 <- jsonlite::read_json("raw_data2.json", simplifyVector = TRUE)$raw_data
j_i_3 <- jsonlite::read_json("raw_data3.json", simplifyVector = TRUE)$raw_data
j_i_4 <- jsonlite::read_json("raw_data4.json", simplifyVector = TRUE)$raw_data
j_i_5 <- jsonlite::read_json("raw_data5.json", simplifyVector = TRUE)$raw_data
j_i_6 <- jsonlite::read_json("raw_data6.json", simplifyVector = TRUE)$raw_data
j_i_7 <- jsonlite::read_json("raw_data7.json", simplifyVector = TRUE)$raw_data

################################################################################

# UNIFY COLUMN PRESENCE / NAMES

j_i_1 <- j_i_1[ , !(names(j_i_1) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]
j_i_2 <- j_i_2[ , !(names(j_i_2) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]
j_i_3 <- j_i_3[ , !(names(j_i_3) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]
j_i_4 <- j_i_4[ , !(names(j_i_4) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]
j_i_5 <- j_i_5[ , !(names(j_i_5) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]
j_i_6 <- j_i_6[ , !(names(j_i_6) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]
j_i_7 <- j_i_7[ , !(names(j_i_7) %in% c("backupnotes", "entryid", "estimatedonsetdate"))]

################################################################################

# UNIFY DATAFRAMES

j_i <- rbind(j_i_1, j_i_2, j_i_3, j_i_4, j_i_5, j_i_6, j_i_7)

remove(j_i_1)
remove(j_i_2)
remove(j_i_3)
remove(j_i_4)
remove(j_i_5)
remove(j_i_6)
remove(j_i_7)

################################################################################

# SUBSET DATAFRAME

j_i <- j_i[c("agebracket", "dateannounced", "detectedstate", "gender")]

################################################################################

# RESTRICT TO NORTHERN REGIONS

# Standardize names
j_i <- mutate(j_i, detectedstate = recode(detectedstate,
                                            # of which #
                                            'Uttar Pradesh' = 'UttarPradesh',
                                            'Himachal Pradesh' = 'HimachalPradesh',
                                            'Jammu and Kashmir'  = 'JammuKashmir'))

# Define "Northern Regions of India"
north_regions <- c('UttarPradesh',
                   'Chandigarh',
                   'Haryana',
                   'Delhi',
                   'HimachalPradesh',
                   'JammuKashmir',
                   'Ladakh',
                   'Punjab',
                   'Rajasthan',
                   'Uttarakhand')

# Actual filter
j_i <- dplyr::filter(j_i, j_i$detectedstate %in% north_regions)

################################################################################

# REMOVE DATA WHICH ARE NOT INFORMATIVE
# (for our purposes)

j_i <- j_i[j_i$agebracket != "" | j_i$gender != "",]
j_i <- j_i[j_i$dateannounced != "",]

################################################################################

# FIX THE EVERYTHING-IS-A-STRING JSON-NITY

j_i <- transform(j_i, agebracket = as.numeric(agebracket))
j_i <- transform(j_i, dateannounced = as.Date(dateannounced))

# Convert genders to numbers
j_i$gender <- gsub("M", "10.0", j_i$gender)          # Avoid confusing
j_i$gender <- gsub("F", "11.0", j_i$gender)          # 0 as nothing with 0
j_i$gender <- gsub("Non-Binary", "10.5", j_i$gender) # as all-male! (pt. 1)
#j_i$gender <- gsub("", "NA", j_i$gender)
j_i <- transform(j_i, gender = as.numeric(gender))

################################################################################

j_i_scraped <- j_i

save(j_i_scraped,file="individual_scraped.Rda")
