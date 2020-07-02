# LIBRARIES

library(readr)
library(dplyr)

################################################################################

# HOUSEHOLD TASKS

setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")

################################################################################

# INGEST DATA

j_i <- readr::read_csv("IndividualDetails.csv",
                                            col_types = cols(
                                                id = col_skip(),                                   # Useless, just index
                                                government_id = col_skip(),                        # Irrelevant
                                                diagnosed_date = col_character(),    # Use ISO-compliant format
                                                age = col_character(),                               # Numeric
                                                gender = col_character(),                          # Do not factorize early
                                                detected_city = col_skip(),
                                                detected_district = col_skip(),
                                                current_status = col_skip(),                       # Not relevant for the task
                                                status_change_date = col_skip(),                   # Not relevant for the task
                                                notes = col_skip(),
                                                nationality = col_skip()
                                            )
)

################################################################################

# RENAME DATAFRAME COLS

j_i  <- dplyr::rename(j_i,
                                        # of which #
                                        agebracket=age,
                                        detectedstate=detected_state,
                                        dateannounced=diagnosed_date)

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
j_i <- j_i[!is.nan(j_i$agebracket) | !is.nan(j_i$gender) != "",]
j_i <- j_i[!is.na(j_i$agebracket) | !is.na(j_i$gender) != "",]
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

j_i_kaggle <- j_i

save(j_i_kaggle,file="individual_kaggle.Rda")
