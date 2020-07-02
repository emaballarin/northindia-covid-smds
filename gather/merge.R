setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")

load("individual_kaggle.Rda")
load("individual_scraped.Rda")


individual_stats <- data.frame(state=character(),
                               date=character(),    # Fix it later!
                               avg_age=numeric(),
                               avg_sex=numeric(),
                               stringsAsFactors=FALSE)


j_i <- rbind(j_i_kaggle, j_i_scraped)


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

# Method dispatch! ;)
is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))

j_i[is.nan(j_i)] <- NA

################################################################################

for (i in 1:length(north_regions))
{
    regional <- j_i[j_i$detectedstate == north_regions[i],]
    regional <- regional[ , !(names(regional) %in% c("detectedstate"))]

    regional_dates <- unique(regional$dateannounced)

    for (j in 1:length(regional_dates))
    {
        avg_age <- 0.0
        avg_sex <- 0.0

        regional_daily <- regional[regional$dateannounced == regional_dates[j],]

        avg_age <- 0.0 + mean(regional_daily$agebracket, na.rm = TRUE)
        avg_sex <- 0.0 + mean(regional_daily$gender, na.rm = TRUE)

        if (avg_sex == 0.0 | is.nan(avg_sex) | is.na(avg_sex))    # Avoid confusing 0 as nothing with 0 as all-male! (pt. 2)
        {
            avg_sex <- NA
        }
        else
        {
            avg_sex = avg_sex - 10.0
        };

        # Add to the dataframe

        individual_stats <- rbind(individual_stats,
                                  data.frame(north_regions[i], regional_dates[j], avg_age, avg_sex))
    }
}

individual_stats[is.nan(individual_stats)] <- NA

remove(j_i)
remove(regional)
remove(regional_daily)
remove(avg_age)
remove(avg_sex)
remove(i)
remove(j)
remove(regional_dates)
remove(north_regions)

individual_stats <- dplyr::rename(individual_stats,
                                  # of which #
                                  state=north_regions.i.,
                                  date=regional_dates.j.)

individual <- individual_stats

save(individual,file="individual.Rda")


