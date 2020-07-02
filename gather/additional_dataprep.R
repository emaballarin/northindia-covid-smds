setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")

load("state_info.Rda")
load("date_state_info.Rda")

# Remove missing DeltaPos/Neg/Swabs
d_s_data <- date_state_info[date_state_info$date >= "2020-04-01",]
d_s_data <- d_s_data[!is.na(d_s_data$deltapos),]

# Manual corrections
d_s_data <- d_s_data[d_s_data$deltapos >= 0,]
d_s_data <- d_s_data[d_s_data$deltaneg >= 0,]
d_s_data <- d_s_data[d_s_data$deltaswabs >= 0,]


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

n_reg_avgage_noladakh <- c(25.571, 28.396, 27.739, 27.959, 30.402, 26.221, 29.991, 25.935, 27.595)

# Ladakh was created by Parliamentary action in 2019 (no census)
avg_ladakh <- mean(n_reg_avgage_noladakh)
n_reg_avgage <- c(25.571, 28.396, 27.739, 27.959, 30.402, 26.221, avg_ladakh, 29.991, 25.935, 27.595)


# Replace Age/Sex NA with census average

for (i in 1:length(north_regions))
{
    if (i != 7)
    {
        current_state_ds <- state_info[state_info$state == north_regions[i],]
        avgsex <- current_state_ds$Females / (current_state_ds$Females + current_state_ds$Males)
    }
    else
    {
        # Ladakh
        avgsex <- 0.4804
    }
    avgage <- n_reg_avgage[i]

    sex_table <- d_s_data[d_s_data$state == north_regions[i],][is.na(d_s_data[d_s_data$state == north_regions[i],]$avg_sex),]
    age_table <- d_s_data[d_s_data$state == north_regions[i],][is.na(d_s_data[d_s_data$state == north_regions[i],]$avg_age),]

    if (nrow(sex_table) != 0)
    {
        sex_table$avg_sex <- avgsex
        d_s_data[d_s_data$state == north_regions[i],][is.na(d_s_data[d_s_data$state == north_regions[i],]$avg_sex),]$avg_sex <- sex_table$avg_sex
    }

    if (nrow(age_table) != 0)
    {
        age_table$avg_age <- avgage
        d_s_data[d_s_data$state == north_regions[i],][is.na(d_s_data[d_s_data$state == north_regions[i],]$avg_age),]$avg_age <- age_table$avg_age
    }
}

firstdate <- as.Date("2020-03-31")
d_s_data$date <- as.numeric(d_s_data$date - firstdate)

save(d_s_data,file="d_s_data.Rda")
