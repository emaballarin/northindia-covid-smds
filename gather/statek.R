library(readr)
library(dplyr)

# HOUSEHOLD TASKS

setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")

################################################################################

all_statewise             <- readr::read_csv("StatewiseTestingDetails.csv",
                                             col_types = cols(
                                                 Date = col_date(format = "%Y-%m-%d"),    # Use ISO-compliant format
                                                 TotalSamples = col_double(),
                                                 Negative = col_double(),
                                                 Positive = col_double()
                                             )
)

all_statewise            <-
    mutate(
        all_statewise,
        State                           = recode(
            State,
            'Uttar Pradesh'      = 'UttarPradesh',
            'Himachal Pradesh'   = 'HimachalPradesh',
            'Jammu and Kashmir'  = 'JammuKashmir'
        )
    )

## DATA FILTERING ##

north_regions <- c('UttarPradesh',    # `Uttar Pradesh`
                   'Chandigarh',
                   'Haryana',
                   'Delhi',
                   'HimachalPradesh', # `Himachal Pradesh`
                   'JammuKashmir',    # `Jammu & Kashmir` || `Jammu and Kashmir`
                   'Ladakh',
                   'Punjab',
                   'Rajasthan',
                   'Uttarakhand')

statewise <- dplyr::filter(all_statewise,
                           all_statewise$State %in% north_regions)
remove(all_statewise)

statewise <- dplyr::rename(statewise,
                           date=Date,
                           state=State,
                           swabs=TotalSamples,
                           pos=Positive,
                           neg=Negative)


# CARRY THE LAST instead of NA - POS
curReg <- " "
cursor <- " "
cnt <- 0

for (i in 1:length(statewise$pos))
{
    cursor <- statewise$state[i]

    if (cursor != curReg)
    {
        cnt <- statewise$pos[i]
    }
    else
    {
        cnt <- statewise$pos[i] - statewise$pos[i-1]
    };

    if (is.na(statewise$pos[i]))
    {
        statewise$pos[i] <- statewise$pos[i-1]
    }

    curReg <- statewise$state[i]
}

remove(curReg)
remove(cursor)
remove(cnt)
remove(i)



# CARRY THE LAST instead of NA - NEG
curReg <- " "
cursor <- " "
cnt <- 0

for (i in 1:length(statewise$neg))
{
    cursor <- statewise$state[i]

    if (cursor != curReg)
    {
        cnt <- statewise$neg[i]
    }
    else
    {
        cnt <- statewise$neg[i] - statewise$neg[i-1]
    };

    if (is.na(statewise$neg[i]))
    {
        statewise$neg[i] <- statewise$neg[i-1]
    }

    curReg <- statewise$state[i]
}

remove(curReg)
remove(cursor)
remove(cnt)
remove(i)

# CARRY THE LAST instead of NA - SWABS
curReg <- " "
cursor <- " "
cnt <- 0

for (i in 1:length(statewise$swabs))
{
    cursor <- statewise$state[i]

    if (cursor != curReg)
    {
        cnt <- statewise$swabs[i]
    }
    else
    {
        cnt <- statewise$swabs[i] - statewise$swabs[i-1]
    };

    if (is.na(statewise$swabs[i]))
    {
        statewise$swabs[i] <- statewise$swabs[i-1]
    }

    curReg <- statewise$state[i]
}

remove(curReg)
remove(cursor)
remove(cnt)
remove(i)


# Compute the variation of positive swabs w.r.t. the previous day
curReg <- " "
cursor <- " "
cnt <- 0

for (i in 1:length(statewise$pos))
{
    cursor <- statewise$state[i]

    if (cursor != curReg)
    {
        cnt <- statewise$pos[i]
    }
    else
    {
        cnt <- statewise$pos[i] - statewise$pos[i-1]
    };

    statewise$deltapos[i] <- cnt
    curReg <- statewise$state[i]
}

remove(curReg)
remove(cursor)
remove(cnt)
remove(i)


# Compute the variation of negative swabs w.r.t. the previous day
curReg <- " "
cursor <- " "
cnt <- 0

for (i in 1:length(statewise$neg))
{
    cursor <- statewise$state[i]

    if (cursor != curReg)
    {
        cnt <- statewise$neg[i]
    }
    else
    {
        cnt <- statewise$neg[i] - statewise$neg[i-1]
    };

    statewise$deltaneg[i] <- cnt
    curReg <- statewise$state[i]
}

remove(curReg)
remove(cursor)
remove(cnt)
remove(i)

# Compute the variation of administered swabs w.r.t. the previous day
curReg <- " "
cursor <- " "
cnt <- 0

for (i in 1:length(statewise$swabs))
{
    cursor <- statewise$state[i]

    if (cursor != curReg)
    {
        cnt <- statewise$swabs[i]
    }
    else
    {
        cnt <- statewise$swabs[i] - statewise$swabs[i-1]
    };

    statewise$deltaswabs[i] <- cnt
    curReg <- statewise$state[i]
}

remove(curReg)
remove(cursor)
remove(cnt)
remove(i)

statewise <- statewise[c("date", "state", "deltapos", "deltaneg", "deltaswabs", "pos")]

statewise <- statewise %>% relocate("pos", .after = last_col())    # Move col to end

save(statewise,file="statewise.Rda")
