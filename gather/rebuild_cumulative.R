setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")
load("final_dataframe.Rda")

final_dataframe[,"pos"] <- NA

################################################################################

states <- unique(final_dataframe$state)

for (j in 1:length(states))
{
    dataf <- final_dataframe[final_dataframe$state == states[j],]
################################################################################

    for (i in 1:length(dataf$deltapos))
    {
        if (i == 1)
        {
            cnt <- as.numeric(dataf[i,][5])
        }
        else
        {
            cnt <- as.numeric(final_dataframe[final_dataframe$state == states[j],][i-1,][13]) + as.numeric(dataf[i,][5])
        };

        final_dataframe[final_dataframe$state == states[j],][i,]$pos[1] <- as.numeric(cnt)
    }
################################################################################
    remove(cnt)
################################################################################
}
