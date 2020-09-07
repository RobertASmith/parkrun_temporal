rm(list=ls())

library(data.table)

dt_imd_10 <- fread("rawdata/IMD/imd_2010.csv")[,.(`LSOA CODE`,`IMD SCORE`)]
dt_imd_15 <- fread("rawdata/IMD/imd_2015.csv")[,.(`LSOA code (2011)`,`Index of Multiple Deprivation (IMD) Score`)]
dt_imd_19 <- fread("rawdata/IMD/imd_2019.csv")[,.(`LSOA code (2011)`,`Index of Multiple Deprivation (IMD) Score`)]

# change column names
colnames(dt_imd_10) <- c("code","imd_10")
colnames(dt_imd_15) <- c("code","imd_15")
colnames(dt_imd_19) <- c("code","imd_19")

# note that some LSOA changed their boundaries, 31672 stayed the same... we could use just these.
dt <- merge(x = dt_imd_10,dt_imd_15, by = "code",all = FALSE)
dt <- merge(dt,dt_imd_19, by = "code", all = FALSE)

# can see they are very closely correlated
plot(dt$imd_15,dt$imd_19)

# less so for 2010 to 2019.
cor(dt[,2:4])

# can plot this fancy:
library("PerformanceAnalytics")
pdf("outputs/correlation_plot.pdf") 
chart.Correlation(R = dt[,2:4], 
                  histogram=TRUE, 
                  pch=19)
dev.off()


