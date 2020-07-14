#===================#
# DoPE TEMPORAL   
# Robert Smith & Paul Schneider                           
# Institution: University of Sheffield                    
# Contact: rasmith3@sheffield.ac.uk
# Funder: Wellcome Trust
#=================#

#============#
#   SETUP
#============#

# clear the environment
rm(list = ls())

# load all necessary packages
pacman::p_load(dplyr,reshape2,data.table,date,
               raster,geosphere,ggplot2,scales,
               RColorBrewer,miceadds,lubridate,
               feather)

# source all functions in R folder
source.all(path = "R")

#============#
#   DATA
#============#

# data is loaded from multiple sources
# 1. event locations from our web scraped data
# 2. IMD data from the ONS
# 3. LSOA centroids from ONS
# 4. Ethnicity from the ONS
# 5. Number of finishers by week & event from parkrun.
# 6. Population density from ONS
# 7. Access - calculated here based on LSOA centoids & event locations data.


# 1. parkrun event locations

event_locations = read.csv("rawdata/event_info_20181212.csv") # read in data
event_locations$month_year = substr(event_locations$Estblsh,1,7) # get month and year
event_locations$Estblsh = as.Date(event_locations$Estblsh) # convert date

# 2. 2015 IMD data: imd, total population and percent non working age.  

lsoa_imd = fread("rawdata/IMD_data.csv")[
  ,.(lsoa = `LSOA code (2011)`,
     imd_score = `Index of Multiple Deprivation (IMD) Score`,
     imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
     total_pop = `Total population: mid 2012 (excluding prisoners)`,
     perc_non_working_age = 1 - (`Working age population 18-59/64: for use with Employment Deprivation Domain (excluding prisoners)`/`Total population: mid 2012 (excluding prisoners)`))
] # only retain the columns we need

lsoa_imd = lsoa_imd[substr(lsoa,start = 1,stop = 1) == "E",]   # restrict to England. 

# weird - there exist 10 locations in which the population working age exceeds total, in this case make 0% non working age
lsoa_imd$perc_non_working_age[lsoa_imd$perc_non_working_age<0] = 0

# 3. LSOA pop-weighted centroid locations

lsoa_locations = shapefile("rawdata/England_lsoa_2011_centroids/england_lsoa_2011_centroids.shp") # read in shape file
lsoa_locations = spTransform(lsoa_locations,
                             crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # use correct projection
lsoa_centr = coordinates(lsoa_locations) # extract coordinates
rownames(lsoa_centr) = lsoa_locations$code

# 4. Ethnicity data-sets

lsoa_ethnicity = fread(input = "rawdata/Ethnicity_data/LSOA_Ethnicity.csv",
                       stringsAsFactors = F)
lsoa_ethnicity = lsoa_ethnicity[,
                                .(lsoa = `geography code`,
                                  perc_bme = 1- (`Sex: All persons; Age: All categories: Age; Ethnic Group: White: Total; measures: Value`/
                                                   `Sex: All persons; Age: All categories: Age; Ethnic Group: All categories: Ethnic group; measures: Value`)
                                )]
lsoa_ethnicity = lsoa_ethnicity[!(grepl("W",lsoa_ethnicity$lsoa)),]   


# 5. parkrun participation data

runs_df = readRDS("rawdata/runs_per_lsoa_2010to2020.Rds") %>% as.data.table # read in parkrun participation data; participation is measured in n of idividuals who finish a parkrun event
runs_df = runs_df[grep(pattern = "E",lsoa)] # select LSOAs in England only
runs_df$month_year = substr(runs_df$date,1,7) # create a month & year variable
runs_df$date = as.Date(unclass(runs_df$date),format="%Y-%m-%d") # convert to Date class
runs_df = runs_df[weekdays(runs_df$date) == "Saturday"] # restrict to saturday events only
runs_df = runs_df[runs_df$date <= as.Date("2018-12-12"),] # restrict to events before 2019
runs_df = runs_df[,.(finishers = sum(finishers)),by = c("month_year","lsoa")]

# merge template fill_dat with runs data.
fill_dat = expand.grid(month_year = unique(runs_df$month_year),
                       lsoa = unique(lsoa_imd$lsoa)) %>% data.table # using all English LSOAs from the imd data set
runs_full = merge(x = fill_dat,
                  y = runs_df, 
                  by=c("month_year","lsoa"), 
                  all.x=T)

runs_full$finishers[is.na(runs_full$finishers)] = 0  # filling missing data

# 6. Density
lsoa_density = fread("rawdata/Mid-2017 Population Density.csv",
                     stringsAsFactors = F)
lsoa_density = lsoa_density[grep(pattern = "E",`Code`),
                            .(lsoa = `Code`,
                              pop_density = `People per Sq Km`)]

# 8. Access - want to calculate for each lsoa and year what the distance to nearest event was on 1st January!!
distM = geosphere::distm(x= lsoa_locations,
                         y=cbind(event_locations$lng,
                                 event_locations$lat))

dimnames(distM) <- list(rownames(lsoa_centr),event_locations$course) # set row and column names

## loop to assess access in any given month  (month_years)
# (may take a while to run) - R.S. this is slow because of rowbind, but not worth taking time to speed up (should really create matrix prior)
distance.df = c()
months_t = as.Date(paste(unique(runs_full$month_year),
                         "-15",
                         sep="")) # middle of the month

for(t in months_t){
  # create a date
  t = as.Date(t, origin = as.Date("1970-01-01"))
  
  # cat("\r  at:",as.character(t))
  events_in_operation = event_locations$Estblsh <= t # which parkrun events were active in month t?
  
  temp.dist = get_min_dist(available_event_cols = events_in_operation,
                           month_year = substr(t,1,7),
                           distance_matrix = distM)
  
  distance.df = rbind(distance.df,temp.dist)
}


#=========#
# MERGE DATA
#=========#


# merge all the data-sets except distance to nearest event
lsoa_df_monthly = Reduce(function(x, y) merge(x, y, by="lsoa", all=TRUE), 
                         list(runs_full, lsoa_imd, lsoa_ethnicity, lsoa_density ))

# merge distance to nearest event
lsoa_df_monthly = merge(lsoa_df_monthly,distance.df,by=c("lsoa","month_year"))


#=========#
# SAVE DATASETS
#=========#

# save files to cleandata
saveRDS(object = lsoa_df_monthly,file = "cleandata/lsoa_df_monthly")
write_feather(x = lsoa_df_monthly,path = "cleandata/lsoa_df_monthly_feather")

# checking data-set
temp <- readRDS("cleandata/lsoa_df_monthly")

summary(temp)

