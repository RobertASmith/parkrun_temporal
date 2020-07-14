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
               RColorBrewer,miceadds,lubridate)

# source all functions in R folder
source.all(path = "R")

#============#
#   DATA
#============#


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

lsoa_imd = lsoa_imd[substr(lsoa,start = 1,stop = 1) == "E",]   # restrict to england. 


# 3. LSOA pop-weighted centroid locations

lsoa_locations = shapefile("rawdata/England_lsoa_2011_centroids/england_lsoa_2011_centroids.shp") # read in shape file
lsoa_locations = spTransform(lsoa_locations,
                             crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # use correct projection
lsoa_centr = coordinates(lsoa_locations) # extract coordinates
rownames(lsoa_centr) = lsoa_locations$code

# 4. Ethnicity datasets
lsoa_ethnicity = fread(input = "rawdata/Ethnicity_data/LSOA_Ethnicity.csv",stringsAsFactors = F)
lsoa_ethnicity = lsoa_ethnicity[,
                  .(lsoa = `geography code`,
                    perc_bme = 1- (`Sex: All persons; Age: All categories: Age; Ethnic Group: White: Total; measures: Value`/
                                     `Sex: All persons; Age: All categories: Age; Ethnic Group: All categories: Ethnic group; measures: Value`)
                    )]
lsoa_ethnicity = lsoa_ethnicity[!(grepl("W",lsoa_ethnicity$lsoa)),]   


# 5. parkrun participation data
fill_dat = expand.grid(year = 2011:2019,
                       lsoa = unique(lsoa_imd$lsoa)) %>% data.table # using all English LSOAs from the imd data set

runs_df = readRDS("rawdata/runs_per_lsoa_2010to2020.Rds") %>% as.data.table # read in parkrun participation data; participation is measured in n of idividuals who finish a parkrun event
runs_df = runs_df[grep(pattern = "E",lsoa)] # select LSOAs in England only
runs_df$year = substr(x = runs_df$date ,start = 1,stop = 4) %>% as.numeric
runs_df = runs_df[,.(finishers = sum(finishers)),by = c("year","lsoa")]

# merge template fill_dat with runs data.
runs_df = merge(x = fill_dat,
                y = runs_df, 
                by=c("year","lsoa"), 
                all.x=T)

runs_df$finishers[is.na(runs_df$finishers)] = 0  # filling missings

# 6. Density
lsoa_density = fread("rawdata/Mid-2017 Population Density.csv",
                     stringsAsFactors = F)
lsoa_density = lsoa_density[grep(pattern = "E",`Code`),.(lsoa = `Code`,
                                              pop_density = `People per Sq Km`)]

# 8. Access - want to calculate for each lsoa and year what the distance to nearest event was on 1st January!!



# merge all the data-sets.
lsoa_df = Reduce(function(x, y) merge(x, y, by="lsoa", all=TRUE), 
                 list(runs_df, lsoa_imd, lsoa_ethnicity, lsoa_density ))


#=============#
# CLEAN & TIDY
#=============#


# 1. parkrun participation

# aggregate by lsoa and year (fast version using data.table)
monthly_runs = runs_df[, lapply(.SD, sum), by = month_year, .SDcols = "finishers"]
monthly_runs_by_lsoa = runs_df[, lapply(.SD, sum), by = c("month_year","lsoa"), .SDcols = "finishers"]

# Before we proceed with the analysis of socio economic disparities, 
# we need to account for the missing data 
# LSOAs with no runs are not in the data 
# (some lsoas might even be missing completely, if they never had a participant)
# so we have to fill them in manually:
fill_dat = expand.grid(month_year = unique(monthly_runs_by_lsoa$month_year),
                       lsoa = lsoa_imd$lsoa) %>% data.table # using all English LSOAs from the imd data set

monthly_runs_by_lsoa = merge(fill_dat, 
                             monthly_runs_by_lsoa, 
                             by=c("month_year","lsoa"), 
                             all.x=T)

monthly_runs_by_lsoa$finishers[is.na(monthly_runs_by_lsoa$finishers)] = 0  # filling missings

# proportion of LSOAs that had 0 runs in any given months
# monthly_runs_by_lsoa[, lapply(.SD, function(x){round(sum(x==0)/length(x),4)*100}), by = c("month_year"), .SDcols = "finishers"]


# 2. Access (= distance to the nearst parkrun event)
# compute the distances between all 32,844 lsoas and all 465 parkrun events 
# CAVE: computationally heavy computation!
distM = geosphere::distm(x= lsoa_locations,y=cbind(event_locations$lng,event_locations$lat))
dimnames(distM) <- list(rownames(lsoa_centr),event_locations$course) # set row and column names

## loop to assess access in any given month  (month_years)
# (may take a while to run) - R.S. this is slow because of rbind, but not worth taking time to speed up (should really create matrix prior)
distance.df = c()
months_t = as.Date(paste(unique(monthly_runs_by_lsoa$month_year),
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

head(distance.df)
dim(distance.df)

# merge
distance.df = data.table(distance.df)
monthly_runs_by_lsoa = merge(monthly_runs_by_lsoa,distance.df,by=c("lsoa","month_year"),all.x=T) 


# 3. now we cmobine the data set with information on LSOA IMD deciles
lsoa_imd = data.table(lsoa_imd)
monthly_runs_by_lsoa = merge(monthly_runs_by_lsoa,lsoa_imd,by="lsoa")
# length(unique(monthly_runs_by_lsoa$lsoa))

# 4. the SII will be computed later on - see below



# Rob's analysis











