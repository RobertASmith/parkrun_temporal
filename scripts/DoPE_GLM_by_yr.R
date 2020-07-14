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
               feather,stargazer, kableExtra,jtools)

# source all functions in R folder
source.all(path = "R")

#=====#
# load data
#=====#

dt_parkrun <- readRDS("cleandata/lsoa_df_monthly")
dt_parkrun$year = substr(x = dt_parkrun$month_year,start = 1,stop = 4) %>% as.numeric # create year variable

#====#
# aggregate by year
#====#

dt_parkrun = dt_parkrun[,.(finishers = sum(finishers),
              imd_score = mean(imd_score),
              mn_dstn = mean(access),
              pop_density = mean(pop_density),
              perc_bme = mean(perc_bme),
              total_pop = mean(total_pop),
              perc_non_working_age = mean(perc_non_working_age)
              ),
           by = c("year","lsoa")]

summ(results[[1]], exp = TRUE)


#=====#
# Create model function
#=====#

f_model = function(x) {
  
  # create model based on data.
  model = glm(
    finishers ~ imd_score + perc_bme +  pop_density + mn_dstn + perc_non_working_age,
    data = dt_parkrun,
    family = poisson(link = "log"),
    offset = log(total_pop),
    subset =  which(dt_parkrun$year == x)
  )
  
  return(model)
}

#=====#
# Run model for each year
#=====#

# store as models
models <- lapply(X = 2010:2018, 
                 FUN = f_model)

#=====#
# Create Stargazer plot
#=====#
# manually output this to glm_by_year - must be a better way.
stargazer(models[[1]], 
          models[[2]], 
          models[[3]],
          models[[4]],
          models[[5]],
          models[[6]],
          models[[7]],
          models[[8]],
          models[[9]],
          header = FALSE,
          column.labels	= paste(2010:2018),
          ci=FALSE, ci.level=0.95, #font.size= 9, 
          title="Poisson Log-link GLM Results",
          dep.var.labels = "Participation",
          covariate.labels = c("IMD Score",
                               "Ethnic-Density",
                               "Pop Density",
                               "Distance(km)",
                               "Non-working-age"),
          type = "html",
          apply.coef = exp,
          apply.se   = exp,
          out = "outputs/results.html")



