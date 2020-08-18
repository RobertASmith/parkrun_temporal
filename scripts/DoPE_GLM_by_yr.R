#===================#
# Socioeconomic inequalities in parkrun access and participation: 2010 to 2019 
# Robert Smith, Paul Schneider & Rami Cosulich
# Institution: University of Sheffield                    
# Contact: rasmith3@sheffield.ac.uk
# Funder: Wellcome Trust Doctoral Training Centre in Public Health Economics and Decision Science [108903]
#=================#

#============#
# AIM:
# This code uses the cleaned data with LSOA characteristics including:
# parkrun finishers by month, imd score, imd_decile, population, 
# percentage non working age, ethnic density, population density and access.
# It runs two models for each year from 2010 to 2019, one a Poisson regression model and the other a pseudo-poisson 
# The other R file contains the code for the plots.
#============#

#============#
#   SETUP
#============#

# clear the environment
rm(list = ls())

# load all necessary packages
pacman::p_load(dplyr,reshape2,data.table,date,
               raster,geosphere,ggplot2,scales,
               RColorBrewer,miceadds,lubridate,
               feather,stargazer, kableExtra,jtools,lme4)

# source all functions in R folder
source.all(path = "R")

#=====#
# load data
#=====#

dt_parkrun_month <- readRDS("cleandata/lsoa_df_monthly19.Rds")
dt_parkrun_month$year = substr(x = dt_parkrun_month$month_year,start = 1,stop = 4) %>% as.numeric # create year variable

#====#
# descriptive stats
#====#

summary(dt_parkrun_month[month_year == "2010-01"])


#====#
# aggregate by year
#====#

dt_parkrun_yr = dt_parkrun_month[,.(finishers = sum(finishers),
              imd_score = mean(imd_score),
              mn_dstn = mean(access),
              pop_density = mean(pop_density),
              ethnic_density = mean(ethnic_density),
              total_pop = mean(total_pop),
              perc_non_working_age = mean(perc_non_working_age)
              ),
           by = c("year","lsoa")]


#=====#
# Create model function
#=====#

f_model = function(x) {
  
  if(model == "quasi"){
  
  # create model based on data.
  model = glm(
    finishers ~ imd_score + ethnic_density +  pop_density + mn_dstn + perc_non_working_age,
    data = dt_parkrun_yr,
    family = quasipoisson(link = "log"),
    offset = log(total_pop),
    subset =  which(dt_parkrun_yr$year == x)
  )
  
  }else{
    
  model = glm(
    finishers ~ imd_score + ethnic_density +  pop_density + mn_dstn + perc_non_working_age,
    data = dt_parkrun_yr,
    family = poisson(link = "log"),
    offset = log(total_pop),
    subset =  which(dt_parkrun_yr$year == x)
  )
  
  }
  
  return(model)
}

#=====#
# Run model for each year
#=====#
model = "quasi"
# store as models
models_quasipoisson <- lapply(X = 2010:2019,FUN = f_model)

model = "poisson"
models_poisson <- lapply(X = 2010:2019,FUN = f_model)

#=====#
# Create Stargazer plot
#=====#

# manually output this to glm_by_year - must be a better way.

# Poisson Regression
stargazer(models_poisson[[1]], 
          models_poisson[[2]], 
          models_poisson[[3]],
          models_poisson[[4]],
          models_poisson[[5]],
          models_poisson[[6]],
          models_poisson[[7]],
          models_poisson[[8]],
          models_poisson[[9]],
          models_poisson[[10]],
          header = FALSE,
          column.labels	= paste(2010:2019),
          ci=FALSE, 
          ci.level=0.95, #font.size= 9, 
          title="Results of the Poisson log-link generalised linear model for each year from 2010 to 2019.",
          dep.var.labels = "Participation",
          covariate.labels = c("IMD Score",
                               "Ethnic-Density",
                               "Pop Density",
                               "Distance(km)",
                               "Non-working-age"),
          type = "latex"
          #apply.coef = exp,
          #apply.se   = exp,
          #out = "outputs/results_poisson.html"
          )

#Quasiregression
# Poisson Regression
stargazer(models_quasipoisson[[1]], 
          models_quasipoisson[[2]], 
          models_quasipoisson[[3]],
          models_quasipoisson[[4]],
          models_quasipoisson[[5]],
          models_quasipoisson[[6]],
          models_quasipoisson[[7]],
          models_quasipoisson[[8]],
          models_quasipoisson[[9]],
          models_quasipoisson[[10]],
          header = FALSE,
          column.labels	= paste(2010:2019),
          ci=FALSE, 
          ci.level=0.95, #font.size= 9, 
          title="Results of the quasipoisson log-link generalised linear model for each year from 2010 to 2019.",
          dep.var.labels = "Participation",
          covariate.labels = c("IMD Score",
                               "Ethnic-Density",
                               "Pop Density",
                               "Distance(km)",
                               "Non-working-age"),
          type = "latex"
          #apply.coef = exp,
          #apply.se   = exp,
          #out = "outputs/results_quasi.html"
)