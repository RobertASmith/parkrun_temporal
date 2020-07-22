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
               feather,stargazer, kableExtra,jtools,lme4)

# source all functions in R folder
source.all(path = "R")

#=====#
# load data
#=====#

dt_parkrun_month <- readRDS("cleandata/lsoa_df_monthly19.Rds")
dt_parkrun_month$year = substr(x = dt_parkrun_month$month_year,start = 1,stop = 4) %>% as.numeric # create year variable

#====#
# aggregate by year
#====#

dt_parkrun_yr = dt_parkrun_month[,.(finishers = sum(finishers),
              imd_score = mean(imd_score),
              mn_dstn = mean(access),
              pop_density = mean(pop_density),
              perc_bme = mean(perc_bme),
              total_pop = mean(total_pop),
              perc_non_working_age = mean(perc_non_working_age)
              ),
           by = c("year","lsoa")]
# PS: ?
# summ(results[[1]], exp = TRUE)


#=====#
# Create model function
#=====#

f_model = function(x) {
  
  # create model based on data.
  model = glm(
    finishers ~ imd_score + perc_bme +  pop_density + mn_dstn + perc_non_working_age,
    data = dt_parkrun_yr,
    family = poisson(link = "log"),
    offset = log(total_pop),
    subset =  which(dt_parkrun_yr$year == x)
  )
  
  return(model)
}

#=====#
# Run model for each year
#=====#

# store as models
models <- lapply(X = 2010:2019, 
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
          models[[10]],
          header = FALSE,
          column.labels	= paste(2010:2019),
          ci=FALSE, ci.level=0.95, #font.size= 9, 
          title="Poisson Log-link GLM Results",
          dep.var.labels = "Participation",
          covariate.labels = c("IMD Score",
                               "Ethnic-Density",
                               "Pop Density",
                               "Distance(km)",
                               "Non-working-age"),
          type = "html",
          #apply.coef = exp,
          #apply.se   = exp,
          out = "outputs/results.html")


#=============
# ECONOMETRIC MODELS
#=============

dt_parkrun_month$obs_n = dt_parkrun_month$month_year %>%  as.factor() %>% as.numeric
dt_parkrun_month$mn_dstn = dt_parkrun_month$access

# 1. simple model adding month as a interaction term:
panel_model = glm(
    finishers ~ imd_score*obs_n + perc_bme +  pop_density + mn_dstn + perc_non_working_age,
    data = dt_parkrun_month,
    family = poisson(link = "log"),
    offset = log(total_pop)
    )

stargazer(panel_model,
          header = FALSE,
          ci=FALSE, ci.level=0.95, #font.size= 9, 
          title="Poisson Log-link GLM Results",
          dep.var.labels = "Participation",
          #covariate.labels = c("IMD Score",
          #                     "Ethnic-Density",
          #                     "Pop Density",
          #                     "Distance(km)",
          #                     "Non-working-age"),
          type = "html",
          apply.coef = exp,
          apply.se   = exp,
          out = "outputs/time_interaction_result.html")

# 2. Can use a random effects model (GLMM) instead:
# The Generalized Linear Mixed Model (GLMM) is an extension of the Generalized Linear Model (GLM) complicated by random effects
# We use the glmer {lme4} function in R to reflect this.

re_model2 = glmer(formula = finishers ~ 1+ year + perc_bme +  pop_density + mn_dstn + perc_non_working_age + (year | imd_score),
      data = dt_parkrun_yr, 
      family = poisson(link = "log"),
      offset = log(total_pop))

stargazer(panel_model2,
          header = FALSE,
          ci=FALSE, ci.level=0.95, #font.size= 9, 
          title="Random Effects Poisson Log-link GLM Results",
          #covariate.labels = c("IMD Score",
          #                     "Ethnic-Density",
          #                     "Pop Density",
          #                     "Distance(km)",
          #                     "Non-working-age"),
          type = "html",
          apply.coef = exp,
          apply.se   = exp,
          out = "outputs/time_interaction_result2.html")

# 3. Another attempt (using a sample)
dt_parkrun_yr

re_model2 = glm(formula = finishers ~ year + imd_score + perc_bme +  pop_density + mn_dstn + perc_non_working_age +
                                      year:imd_score + year:perc_bme + year:mn_dstn,
                data = dt_parkrun_yr, 
                family = poisson(link = "log"),
                offset = log(total_pop))












