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
# It creates the plots seen in the publication.
# The other R file contains the code for the GLM Poisson regression model.
#============#


#============#
#   SETUP
#============#

# clear the environment
rm(list = ls())

# load all necessary packages
pacman::p_load(dplyr,reshape2,data.table,date,
               raster,geosphere,ggplot2,scales,
               RColorBrewer,lubridate,feather,miceadds,
               stargazer, kableExtra, jtools)

# source all functions in R folder
source.all(path = "R")

#=====#
# load data
#=====#

dt_parkrun <- readRDS("cleandata/lsoa_df_monthly19.Rds")
dt_parkrun$year = substr(x = dt_parkrun$month_year,start = 1,stop = 4) %>% as.numeric # create year variable


#====#
# IMD quintile cuts
#====#
imd_quintiles_cuts = quantile(dt_parkrun$imd_score,probs = seq(0,1,0.2))
dt_parkrun$imd_q5 = cut(dt_parkrun$imd_score, imd_quintiles_cuts, include.lowest = T)
levels(dt_parkrun$imd_q5) =  c("Least deprived 20%","4", "3", "2", "Most deprived 20%")
imd_colors = c("orangered","orange","yellow3","yellowgreen","lawngreen")
imd_colors = imd_colors[1:length(imd_colors)]
# reverse order as per steve's comments
dt_parkrun$imd_q5 <- factor(dt_parkrun$imd_q5,levels = rev(c("Least deprived 20%","4", "3", "2", "Most deprived 20%")))

#=====#
# Figure 1: nearest distances by imd by month year
#=====#

fig1_df = agg_parkrun_stat(dt_parkrun,y="access")
plot1 = ggplot(fig1_df,aes(x=plot_date,
                           y=access,
                           col=imd_q5)) +
  geom_point(size=0.2)+
  geom_line() +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  ylab("Mean distance to the nearest parkrun event (km)") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",name="") +
  scale_y_continuous(trans = log2_trans()) +
  theme_classic()+
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))


# store in outputs
ggsave(plot = plot1,width = 10,
       filename = "outputs/Figure1_access.pdf")

#=====#
# Table 4: nearest distances by imd by year
#=====#

tbl4_df = aggregate(access ~ imd_q5 + year, dt_parkrun, make_parkrun_tbl)
tbl4_general = aggregate(access ~ year, dt_parkrun, make_parkrun_tbl)
tbl4_general$imd_q5 = "Overall"
tbl4 = rbind(tbl4_general,tbl4_df)
tbl4 = reshape2::dcast(tbl4, imd_q5~year,value.var = "access")
table4 = tbl4[c(6,5,1,2,3,4),]

# store in outputs
stargazer(summary = FALSE,
          x = table4,
          title = "Mean geodesic distance to the nearest parkrun event by IMD quintile for each year from 2010 to 2019.",
          format = "latex",
          #out = "outputs/Table4_access.html",
          notes = "1 = most socioeconomically deprived quintile, 5 = least socioeconomically deprived quintile, Standard errors in parentheses.")

#=====#
# Figure 2: parkrun participation by imd by month year
#=====#

fig2_df = agg_parkrun_stat(dt_parkrun,y="finishers")
#fig2_df$imd_q5 <- factor(fig2_df$imd_q5,
#                  levels = c("Most deprived 20%","2","3","4","Least deprived 20%","Overall"))
#fig2_df$imd_q5[fig2_df$imd_q5 != "Overall"] <- factor(fig2_df$imd_q5[fig2_df$imd_q5 != "Overall"],
#                                                      levels = rev(levels(fig2_df$imd_q5[fig2_df$imd_q5 != "Overall"])))

plot2 <- ggplot(fig2_df,aes(x=plot_date,
                            y=finishers,
                            col=imd_q5)) +
  ylab(label = "Monthly finishers per 1000 residents")+
  geom_line(alpha=0.7,size=0.3) +
  geom_smooth(alpha=1,se=F) +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  # ylab("Mean distance to the nearest parkrun event") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",name="") +
  theme_classic()+
  theme(legend.justification = c(1, 1), 
        legend.position = c(0.3, 0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_text(size=14),
        axis.title=element_text(size=16))

# store in outputs
ggsave(plot = plot2,
       width = 10,
       filename = "outputs/Figure2_participation.pdf")



#=====#
# Table 5: parkrun participation by imd by year
#=====#

tbl5_df = aggregate(finishers ~ imd_q5 + year, dt_parkrun, make_parkrun_tbl)
tbl5_general = aggregate(finishers ~ year, dt_parkrun, make_parkrun_tbl)
tbl5_general$imd_q5 = "Overall"
tbl5 = rbind(tbl5_general,tbl5_df)
tbl5 = reshape2::dcast(tbl5, imd_q5~year,value.var = "finishers")
table5 = tbl5[c(6:1),]


# store in outputs
stargazer(summary = FALSE,
          x = table5,
          title = "Mean monthly parkrun finishers per 1,000 persons by IMD quintile for each year from 2010 to 2019.",
          format = "latex",
          #out = "outputs/Table5_participation.html",
          notes = "1 = most socioeconomically deprived quintile, 5 = least socioeconomically deprived quintile, Standard errors in parentheses.")


# IMD RANGE

IMD_RANGE = range(dt_parkrun$imd_score)

#=====#
# Figure 3: Ratio Index of Inequality: Access
#=====#
fig3 = data.frame(month_year = unique(dt_parkrun$month_year))

fig3$rii = apply(X = fig3,
      MARGIN = 1,
       FUN = function(m){

         lm.temp = lm(data = subset(dt_parkrun,month_year == m),
                      formula = access ~ imd_score)
        
         pred.temp = predict(lm.temp,
                             newdata = data.frame(imd_score = IMD_RANGE))
         
         tb_ratio.temp = pred.temp[1] / pred.temp[2]
         
       return(tb_ratio.temp)
         
         })

fig3$month_year = as.Date(paste(fig3$month_year,"01",sep="-"))

plot3 = ggplot(fig3,aes(x=month_year,y=rii)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  ylab("RII - Distance to the nearest parkrun event") +
  xlab("Year")+
  theme_classic() +
  theme(axis.text   = element_text(size=14),
        axis.title  = element_text(size=16),
        axis.title.x = element_blank())

# store in outputs
ggsave(plot = plot3,width = 10,
       filename = "outputs/Figure3_rii_access.pdf")




#=====#
# Unused in publication - numerical results: Ratio Index of Inequality: Access
#=====#

tab3 = data.frame(year = unique(dt_parkrun$year))

tab3$rii = apply(X = tab3,
                 MARGIN = 1,
                 FUN = function(y) {

                       lm.temp = lm(access ~ imd_score, data = subset(dt_parkrun, year == y))
                       
                       pred.temp = predict(lm.temp,
                                           newdata = data.frame(imd_score = IMD_RANGE))
                       
                       tb_ratio.temp = pred.temp[1] / pred.temp[2]
                       
                       return(tb_ratio.temp)
                       
                       })

# store in outputs
stargazer(summary = FALSE,
          x = tab3,
          format = "html",
          out = "outputs/Table3_rii_access.html")

tab3



#=====#
# Figure 4: Ratio Index of Inequality: Participation
#=====#

fig4 = data.frame(month_year = unique(dt_parkrun$month_year))

fig4$rii = apply(X = fig4,
                 MARGIN = 1,
                 FUN = function(m){
                   
                   glm.temp = glm(data = subset(dt_parkrun,month_year == m),
                                  formula = finishers ~imd_score,
                                  offset = log(total_pop),
                                  family = poisson(link="log"))
                   
                   pred.temp = predict(object = glm.temp,
                                       newdata = data.frame(imd_score = IMD_RANGE,
                                                            total_pop = mean( log(subset(dt_parkrun,month_year == m)$total_pop))),
                                       type="response")
                   
                   tb_ratio.temp = pred.temp[1] / pred.temp[2]
                   
                   return(tb_ratio.temp)
                   
                   })


fig4$month_year = as.Date(paste(fig4$month_year,"01",sep="-"))

plot4 <- ggplot(data = fig4,
       aes(x=month_year,y=rii)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  ylab("RII - Monthly parkrun participation ") +
  theme_classic() +
  theme(axis.text   = element_text(size=14),
        axis.title  = element_text(size=16),
        axis.title.x = element_blank())

# store in outputs
ggsave(plot = plot4,width = 10,
       filename = "outputs/Figure4_rii_participation.pdf")


# ====
# unused in publication
# ====


# looking into the month effect:
monthly_rii_trend = aggregate(rii ~ month(fig4$month_year),
                              fig4,median)

names(monthly_rii_trend) = c("month","median_rii")

ggplot(monthly_rii_trend,
       aes(x=month,y=median_rii))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks=1:12,labels=month.name[1:12]) +
  ylab("Median participation RII between 2010 and 2019") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=35)) 


#=====#
# Unused in publication: Ratio Index of Inequality: Participation table
#=====#

uniq.years = unique(dt_parkrun$year)
ri_finishers.y = c()
for(y in uniq.years){
  # cat("\r",m)
  temp.df = dt_parkrun[dt_parkrun$year==y, ]
  glm.temp = glm(finishers ~imd_score,temp.df,offset = log(total_pop),family = poisson(link="log"))
  pred.temp = predict(glm.temp,newdata = data.frame(imd_score = IMD_RANGE,
                                                    total_pop = mean( log(temp.df$total_pop))),type="response")
  tb_ratio.temp = pred.temp[1] / pred.temp[2]
  res.df = data.frame(year=y,rii = tb_ratio.temp)
  ri_finishers.y = rbind(ri_finishers.y,res.df)
}

ri_finishers.y

# store in outputs
stargazer(summary = FALSE,
          x = ri_finishers.y,
          format = "latex"#,
          #out = "outputs/Table_RII_part.html"
          )






