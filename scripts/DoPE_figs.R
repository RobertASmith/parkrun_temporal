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
               RColorBrewer,lubridate,feather,
               stargazer, kableExtra, jtools)

# source all functions in R folder
source.all(path = "R")

#=====#
# load data
#=====#

dt_parkrun <- readRDS("cleandata/lsoa_df_monthly.Rds")
dt_parkrun$year = substr(x = dt_parkrun$month_year,start = 1,stop = 4) %>% as.numeric # create year variable


#====#
# IMD quintile cuts
#====#
imd_quintiles_cuts = quantile(dt_parkrun$imd_score,probs = seq(0,1,0.2))
dt_parkrun$imd_q5 = cut(dt_parkrun$imd_score, imd_quintiles_cuts, include.lowest = T)
levels(dt_parkrun$imd_q5) =  c("Least deprived 20%","Less deprived 20%", "Median 20%", "More deprived 20%", "Most deprived 20%")
imd_colors = c("orangered","orange","yellow3","yellowgreen","lawngreen")
imd_colors = imd_colors[length(imd_colors):1]



#=====#
# Figure 1: nearest distances by imd by month year
#=====#
fig1_df = agg_parkrun_stat(dt_parkrun,y="access")
ggplot(fig1_df,aes(x=plot_date,y=access,col=imd_q5)) +
  geom_point(size=0.2)+
  geom_line() +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  ylab("Mean distance to the nearest parkrun event (km)") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",name="") +
  theme_minimal()

#=====#
# Table 1: nearest distances by imd by year
#=====#
tbl1_df = aggregate(access ~ imd_q5 + year, dt_parkrun, make_parkrun_tbl)
tbl1_general = aggregate(access ~ year, dt_parkrun, make_parkrun_tbl)
tbl1_general$imd_q5 = "Overall"
tbl1 = rbind(tbl1_general,tbl1_df)
tbl1 = reshape2::dcast(tbl1, imd_q5~year,value.var = "access")
tbl1[c(6:1),]


#=====#
# Figure 2: parkrun participation by imd by month year
#=====#

fig2_df = agg_parkrun_stat(dt_parkrun,y="finishers")
ggplot(fig2_df,aes(x=plot_date,y=finishers,col=imd_q5)) +
  # geom_point(size=0.2)+
  geom_line(alpha=0.7,size=0.3) +
  geom_smooth(alpha=1,se=F) +
  scale_color_manual(values=c(imd_colors,1),name="IMD quintile") +
  # ylab("Mean distance to the nearest parkrun event") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",name="") +
  theme_minimal()


#=====#
# Table 2: parkrun participation by imd by year
#=====#

tbl2_df = aggregate(finishers ~ imd_q5 + year, dt_parkrun, make_parkrun_tbl)
tbl2_general = aggregate(finishers ~ year, dt_parkrun, make_parkrun_tbl)
tbl2_general$imd_q5 = "Overall"
tbl2 = rbind(tbl2_general,tbl2_df)
tbl2 = reshape2::dcast(tbl2, imd_q5~year,value.var = "finishers")
tbl2[c(6:1),]



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

ggplot(fig3,aes(x=month_year,y=rii)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  ylab("RII - Distance to the nearest parkrun event") +
  xlab("Year")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=35))

#=====#
# numerical results: Ratio Index of Inequality: Access
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
                                                            total_pop = mean( log(temp.df$total_pop))),
                                       type="response")
                   
                   tb_ratio.temp = pred.temp[1] / pred.temp[2]
                   
                   return(tb_ratio.temp)
                   
                   })


fig4$month_year = as.Date(paste(ri_finishers$month_year,"01",sep="-"))

ggplot(data = fig4,
       aes(x=month_year,y=rii)) +
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  ylab("RII - Monthly parkrun participation ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=35))

monthly_rii_trend = aggregate(rii ~ month(fig4$month_year),
                              ri_finishers,median)

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
# numerical results: Ratio Index of Inequality: Participation
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







# R.S. What does this do? can't get it to work.


runs_by_imd_shares = dcast(data = runs_by_imd,formula = month_year ~ imd_decile,value.var = "finishers") 
runs_by_imd_shares$totals = rowSums(runs_by_imd_shares[,-1])
runs_by_imd_shares = as.data.table(runs_by_imd_shares)
runs_by_imd_shares2 = runs_by_imd_shares[, lapply(.SD, function(x){x/totals}), .SDcols = 2:11]
runs_by_imd_shares2$month_year = runs_by_imd_shares$month_year
runs_by_imd_shares2 = melt(runs_by_imd_shares2,id.vars = "month_year")

ggplot(runs_by_imd_shares2,aes(x=month_year,y=value,fill=variable)) +
  geom_area(col="black",size=0.2,alpha=0.75) +
  scale_fill_manual(values = imd_colors,name="IMD decile",labels=imd_labels) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  ylab("Share in total runs per month by IMD deciles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=35))




