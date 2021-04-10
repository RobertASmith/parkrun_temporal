#====
# make colour plots of finishers by rural/urban status
#====

makeColourFacet <- function(dt = dt_parkrun_yr){
  
  dt_temp <- dt[,.(lsoa,
                   year,
                   urban,
                   run_rate = finishers / total_pop / 52 * 1000,
                   
                   imd_dec = cut(x = imd_score,
                                 breaks = seq(0,100,10),        #  quantile(imd,seq(0,1,0.1)),
                                 ordered_result = T,
                                 labels = F)*10,
                   
                   ED_dec= cut(x = ethnic_density,
                               breaks = seq(0,100,10),                  # quantile(perc_bme,seq(0,1,0.1)),
                               ordered_result = T,
                               labels = F)*10)]
  
  
  # aggregate data - now ready for plot with facets
  dt_plot <- aggregate(run_rate ~ ED_dec + imd_dec + urban + year,
                       data = dt_temp, 
                       FUN= "mean")
  
  
  
  ggplot(data = dt_plot[dt_plot$year %in% c(2010, 2013, 2016, 2019),],
         aes(as.factor(ED_dec), as.factor(imd_dec), fill= run_rate)) + 
    geom_tile()+
    theme_classic()+
    scale_fill_viridis(discrete=FALSE, name = "Participation Rate") +
    facet_grid(year ~ urban, scales = "free",labeller = labeller(urban = c("0" = "Rural", "1" = "Urban"))) +
    
    xlab("Ethnic Density (%)")+
    ylab("Index of Multiple Deprivation")  + 
    
    labs(caption="Sources: Office for National Statistics and parkrunUK")+
    
    theme(legend.position = 'bottom',
          axis.text.x = element_text(hjust = -1.25),
          axis.text.y = element_text(vjust = -0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()) #+
  
  #annotate("text", x=9.5, y=9.5, 
  #         label = "Most Deprived & \n Highest Ethnic Density", 
  #         color = "black", size = 2, 
  #         fontface = "bold")
  
}