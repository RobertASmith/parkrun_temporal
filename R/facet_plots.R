# Additional reviewer request for facet plots for each variable

# plot 1 is non-finisher variables,
# plot 2 is number of finishers by year
# plot 3 is distance by year

makeFacetHist <- function(dt = dt_parkrun_yr){
  
  # melt data-table to long format
  dt <- melt.data.table(data = dt_parkrun_yr,
                        id.vars = c("year","lsoa"))
  
  # include only 2019 data
  dt_plot1 <- dt[year == "2019" & variable %in% c('imd_score', 'mn_dstn', 'pop_density', 
                                                  'ethnic_density', 'total_pop')]
  
  
  dt_plot2 <- dt[variable %in% c('finishers'), .(lsoa, variable = year, year = year, value)]
  
  dt_plot3 <- dt[variable %in% c('mn_dstn'), .(lsoa, variable = paste0(year,"D"), year = year, value)]
  
  dt_plot  <- rbind(   rbind(dt_plot1, dt_plot2),  dt_plot3)  
  
  # titles for facets
  facet_titles <- c('2010' = 'Finishers in 2010',
                    '2011' = 'Finishers in 2011',
                    '2012' = 'Finishers in 2012',
                    '2013' = 'Finishers in 2013',
                    '2014' = 'Finishers in 2014',
                    '2015' = 'Finishers in 2015',
                    '2016' = 'Finishers in 2016',
                    '2017' = 'Finishers in 2017',
                    '2018' = 'Finishers in 2018',
                    '2019' = 'Finishers in 2019',
                    '2020' = 'Finishers in 2020',
                    
                    '2010D' = 'Geodesic Distance in 2010',
                    '2011D' = 'Geodesic Distance in 2011',
                    '2012D' = 'Geodesic Distance in 2012',
                    '2013D' = 'Geodesic Distance in 2013',
                    '2014D' = 'Geodesic Distance in 2014',
                    '2015D' = 'Geodesic Distance in 2015',
                    '2016D' = 'Geodesic Distance in 2016',
                    '2017D' = 'Geodesic Distance in 2017',
                    '2018D' = 'Geodesic Distance in 2018',
                    '2019D' = 'Geodesic Distance in 2019',
                    '2020D' = 'Geodesic Distance in 2020',
                    
                    
                    'finishers' = 'Annual number of finishers', 
                    'imd_score' = 'Index of Multiple Deprivation Score',
                    'mn_dstn' = 'Geodesic distance to nearest event (km)',
                    'pop_density' = 'Population density (pop/km2)',
                    'ethnic_density' = "Ethnic Density (%)",
                    'total_pop' = "Population",
                    'perc_non_working_age' = "Percent non-working age")
  
  # INDEPENDANT VARIABLES
  
  # create faceted ggplot
  varsPlot <- ggplot(data = dt_plot1, aes(x = value))+
    
    theme_classic()+
    
    geom_density(alpha=.5, fill="lightblue") +
    
    xlab("") + ylab("Density") +
    
    facet_wrap(facets = "variable", 
               scales = "free",
               labeller = labeller(variable = facet_titles))
  
  # FINISHERS 
  
  finishersPlot <- ggplot(data = dt_plot2, 
                          aes(x = value))+
    
    theme_classic() +
    
    geom_density(alpha=.5, fill="lightblue") +
    
    xlim(c(0,1000))+
    
    xlab("Number of annual finishers") + ylab("Density") +
    
    facet_wrap(facets = "variable", scales = "free_y")
  
  # DISTANCE
  
  distancePlot <- ggplot(data = dt_plot3, 
                         aes(x = value)) +
    
    theme_classic() +
    
    xlim(c(0,100))+
    
    geom_density(alpha=.5, fill="lightblue") +
    
    xlab("Geodesic distance to nearest event (km)") + ylab("Density") +
    
    facet_wrap(facets = "year", 
               scales = "free_y")#,
               #labeller = labeller(variable = facet_titles))
  
  return(list("finishersPlot" = finishersPlot,
              "distancePlot" = distancePlot,
              "variablesPlot" = varsPlot))
  
  
}