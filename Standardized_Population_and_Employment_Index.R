## R Calculate Z Score Script

## Abraham Dailey

## adailey@gpcog.org

## abraham.dailey@gmail.com

## 2020 10 20

## Import Libraries
library(rgdal)
library(sp)
## library(dplyr)

## Set Environmental Variables

## Get the current warning level
oldw <- getOption("warn")

## Set the warning level to surpress warnings
options(warn = -1)



## Define Global Variables

aggregated_data_column_headings <- c("Town", "Total Acres", "Total Population", "Total Jobs","Average Population Density", "Average Employment Density")

summary_data_column_headings <- c("Town","Variable","Mean","Median","Standard Deviation","Minimum","Maximum","Range","Break 1", "Break 2", "Break 3", "Break 4", "Break 5", "z-score Break 1", "z-score Break 2", "z-score Break 3", "z-score Break 4", "z-score Break 5")



## Define Functions


## Aggregate Data


aggregate_data <- function(input_data, geographic_area) {
  
  town <- geographic_area
  
  total_acres <- sum(input_data$Acres)
  
  total_population <- sum(input_data$POP_14_18)
  
  total_jobs <- sum(input_data$JOBS_17)
  
  mean_population_density <- mean(input_data$Pop_Acre)
  
  mean_job_density <- mean(input_data$Job_Acre)
  
  result <- data.frame(town, total_acres, total_population, total_jobs, mean_population_density, mean_job_density)
  
  colnames(result) <- aggregated_data_column_headings
  
  return(result)
  
} ## End aggregate_data function



## Summarize Data

summarize_population_density <- function(input_data, geographic_area) {
  
  town <- geographic_area
  
  variable <- "Population Density"
  
  
  mean_v <- mean(input_data$Pop_Acre)
  
  median_v <- median(input_data$Pop_Acre)
  
  StdDev_v <- sd(input_data$Pop_Acre)
  
  min_v <- min(input_data$Pop_Acre)
  
  max_v <- max(input_data$Pop_Acre)
  
  range_v <- max_v - min_v
  
  break_1 <- median_v
  
  break_2 <- 2*median_v
  
  break_3 <- 3*median_v
  
  break_4 <- 4*median_v
  
  break_5 <- 5*median_v
  
  z_break_1 <- (break_1-mean_v)/StdDev_v
  
  z_break_2 <- (break_2-mean_v)/StdDev_v
  
  z_break_3 <- (break_3-mean_v)/StdDev_v
  
  z_break_4 <- (break_4-mean_v)/StdDev_v
  
  z_break_5 <- (break_5-mean_v)/StdDev_v
  
  result <- data.frame(town, variable, mean_v, median_v, StdDev_v, min_v, max_v, range_v, break_1, break_2, break_3, break_4, break_5, z_break_1, z_break_2, z_break_3, z_break_4, z_break_5)
  
  colnames(result) <- summary_data_column_headings
  
  return(result)
  
} ## End summarize_data function


summarize_employment_density <- function(input_data, geographic_area) {
  
  town <- geographic_area
  
  variable <- "Employment Density"
  
  
  mean_v <- mean(input_data$Job_Acre)
  
  median_v <- median(input_data$Job_Acre)
  
  StdDev_v <- sd(input_data$Job_Acre)
  
  min_v <- min(input_data$Job_Acre)
  
  max_v <- max(input_data$Job_Acre)
  
  range_v <- max_v - min_v
  
  break_1 <- median_v
  
  break_2 <- 2*median_v
  
  break_3 <- 3*median_v
  
  break_4 <- 4*median_v
  
  break_5 <- 5*median_v
  
  z_break_1 <- (break_1-mean_v)/StdDev_v
  
  z_break_2 <- (break_2-mean_v)/StdDev_v
  
  z_break_3 <- (break_3-mean_v)/StdDev_v
  
  z_break_4 <- (break_4-mean_v)/StdDev_v
  
  z_break_5 <- (break_5-mean_v)/StdDev_v
  
  ## print("about to calculate result")
  result <- data.frame(town, variable, mean_v, median_v, StdDev_v, min_v, max_v, range_v, break_1, break_2, break_3, break_4, break_5, z_break_1, z_break_2, z_break_3, z_break_4, z_break_5)
  
  colnames(result) <- summary_data_column_headings
  
  ## print("returned employment summary")
  
  return(result)
  
  
} ## End summarize_data function


## Summarize combined index function

summarize_combined_index <- function(input_data, geographic_area) {
  
  town <- geographic_area
  
  variable <- "Combined Population and Employment Index"
  
  
  mean_v <- mean(input_data$totZscore)
  
  median_v <- median(input_data$totZscore)
  
  StdDev_v <- sd(input_data$totZscore)
  
  min_v <- min(input_data$totZscore)
  
  max_v <- max(input_data$totZscore)
  
  range_v <- max_v - min_v
  
  break_1 <- median_v
  
  break_2 <- 2*median_v
  
  break_3 <- 3*median_v
  
  break_4 <- 4*median_v
  
  break_5 <- 5*median_v
  
  z_break_1 <- (break_1-mean_v)/StdDev_v
  
  z_break_2 <- (break_2-mean_v)/StdDev_v
  
  z_break_3 <- (break_3-mean_v)/StdDev_v
  
  z_break_4 <- (break_4-mean_v)/StdDev_v
  
  z_break_5 <- (break_5-mean_v)/StdDev_v
  
  
  result <- data.frame(town, variable, mean_v, median_v, StdDev_v, min_v, max_v, range_v, break_1, break_2, break_3, break_4, break_5, z_break_1, z_break_2, z_break_3, z_break_4, z_break_5)
  
  colnames(result) <- summary_data_column_headings
  
  
  return(result)
  
  
} ## End summarize_combined_index function





## Calculate Field

calculate_field <- function(field, mean, standard_deviation) {
  
  field_index <- 1
  
  field_length <- length(field)
  
  result <- field
  
  while(field_index<=field_length) {
    
    result[field_index] <- (field[field_index]-mean)/standard_deviation
    
    field_index <- field_index + 1
    
  }
  
  return(result)
  
  
} ## End Calculate Field



## Standardize Data

standardize_data <- function(input_data, geographic_area) {
  
  ## x <- data.frame(input_data)
  
  ## length_x <- length(input_data)
  
  pop_mean <- mean(input_data$Pop_Acre)
  
  ##print(pop_mean)
  pop_standard_deviation <- sd(input_data$Pop_Acre)
  
  ##print("Population Density Standard Deviation:")
  ##print(pop_standard_deviation)
  
  job_mean <- mean(input_data$Job_Acre)
  
  job_standard_deviation <- sd(input_data$Job_Acre)
  
  ##print("Employment Density Standard Deviation:")
  ##print(job_standard_deviation)
  
  ## Note you shouldn't really manipulate the chn@data data frame directly, you can work with chn like it is a data frame in many respects, for example chn$foo gets the column named foo, or chn$popden = chn$pop/chn$area would create a new column of population density if you have population and area columns.
  
  ## print("things are going south here?")
  
  ## print(input_data$Pop_Acre)
  
  ##print("about to calculate population z score")
  input_data$popZscore <- calculate_field(input_data$Pop_Acre, pop_mean, pop_standard_deviation)
  ##print("just ran calculate field function. Result is:")
  ##print(input_data$popZscore)
  
  ##print("about to calculate job z score")
  input_data$jobZscore <- calculate_field(input_data$Job_Acre, job_mean, job_standard_deviation)
  
  input_data$totRawZscore <- input_data$popZscore + input_data$jobZscore
  
  rawZscore_mean <- mean(input_data$totRawZscore)
  
  rawZscore_standard_deviation <- sd(input_data$totRawZscore)
  
  input_data$totZscore <- calculate_field(input_data$totRawZscore, rawZscore_mean, rawZscore_standard_deviation)
  
  ##print("about to return result")
  return(input_data)
  
  ##print("just returned standarized data")
  ##print(geographic_area)
  
} ## End summarize_data function



## Main Section

## Read input shapefile
## This is the data for the entire PACTS region
  
pop_jobs_bg_data <- readOGR(".","PopJobs_BlockGroup") # first arg is path, second is shapefile name w/o .shp

## Now pop_jobs_bg_data is like a data frame. In fact pop_jobs_bg_data@data is a data frame. Do what you like to that data frame but keep it in the same order

## should show you the first few lines of the shapefile data.  
## head(as.data.frame(pop_jobs_bg_data))

## spplot(pop_jobs_bg_data, "popZscore")

## will map by the popden column you just created, and:




## aggregate data for the PACTS region

agregated_data_summary_row <- aggregate_data(pop_jobs_bg_data, "PACTS Region")

##print("aggregated data summary")

## summarize data for the PACTS region

population_data_summary_row <- summarize_population_density(pop_jobs_bg_data,"PACTS Region")

##print("population data summary")

employment_data_summary_row <- summarize_employment_density(pop_jobs_bg_data, "PACTS Region")

##print("employment data summary")

## standardize data for the PACTS region

##print("entering standardized function for PACTS Region")
standardized_data_PACTS <- standardize_data(pop_jobs_bg_data,"PACTS Region")

combined_index_summary_row <- summarize_combined_index(standardized_data_PACTS, "PACTS Region")


## write the output to output tables

aggregated_data_table <- agregated_data_summary_row

colnames(aggregated_data_table) <- aggregated_data_column_headings

population_output_table <- population_data_summary_row

colnames(population_output_table) <- summary_data_column_headings

employment_output_table <- employment_data_summary_row

colnames(employment_output_table) <- summary_data_column_headings

combined_index_output_table <- combined_index_summary_row

colnames(combined_index_output_table) <- summary_data_column_headings

## save the standardized data for the PACTS region as a new shapefile
writeOGR(standardized_data_PACTS, "Shp/", "PopJobs_BlockGroup_z_scores_PACTS_Region", driver="ESRI Shapefile")


## Now standardize data by town

town_index <- 1

town_list <- levels(pop_jobs_bg_data$Town)

town_list_length <- length(town_list)

while(town_index<=town_list_length) {
  
  town_at_index_value <- town_list[town_index]
  
  pop_jobs_bg_data_town_subset <- pop_jobs_bg_data[ which(pop_jobs_bg_data$Town==paste(town_at_index_value)), ]
  
  agregated_data_summary_row <- aggregate_data(pop_jobs_bg_data_town_subset, town_at_index_value)
  
  population_data_summary_row <- summarize_population_density(pop_jobs_bg_data_town_subset,town_at_index_value)
  
  employment_data_summary_row <- summarize_employment_density(pop_jobs_bg_data_town_subset, town_at_index_value)
  
  standardized_data <- standardize_data(pop_jobs_bg_data_town_subset,town_at_index_value)
  
  combined_index_summary_row <- summarize_combined_index(standardized_data, town_at_index_value)
  
  writeOGR(standardized_data, "Shp/", paste("PopJobs_BlockGroup_z_scores_for_",town_at_index_value,sep=""), driver="ESRI Shapefile")
  
  
  aggregated_data_table <- rbind(aggregated_data_table, agregated_data_summary_row)
  
  population_output_table <- rbind(population_output_table, population_data_summary_row)
  
  employment_output_table <- rbind(employment_output_table, employment_data_summary_row)
  
  combined_index_output_table <- rbind(combined_index_output_table, combined_index_summary_row)
    
  
  town_index <- town_index + 1
  
  
}

write.table(aggregated_data_table, file = "Tables/Town Total Area Population and Employment PACTS Region.csv", append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")

write.table(population_output_table, file = "Tables/Town Population Density Summary.csv", append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")

write.table(employment_output_table, file = "Tables/Town Employment Density Summary.csv", append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")

write.table(combined_index_output_table, file = "Tables/Town Combined Index Summary.csv", append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")


## Now that the script has finished, return to previous warning level
options(warn = oldw)