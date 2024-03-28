########################################################################
### Felix Pabon-Rodriguez
### Longitudinal assessment of health outcomes and social determinants 
### of health in United States nursing homes
########################################################################

########################################################################
# Loading libraries
########################################################################

# Loading libraries
options(repos="https://cran.rstudio.com")
install_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

my_packages <- c("readxl", "dplyr", "coda", "ggplot2",
                 "lattice", "bayesplot", "BayesPostEst", "ggmcmc", 
                 "RCurl", "truncnorm", "kableExtra", "mvtnorm", "rlist", 
                 "extraDistr", "msm", "tmvtnorm", "runjags", "plotrix",
                 "lubridate", "ggpubr", "stringr", "nimble",
                 "igraph", "parallel", "doParallel", "MCMCvis",
                 "usmap", "maps", "mapview", "leaflet", "tmap",
                 "tigris", "tmaptools", "tidycensus")
invisible(install_load(my_packages))


########################################################################
# Loading Data
########################################################################

# Year 2020
medq1y20 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q1_2020.csv",
                     header = TRUE,
                     na.strings = "*")
medq2y20 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q2_2020.csv",
                     header = TRUE,
                     na.strings = "*")
medq3y20 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q3_2020.csv",
                     header = TRUE,
                     na.strings = "*")
medq4y20 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q4_2020.csv",
                     header = TRUE,
                     na.strings = "*")

# Year 2021
medq1y21 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q1_2021.csv",
                     header = TRUE,
                     na.strings = "*")
medq2y21 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q2_2021.csv",
                     header = TRUE,
                     na.strings = "*")
medq3y21 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q3_2021.csv",
                     header = TRUE,
                     na.strings = "*")
medq4y21 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q4_2021.csv",
                     header = TRUE,
                     na.strings = "*")

# Year 2022
medq1y22 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q1_2022.csv",
                     header = TRUE,
                     na.strings = "*")
medq2y22 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q2_2022.csv",
                     header = TRUE,
                     na.strings = "*")
medq3y22 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q3_2022.csv",
                     header = TRUE,
                     na.strings = "*")
medq4y22 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q4_2022.csv",
                     header = TRUE,
                     na.strings = "*")


# Year 2023
medq1y23 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q1_2023.csv",
                     header = TRUE,
                     na.strings = "*")
medq2y23 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q2_2023.csv",
                     header = TRUE,
                     na.strings = "*")
medq3y23 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q3_2023.csv",
                     header = TRUE,
                     na.strings = "*")
medq4y23 <- read.csv(file = "./MinimumDatasets/MDS_Frequency_Q4_2023.csv",
                     header = TRUE,
                     na.strings = "*")

# Vector of states and features within the data
unique_states <- unique(medq1y20$Geographical.Location)
unique_features <- unique(medq1y20$MDS.Item.Question.Description)

########################################################################
# Example of Infections 
########################################################################

# How many features/characteristics includes Infection?
sum(grepl("Infection", unique_features, fixed = TRUE))
infection_ft <- unique_features[grepl("Infection", unique_features, fixed = TRUE)]

# Filter by Infection
inf_data <- medq1y20[medq1y20$MDS.Item.Question.Description%in%infection_ft,]


census_api_key(key = "036a5953d2801a8b3f63b6224e7880f38cd204b5",
               install = TRUE, overwrite = TRUE)

USmap <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2021,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m") %>%
  shift_geometry()


#unique_inf <- unique(inf_data$MDS.Item.Question.Description)
#for(i in 1:length(unique_inf)){
#  
#  data <- inf_data[inf_data$MDS.Item.Question.Description==unique_inf[i],]  
#  data$Percent <- as.numeric(data$Percent)
#  colnames(data)[1] <- "NAME"
#  
#  newdata <- data %>%
#    group_by(NAME) %>%
#    mutate(Diff.Percent.YesNo = abs(Percent - lag(Percent))) %>%
#    summarise_all(last)
#  
#  combine <- left_join(USmap, newdata, by = "NAME")
#  
#  g <- ggplot(data = combine, aes(fill = Diff.Percent.YesNo)) + 
#    geom_sf() + 
#    scale_fill_distiller(palette = "RdYlGn", 
#                         direction = -1,
#                         type = "div") + 
#    labs(title = unique_inf[i],
#         caption = "Data source: Minimum Data Set (MDS), Centers for Medicare & Medicaid Services",
#         fill = "Difference in Percent \n(Yes vs No)") + 
#    theme_void() +
#    theme(plot.title = element_text(hjust = 0.5))
#  
#  print(g)
#}

 
########################################################################
# Identify Social Determinants of Health of Medicare Beneficiaries
# Living in US Nursing Homes from 2020-2023
########################################################################

# Based on the vector unique_features, the following variables will
# be explored for the purpose of the study

# social
social_var <- c(1:8,11,12,23,40,73,103,121,174,176,179,190,193,
                341:347,373,596,597)

# non-communicable diseases
noncomm_var <- c(295,296,298,300,301,304,318,324,327,331,335,
                 336,337,338,349,350)

# communicable diseases
comm_var <- 311:317

# healthcare services
services_var <- c(488,492,493,496,526,531,532,533,534)

# indicator of overall health (as life expectancy, less than 6 months)
overall_var <- c(374,602)


########################################################################
# Combining Datasets
########################################################################

yr2020 <- rbind(medq1y20,
             medq2y20,
             medq3y20,
             medq4y20)
yr2021 <- rbind(medq1y21,
             medq2y21,
             medq3y21,
             medq4y21)
yr2022 <- rbind(medq1y22,
             medq2y22,
             medq3y22,
             medq4y22)
yr2023 <- rbind(medq1y23,
             medq2y23,
             medq3y23,
             medq4y23)


########################################################################
# Mapping
########################################################################

get_temp_map <- function(data,feature,type,year,foldername,...){

  for(i in 1:length(feature)){
    df <- data[data$MDS.Item.Question.Description==feature[i],]  
    df$Percent <- as.numeric(df$Percent)
    colnames(df)[1] <- "NAME"
    
    combine <- left_join(USmap, df, by = "NAME")
    
    pdf(file = paste0("./LongitudinalMaps/",foldername,"/",
                      year,type,"Feature",i,".pdf")) 
    g <- ggplot(data = combine, aes(fill = Percent)) + 
      geom_sf() + 
      scale_fill_distiller(palette = "YlOrRd", 
                           direction = 1,
                           type = "div",
                           guide = "colourbar",
                           limits = c(0,100)) + 
      labs(title = feature[i], fill = "Percent Individuals Represented") + 
      theme_void() +
      theme(plot.title = element_text(size=800/nchar(feature[i]),
                                      hjust = 0.5),
            legend.position = "bottom",
            legend.key.height = unit(0.5, "cm"),
            legend.key.width = unit(2, "cm"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            strip.text = element_text(size=4)) +
      facet_wrap(MDS.Item.Response ~ Report.Date, ncol = 4)
    print(g)
    dev.off()
  }
}


# social
get_temp_map(data = yr2020, year = "2020", type = "Social",
             feature = unique_features[social_var],
             foldername = "Social")
get_temp_map(data = yr2021, year = "2021", type = "Social",
             feature = unique_features[social_var],
             foldername = "Social")
get_temp_map(data = yr2022, year = "2022", type = "Social",
             feature = unique_features[social_var],
             foldername = "Social")
get_temp_map(data = yr2023, year = "2023", type = "Social",
             feature = unique_features[social_var],
             foldername = "Social")


# non-communicable diseases
get_temp_map(data = yr2020, year = "2020", type = "NonCD",
             feature = unique_features[noncomm_var],
             foldername = "NonCommunicableDiseases")
get_temp_map(data = yr2021, year = "2021", type = "NonCD",
             feature = unique_features[noncomm_var],
             foldername = "NonCommunicableDiseases")
get_temp_map(data = yr2022, year = "2022", type = "NonCD",
             feature = unique_features[noncomm_var],
             foldername = "NonCommunicableDiseases")
get_temp_map(data = yr2023, year = "2023", type = "NonCD",
             feature = unique_features[noncomm_var],
             foldername = "NonCommunicableDiseases")


# communicable diseases
get_temp_map(data = yr2020, year = "2020", type = "CD",
             feature = unique_features[comm_var],
             foldername = "CommunicableDiseases")
get_temp_map(data = yr2021, year = "2021", type = "CD",
             feature = unique_features[comm_var],
             foldername = "CommunicableDiseases")
get_temp_map(data = yr2022, year = "2022", type = "CD",
             feature = unique_features[comm_var],
             foldername = "CommunicableDiseases")
get_temp_map(data = yr2023, year = "2023", type = "CD",
             feature = unique_features[comm_var],
             foldername = "CommunicableDiseases")


# healthcare services
get_temp_map(data = yr2020, year = "2020", type = "Services",
             feature = unique_features[services_var],
             foldername = "HealthcareServices")
get_temp_map(data = yr2021, year = "2021", type = "Services",
             feature = unique_features[services_var],
             foldername = "HealthcareServices")
get_temp_map(data = yr2022, year = "2022", type = "Services",
             feature = unique_features[services_var],
             foldername = "HealthcareServices")
get_temp_map(data = yr2023, year = "2023", type = "Services",
             feature = unique_features[services_var],
             foldername = "HealthcareServices")


# indicator of overall health (as life expectancy, less than 6 months)
get_temp_map(data = yr2020, year = "2020", type = "Status",
             feature = unique_features[overall_var],
             foldername = "OverallHealthStatus")
get_temp_map(data = yr2021, year = "2021", type = "Status",
             feature = unique_features[overall_var],
             foldername = "OverallHealthStatus")
get_temp_map(data = yr2022, year = "2022", type = "Status",
             feature = unique_features[overall_var],
             foldername = "OverallHealthStatus")
get_temp_map(data = yr2023, year = "2023", type = "Status",
             feature = unique_features[overall_var],
             foldername = "OverallHealthStatus")




