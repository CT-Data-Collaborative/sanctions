library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Sanctions
# Created by Jenna Daly
# On 04/26/17
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = ".csv") 
all_state_csvs <- dir(path_to_raw_data, recursive=T, pattern = "ct.csv") 
all_dist_csvs <- all_csvs[!all_csvs %in% all_state_csvs]

#District level data
sanctions_dist <- data.frame(stringsAsFactors = F)
sanctions_dist_noTrend <- grep("trend", all_dist_csvs, value=T, invert=T)
for (i in 1:length(sanctions_dist_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sanctions_dist_noTrend[i]), stringsAsFactors=F, header=F )
  current_file <- current_file[-c(1:3),]
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sanctions_dist_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sanctions_dist <- rbind(sanctions_dist, current_file)
}

#State level data
sanctions_state <- data.frame(stringsAsFactors = F)
sanctions_state_noTrend <- grep("trend", all_state_csvs, value=T, invert=T)
for (i in 1:length(sanctions_state_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", sanctions_state_noTrend[i]), stringsAsFactors=F, header=F )
  current_file <- current_file[-c(1:3),]
  colnames(current_file) <- current_file[1,]
  current_file <- current_file[-1,]
  names(current_file)[names(current_file)=="State"] <- "District"
  current_file$District <- "Connecticut"
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(sanctions_state_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  sanctions_state <- rbind(sanctions_state, current_file)
}

#Combine district and state
sanctions <- rbind(sanctions_dist, sanctions_state)

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

sanctions_fips <- merge(sanctions, districts, by.x = "District", by.y = "District", all=T)

sanctions_fips$District <- NULL

sanctions_fips<-sanctions_fips[!duplicated(sanctions_fips), ]

#backfill year
years <- c("2009-2010", 
           "2010-2011",
           "2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016", 
           "2016-2017")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years 
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_sanctions <- merge(sanctions_fips, backfill_years, all=T)

#remove duplicated Year rows
complete_sanctions <- complete_sanctions[!with(complete_sanctions, is.na(complete_sanctions$Year)),]

#return blank in FIPS if not reported
complete_sanctions$FIPS <- as.character(complete_sanctions$FIPS)
complete_sanctions[["FIPS"]][is.na(complete_sanctions[["FIPS"]])] <- ""

#recode missing data with -6666
complete_sanctions[is.na(complete_sanctions)] <- -6666

#recode suppressed data with -9999
complete_sanctions[complete_sanctions == "*"]<- -9999

#reshape from wide to long format
cols_to_stack <- c("In-School Suspension",     
                   "Out-of-School Suspension",
                   "Expulsion",               
                   "Bus Suspension")

long_row_count = nrow(complete_sanctions) * length(cols_to_stack)

complete_sanctions_long <- reshape(complete_sanctions,
                                   varying = cols_to_stack,
                                   v.names = "Value",
                                   timevar = "Sanction Type",
                                   times = cols_to_stack,
                                   new.row.names = 1:long_row_count,
                                   direction = "long"
)

#Rename FixedDistrict to District
names(complete_sanctions_long)[names(complete_sanctions_long) == 'FixedDistrict'] <- 'District'

#reorder columns and remove ID column
complete_sanctions_long <- complete_sanctions_long[order(complete_sanctions_long$District, complete_sanctions_long$Year),]
complete_sanctions_long$id <- NULL

#Add Measure Type
complete_sanctions_long$`Measure Type` <- "Number"

#Rename Variable columns
complete_sanctions_long$`Variable` <- "Sanctions"


#Order columns
complete_sanctions_long <- complete_sanctions_long %>% 
  select(`District`, `FIPS`, `Year`, `Sanction Type`, `Variable`, `Measure Type`, `Value`)

#Use this to find if there are any duplicate entires for a given district
# test <- complete_sanctions_long[,c("District", "Year", "Sanction Type")]
# test2<-test[duplicated(test), ]

#Write CSV
write.table(
  complete_sanctions_long,
  file.path(getwd(), "data", "sanctions_2010-2017.csv"),
  sep = ",",
  row.names = F
)

