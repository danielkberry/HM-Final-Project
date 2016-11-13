#########################################
## Author: Daniel Berry                ##
## Description: Code to transform data ##
#########################################

getwd()

blocks_raw <- read.csv('CensusBlockTIGER2010.csv', stringsAsFactors = FALSE)

vacant_raw <- read.csv('311_Service_Requests_-_Vacant_and_Abandoned_Buildings_Reported_-_Map.csv',
                       stringsAsFactors = FALSE,
                       skip = 1)

names(vacant_raw) <- c('Type',
                       'ID',
                       'Date_Recieved',
                       'Lot_Location',
                       'Open',
                       'Dangerous',
                       'Entry',
                       'Vacant',
                       'Fire',
                       'Users',
                       'Address_Number',
                       'Address_Direction',
                       'Address_Street',
                       'Address_Suffic',
                       'Zip',
                       'X_Coordinate',
                       'Y_Coordinate',
                       'Latitude',
                       'Longitude',
                       'Location_string')


