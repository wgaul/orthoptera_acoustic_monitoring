############################
## Orthoptera acoustic monitoring:
## Match orthoptera monitoring sound recordings to 
## metatada collected during fieldwork
## 
## Run this script after entering data from each round of fieldwork, to match
## clipboard data to sound recordings.  This script requires the new sound 
## recordings to be stored on the local computer.  Data for new recordings 
## stored in the local folder will be appended to the existing main 
## file METADATA_orthoptera_monitoring_recording.csv, and a backup file with
## only the info for the new recordings will be stored in 
## /old/METADATA_orthoptera_monitoring_recording_[date].csv  
## 
## inputs:  - .wav files stored locally
##          - clipboard data entered into a .csv
##          
##  outputs:  - METADATA_orthoptera_monitoring_recording.csv
##            - METADATA_orthoptera_monitoring_recording_[date].csv (only new 
##              recordings)
## 
## Willson Gaul  willson.gaul@gmail.com
## created: 21 July 2025
## modified: 26 July 2025
############################

setwd("~/Documents/Data_Analysis/orthoptera/monitoring/")
library(tuneR)
library(tidyverse)

# set field data filename to most recent version
field_data_fn <- "orthoptera_point_count_DATA_21July2025.csv"
field_driving_data_fn <- "orthoptera_driving_survey_DATA_26July2025.csv"

##### 1) Match stationary recording files to metadata -------------------------
## get filenames for recordings
rec_fn <- list.files("~/Documents/Saipan_ecology/orthoptera/acoustic_monitoring_recordings_temporaryLocal/new/", pattern = ".*.wav", 
                     recursive = TRUE, ignore.case = TRUE, 
                     include.dirs = FALSE)

recordings_df <- data.frame(filename = rec_fn)
recordings_df <- cbind(recordings_df, 
                       file.info(paste0("~/Documents/Saipan_ecology/orthoptera/acoustic_monitoring_recordings_temporaryLocal/new/", 
                                        recordings_df$filename))[, c(1, 4)])
row.names(recordings_df) <- NULL

# dates recorded by mtime seem to be wrongly automatically converting the 
# local time (which is correct) as if that was UTC+0, instead of the UTC+10
# that it actually is.  This seems to be happening to both the Roland R26 and
# the Zoom H5 recordings. So, I need to manually set the dates back 1 day to 
# get the correct date.
# recordings_df$date <- as_date(recordings_df$mtime) - 1
recordings_df$date <- as_date(as.POSIXct(recordings_df$mtime - hms("10:00:00")))
recordings_df$tod <- as.POSIXct(recordings_df$mtime - hms("10:00:00"), 
                                format="%H:%M:%S")

# remove subdirectory names from filenames
recordings_df$filename <- gsub(".*/", "", recordings_df$filename)


## get data collected during fieldwork
# stationary recordings
fld_dat <- read_csv(paste0("~/Documents/Saipan_ecology/orthoptera/old/", 
                           field_data_fn), skip = 1)
fld_dat$date <- as_date(paste(as.character(fld_dat$Date_ddmmyyyy), sep = " "), 
                        format = "%d/%m/%Y")
fld_dat$time_hms <- hms(fld_dat$Time_of_Day)
fld_dat$date_posixct <- as.POSIXct(paste(fld_dat$date, fld_dat$Time_of_Day), 
                                   format = "%Y-%m-%d %H:%M:%S")

# driving transect recordings
drv_dat <- read_csv(paste0("~/Documents/Saipan_ecology/orthoptera/old/", 
                           field_driving_data_fn), skip = 1)
drv_dat$date <- as_date(paste(as.character(drv_dat$Date_ddmmyyyy), sep = " "), 
                        format = "%d/%m/%Y")
drv_dat$time_hms <- hms(drv_dat$Time_of_Day)
drv_dat$date_posixct <- as.POSIXct(paste(drv_dat$date, drv_dat$Time_of_Day), 
                                   format = "%Y-%m-%d %H:%M:%S")

## match field data to recording filename
# make ID column using date, hour, and point name
fld_dat$survey_ID <- paste0(as.character(fld_dat$date), "_hr", 
                           hour(fld_dat$Time_of_Day), "_pt", fld_dat$Point_ID)
drv_dat$survey_ID <- paste0(as.character(drv_dat$date), "_hr", 
                            hour(drv_dat$Time_of_Day), "_pt", drv_dat$Point_ID)
recordings_df$survey_ID <- NA

## join clipboard field data from stationary and driving surveys
nr_out <- nrow(fld_dat) + nrow(drv_dat) # how many rows should be in the output?
fld_dat <- full_join(fld_dat, drv_dat) # join clipboard field datasets
if(nrow(fld_dat) != nr_out) stop("Joined point and driving field data does not have the expected number of rows.  Check the full join between point and driving field data, to make sure all data was preserved.")


## Add survey_ID from field data to recordings_df based on date and time
# make list of recordings with ambiguous matching to field data.
recs_to_set_manually <- c()
for(i in 1:nrow(fld_dat)) {
  rec_name <- NA
  if(!is.na(fld_dat$date_posixct[i]) & any(recordings_df$tod -
         fld_dat$date_posixct[i] <= 2)) {
    rec_name <- tryCatch(
      recordings_df$filename[abs(recordings_df$tod -
                               fld_dat$date_posixct[i]) <= 
                               as.difftime(120, units = "secs")], 
      error = function(x) NA)
    
    ## If more than one set of recordings have times starting within 2 minutes
    ## of the time recorded on the clipboard, assign to the clipboard data
    ##the recordings with start times closest to the time from the clipboard 
    ## data.  This should work as long as I check to see if another survey_ID
    ## has already been assigned, and manually set it if so.  This is because, 
    ## if this loops through field data in the sequence in which sites were
    ## visited, then if the minimum time gap from a recording is to the next 
    ## unmatched recording, then it will either turn out that the next 
    ## unmatched recording is the correct match, or it will be caught on the 
    ## next time through the loop.  If the minimum time gap is to a recording 
    ## that has already been assigned a survey_ID, then it is an error.  

    if(length(rec_name) > 3) {
      # match based on minimum difference between times
      rec_name <- tryCatch(
        recordings_df$filename[abs(
          recordings_df$tod - fld_dat$date_posixct[i]) ==
            min(abs(recordings_df$tod - fld_dat$date_posixct[i]))]
      )
    }
    
    # test if there is already a survey_ID set for these recordings
    if(any(!is.na(
      recordings_df$survey_ID[recordings_df$filename %in% rec_name]))) {
      warning(paste("Multiple recordings are close in time to survey_ID ",
                    fld_dat$survey_ID[i]))
      # add these recording names to list of ambiguous matches
      recs_to_set_manually <- c(recs_to_set_manually, rec_name)
    } else {
      # add survey ID to the recordings_df data frame
      recordings_df$survey_ID[recordings_df$filename %in% rec_name] <- 
        fld_dat$survey_ID[i]
    }
  } else recordings_df$survey_ID[recordings_df$filename %in% rec_name]  <- NA
}

warning("The following recordings have ambiguous matching to clipboard data based on time of day.  Manually match these recording files to clipboard data:",
        immediate. = TRUE)
try(print(recs_to_set_manually))


## Manually set survey IDs for ambiguous cases:
ambig_ls <- list("2025-07-13_hr20_ptM05" = "0713_204607.*", 
                 "2025-07-13_hr21_ptM16" = "0713_212551.*", 
                 "2025-07-13_hr22_ptM28" = "0713_223351.*",
                 "2025-07-13_hr22_ptM35" = "0713_225253.*", 
                 "2025-07-14_hr22_ptM32" = "0714_223349.*")
for(i in 1:length(ambig_ls)) {
  recordings_df$survey_ID[grepl(ambig_ls[[i]], recordings_df$filename)] <- 
    names(ambig_ls)[i]
}

# check to make sure there are 3 recordings for each survey ID for stationary 
# recordings using the Roland R26 with 3 microphone tracks, and 1 recording for
# each survey ID for driving transects.
if(min(as.numeric(table(
  recordings_df$survey_ID[!grepl("D.*", recordings_df$survey_ID)]))) != 3 | 
  max(as.numeric(table(
    recordings_df$survey_ID[!grepl("D.*", recordings_df$survey_ID)]))) != 3) {
  print(summary(as.numeric(table(recordings_df$survey_ID))))
  stop("There are 3 not recordings for each survey ID")
}

### Merge clipboard and recording file data
recordings_df <- left_join(recordings_df, fld_dat, 
                           by = c("survey_ID", "date"))

### write out recordings_df as the main recording metadata file.
# save backup of only the new recording info
write_csv(
  recordings_df, 
  file = paste0("./data/old/METADATA_orthoptera_monitoring_recordings_", 
                gsub("-", "", as_date(Sys.time())), ".csv"))

# drop files for which no clipboard data has been entered yet
recordings_df <- recordings_df[which(!is.na(recordings_df$survey_ID)), ]
# append new recordings info to existing METADATA
write_csv(
  recordings_df, 
  file = paste0("./data/METADATA_orthoptera_monitoring_recordings.csv"), 
  append = TRUE)
##### end stationary recording files -------------------------------------------


##### 2) Match driving survey recording files to metadata ----------------------


##### end driving survey recording files --------------------------------------