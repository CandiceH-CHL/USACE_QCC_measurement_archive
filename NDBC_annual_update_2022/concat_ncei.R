concat_ncei <- function(files = "list of files", start_year = "earliest dataset year", input = "stdmet or spec", drive = "drive", buoy = "buoy", file_list = "file_list", t = "variable"){
  
  NA_strings <- c(9.96920996838687E+36,"9.96920996838687E+36","9.96921e+36",9.96921e+36,999.00,999, 
                  "-32767",-32767,"-2147483647",-2147483647,NA, NaN,"NA","NaN","9969209968386869046778552952102584320.000",
                  "9.96920996838686E+36","99.0","9999.0","999","999.0","99.00","999.00","", " ")
  # format header 
  read_header <- function(df){
      years <- df[1:3]
      freq <- df[4:length(df)]
      freq <- as.numeric(freq)
      freq <- sprintf("%1.4f", freq)
      freq <- as.character(freq)
      df <- c(years, freq)
  }
  # functions to test for date formats
  library(lubridate)
  IsDate_dmy_hms <- function(mydate, date.format = "%d-%m-%Y %h:%m:%s") {
    tryCatch(!is.na(dmy_hms(mydate, date.format)),
             error = function(err) {FALSE})
  }
  IsDate_ymd_hms <- function(mydate, date.format = "%Y-%m-%d %h:%m:%s") {
    tryCatch(!is.na(ymd_hms(mydate, date.format)),
             error = function(err) {FALSE})
  }
  IsDate_mdy_hm <- function(mydate, date.format = "%m/%d/%Y %h:%m") {
    tryCatch(!is.na(mdy_hm(mydate, date.format)),
             error = function(err) {FALSE})
  }
  
  setwd(data_dir)
  
  raw_dir <- paste0(data_dir,"raw_data/")
  setwd(data_dir)
  # set input directories
  unzip_ndbc_dir <- paste0(raw_dir,"ndbc/unzipped/")
  ascii_ncei_dir <- paste0(raw_dir,"ncei/ascii/")
  
  # set new out_dir
  if (!file.exists(paste0(data_dir,"concat_data/"))) {dir.create((paste0(data_dir,"concat_data/")))}
  out_dir <- paste0(data_dir,"concat_data/")
  # ndbc 
  if (!file.exists(paste0(out_dir,"ndbc/"))) {dir.create((paste0(out_dir,"ndbc/")))}
  ndbc_dir <- paste0(out_dir,"ndbc/")
  # ncei 
  if (!file.exists(paste0(out_dir,"ncei/"))) {dir.create((paste0(out_dir,"ncei/")))}
  ncei_dir <- paste0(out_dir,"ncei/")
  
  # data types
  dataTypes <- c("stdmet", "swden", "swdir", "swdir2", "swr1","swr2")
  dataType_ab <- c("h", "w", "d", "i","j","k")
  data_types_stdmet <- c("lat", "lon", "wind_direction", "wind_speed", "wind_gust",
                         "significant_wave_height", "dominant_period", "average_period", "mean_wave_direction",
                         "air_pressure", "air_temperature", "sea_surface_temperature", "dew_point_temperature")
  data_types_spec <- c("c11", "c11m", "alpha1", "alpha2", "r1", "r2", "C12", "C13", "C22", "C33",
                       "Q12", "Q13", "gamma2", "gamma3", "phih", "rhq", "sensor_output") # c11 = spectral energy, c11m = uncorrected spectral energy
  

  if(input == "stdmet"){
        #----------------------------------------------------------------------------------------
        # met data
        #----------------------------------------------------------------------------------------
        # # merging yearly and monthly datafiles of each type 
        library(lubridate)
        library(tidyr)
        # standard met data - create NCEI matching NDBC web file nomenclature
        for (t in data_types_stdmet){
            print(paste0("Working on...",t))
          
          
            if (!file.exists(paste0(ncei_dir,buoy,"/data_availability"))) {dir.create((paste0(ncei_dir,buoy,"/data_availability")))}
            data_avail_dir <- paste0(ncei_dir,buoy,"/data_availability/s_")
            
            # add sensor information
            metadata <- read.csv(paste0(raw_dir,"ncei/metadata/",buoy,"_metadata_ALL.csv"))

            # subset conditions for each variable - because NBDC loves duplication and format change for no reason
            # handle different netcdf variable names for waves
            if(grepl("dominant", t)){t1 <- "dominant_wave_period"; files2 <- file_list[grep(pattern = paste0("_",t1), file_list)]}
            if(grepl("average", t)){t1 <- "average_wave_period"; files2 <- file_list[grep(pattern = paste0("_",t1), file_list)]}
            files <- file_list[grep(pattern = paste0("_",t), file_list)]
            if(exists("files2")){files <- c(files2, files); rm(files2)}
            
            # available files ALL
            files <- sort(files)
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_all.csv"), row.names=FALSE)
            
            if(t == "lat"){
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
            }else if(t == "lon"){
                  # removed:
                  variable2 <- files[grep(pattern = "_longwave_", files)]
                  if(length(variable2) != 0){files <- files[!grepl(paste(variable2, collapse="|"), files)]}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
              
            }else if(t == "wind_direction"){
                  # removed:
                  continuous <- files[grep(pattern = paste0("_continuous_",t), files)]
                  if(length(continuous) != 0){files <- files[!grepl(paste(continuous, collapse="|"), files)]}
                  wind_dir_58 <- files[grep(pattern = "_58", files)]
                  if(length(wind_dir_58) != 0){files <- files[!grepl(paste(wind_dir_58, collapse="|"), files)]}
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("anemometer",payload_sensor))
                  
            }else if(t == "wind_speed"){
                  # removed:
                  continuous <- files[grep(pattern = paste0("_continuous_",t), files)]
                  if(length(continuous) != 0){files <- files[!grepl(paste(continuous, collapse="|"), files)]}
                  max_1 <- files[grep(pattern = "_max_1", files)]
                  if(length(max_1) != 0){files <- files[!grepl(paste(max_1, collapse="|"), files)]}
                  peak <- files[grep(pattern = "_peak_", files)]
                  if(length(peak) != 0){files <- files[!grepl(paste(peak, collapse="|"), files)]}
                  wind_speed_58 <- files[grep(pattern = "_58", files)]
                  if(length(wind_speed_58) != 0){files <- files[!grepl(paste(wind_speed_58, collapse="|"), files)]}
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("anemometer",payload_sensor))
                  
            }else if(t == "wind_gust"){
                  # removed:
                  ave <- files[grep(pattern = paste0(t, "_averaging_period"), files)]
                  if(length(ave) != 0){files <- files[!grepl(paste(ave, collapse="|"), files)]}      
                  windgust_2 <- files[grep(pattern = "_wind_gust_2", files)]
                  if(length(windgust_2) != 0){files <- files[!grepl(paste(windgust_2, collapse="|"), files)]}
                  wind_gust_58 <- files[grep(pattern = "_58", files)]
                  if(length(wind_gust_58) != 0){files <- files[!grepl(paste(wind_gust_58, collapse="|"), files)]}
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("anemometer",payload_sensor))
                  
            }else if(t == "dominant_period"){
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("wave_sensor",payload_sensor))
                  
            }else if(t == "average_period"){
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("wave_sensor",payload_sensor))

            }else if(t == "air_pressure"){
                  # removed:
                  minute_air <- files[grep(pattern = "_minimum_1_minute_air_", files)]
                  if(length(minute_air) != 0){files <- files[!grepl(paste(minute_air, collapse="|"), files)]}
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("barometer",payload_sensor))
                  
            }else if(t == "air_temperature"){
                  # removing dew point temperature 
                  dew <- files[grep(pattern = "_dew_", files)]
                  if(length(dew) != 0){files <- files[!grepl(paste(dew, collapse="|"), files)]}
                  # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  # filter metadata
                  metadata <- dplyr::filter(metadata, grepl("air_temperature",payload_sensor))
                  
            }else { # significant wave height, mean wave direction, sea surface temperature, dew point temperature
                  # # isolate secondary files
                  files_secondary <- files[grep(pattern = "_2_", files)]
                  if(length(files_secondary) != 0){if(unique(files == files_secondary)==TRUE){files_secondary <- NULL}}
                  if(length(files_secondary) != 0){files <- files[!grepl(paste(files_secondary, collapse="|"), files)]}
                  if(length(files_secondary) == 0){rm(files_secondary)}
                  # used:
                  df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
                  df_files <- df_files[order(files)]
                  df_files <- unique(df_files)
                  df_files <- data.frame(df_files, stringsAsFactors = F)
                  df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
                  write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
                  rm(df_files)
                  
                  if(t == "dew_point_temperature"){metadata <- dplyr::filter(metadata, grepl("air_temperature",payload_sensor))
                  }else if(t == "sea_surface_temperature"){metadata <- dplyr::filter(metadata, grepl("ocean_temperature",payload_sensor))
                  }else {metadata <- dplyr::filter(metadata, grepl("wave_sensor",payload_sensor))}
            }
            print(head(files))
            print(tail(files))

            if(length(files)>0){
                # loop for each data type
                if(exists("files_secondary")){
                  data_sets <- c("files", "files_secondary")
                } else{
                  data_sets <- "files"
                }
                # order metadata file
                if(dim(metadata)[1]>0){
                    metadata <- metadata[order(metadata$file),]
                    # rename the rows to reflect unique data
                    row.names(metadata) <- 1:nrow(metadata)
                }
                for (df in data_sets){
                    df_2 <- get(df)
                    for (file in df_2){
                          print(file)
                          # if the merged dataset doesn't exist, create it
                          if (!exists("dataset")){
                                dataset <- read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                                # set date format
                                if(IsDate_dmy_hms(dataset[1,1])==TRUE){dataset[,1] <- dmy_hms(dataset[,1]); print("dmy_hms dates converted")}
                                if(IsDate_ymd_hms(dataset[1,1])==TRUE){dataset[,1] <- ymd_hms(dataset[,1]); print("ymd_hms dates converted")}
                                if(IsDate_mdy_hm(dataset[1,1])==TRUE){dataset[,1] <- mdy_hm(dataset[,1]); print("mdy_hm dates converted")}
                                
                                # reformatting the data structure
                                if("qc_flag" %in% names(dataset)){print("qc data available")}else{dataset$qc_flag <- NA}
                                if("release_flag" %in% names(dataset)){print("release_flag available")}else{dataset$release_flag <- NA}
                                
                                if(dim(dataset)[2]>4){
                                    if(t == "lat"){dataset <- dplyr::select(dataset, time, lat, qc_flag, release_flag)}
                                    if(t == "lon"){dataset <- dplyr::select(dataset, time, lon, qc_flag, release_flag)}
                                }
                                # converting Kelvin to deg C if necessary
                                if(t != "lat" & t != "lon"){
                                    if("temperature" %in% unlist(strsplit(t,"_"))){
                                      dataset[,4] <- ifelse(dataset[,4] >= 100,
                                                            dataset[,4] - 273.15, 
                                                            dataset[,4])
                                      print("temperature converted from Kelvin to C")
                                    }
                                    # converting pressure to bars if necessary
                                    if("pressure" %in% unlist(strsplit(t,"_"))){
                                      dataset[,4] <- ifelse(dataset[,4] > 10000,
                                                            dataset[,4] / 100, 
                                                            dataset[,4])
                                      print("pressure convert to bars")
                                    }
                                    if("lat" %in% names(dataset)){dataset <- dplyr::select(dataset, -"lat"); print("lat removed before merge")}
                                    if("lon" %in% names(dataset)){dataset <- dplyr::select(dataset, -"lon"); print("lon removed before merge")}
                                }
                                colnames(dataset) <- c("DateTime", t, "qc_flag", "release_flag")
                                # save only unique data
                                dataset <- unique(dataset)
                                # add sensor information
                                dataset$payload_sensor <- gsub(".csv","",unlist(strsplit(file,"/"))[length(unlist(strsplit(file,"/")))])
                          }
                          # if the merged dataset does exist, append to it
                          if (exists("dataset")){
                                temp_dataset <-read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)

                                # set date format
                                if(IsDate_dmy_hms(temp_dataset[1,1])==TRUE){temp_dataset[,1] <- dmy_hms(temp_dataset[,1]); print("dmy_hms dates converted")}
                                if(IsDate_ymd_hms(temp_dataset[1,1])==TRUE){temp_dataset[,1] <- ymd_hms(temp_dataset[,1]); print("ymd_hms dates converted")}
                                if(IsDate_mdy_hm(temp_dataset[1,1])==TRUE){temp_dataset[,1] <- mdy_hm(temp_dataset[,1]); print("mdy_hm dates converted")}
                                
                                # reformatting the data structure
                                if("QC_Flags" %in% names(temp_dataset)){temp_dataset <- dplyr::rename(temp_dataset, "qc_flag" = "QC_Flags")}
                                if("qc_flag" %in% names(temp_dataset)){print("qc data available")}else{temp_dataset$qc_flag <- NA}
                                if("release_flag" %in% names(temp_dataset)){print("release_flag available")}else{temp_dataset$release_flag <- NA}
                                
                                if(dim(temp_dataset)[2]>4){
                                    if(t == "lat"){temp_dataset <- dplyr::select(temp_dataset, time, lat, qc_flag, release_flag)}
                                    if(t == "lon"){temp_dataset <- dplyr::select(temp_dataset, time, lon, qc_flag, release_flag)}
                                }
                                # converting Kelvin to deg C if necessary
                                if(t != "lat" & t != "lon"){
                                    if("temperature" %in% unlist(strsplit(t,"_"))){
                                      temp_dataset[,4] <- ifelse(temp_dataset[,4] > 100,
                                                                 temp_dataset[,4] - 273.15, 
                                                                 temp_dataset[,4])
                                      print("temperature converted from Kelvin to C")
                                    }
                                    # converting pressure to bars if necessary
                                    if("pressure" %in% unlist(strsplit(t,"_"))){
                                      temp_dataset[,4] <- ifelse(temp_dataset[,4] > 10000,
                                                                 temp_dataset[,4] / 100, 
                                                                 temp_dataset[,4])
                                      print("pressure convert to bars")
                                    }
                                    if("lat" %in% names(temp_dataset)){temp_dataset <- dplyr::select(temp_dataset, -"lat"); print("lat removed before merge")}
                                    if("lon" %in% names(temp_dataset)){temp_dataset <- dplyr::select(temp_dataset, -"lon"); print("lon removed before merge")}
                                }
                                colnames(temp_dataset) <- c("DateTime", t, "qc_flag", "release_flag")
                                 # save only unique data
                                temp_dataset <- unique(temp_dataset)
                                # add sensor information
                                temp_dataset$payload_sensor <- gsub(".csv","",unlist(strsplit(file,"/"))[length(unlist(strsplit(file,"/")))])
                                # merge datasets
                                dataset<-rbind(dataset, temp_dataset)
                                rm(temp_dataset)
                          }
                    }
                    # ordering the dataset by date and selecting unique values only
                    dataset <- dataset[order(dataset$DateTime),]
                    dataset <- unique(dataset)
                    # rename the rows to reflect unique data
                    row.names(dataset) <- 1:nrow(dataset)
                    # for saving
                    year_range <- paste0(year(dataset$DateTime[1]),"_", year(dataset$DateTime[nrow(dataset)]))
                    # sep primary and secondary data
                    if(df == "files"){concat_name <- paste0(buoy, "_",t,"_",year_range,"_concat_primary")}
                    if(df == "files_secondary"){concat_name <- paste0(buoy, "_",t,"_",year_range,"_concat_secondary")}
                    # save individual datasets
                    library(lubridate)
                    # write.csv(dataset, paste0(concat_ind_dir,concat_name,".csv"), row.names=FALSE)
                    # saveRDS(dataset, file = paste0(concat_ind_dir,concat_name,".rds"))
                    
                    # remove flagged data
                    # Looping through all NDBC QC flags and removing flagged raw data
                    dataset[,3][is.na(dataset[,3])] <- 0                  # replace NA with zero when flags not included in original dataset
                    dataset[,4][is.na(dataset[,4])] <- 0
                    flags <- c("W", "R", "V", "M", "T", "D", "U", "L", "H", "2", "3","_")   # ignore S flags
                    for (i in flags){
                        # i <- flags[1]
                        index <- dataset$qc_flag == i
                        dataset[index,2] <- NA
                        # dataset <- subset(dataset, qc_flag != i)
                    }
                    # order for released data and remove duplicated date rows
                    if(nrow(dataset[!duplicated(dataset$DateTime),]) != nrow(dataset)){
                        if(length(unique(dataset$release_flag))>2){
                              dataset = dataset[order(dataset[,'DateTime'],-dataset[,'release_flag']),]
                              dataset = dataset[!duplicated(dataset$DateTime),]
                              print("removing duplicated data based on release flag")
                        }else{
                              dataset = dataset[!duplicated(dataset$DateTime),]
                              print("removing duplicated data NOT based on release flag")
                        }
                    }

                    # remove redundant columns
                    library(dplyr)
                    dataset <- dplyr::select(dataset,-"qc_flag")
                    dataset <- dplyr::select(dataset,-"release_flag")
      
                    # sep primary and secondary data
                    if(df == "files"){dataset1 <- dataset}
                    if(df == "files_secondary"){dataset2 <- dataset}
                    rm(dataset)
                } # loop through datasets
              
                # loop through multiple payloads
                if(length(data_sets) > 1){
                    if(dim(metadata)[1]>0){
                          # prepping the metadata
                          metadata1 <- metadata
                          metadata1$join <- gsub("/","_",paste0(gsub(".nc","",metadata1$file), "_",metadata1$payload_sensor,"_",t))
                          metadata1 <- metadata1[,7:ncol(metadata1)]
                          # handling different air pressure nomenclature
                          if(t == "air_pressure"){
                            metadata1$join <- gsub("_air_pressure_at_sea_level","_air_pressure", metadata1$join)
                            dataset1$payload_sensor <- gsub("_air_pressure_at_sea_level","_air_pressure", dataset1$payload_sensor)
                            dataset2$payload_sensor <- gsub("_air_pressure_at_sea_level","_air_pressure", dataset2$payload_sensor)
                          }
                          # add primary sensor information
                          dataset1 <- dplyr::rename(dataset1, join = payload_sensor)
                          dataset1 <- left_join(dataset1, metadata1, by = "join")
                          # add secondary sensor information
                          dataset2 <- dplyr::rename(dataset2, join = payload_sensor)
                          dataset2 <- left_join(dataset2, metadata1, by = "join")
                          # rename columns
                          colnames(dataset1) <- c("DateTime", paste0(colnames(dataset1[2:ncol(dataset1)]), "_1"))
                          colnames(dataset2) <- c("DateTime", paste0(colnames(dataset2[2:ncol(dataset2)]), "_2"))
                          rm(metadata, metadata1)
                    }else{
                          dataset$payload_sensor <- "no information available"
                          colnames(dataset) <- c("DateTime", t, paste0(t,"_payload_sensor"))
                    }
                    # Prep for secondary sensor data if primary sensor data is NA
                    dataset <- full_join(dataset1, dataset2, by = "DateTime")
                    rm(dataset1,dataset2)
                    # rename columns
                    colnames(dataset) = gsub("\\.x", "_1", colnames(dataset))
                    colnames(dataset) = gsub("\\.y", "_2", colnames(dataset))
                    # check for duplicates
                    dataset <- dataset[order(dataset$DateTime),]
                    dataset <- unique(dataset)
                    # rename the rows to reflect unique data
                    row.names(dataset) <- 1:nrow(dataset)
                    # export data
                    year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                    # write.csv(dataset, paste0(ncei_dir,buoy,"/s_",buoy, "_ncei_",t,"_primary_secondary_",year_range,".csv"), row.names=FALSE)
                    # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/s_",buoy,"_ncei_",t,"_primary_secondary_",year_range,".rds"))
                    
                    # concat metadata
                    dataset$metadata_1 <- paste0(dataset[,3],"__",dataset[,4],"__",dataset[,5],"__",dataset[,7],"__",dataset[,8],"__",dataset[,9],"__",dataset[,10],"__",dataset[,11],"__",dataset[,12])
                    dataset$metadata_1 <- gsub("__NA","",dataset$metadata_1)
                    dataset$metadata_2 <- paste0(dataset[,14],"__",dataset[,15],"__",dataset[,16],"__",dataset[,18],"__",dataset[,19],"__",dataset[,20],"__",dataset[,21],"__",dataset[,22],"__",dataset[,23])
                    dataset$metadata_2 <- gsub("__NA","",dataset$metadata_2)
                    # subset relevant data
                    dataset <- dataset[,c(1,2,24,13,25)]
                    # rename columns
                    if(dim(dataset)[2]==3){
                          colnames(dataset)<-c("DateTime",t,paste0(t, "_metadata"))
                    }else if(dim(dataset)[2]==2){
                          if(t %in% names(dataset)){print("")
                          }else{dataset$dat <- NA; dat_names <- names(dataset); dataset <- dplyr::select(dataset, DateTime, dat,dat_names[2])
                                colnames(dataset)<-c("DateTime",t,paste0(t, "_metadata"))
                          }
                    }else{
                          colnames(dataset) <- c("DateTime", paste0(t,"_1"), paste0(t,"_metadata_1"),paste0(t,"_2"), paste0(t,"_metadata_2"))
                    }
                    # export data
                    year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                    # write.csv(dataset, paste0(ncei_dir,buoy,"/s_",buoy, "_ncei_",t,"_concat_primary_secondary_metadata_",year_range,".csv"), row.names=FALSE)
                    # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/s_",buoy,"_ncei_",t,"_concat_primary_secondary_metadata_",year_range,".rds"))
                    # housekeeping
                    rm(dataset1, dataset2, metadata1, metadata2)
                }else{
                    dataset <- dataset1
                    # add sensor information
                    if(dim(metadata)[1]>0){
                          metadata1 <- metadata
                          metadata1$join <- gsub("/","_",paste0(gsub(".nc","",metadata1$file), "_",metadata1$payload_sensor,"_",t))
                          metadata1 <- metadata1[,7:ncol(metadata1)]
                          # handling different air pressure nomenclature
                          if(t == "air_pressure"){
                            metadata1$join <- gsub("_air_pressure_at_sea_level","_air_pressure", metadata1$join)
                            dataset$payload_sensor <- gsub("_air_pressure_at_sea_level","_air_pressure", dataset$payload_sensor)
                          }
                          # add primary sensor information
                          dataset <- dplyr::rename(dataset, join = payload_sensor)
                          dataset <- left_join(dataset, metadata1, by = "join")
                          # export data
                          year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                          # write.csv(dataset, paste0(ncei_dir,buoy,"/s_",buoy, "_ncei_",t,"_primary_",year_range,".csv"), row.names=FALSE)
                          # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/s_",buoy,"_ncei_",t,"_primary_",year_range,".rds"))
                          
                          # convert for concat
                          dataset$join <- gsub(paste0("_",t),"",dataset$join)
                          dataset$metadata <- paste0(dataset[,3],"__",dataset[,4],"__",dataset[,5],"__",dataset[,7],"__",dataset[,8],"__",dataset[,9],"__",dataset[,10],"__",dataset[,11],"__",dataset[,12])
                          dataset$metadata <- gsub("__NA","",dataset$metadata)
                          dataset <- dataset[,c(1,2,13)]
                          # rename columns
                          colnames(dataset) <- c("DateTime", t, paste0(t,"_metadata"))
                          rm(dataset1, metadata1)
                    }else{
                          dataset$payload_sensor <- "no information available"
                          colnames(dataset) <- c("DateTime", t, paste0(t,"_metadata"))
                    }
                    rm(dataset1)
                }
                # removing duplicated date/times added from secondary dataset (accounts for slightly different data values caused by significant places)
                dataset <- dataset[!duplicated(dataset$DateTime),]
                # ordering the dataset by date and selecting unique values only
                dataset <- dataset[order(dataset$DateTime),]
                dataset <- unique(dataset)
                # rename the rows to reflect unique data
                row.names(dataset) <- 1:nrow(dataset)
                # concat files
                if (!exists("ncei_concat_stdmet")){
                    ncei_concat_stdmet <- dataset
                }else{
                    ncei_concat_stdmet<-dplyr::full_join(ncei_concat_stdmet, dataset, by = "DateTime")
                    ncei_concat_stdmet <- ncei_concat_stdmet[order(ncei_concat_stdmet$DateTime),]
                }
                rm(dataset)
            }else{print(paste("no data for ",t))}
        } # end of concat data loop
    
        dataset <- ncei_concat_stdmet
        # # save dataset
        # year_range <- paste0(year(dataset$DateTime[1]),"_", year(dataset$DateTime[nrow(dataset)]))
        # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/s_",buoy,"_ncei_concat_stdmet_",year_range,"_v1.rds"))
        
        # remove '0' wind gust data when no wind speed is present (not flagged in orig files)
        name_list <- names(dataset)
        if("lat_1" %in% name_list){
            dataset$lat_2 <- NULL; dataset$lat_metadata_2 <- NULL; dataset$lon_2 <- NULL; dataset$lon_metadata_2 <- NULL
            dataset <- dplyr::rename(dataset, lat = lat_1, lon = lon_1,lat_metadata = lat_metadata_1, lon_metadata = lon_metadata_1 )
        }
        # account for erroneous data
        if("wind_gust" %in% name_list){
          library(stringr)
          if(sum(str_count(name_list,"wind_gust"))>2){ # accounts for wind_gust and wind_gust_metadata
              dataset$wind_gust_1 <- ifelse(dataset$wind_gust_1 == 0 & dataset$wind_speed_1 != 0,NA, dataset$wind_gust_1)
              dataset$wind_gust_2 <- ifelse(dataset$wind_gust_2 == 0 & dataset$wind_speed_2 != 0,NA, dataset$wind_gust_2)
          }else{dataset$wind_gust <- ifelse(dataset$wind_gust == 0 & dataset$wind_speed != 0,NA, dataset$wind_gust)}
        }
        assign("dataset", dataset, envir=parent.frame())
    
  }else if(input == "spec"){
      #----------------------------------------------------------------------------------------
      # spectral data - 
      #----------------------------------------------------------------------------------------
      # data_types_spec <- c("c11", "c11m", "alpha1", "alpha2", "r1", "r2", "C12", "C13", "C22", "C33",
      #                      "Q12", "Q13", "gamma2", "gamma3", "phih", "rhq", "sensor_output") # c11 = spectral energy, c11m = uncorrected spectral energy

      # start_year <- 1979
      # create data avail dir and folder
      if (!file.exists(paste0(ncei_dir,buoy,"/data_availability"))) {dir.create((paste0(ncei_dir,buoy,"/data_availability")))}
      data_avail_dir <- paste0(ncei_dir,buoy,"/data_availability/s_")
      print(paste0("Working on...",t))
      library(lubridate)
      library(tidyr)
      # subset conditions for each variable - because NBDC loves duplication and format change for no reason
      if(t == "c11"){   # I believe k is corrected
            # see what's available:
            files1 <- file_list[grep(pattern = "spectral_density", file_list)]
            files2 <- file_list[grep(pattern = t, file_list)]
            files3 <- file_list[grep(pattern = "C11", file_list)]
            files4 <- file_list[grep(pattern = "c11", file_list)]
            files <- c(files1, files2, files3, files4)  
            c11m <- files[grep(pattern = "_c11m.csv", files)]
            if(length(c11m) != 0){files <- files[!grepl(paste(c11m, collapse="|"), files)]}
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_c11_nondir_spectral_available_files_all.csv"), row.names=FALSE)
            # used:
            files1 <- file_list[grep(pattern = "spectral_density", file_list)]
            files2 <- file_list[grep(pattern = t, file_list)]
            files <- c(files1, files2)
            files <- sort(files)
            c11m <- files[grep(pattern = "_c11m.csv", files)]
            if(length(c11m) != 0){files <- files[!grepl(paste(c11m, collapse="|"), files)]}
            c11i <- files[grep(pattern = "_c11_i.csv", files)]
            if(length(c11i) != 0){files <- files[!grepl(paste(c11i, collapse="|"), files)] }
            # payload_2 <- files[grep(pattern = "_payload_2", files)]
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_c11_nondir_spectral_available_files_USED.csv"), row.names=FALSE)
            
      } else if(t == "c11m"){  # I believe i is uncorrected
            # see what's available:
            files1 <- file_list[grep(pattern = "c11", file_list)]
            files2 <- file_list[grep(pattern = "C11", file_list)]
            files <- c(files1, files2)  
            c11 <- files[grep(pattern = "_c11.csv", files)]
            if(length(c11) != 0){files <- files[!grepl(paste(c11, collapse="|"), files)]}
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_c11m_uncorrected_nondir_spectral_available_files_all.csv"), row.names=FALSE)
            # used:
            files1 <- file_list[grep(pattern = "c11", file_list)]
            files2 <- file_list[grep(pattern = "C11", file_list)]
            files <- c(files1, files2)
            files <- sort(files)
            c11 <- files[grep(pattern = "_c11.csv", files)]
            if(length(c11) != 0){files <- files[!grepl(paste(c11, collapse="|"), files)]}
            c11k <- files[grep(pattern = "_c11_k.csv", files)]
            if(length(c11k) != 0){files <- files[!grepl(paste(c11k, collapse="|"), files)]}
            c11_i <- files[grep(pattern = "_c11_i.csv", files)]
            if(length(c11_i) != 0){files <- files[!grepl(paste(c11_i, collapse="|"), files)]}
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_c11m_uncorrected_nondir_spectral_available_files_USED.csv"), row.names=FALSE)
        
      }else if(t == "alpha1"){
            # see what's available:
            files <- file_list[grep(pattern = t, file_list)]
            files <- sort(files)
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_alpha1_available_files_all.csv"), row.names=FALSE)
            # used:
            files <- file_list[grep(pattern = t, file_list)]
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_alpha1_available_files_USED.csv"), row.names=FALSE)
        
      }else if(t == "alpha2"){
            # see what's available:
            files <- file_list[grep(pattern = t, file_list)]
            files <- sort(files)
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_alpha2_available_files_all.csv"), row.names=FALSE)
            # used:
            files <- file_list[grep(pattern = t, file_list)]
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_alpha2_available_files_USED.csv"), row.names=FALSE)
        
      }else if(t == "r1"){
            # see what's available:
            files <- file_list[grep(pattern = t, file_list)]
            files <- sort(files)
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_r1_available_files_all.csv"), row.names=FALSE)
            # used:
            files <- file_list[grep(pattern = t, file_list)]
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_r1_available_files_USED.csv"), row.names=FALSE)
        
      }else if(t == "r2"){
            # see what's available:
            files <- file_list[grep(pattern = t, file_list)]
            files <- sort(files)
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_r2_available_files_all.csv"), row.names=FALSE)
            # used:
            files <- file_list[grep(pattern = t, file_list)]
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_r2_available_files_USED.csv"), row.names=FALSE)
        
      }else{ # "C12", "C13", "C22", "C33", "Q12", "Q13", "gamma2", "gamma3", "phih", "rhq", "sensor_output"
            # see what's available:
            files <- file_list[grep(pattern = t, file_list)]
            files <- sort(files)
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_all.csv"), row.names=FALSE)
            # used:
            files <- file_list[grep(pattern = t, file_list)]
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",files)
            df_files <- df_files[order(files)]
            df_files <- unique(df_files)
            df_files <- data.frame(df_files, stringsAsFactors = F)
            df_files <- separate(df_files, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files, paste0(data_avail_dir,buoy, "_",t,"_available_files_USED.csv"), row.names=FALSE)
      }
      print(head(files))
      print(tail(files))
      rm(df_files)
      
      if(length(files)>0){
          # loop for each data type
          if(t == "sensor_output"){
                for (file in files){
                      if (!exists("dataset")){
                              dataset <- read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                              # set date format
                              if(IsDate_dmy_hms(dataset[1,1])==TRUE){dataset[,1] <- dmy_hms(dataset[,1]); print("dmy_hms dates converted")}
                              if(IsDate_ymd_hms(dataset[1,1])==TRUE){dataset[,1] <- ymd_hms(dataset[,1]); print("ymd_hms dates converted")}
                              if(IsDate_mdy_hm(dataset[1,1])==TRUE){dataset[,1] <- mdy_hm(dataset[,1]); print("mdy_hm dates converted")}
                              dataset <- dplyr::select(dataset, -"lat")
                              dataset <- dplyr::select(dataset, -"lon")
                              dataset <- dplyr::rename(dataset, "DateTime" = "time")
                      }
                      # if the merged dataset does exist, append to it
                      if (exists("dataset")){
                              temp_dataset <-read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                              # set date format
                              if(IsDate_dmy_hms(temp_dataset[1,1])==TRUE){temp_dataset[,1] <- dmy_hms(temp_dataset[,1]); print("dmy_hms dates converted")}
                              if(IsDate_ymd_hms(temp_dataset[1,1])==TRUE){temp_dataset[,1] <- ymd_hms(temp_dataset[,1]); print("ymd_hms dates converted")}
                              if(IsDate_mdy_hm(temp_dataset[1,1])==TRUE){temp_dataset[,1] <- mdy_hm(temp_dataset[,1]); print("mdy_hm dates converted")}
                              # temp_dataset[,1] <- ymd_hms(temp_dataset[,1])   # set date format
                              temp_dataset <- dplyr::select(temp_dataset, -"lat")
                              temp_dataset <- dplyr::select(temp_dataset, -"lon")
                              colnames(temp_dataset) <- names(dataset)
                              # merge datasets
                              dataset<-rbind(dataset, temp_dataset)
                              rm(temp_dataset)
                      }
                }
                # ordering the dataset by date and selecting unique values only
                dataset <- dataset[order(dataset$DateTime),]
                dataset <- unique(dataset)
                # rename the rows to reflect unique data
                row.names(dataset) <- 1:nrow(dataset)
                # save individual datasets
                library(lubridate)
                year_range <- paste0(year(dataset$DateTime[1]),"_", year(dataset$DateTime[nrow(dataset)]))
                # write.csv(dataset, paste0(concat_ind_dir,buoy, "_",t,"_",year_range,"_concat.csv"), row.names=FALSE)
                # saveRDS(dataset, file = paste0(concat_ind_dir,buoy, "_",t,"_",year_range,"_concat.rds"))
                # remove redundant rows                  
                dataset <- unique(dataset)
                library(dplyr)
                dataset <- dplyr::select(dataset,-"qc_flag")
                dataset <- dplyr::select(dataset,-"release_flag")
                dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset)-1, ]
                assign("dataset", dataset, envir=parent.frame())
    
          }else{

              colNames_spec_new <- c("DateTime","lat","lon","0.0200", "0.0325", "0.0375", "0.0425", "0.0475", "0.0525",
                                     "0.0575", "0.0625", "0.0675", "0.0725", "0.0775", "0.0825", "0.0875", "0.0925", "0.1000", "0.1100",
                                     "0.1200", "0.1300", "0.1400", "0.1500", "0.1600", "0.1700", "0.1800", "0.1900", "0.2000", "0.2100", 
                                     "0.2200", "0.2300", "0.2400", "0.2500", "0.2600", "0.2700", "0.2800", "0.2900", "0.3000", "0.3100", 
                                     "0.3200", "0.3300", "0.3400", "0.3500", "0.3650", "0.3850", "0.4050", "0.4250", "0.4450", "0.4650", 
                                     "0.4850")
              colNames_spec_old <- c("DateTime","lat","lon","0.0100","0.0200","0.0300","0.0400","0.0500","0.0600","0.0700","0.0800","0.0900","0.1000","0.1100","0.1200",
                                     "0.1300","0.1400","0.1500","0.1600","0.1700","0.1800","0.1900","0.2000","0.2100","0.2200",   
                                     "0.2300","0.2400","0.2500","0.2600","0.2700","0.2800","0.2900","0.3000","0.3100","0.3200",   
                                     "0.3300","0.3400","0.3500","0.3600","0.3700","0.3800","0.3900","0.4000")
              
              dataset_spec_new <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames_spec_new)))
              colnames(dataset_spec_new) <- colNames_spec_new
              dataset_spec_new$DateTime <- lubridate::ymd_hms(dataset_spec_new$DateTime)
                
              dataset_spec_old <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames_spec_old)))
              colnames(dataset_spec_old) <- colNames_spec_old
              dataset_spec_old$DateTime <- lubridate::ymd_hms(dataset_spec_old$DateTime)
              
              count <- 0

              for (file in files){  
                    print(file)
                    # load dataset
                    dataset <- read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                    # set date format
                    if(IsDate_dmy_hms(dataset[1,1])==TRUE){dataset[,1] <- dmy_hms(dataset[,1]); print("dmy_hms dates converted")}
                    if(IsDate_ymd_hms(dataset[1,1])==TRUE){dataset[,1] <- ymd_hms(dataset[,1]); print("ymd_hms dates converted")}
                    if(IsDate_mdy_hm(dataset[1,1])==TRUE){dataset[,1] <- mdy_hm(dataset[,1]); print("mdy_hm dates converted")}
                    library(lubridate)
                    # remove the X from the column headers
                    colnames(dataset) <-  sub("X", "", colnames(dataset))
                    header <- names(dataset)
                    header <- read_header(header)
                    colnames(dataset) <- header
                    if("time" %in% names(dataset)){dataset <- dplyr::rename(dataset,"DateTime" = "time")}
                    if("lat" %in% names(dataset)){
                          print("lat and lon present")
                    }else{
                          dataset$lat <- NA; dataset$lon <- NA
                          print("adding lat and lon")
                    }
                    
                    # find datasets
                    dataset_list <- ls(pattern = "dataset_spec_")
                    dataset_list <- dataset_list[dataset_list %in% c("dataset_spec_new","dataset_spec_old") == TRUE]
                    print(dataset_list)
                    # build datasets
                    
                    for(matchable in dataset_list){
                        print(matchable)
                        dat <- get(matchable)
                        
                        if(exists("dataset")){
                              ndbc_freq <- names(dat)
                              dat_names <- names(dataset)
                              setdiff(ndbc_freq, dat_names)
                              print(unique(dat_names %in% ndbc_freq))
                              # test if columns match
                              if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                    print(paste0(matchable," didn't match"))
                              }else{
                                    tryCatch({
                                        library(plyr)
                                        dat <- rbind.fill(dat,dataset)
                                        print(paste0("added to: ",matchable))
                                        assign(matchable,dat)
                                        rm(dataset)
                                   }, error = function(e) {
                                        print("dataset doesn't match")
                                   })
                              }
                        }
                        rm(dat)
                    }
                    if(exists("dataset")){
                        dataset_list <- ls(pattern = "dataset_spec_")
                        dataset_list <- dataset_list[dataset_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                        print(dataset_list)
                        # build datasets
                        
                        for(matchable in dataset_list){
                            print(matchable)
                            dat <- get(matchable)
                            
                            if(exists("dataset")){
                                ndbc_freq <- names(dat)
                                dat_names <- names(dataset)
                                setdiff(ndbc_freq, dat_names)
                                print(unique(dat_names %in% ndbc_freq))
                                # test if columns match
                                if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                    print(paste0(matchable," didn't match"))
                                }else{
                                    tryCatch({
                                        library(plyr)
                                        dat <- rbind.fill(dat,dataset)
                                        print(paste0("added to: ",matchable))
                                        assign(matchable,dat)
                                        rm(dataset)
                                    },  error = function(e) {
                                        print("dataset doesn't match")
                                    })
                                }
                            }
                            rm(dat)
                        }
                    }
                    if(exists("dataset")){
                        count <- count + 1  
                        dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = dim(dataset)[2]))
                        dataset_spec<-rbind(dataset_spec, dataset)
                        new_name <- paste0("dataset_spec_",count)
                        assign(new_name, dataset_spec)
                        print(paste0("data added to NEW DF:: ",new_name))
                        rm(dataset, dataset_spec)
                    }
              }
              
              # export from function
              dat_list <- ls(pattern = "dataset_spec")
              print(dat_list)
              for(d in dat_list){
                    df <- get(d)
                    if(dim(df)[1] > 0){
                          # remove rows with no data
                          completeFun <- function(data, desiredCols) {completeVec <- complete.cases(data[, desiredCols]); return(data[completeVec, ])}
                          # remove empty rows across df
                          df <- df[rowSums(is.na(df)) != ncol(df),]
                          # remove rows with no DateTime
                          df <- df[!is.na(df$DateTime),]
                          # remove rows with multiple version of missing data
                          df <- completeFun(df,1:3) # no DateTime,lat,lon
                          for(desiredCount in 3:5){df <- df[rowSums(is.na(df)) != ncol(df)-desiredCount,]} # no data in data columns
                          # ordering the dataset by date and selecting unique values only
                          if(dim(df)[1]>0){
                                  df <- df[order(df$DateTime),]
                                  df <- unique(df)
                                  # rename the rows to reflect unique data
                                  row.names(df) <- 1:nrow(df)        
                          }
                    }
                    # export from function
                    if(dim(df)[1] !=0){assign(d, df, envir=parent.frame())}
              }
              rm(d)
          }
      }
  }
}