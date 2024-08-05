concat_ncei_2023 <- function(files = "list of files", start_year = "earliest dataset year", input = "stdmet or spec", drive = "drive", buoy = "buoy"){
  
  NA_strings <- c(9.96920996838687E+36,"9.96920996838687E+36","9.96921e+36",9.96921e+36,999.00,999, 
                  "-32767",-32767,"-2147483647",-2147483647,NA, NaN,"NA","NaN","9969209968386869046778552952102584320.000",
                  "9.96920996838686E+36","99.0","9999.0","999","999.0","99.00","999.00","", " ")
  # # format header 
  # read_header <- function(df){
  #     years <- df[1:3]
  #     freq <- df[4:length(df)]
  #     freq <- as.numeric(freq)
  #     freq <- sprintf("%1.4f", freq)
  #     freq <- as.character(freq)
  #     df <- c(years, freq)
  # }
  # # functions to test for date formats
  # library(lubridate)
  # IsDate_dmy_hms <- function(mydate, date.format = "%d-%m-%Y %h:%m:%s") {
  #   tryCatch(!is.na(dmy_hms(mydate, date.format)),
  #            error = function(err) {FALSE})
  # }
  # IsDate_ymd_hms <- function(mydate, date.format = "%Y-%m-%d %h:%m:%s") {
  #   tryCatch(!is.na(ymd_hms(mydate, date.format)),
  #            error = function(err) {FALSE})
  # }
  # IsDate_mdy_hm <- function(mydate, date.format = "%m/%d/%Y %h:%m") {
  #   tryCatch(!is.na(mdy_hm(mydate, date.format)),
  #            error = function(err) {FALSE})
  # }
  
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
  
  data_types_stdmet <- c('lat','lon',  
                         'air_pressure_at_sea_level_primary_sensor','air_pressure_at_sea_level_secondary_sensor',                                   
                         'air_pressure_primary_sensor','air_pressure_secondary_sensor',                                                     
                         'air_temperature_primary_sensor', 
                         'air_temperature_secondary_sensor',
                         'dew_point_temperature_primary_sensor','dew_point_temperature_secondary_sensor',
                         'relative_humidity_primary_sensor','relative_humidity_secondary_sensor',
                         'sea_surface_temperature',                                                         
                         'sea_surface_wave_from_direction',                                                    
                         'sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment',
                         'sea_surface_wave_period_at_variance_spectral_density_maximum',                       
                         'sea_surface_wave_significant_height',                                              
                         'wind_direction_primary_sensor','wind_direction_secondary_sensor',                                                    
                         'wind_gust_primary_sensor','wind_gust_secondary_sensor',                                                         
                         'wind_speed_primary_sensor','wind_speed_secondary_sensor')#, 
                         # 'max_1_minute_wind_speed_primary_sensor','max_1_minute_wind_speed_secondary_sensor')
  
  data_types_spec <- c('sea_surface_wave_variance_spectral_density',                                         
                       'sea_surface_wave_variance_spectral_density_uncorrected', 
                       'sea_surface_wave_spectral_mean_direction',                                           
                       'sea_surface_wave_spectral_principal_direction',                                      
                       'gamma2', 'gamma3',
                       'phih',                                                                               
                       'r1','r2',    
                       'rhq')

  if(input == "stdmet"){
        #----------------------------------------------------------------------------------------
        # met data
        #----------------------------------------------------------------------------------------
        # # merging yearly and monthly datafiles of each type 
        library(lubridate)
        library(tidyr)
        # standard met data - create NCEI matching NDBC web file nomenclature
        for (t in data_types_stdmet){
            # t <- data_types_stdmet[23]
            print(paste0("Working on...",t))
          
          
            if (!file.exists(paste0(ncei_dir,buoy,"/data_availability"))) {dir.create((paste0(ncei_dir,buoy,"/data_availability")))}
            data_avail_dir <- paste0(ncei_dir,buoy,"/data_availability/s_")
            
            # add sensor information
            metadata <- read.csv(paste0(raw_dir,"ncei/metadata/",buoy,"_metadata_ALL.csv"))

            # available files ALL
            files <- sort(files)
            exact_match <- paste0(t,'.csv$')
            df_files_use <- files[grepl(exact_match,files)]
            if(length(df_files_use)<1){ exact_match <- t; df_files_use <- files[grepl(exact_match,files)]} #46035
            if(length(df_files_use)<1){exact_match <- paste0(unlist(strsplit(t,'_primary_sensor'))[1],'.csv'); df_files_use <- files[grepl(exact_match,files)]} # 46077
            if(grepl('wind_',t)){df_files_use <- df_files_use[!grepl('max_1_minute',df_files_use)]} # 46035
            if(grepl('wind_',t)){df_files_use <- df_files_use[!grepl('continuous_',df_files_use)]}  # 46035
            library(stringr)
            if(length(df_files_use) < sum(str_count(df_files_use, "_sensor"))){                       # 46077 with 8 dew point files - wrong!
                  df_files_use <- df_files_use[!grepl('telemetry_primary_sensor.csv',df_files_use)]
                  df_files_use <- df_files_use[!grepl('telemetry_secondary_sensor.csv',df_files_use)]
            }
            if(grepl('_secondary_telemetry', df_files_use[2])){df_files_use <- df_files_use[!grepl('_secondary_telemetry',df_files_use)]}  # 46035, 46077
            df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",df_files_use)
            df_files <- df_files[order(df_files)]
            df_files <- unique(df_files)
            df_files_out <- data.frame(df_files, stringsAsFactors = F)
            df_files_out <- separate(df_files_out, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
            write.csv(df_files_out, paste0(data_avail_dir,buoy, "_",t,"_available_files_all.csv"), row.names=FALSE)
            rm(df_files_out, df_files)
            
            print(head(df_files_use))
            print(tail(df_files_use))

            if(length(df_files_use)>0){
              
                # order metadata file
                if(dim(metadata)[1]>0){
                    metadata <- metadata[order(metadata$file),]
                    # rename the rows to reflect unique data
                    row.names(metadata) <- 1:nrow(metadata)
                }
                
                for (file in df_files_use){
                    # file <- df_files_use[2]
                    print(file)
                    # if the merged dataset doesn't exist, create it
                    if (!exists("dataset")){
                        dataset <- read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                        dataset<- dataset %>% rename(DateTime = time)

                        # reformatting the data structure
                        if("qc_flag" %in% names(dataset)){print("qc data available")}else{dataset$qc_flag <- NA}
                        if("release_flag" %in% names(dataset)){print("release_flag available")}else{dataset$release_flag <- NA}
                        
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
                        }
                        # save only unique data
                        dataset <- unique(dataset)
                        # add sensor information
                        dataset$payload_sensor <- gsub(".csv","",unlist(strsplit(file,"/"))[length(unlist(strsplit(file,"/")))])
                    }else if (exists("dataset")){
                        temp_dataset <-read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                        temp_dataset<- temp_dataset %>% rename(DateTime = time)
                        
                        # reformatting the data structure
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
                        }
                        # colnames(temp_dataset) <- c("DateTime", t, "qc_flag", "release_flag")
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
                concat_name <- paste0(buoy, "_",t,"_",year_range,"_concat")
                
                # remove flagged data
                # Looping through all NDBC QC flags and removing flagged raw data
                dataset$qc_flag[is.na(dataset$qc_flag)] <- 0                  # replace NA with zero when flags not included in original dataset
                dataset$release_flag[is.na(dataset$release_flag)] <- 0

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
                
                # removing duplicated date/times added from secondary dataset (accounts for slightly different data values caused by significant places)
                dataset <- dataset[!duplicated(dataset$DateTime),]
                # ordering the dataset by date and selecting unique values only
                dataset <- dataset[order(dataset$DateTime),]
                dataset <- unique(dataset)
                # rename the rows and columns to reflect unique data
                row.names(dataset) <- 1:nrow(dataset)
                dataset<- dataset %>% rename(setNames("qc_flag", paste0(t, "_qc_flag")))
                dataset<- dataset %>% rename(setNames("release_flag", paste0(t, "_release_flag")))
                dataset<- dataset %>% rename(setNames("payload_sensor", paste0(t, "_payload_sensor")))

                # save individual datasets
                library(lubridate)
                # set new individual concat out_dir
                concat_ind_dir <- gsub('data_availability/s_','concat_ind/',data_avail_dir)
                if (!file.exists(concat_ind_dir)) {dir.create(concat_ind_dir)}
                write.csv(dataset, paste0(concat_ind_dir,concat_name,".csv"), row.names=FALSE)
                saveRDS(dataset, file = paste0(concat_ind_dir,concat_name,".rds"))
                
                # clean up lat lon release
                if(t == 'lat'){dataset$lat_release_flag <- NULL;dataset$lat_payload_sensor <- NULL}
                if(t == 'lon'){dataset$lon_release_flag <- NULL;dataset$lon_payload_sensor <- NULL}
                if(t != 'lat' & t != 'lon'){dataset$lat <- NULL;dataset$lon <- NULL}

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

        # remove '0' wind gust data when no wind speed is present (not flagged in orig files)
        name_list <- names(dataset)
        if("lat_1" %in% name_list){
            dataset$lat_2 <- NULL; dataset$lat_metadata_2 <- NULL; dataset$lon_2 <- NULL; dataset$lon_metadata_2 <- NULL
            dataset <- dplyr::rename(dataset, lat = lat_1, lon = lon_1,lat_metadata = lat_metadata_1, lon_metadata = lon_metadata_1 )
        }
        assign("dataset", dataset, envir=parent.frame())
    
  }else if(input == "spec"){
      #----------------------------------------------------------------------------------------
      # spectral data - 
      #----------------------------------------------------------------------------------------

      # create data avail dir and folder
      if (!file.exists(paste0(ncei_dir,buoy,"/data_availability"))) {dir.create((paste0(ncei_dir,buoy,"/data_availability")))}
      data_avail_dir <- paste0(ncei_dir,buoy,"/data_availability/s_")
      print(paste0("Working on...",t))
      library(lubridate)
      library(tidyr)
      
      # available files ALL
      files <- sort(files)
      exact_match <- paste0(t,'.csv$')
      df_files_use <- files[grepl(exact_match,files)]
      if(length(df_files_use)<1){ #46035
        exact_match <- t
        df_files_use <- files[grepl(exact_match,files)]
      }
      if(grepl('_telemetry', df_files_use[2])){df_files_use <- df_files_use[!grepl('_secondary_telemetry',df_files_use)]}  # 46035
      df_files <- gsub(paste0(ascii_ncei_dir, buoy,"/",buoy,"_"),"",df_files_use)
      df_files <- df_files[order(df_files)]
      df_files <- unique(df_files)
      df_files_out <- data.frame(df_files, stringsAsFactors = F)
      df_files_out <- separate(df_files_out, df_files, into = c('date', 'file'),sep = "/", remove = TRUE)
      write.csv(df_files_out, paste0(data_avail_dir,buoy, "_",t,"_available_files_all.csv"), row.names=FALSE)
      rm(df_files_out, df_files)
      
      print(head(df_files_use))
      print(tail(df_files_use))
      
      if(length(files)>0){
          
          colNames_spec_new <- c("DateTime","lat","lon","0.0200", "0.0325", "0.0375", "0.0425", "0.0475", "0.0525",
                                 "0.0575", "0.0625", "0.0675", "0.0725", "0.0775", "0.0825", "0.0875", "0.0925", "0.1000", "0.1100",
                                 "0.1200", "0.1300", "0.1400", "0.1500", "0.1600", "0.1700", "0.1800", "0.1900", "0.2000", "0.2100", 
                                 "0.2200", "0.2300", "0.2400", "0.2500", "0.2600", "0.2700", "0.2800", "0.2900", "0.3000", "0.3100", 
                                 "0.3200", "0.3300", "0.3400", "0.3500", "0.3650", "0.3850", "0.4050", "0.4250", "0.4450", "0.4650", 
                                 "0.4850")
          
          dataset <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames_spec_new)))
          colnames(dataset) <- colNames_spec_new
          dataset$DateTime <- lubridate::ymd_hms(dataset$DateTime)

          for (file in df_files_use){
                # file <- df_files_use[1]
                print(file)
                # load dataset
                temp.dataset <- read.table(file, header=TRUE, na.strings = NA_strings, fill = T, sep = ",", stringsAsFactors = FALSE)
                colnames(temp.dataset) <- colNames_spec_new
                # set date format
                library(lubridate)
                temp.dataset$DateTime <- ymd_hms(temp.dataset$DateTime)
                
                # merge files
                dataset <- rbind(dataset,temp.dataset)
                rm(temp.dataset)
          }
          
          # removing duplicated date/times added from secondary dataset (accounts for slightly different data values caused by significant places)
          dataset <- dataset[!duplicated(dataset$DateTime),]
          # ordering the dataset by date and selecting unique values only
          dataset <- dataset[order(dataset$DateTime),]
          dataset <- unique(dataset)
          # rename the rows and columns to reflect unique data
          row.names(dataset) <- 1:nrow(dataset)
          
          # export from function
          assign("dataset_spec", dataset, envir=parent.frame())
          
      }
  }
}