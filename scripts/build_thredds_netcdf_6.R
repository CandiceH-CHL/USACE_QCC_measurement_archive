build_thredds_netcdf_6 <- function(buoys = "list of buoys"){
   
     ##----------------------------------------------------------------------------------------
     ## script to create netCDF data from combined NDBC web files and NCEI netcdf files
     ## Hall, Candice 
     ##----------------------------------------------------------------------------------------
    
     ## netCDF structure is cf compliant as per netCDF NCEI point reference: 
     ## https://www.nodc.noaa.gov/data/formats/netcdf/v2.0/point.cdl
     ## cf compliant standard names: https://cfconventions.org/Data/cf-standard-names/78/build/cf-standard-name-table.html
     
     ## Actions:
     ## 1.  Sets data locations
     ## 2.  This step creates monthly netCDF NDBC data files that contains the best available data and metadata 
     ##     variables that were selected in the step above.
     ## 3.  The 'build_thredds_netcdf_6.R' function requires a buoy list and a data directory. The script also 
     ##     needs access to the 'NDBC_buoy.csv' and 'NDBC_buoy_descriptions.csv' spreadsheets that are found in the /data directory.
     ## 4.  NetCDF files are subset to create individual station files for each month and year (year_month)
     ## 5.  NetCDF structure is cf compliant as per netCDF NCEI point reference: https://www.nodc.noaa.gov/data/formats/netcdf/v2.0/point.cdl
     ## 6.  NetCDF standard names are cf compliant as per: https://cfconventions.org/Data/cf-standard-names/78/build/cf-standard-name-table.html
     ## 7.  Flag conventions are consistent with: Paris. Intergovernmental Oceanographic Commission of UNESCO. 2013. Ocean Data Standards,Vol.3: 
     ##     Recommendation for a Quality Flag Scheme for the Exchange of Oceanographic and Marine Meteorological Data. (IOC Manuals and Guides, 54, 
     ##     Vol. 3.) 12 pp. (English.)(IOC/2013/MG/54-3)
     
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     # load libraries (local run)
     library(lubridate)
     library(plyr)
     library(dplyr)
     library(gridExtra)
     library(data.table)
     library(oce)
     library(naniar)
     library(tidyverse)
     library(broom)
     library(openair) # polarplots
     library(plotly)
     library(magrittr)
     library(tidyr)
     library(grid)
     library(devtools)
     library(lsr)
     library(stringr)
     library(RColorBrewer)
     library(viridis)
     library(colorRamps)
     library(ggplot2)
     library(ggmap)
     library(maps)
     library(mapdata)
     library(broman)
     library(ncdf4)
    
     # # load libraries (HPC run)
     # library(lubridate, lib="/p/home/candice/Rlibs/")
     # library(plyr, lib="/p/home/candice/Rlibs/")
     # library(crayon, lib="/p/home/candice/Rlibs/")
     # library(pillar, lib="/p/home/candice/Rlibs/")
     # library(dplyr, lib="/p/home/candice/Rlibs/")
     # library(gridExtra, lib="/p/home/candice/Rlibs/")
     # library(data.table, lib="/p/home/candice/Rlibs/")
     # library(oce, lib="/p/home/candice/Rlibs/")
     # library(naniar, lib="/p/home/candice/Rlibs/")
     # library(tidyverse, lib="/p/home/candice/Rlibs/")
     # library(broom, lib="/p/home/candice/Rlibs/")
     # library(openair, lib="/p/home/candice/Rlibs/") # polarplots
     # library(plotly, lib="/p/home/candice/Rlibs/")
     # library(magrittr, lib="/p/home/candice/Rlibs/")
     # library(tidyr, lib="/p/home/candice/Rlibs/")
     # library(grid, lib="/p/home/candice/Rlibs/")
     # library(devtools, lib="/p/home/candice/Rlibs/")
     # library(lsr, lib="/p/home/candice/Rlibs/")
     # library(stringr, lib="/p/home/candice/Rlibs/")
     # library(RColorBrewer, lib="/p/home/candice/Rlibs/")
     # library(viridis, lib="/p/home/candice/Rlibs/")
     # library(colorRamps, lib="/p/home/candice/Rlibs/")
     # library(ggplot2, lib="/p/home/candice/Rlibs/")
     # library(ggmap, lib="/p/home/candice/Rlibs/")
     # library(maps, lib="/p/home/candice/Rlibs/")
     # library(mapdata, lib="/p/home/candice/Rlibs/")
     # library(broman, lib="/p/home/candice/Rlibs/")
     # library(ncdf4, lib="/p/home/candice/Rlibs/")
     
     
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     # set latest NDBC spreadsheet update from Steven DiNapoli, NDBC contract scientist
     DiNapoli_year= 2021
     
     ##----------------------------------------------------------------------------------------
     ## set paths
     ##----------------------------------------------------------------------------------------
     drive <- "E:/Candice/"
     # main dir
     data_dir <- paste0(drive, "projects/WaveTrends/data/")
     setwd(data_dir)
     # set input directories
     input_dir <- paste0(data_dir,"best_data/data/")
     # set new output directories for datasets
     if (!file.exists(paste0(data_dir,"best_netCDF_1/"))){dir.create((paste0(data_dir,"best_netCDF_1/")))}
     best_netCDF_dir <- paste0("D:/best_netCDF/")

     # function to perform capitalization
     simpleCap <- function(x) {s <- strsplit(x, " ")[[1]]; paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=" ")}
     lowerFirst <- function(x) {substr(x, 1, 1) <- tolower(substr(x, 1, 1)); x}

     ##----------------------------------------------------------------------------------------
     ## set presets
     ##----------------------------------------------------------------------------------------
     
     # setting flags
     # Ref: Paris. Intergovernmental Oceanographic Commission of UNESCO. 2013.Ocean Data Standards,Vol.3: 
     # Recommendation for a Quality Flag Scheme for the Exchange of Oceanographic and Marine Meteorological 
     # Data. (IOC Manuals and Guides, 54, Vol. 3.) 12 pp. (English.)(IOC/2013/MG/54-3)
     flag_descrip = "Good [1] = Passed documented required QC tests; Not Evaluated [2] = Used for data when no QC test performed or the information on quality is not available; Questionable/Suspect [3] = Failed non-critical documented metric or subjective test(s); 
     Bad [4] = Failed critical documented QC test(s) or as assigned by the data provider; Missing Data [5] = Used as place holder when data are missing [UNESCO 2013 Ocean Data Standards Vol. 3.]."
     flag_good = 1; flag_good_desc = "Passed documented required QC tests"
     # flag_notEvaluated = 2; flag_notEvaluated_desc = "Used for data when no QC test performed or the information on quality is not available"
     flag_questionableSuspect = 3; flag_questionableSuspect_desc = "Failed non-critical documented metric or subjective test(s)"
     flag_bad = 4; flag_bad_desc = "Failed critical documented QC test(s) or as assigned by the data provider"
     flag_missingData = 5; flag_missingData_desc = "Used as place holder when data are missing"
     
     # set precision
     miss_value <- -999.99
     variable_prec_df <- 'double'
     variable_prec_flg <- 'integer'
     variable_prec_metadata <- 'char'
     date_range_increment <- 1
     
     # load buoy stations
     list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
     list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC"); list_ndbc_buoy <- as.character(list_ndbc$station)
     buoys <- list_ndbc_buoy; print(buoys)
     # load buoy descriptions
     buoy_desc <- read.csv(paste0(data_dir,"NDBC_buoy_descriptions.csv"),header = TRUE)
     buoy_desc$Buoy <- as.numeric(buoy_desc$Buoy); buoy_desc$Description <- as.character(buoy_desc$Description)
     
     #----------------------------------------------------------------------------------------
     # Creating netCDF files and add flags
     #----------------------------------------------------------------------------------------

     for(buoy in buoys){
          Sys.time()
         
          print(paste0("starting build thredds on buoy: ", buoy))

          # load buoy description
          station_info = dplyr::filter(buoy_desc, buoy_desc$Buoy==buoy) 
          station_name= station_info$Description
          rm(station_info)
          
          # load buoy data
          load(file = paste0(input_dir,buoy,"/s_",buoy,"_best_data.RData"))
          
          # clear unnecessary spectral data from input vector
          rm(list = ls(pattern = "_gamma"));rm(list = ls(pattern = "_rhq")); rm(list = ls(pattern = "_phih"))
          rm(list = ls(pattern = "_Q"));rm(list = ls(pattern = "_C12_")); rm(list = ls(pattern = "_C13")); rm(list = ls(pattern = "_C22")); rm(list = ls(pattern = "_C33")) 
          
          # list data files
          data_ls <- ls(pattern = buoy)
          print(data_ls)
          
          #----------------------------------------------------------------------------------------
          # subset data for time periods and create netcdf files
          #----------------------------------------------------------------------------------------
          
          # find date range
          # load stdmet data
          dat <- get(data_ls[data_ls %like% 'ndbc_stdmet'])
          date_year_start <- year(dat$DateTime[1])
          date_year_end <- year(dat$DateTime[nrow(dat)])
          ## for subset runs
          # date_year_start <- as.integer(2016)
          # date_year_end <- as.integer(2021)
          rm(dat)
          date_range <- seq(date_year_start,date_year_end,date_range_increment)
          print(date_range)
          
          for(dateRange in date_range){
              print(dateRange)
              # subset per month
              monthRange <- 1:12
              
              for(m in monthRange){
                  print(m)
                  m = sprintf("%02d", as.numeric(m))

                  for(df in data_ls){
                      dat <-get(df)
                      dat <-dplyr::filter(dat, year(DateTime) == as.numeric(dateRange))
                      dat <-dplyr::filter(dat, month(DateTime)== as.numeric(m))
                      new_name <- paste0("data_",as.numeric(dateRange),"_",m,"_",df)
                      if(dim(dat)[1] > 0){if(grepl("cols_",df)==FALSE){assign(new_name, dat)}}
                      rm(dat)
                  }
                  rm(new_name,df)
    
                  # Create and write a netCDF file
                  
                  # set new output directories for datasets
                  if (!file.exists(paste0(best_netCDF_dir,buoy,"/"))){dir.create((paste0(best_netCDF_dir,buoy,"/")))}
                  ncname <- paste0("s_",buoy,"_best_ndbc_ncei_", as.numeric(dateRange),"_",m)
                  print(ncname)
                  ncfname <- paste0(best_netCDF_dir, buoy,"/",ncname, ".nc")
                  miss_values <- miss_value
                  
                  # subset relevant datasets
                  dat_ls <- ls(pattern = paste0("_",dateRange,"_"))
                  # delete list if no stdmet data present
                  if(sum(str_count(dat_ls, "_stdmet"))==0){
                       rm(list = ls(pattern = paste0("data_",dateRange,"_",m,"_s_",buoy)))
                       dat_ls <- vector()
                  }
                  print(dat_ls)
                  
                  #----------------------------------------------------------------------------------------
                  # load relevant datasets for dateRange
                  #----------------------------------------------------------------------------------------
                  if(length(dat_ls)>0){
                      # load stdmet data
                      dat <- get(dat_ls[dat_ls %like% 'ndbc_stdmet'])

                      # dealing with pre_NDBC NCEI stdmet data
                      if(sum(str_count(dat_ls, "_ncei_stdmet_preNDBC_"))>0){
                          col_names <- names(dat)
                          secondary_sensor_ls <- col_names[col_names %like% '_2']; secondary_sensor_ls <- secondary_sensor_ls[!grepl('_metadata_', secondary_sensor_ls)]
                          if("wind_direction_2" %in% col_names){if(sum(is.na(dat$wind_direction_2)) == nrow(dat)){dat$wind_direction_2 <-NULL; dat$wind_direction_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_direction=wind_direction_1, wind_direction_metadata=wind_direction_metadata_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$wind_direction_1[i]) & !is.na(dat$wind_direction_2[i])){dat$wind_direction_1[i]=dat$wind_direction_2[i]; dat$wind_direction_metadata_1[i]=dat$wind_direction_metadata_2[i]
                              dat$wind_direction_2 <-NULL; dat$wind_direction_metadata_2 <- NULL;dat <- dplyr::rename(dat, wind_direction=wind_direction_1, wind_direction_metadata=wind_direction_metadata_1)}}}
                          }
                          if("wind_speed_2" %in% col_names){if(sum(is.na(dat$wind_speed_2)) == nrow(dat)){dat$wind_speed_2 <-NULL; dat$wind_speed_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_speed=wind_speed_1, wind_speed_metadata=wind_speed_metadata_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$wind_speed_1[i]) & !is.na(dat$wind_speed_2[i])){dat$wind_speed_1[i]=dat$wind_speed_2[i]; dat$wind_speed_metadata_1[i]=dat$wind_speed_metadata_2[i]
                                          dat$wind_speed_2 <-NULL; dat$wind_speed_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_speed=wind_speed_1, wind_speed_metadata=wind_speed_metadata_1)}}}
                          }
                          if("wind_gust_2" %in% col_names){if(sum(is.na(dat$wind_gust_2)) == nrow(dat)){dat$wind_gust_2 <-NULL; dat$wind_gust_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_gust=wind_gust_1, wind_gust_metadata=wind_gust_metadata_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$wind_gust_1[i]) & !is.na(dat$wind_gust_2[i])){dat$wind_gust_1[i]=dat$wind_gust_2[i]; dat$wind_gust_metadata_1[i]=dat$wind_gust_metadata_2[i]
                                          dat$wind_gust_2 <-NULL; dat$wind_gust_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_gust=wind_gust_1, wind_gust_metadata=wind_gust_metadata_1)}}}
                          }
                          if("air_pressure_at_sea_level_2" %in% col_names){if(sum(is.na(dat$air_pressure_at_sea_level_2)) == nrow(dat)){dat$air_pressure_at_sea_level_2 <-NULL; dat$air_pressure_at_sea_level_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_pressure_at_sea_level=air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata=air_pressure_at_sea_level_metadata_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$air_pressure_at_sea_level_1[i]) & !is.na(dat$air_pressure_at_sea_level_2[i])){dat$air_pressure_at_sea_level_1[i]=dat$air_pressure_at_sea_level_2[i]; dat$air_pressure_at_sea_level_metadata_1[i]=dat$air_pressure_at_sea_level_metadata_2[i]
                                          dat$air_pressure_at_sea_level_2 <-NULL; dat$air_pressure_at_sea_level_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_pressure_at_sea_level=air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata=air_pressure_at_sea_level_metadata_1)}}}
                          }
                          if("air_temperature_2" %in% col_names){if(sum(is.na(dat$air_temperature_2)) == nrow(dat)){dat$air_temperature_2 <-NULL; dat$air_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_temperature=air_temperature_1, air_temperature_metadata=air_temperature_metadata_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$air_temperature_1[i]) & !is.na(dat$air_temperature_2[i])){dat$air_temperature_1[i]=dat$air_temperature_2[i]; dat$air_temperature_metadata_1[i]=dat$air_temperature_metadata_2[i]
                                          dat$air_temperature_2 <-NULL; dat$air_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_temperature=air_temperature_1, air_temperature_metadata=air_temperature_metadata_1)}}}
                          }
                          if("dew_point_temperature_2" %in% col_names){if(sum(is.na(dat$dew_point_temperature_2)) == nrow(dat)){dat$dew_point_temperature_2 <-NULL; dat$dew_point_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, dew_point_temperature=dew_point_temperature_1, dew_point_temperature_metadata=dew_point_temperature_metadata_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$dew_point_temperature_1[i]) & !is.na(dat$dew_point_temperature_2[i])){dat$dew_point_temperature_1[i]=dat$dew_point_temperature_2[i]; dat$dew_point_temperature_metadata_1[i]=dat$dew_point_temperature_metadata_2[i]
                                          dat$dew_point_temperature_2 <-NULL; dat$dew_point_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, dew_point_temperature=dew_point_temperature_1, dew_point_temperature_metadata=dew_point_temperature_metadata_1)}}}
                          }
                          if("sea_surface_temperature_2" %in% col_names){if(sum(is.na(dat$sea_surface_temperature_2)) == nrow(dat)){dat$sea_surface_temperature_2 <-NULL; dat$sea_surface_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, sea_surface_temperature=sea_surface_temperature_1, sea_surface_temperature_metadata=sea_surface_temperature_1)
                              }else{for(i in 1:nrow(dat)){if(is.na(dat$sea_surface_temperature_1[i]) & !is.na(dat$sea_surface_temperature_2[i])){dat$sea_surface_temperature_1[i]=dat$sea_surface_temperature_2[i]; dat$sea_surface_temperature_metadata_1[i]=dat$sea_surface_temperature_metadata_2[i]
                                          dat$sea_surface_temperature_2 <-NULL; dat$sea_surface_temperature_2 <- NULL; dat <- dplyr::rename(dat, sea_surface_temperature=sea_surface_temperature_1, sea_surface_temperature_metadata=sea_surface_temperature_metadata_1)}}}
                          }
                      }
                          
                      # remove stdmet data from list    
                      dat_ls <- dat_ls[!grepl('_stdmet', dat_ls)]

                      # load station metadata
                      if(sum(str_count(dat_ls, "_station_metadata"))>0){
                          dat_station <- get(dat_ls[dat_ls %like% '_station_metadata'])
                          dat_station$lat <- NULL; dat_station$lon <- NULL
                          # concatenate df
                          dat <- left_join(dat, dat_station, by = "DateTime")
                          rm(dat_station)
                          # clear loaded data from input vector
                          dat_ls <- dat_ls[!grepl('_station_metadata', dat_ls)]
                      }else{print(paste0("no station metadata data for ",ncname))}
                      
                      # check for wave sensor data - common in old datasets
                      if(sum(str_count(dat_ls, "_sensor_output"))>0){
                          dat_sensor <- get(dat_ls[dat_ls %like% '_sensor_output'])
                          if("lat" %in% names(dat_sensor)){dat_sensor$lat <- NULL; dat_sensor$lon <-NULL}
                          colnames(dat_sensor) <- c("DateTime", "wave_sensor_output")
                          # concatenate df
                          dat <- left_join(dat, dat_sensor, by = "DateTime")
                          rm(dat_sensor)
                          # clear loaded data from input vector
                          dat_ls <- dat_ls[!grepl('_sensor_output', dat_ls)]
                      }else{print(paste0("no sensor output data for ",ncname))}
                      
                      # subset time variable for spec matching
                      dat_time <- dplyr::select(dat, DateTime, lat,lon)
                      dat_start_date <- as.character(dat_time$DateTime[1])
                      dat_end_date <- as.character(dat_time$DateTime[nrow(dat_time)])
                      
                      # load spectral data if present
                      if(sum(str_count(dat_ls, "_freq"))>0){
                          for(df in dat_ls){
                              # df <- dat_ls[1]
                              df_name <- paste0("dat_spec_",gsub("_geoClean","",unlist(strsplit(df,paste0("_",buoy,"_")))[2]))
                              dat_spec <- get(df)
                              dat_spec$lat <- NULL; dat_spec$lon <- NULL
                              # remove any old frequency columns previously added to support data ingestion
                              if(grepl("_old_",df)){
                                   if("0.0100" %in% names(dat_spec)){dat_spec$`0.0100`<-NULL}
                                   if("0.0200" %in% names(dat_spec)){dat_spec$`0.0200`<-NULL}
                              }
                              dat_spec <- left_join(dat_time,dat_spec, by = "DateTime")
                              assign(df_name,dat_spec)
                              assign(df,dat_spec)
                              # clear loaded data 
                              rm(dat_spec, df_name)
                          }
                      }else{print(paste0("no spectral data for ",ncname))}
                      rm(dat_time)
                      
                      # clear loaded data from input vector
                      dat_ls <- dat_ls[!grepl('_freq_', dat_ls)]
                      # housekeeping
                      if(length(dat_ls)==0){
                          rm(list = ls(pattern = paste0("_",dateRange,"_")))
                          rm(ncname)
                          print("all data are assimilated")
                      }else{print("EXTRA DATA - check")}
                      
                      #----------------------------------------------------------------------------------------
                      # prep data for netcdf creation
                      #----------------------------------------------------------------------------------------
                      print("starting with time data")
                      
                      # create time variable
                      data.time <- julian(dat$DateTime, origin = as.POSIXct("1900-01-01 00:00:00", tz = "UTC"))
                      tunits1 <- "days since 1900-01-01 00:00:00"
                      dimTime <- ncdim_def("time",tunits1,as.double(data.time))
                      rm(tunits1, data.time)
                      
                      # create character variables
                      dimnchar <- ncdim_def(name="nchar", unit="", vals=1:nrow(dat), create_dimvar=FALSE )
        
                      # select variables
                      var_ls <- names(dat)
                      var_ls <- var_ls[!(var_ls %in% "DateTime")]

                      #----------------------------------------------------------------------------------------
                      # prep data to netcdf file
                      #----------------------------------------------------------------------------------------
                      freq_ls <- ls(pattern = "dat_spec_")
                      # collate time from the various old and new freq
                      if(length(freq_ls)>0){
                          # select wave frequency bands for spec data
                          if(sum(str_count(freq_ls, "_freq_new"))>0){
                              dat_spec <- get(freq_ls[grep("_freq_new",freq_ls)[1]])
                              wave_wpm_new <- as.numeric(names(dat_spec[4:ncol(dat_spec)]))
                              wave_new_length <- length(wave_wpm_new)
                              dimwave_wpm_new <- ncdim_def(paste0("waveFrequency_",as.character(length(wave_wpm_new))),"Hz",as.double(wave_wpm_new))
                              rm(dat_spec, wave_wpm_new)
                          }
                          if(sum(str_count(freq_ls, "_freq_old"))>0){
                              # looping through all old spec to account for 0.01 Hz
                              old_spec <- freq_ls[grep("_freq_old",freq_ls)]
                              wave_wpm_old <- vector()
                              for(old in old_spec){
                                  df <- get(old)
                                  if("0.0100" %in% names(df)==TRUE){df$`0.0100`<-NULL}
                                  if("0.0200" %in% names(df)==TRUE){df$`0.0200`<-NULL}
                                  cols_old <- names(df)
                                  wave_wpm_old <- c(wave_wpm_old,cols_old)
                                  rm(df)
                              }
                              wave_wpm_old <- unique(wave_wpm_old);removed <- c("DateTime","lat","lon")
                              wave_wpm_old <- wave_wpm_old[!wave_wpm_old %in% removed]
                              wave_old_length <- length(wave_wpm_old)
                              dimwave_wpm_old <- ncdim_def(paste0("waveFrequency_",as.character(length(wave_wpm_old))),"Hz",as.double(wave_wpm_old))
                              rm(wave_wpm_old,removed)
                          }
                          rm(old_freq, df1,old_spec, old,cols_old)

                          # select variables
                          var_ls <- c(var_ls,freq_ls)
                      }else(print(paste0("no freq data for ", dateRange, "_", m)))
                  
                      # create variable dimensions for the netcdf file
                      for(var_df in var_ls){
                           print(var_df)
                          # set spectral constants
                          if(var_df != "lat" & var_df != "lon"){
                              if(grepl("_freq_", var_df)){
                                  # load the data if its frequency 
                                  dat_spec <- get(var_df)
                                  if(exists("wave_new_length")){name_var <- gsub("new",wave_new_length,var_df)}
                                  if(exists("wave_old_length")){name_var <- gsub("old",wave_old_length,var_df)}
                                  var_longname = gsub("_"," ",gsub("dat_spec_","",name_var))
                                  var_name = lowerFirst(gsub(" ","",simpleCap(var_longname)))
                              }else{
                                  var_longname = gsub("_"," ",var_df)
                                  var_name = lowerFirst(gsub(" ","",simpleCap(var_longname)))
                              }
                          }
                          miss_values <- miss_value
                          variable_prec <- variable_prec_df

                          # set variable constants
                          if(var_df == "lat") {var_name = "latitude"; var_units = 'degreesNorth'; var_longname='latitude'; variable_prec= variable_prec_df}
                          if(var_df == "lon") {var_name = "longitude"; var_units = 'degreesEast'; var_longname='longitude'; variable_prec= variable_prec_df}
                          if(var_df == "wind_direction"|var_df == "mean_wave_direction"|var_df == "wind_direction_metadata"|var_df == "mean_wave_direction_metadata") {var_units = 'degT'; variable_prec = variable_prec_flg}
                          if(var_df == "wind_speed"|var_df == "wind_gust"|var_df == "wind_speed_metadata"|var_df == "wind_gust_metadata") {var_units <- 'm/s'}
                          if(var_df == "significant_wave_height"|var_df == "significant_wave_height_metadata") {var_name = "waveHs"; var_units = 'm'}
                          if(var_df == "dominant_wave_period"|var_df == "dominant_wave_period_metadata") {var_name = "waveTp"; var_units = 's'}
                          if(var_df == "average_wave_period"|var_df == "average_wave_period_metadata") {var_name = "waveTm"; var_units = 's'}
                          if(var_df == "air_pressure_at_sea_level"|var_df == "air_pressure_at_sea_level_metadata") {var_units = 'hPa'}
                          if(var_df == "air_temperature"|var_df == "sea_surface_temperature"|var_df == "dew_point_temperature"|var_df == "air_temperature_metadata"|var_df == "sea_surface_temperature_metadata"|var_df == "dew_point_temperature_metadata") {var_units = 'Celsius'}
                          if(var_df == "air_pressure_at_sea_level") {var_name = "surfaceAirPressure"};if(var_df == "air_pressure_at_sea_level_metadata") {var_name = "surfaceAirPressureMetadata"}
                          if(var_df == "air_temperature") {var_name = "surfaceAirTemperature"};if(var_df == "air_temperature_metadata") {var_name = "surfaceAirTemperatureMetadata"}
                          if(var_df == "sea_surface_temperature"){var_name = "surfaceSeaTemperature"};if(var_df == "sea_surface_temperature_metadata") {var_name = "surfaceSeaTemperatureMetadata"}
                          if(var_df == "dew_point_temperature") {var_name = "surfaceDewPointTemperature"};if(var_df == "dew_point_temperature_metadata") {var_name = "surfaceDewPointTemperatureMetadata"}
                          if(grepl("_metadata", var_df)==TRUE){variable_prec = variable_prec_metadata; var_units = ''; rm(miss_values)}
                          if(var_df == "mooring"|var_df == "hull"|var_df == "payload"){variable_prec = variable_prec_metadata; var_units = ''; rm(miss_values); var_longname = paste0("NDBC ",var_df, " type")}
                          if(var_df == "significant_wave_height_metadata"|var_df == "dominant_wave_period_metadata"|var_df == "average_wave_period_metadata"){var_name = paste0(var_name,"Metadata")}
                          if(var_df == "depth"){var_units = 'm'; var_longname = "sea_floor_depth_below_sea_level"; variable_prec = variable_prec_flg}
                          if(var_df == "wave_sensor_output"){var_units = ''; var_longname = "wave_sensor_output:1=Displacement_2=Acceleration"; variable_prec = variable_prec_flg}
                          # set freq constants, standard and long names
                          if(grepl("_freq_", var_df)){
                              if(grepl("_c11_", var_df)){var_units <- 'm2/Hz'; var_name="waveEnergyDensity"; variable_prec=variable_prec_df;var_longname = "sea_surface_wave_variance_spectral_density"}
                              if(grepl("_c11m_", var_df)){var_units <- '(m/s2)2/Hz'; var_name="waveEnergyDensityUncorrected"; variable_prec=variable_prec_df;var_longname = "sea_surface_wave_variance_spectral_density_uncorrected"}
                              if(grepl("_alpha1_", var_df)){var_units <- 'degT'; var_name="waveAlpha1";var_longname = "mean_wave_direction_for_each_spectrum_frequency_bin_of_the_sea_surface"}
                              if(grepl("_alpha2_", var_df)){var_units <- 'degT'; var_name="waveAlpha2";var_longname = "principal_wave_direction_for_each_spectrum_frequency_bin_of_the_sea_surface"}
                              if(grepl("_r1_", var_df)){var_units <- 'unitless'; var_name="waveR1"; var_longname = "first_normalized_polar_coordinate_of_the_Fourier_coefficients"}
                              if(grepl("_r2_", var_df)){var_units <- 'unitless'; var_name="waveR2"; var_longname = "second_normalized_polar_coordinate_of_the_Fourier_coefficients"}
                          }
                          # set long names
                          if(var_df == "wind_direction") {var_longname='wind_from_direction'}
                          if(var_df == "wind_direction_metadata") {var_longname='wind_from_direction_metadata'}
                          if(var_df == "wind_speed") {var_longname='wind_speed'}
                          if(var_df == "wind_speed_metadata") {var_longname='wind_speed_metadata'}
                          if(var_df == "wind_gust") {var_longname='wind_speed_of_gust'}
                          if(var_df == "wind_gust_metadata") {var_longname='wind_speed_of_gust_metadata'}
                          if(var_df == "significant_wave_height") {var_longname='sea_surface_significant_wave_height'}
                          if(var_df == "significant_wave_height_metadata") {var_longname='sea_surface_significant_wave_height_metadata'}
                          if(var_df == "dominant_wave_period") {var_longname='sea_surface_wave_period_at_variance_spectral_density_maximum'}
                          if(var_df == "dominant_wave_period_metadata") {var_longname='sea_surface_wave_period_at_variance_spectral_density_maximum_metadata'}
                          if(var_df == "average_wave_period") {var_longname='sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment'}
                          if(var_df == "average_wave_period_metadata") {var_longname='sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment_metadata'}
                          if(var_df == "mean_wave_direction") {var_longname='sea_surface_wave_from_direction'}
                          if(var_df == "mean_wave_direction_metadata") {var_longname='sea_surface_wave_from_direction_metadata'}
                          if(var_df == "air_pressure_at_sea_level") {var_longname='air_pressure'}
                          if(var_df == "air_pressure_at_sea_level_metadata") {var_longname='air_pressure_metadata'}
                          if(var_df == "air_temperature") {var_longname='air_temperature'}
                          if(var_df == "air_temperature_metadata") {var_longname='air_temperature_metadata'}
                          if(var_df == "sea_surface_temperature") {var_longname='sea_surface_temperature'}
                          if(var_df == "sea_surface_temperature_metadata") {var_longname='sea_surface_temperature_metadata'}
                          if(var_df == "dew_point_temperature") {var_longname='dew_point_temperature'}
                          if(var_df == "dew_point_temperature_metadata") {var_longname='dew_point_temperature_metadata'}
                          # set frequency bands
                          if(grepl("_new", var_df)){var_name = paste0(var_name,"_",wave_new_length, "Frequencies")}
                          if(grepl("_old", var_df)){var_name = paste0(var_name,"_",wave_old_length, "Frequencies")}
                                
                          # create matrix
                          if(grepl("_freq_", var_df)){
                              # subset flag field
                              df_df <- dplyr::select(dat_spec,DateTime)
                              # format matrix df
                              dat_spec$lat <- NULL; dat_spec$lon <- NULL; dat_spec$DateTime <- NULL
                              # create matrix
                              mat_df <- matrix(t(dat_spec), nrow=nrow(dat_spec),ncol=ncol(dat_spec))
                              df_name <- paste0("df_",var_name)
                              # assign(df_name, mat_df)
                              assign(df_name, mat_df)
                              rm(df_name, mat_df)
                              # create flag field
                              df_df$flag <-flag_good
                              for(r in 1:nrow(df_df)){if(is.na(df_df[r,1])){df_df[r,2]=flag_missingData}}
                              if(grepl("_preNDBC", var_df)){df_df$flag <-flag_questionableSuspect}
                              # subset df
                              df_df_flag <- dplyr::select(df_df,flag)
                              # rename flag df
                              df_name <- paste0("df_",var_name,"_flag")
                              assign(df_name, df_df_flag)
                              rm(df_df, df_df_flag, df_name,r)
                          }else{
                              # create df
                              df_df <- dplyr::select(dat,all_of(var_df))
                              # create flag field
                              df_df$flag <-flag_good
                              for(r in 1:nrow(df_df)){if(is.na(df_df[r,1])){df_df[r,2]=flag_missingData}}
                              # subset df
                              df_df_flag <- dplyr::select(df_df,flag)
                              df_df$flag <- NULL
                              # rename df
                              df_name <- paste0("df_",var_name)
                              assign(df_name, df_df)
                              df_name <- paste0("df_",var_name,"_flag")
                              assign(df_name, df_df_flag)
                              rm(df_df, df_df_flag, df_name,r)
                          }
                          
                          # Add dimensions and variables which accompany the dimensions (avoided by create_dimvar = FALSE)
                          if(grepl("_metadata", var_df)){
                              vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime,dimnchar), prec=variable_prec)
                          }else if(var_df == "mooring"|var_df == "hull"|var_df == "payload"){
                              vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime,dimnchar), prec=variable_prec)
                          }else if(var_df == "lat" | var_df == "lon"){
                              vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime), missval=miss_values, prec=variable_prec)
                          }else if(grepl("_freq_old", var_df)){
                              vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimwave_wpm_old,dimTime), missval=miss_values, prec=variable_prec)
                          }else if(grepl("_freq_new", var_df)){
                              vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimwave_wpm_new,dimTime), missval=miss_values,  prec=variable_prec)
                          }else{    
                              vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime), missval=miss_values)
                          }
                          df_name <- paste0("var_",var_name)
                          assign(df_name, vari_df)
                          rm(df_name, vari_df)
                          rm(var_longname, var_units, var_name)
                      }
                      rm(var_df)
                      rm(list = ls(pattern = "dim_"))
                      if(exists("dat_spec")){rm(dat_spec)}
                      rm(list = ls(pattern = "dat_spec_"))
                      if(exists("wave_old_length")){rm(wave_old_length)}
                      if(exists("wave_new_length")){rm(wave_new_length)}
                      
                      #----------------------------------------------------------------------------------------
                      # prep flag data for netcdf file
                      #----------------------------------------------------------------------------------------
                      flag_ls <- ls(pattern = "_flag")
                      miss_values <- miss_value
                      
                      # function to perform capitalization
                      simpleCap <- function(x) {
                          s <- strsplit(x, " ")[[1]]
                          paste(toupper(substring(s, 1,1)), substring(s, 2),
                                sep="", collapse=" ")
                      }
                      lowerFirst <- function(x) {substr(x, 1, 1) <- tolower(substr(x, 1, 1));x}
                      
                      # create dimensions for the flag data
                      for(fl in flag_ls){
                          var_name = gsub("df_","",fl)
                          var_longname = gsub("_"," ",tolower(gsub("([a-z])([A-Z])", "\\1 \\2", var_name)))
                          var_longname <- gsub("frequencies", " frequencies", var_longname)
                          var_name = gsub("_flag","Flag",var_name)
                          vari_df <- ncvar_def(name=var_name, units="none", longname=var_longname, dim=list(dimTime), missval=miss_values, prec=variable_prec_flg)
                          df_name <- paste0("var_",var_name)
                          assign(df_name, vari_df)
                          rm(df_name, vari_df,var_longname, var_name)
                      }
                      rm(fl, flag_ls, dimTime)
                      
                      #----------------------------------------------------------------------------------------
                      # build netcdf file
                      #----------------------------------------------------------------------------------------
                      rm(var__flag, var_df, var_list, var_units)
        
                      # create a list of all variables to add them all at once
                      var_list <- ls(pattern = "var_"); var_list
                      var_list <-var_list[!var_list %in% "var_ls"]; var_list
                      if("var_list" %in% var_list){var_list<- var_list[!var_list %in% "var_list"]}
                      var_list
             
                      # create list for quick add to con nc file
                      vars <- list()
                      for(i in var_list){vars[[gsub("var_","",i)]] <- get(i)}#print(i)
                      rm(i)
                      
                      # Make the file
                      # Create a new empty netcdf file.
                      con <- nc_create(ncfname, vars)#, verbose = TRUE)
                      rm(ncfname, vars)
                      
                      # This variable was implicitly created by the dimension, so just specifying it by name
                      ncatt_put(con, 'time', 'standard_name', 'time')
                      ncatt_put(con, var_longitude, 'axis', 'X')
                      ncatt_put(con, var_latitude, 'axis', 'Y')
                      ncatt_put(con, 'time', 'axis', 'T')
                      
                      # Add some extra attributes
                      for(var_df in var_list){
                          print(var_df)
                          std_name <- gsub("var_","",var_df)
                          print(std_name)
                          ndbc_blurb <- '(NDBC,2018). https://www.ndbc.noaa.gov/measdes.shtml'
                          DiNapoli_blurb <- 'Source:NDBC metadata spreadsheet (DiNapoli, '
                          metadata_blurb <- paste0(DiNapoli_blurb, DiNapoli_year,') / NDBC NCEI netCDF metadata.')
                          # set standard names
                          if(std_name == "latitude") {var_standard = "latitude"; desc_name="latitude"}
                          if(std_name == "longitude") {var_standard = "longitude"; desc_name="longitude"}
                          if(std_name == "mooring"){var_standard = "NDBC_mooring_type"; desc_name=paste0(DiNapoli_blurb, DiNapoli_year)}
                          if(std_name == "hull"){var_standard = "NDBC_hull_type"; desc_name=paste0(DiNapoli_blurb, DiNapoli_year)}
                          if(std_name == "payload"){var_standard = "NDBC_payload_type"; desc_name=paste0(DiNapoli_blurb, DiNapoli_year)}
                          if(std_name == "depth"){var_standard = "sea_floor_depth_below_mean_sea_level"; desc_name=paste0(DiNapoli_blurb, DiNapoli_year)}
                          if(std_name == "waveSensorOutput"){var_standard = ''; desc_name = "1=Displacement; 2=Acceleration. If the sensor output is displacement, waveEnergyDensity units m2/Hz. If the sensor output is acceleration, waveEnergyDensityUncorrected units are (m/s2)2/Hz."}
                           
                          if(std_name == "windDirection") {var_standard = 'wind_from_direction'; desc_name=paste0('Wind direction (the direction the wind is coming from in degrees clockwise from true N) during the same period used for wind speed ',ndbc_blurb)}
                          if(std_name == "windDirectionMetadata") {var_standard = 'wind_from_direction_metadata'; desc_name=paste0('wind direction metadata. ',metadata_blurb)}
                          if(std_name == "windSpeed") {var_standard = 'wind_speed'; desc_name=paste0('wind speed (m/s) ',ndbc_blurb)}
                          if(std_name == "windSpeedMetadata") {var_standard = 'wind_speed_metadata'; desc_name=paste0('wind speed metadata. ',metadata_blurb)}
                          if(std_name == "windGust") {var_standard = 'wind_speed_of_gust'; desc_name=paste0('Peak 5 or 8 second gust speed (m/s).  ',ndbc_blurb)}
                          if(std_name == "windGustMetadata") {var_standard = 'wind_speed_of_gust_metadata'; desc_name=paste0('wind gust metadata. ',metadata_blurb)}
                          
                          if(std_name == "waveHs") {var_standard = 'sea_surface_significant_wave_height'; desc_name=paste0('Significant wave height (meters) is calculated as the average of the highest one-third of all of the wave heights during the 20-minute sampling period ',ndbc_blurb)}
                          if(std_name == "waveHsMetadata") {var_standard = 'sea_surface_significant_wave_height_metadata'; desc_name=paste0('sea surface significant wave height metadata. ',metadata_blurb)}
                          if(std_name == "waveTp") {var_standard = 'sea_surface_wave_period_at_variance_spectral_density_maximum'; desc_name=paste0('Dominant wave period (seconds) is the period with the maximum wave energy ',ndbc_blurb)}
                          if(std_name == "waveTpMetadata") {var_standard = 'sea_surface_wave_period_at_variance_spectral_density_maximum_metadata'; desc_name=paste0('sea surface wave period at variance spectral density maximum metadata. ',metadata_blurb)}
                          if(std_name == "waveTm") {var_standard = 'sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment'; desc_name=paste0('Average wave period (seconds) of all waves during the 20-minute period ',ndbc_blurb)}
                          if(std_name == "waveTmMetadata") {var_standard = 'sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment_metadata'; desc_name=paste0('sea surface wave mean period from variance spectral density second frequency moment metadata. ',metadata_blurb)}
                          if(std_name == "meanWaveDirection") {var_standard = 'sea_surface_wave_from_direction'; desc_name=paste0('The direction from which the waves at the dominant period are coming. The units are degrees from true North, increasing clockwise, with North as 0 (zero) degrees and East as 90 degrees ',ndbc_blurb)}
                          if(std_name == "meanWaveDirectionMetadata") {var_standard = 'sea_surface_wave_from_direction_metadata'; desc_name=paste0('sea surface wave from direction metadata. ',metadata_blurb)}
                          
                          if(std_name == "surfaceAirPressure") {var_standard = 'air_pressure'; desc_name=paste0('Sea level pressure (hPa). For the Great Lakes buoys, the recorded pressure is reduced to sea level using the method described in NWS Technical Procedures Bulletin 291 (11/14/80) ',ndbc_blurb)}
                          if(std_name == "surfaceAirPressureMetadata") {var_standard = 'air_pressure_metadata'; desc_name=paste0('air pressure metadata. ',metadata_blurb)}
                          if(std_name == "surfaceAirTemperature") {var_standard = 'air_temperature'; desc_name=paste0('Air temperature (Celsius) ',ndbc_blurb)}
                          if(std_name == "surfaceAirTemperatureMetadata") {var_standard = 'air_temperature_metadata'; desc_name=paste0('air temperature metadata. ',metadata_blurb)}
                          if(std_name == "surfaceSeaTemperature") {var_standard = 'sea_surface_temperature'; desc_name=paste0('Sea surface temperature (Celsius) ',ndbc_blurb)}
                          if(std_name == "surfaceSeaTemperatureMetadata") {var_standard = 'sea_surface_temperature_metadata'; desc_name=paste0('sea surface temperature metadata. ',metadata_blurb)}
                          if(std_name == "surfaceDewPointTemperature") {var_standard = 'dew_point_temperature'; desc_name=paste0('Dewpoint temperature taken at the same height as the air temperature measurement ',ndbc_blurb)}
                          if(std_name == "surfaceDewPointTemperatureMetadata") {var_standard = 'dew_point_temperature_metadata'; desc_name=paste0('dew point temperature metadata. ',metadata_blurb)}
                          # set freq standard names
                          if(grepl("Frequencies", std_name)){
                                if(grepl("EnergyDensity", std_name)){var_standard = 'sea_surface_wave_variance_spectral_density'; desc_name="Energy density, displacement in m2/Hz, for each frequency bin"}
                                if(grepl("waveEnergyDensityUncorrected", std_name)){var_standard = 'sea_surface_wave_variance_spectral_density_uncorrected'; desc_name="Uncorrected energy density, acceleration in (m/s2)2/Hz, for each frequency bin"}
                                if(grepl("Alpha1", std_name)){var_standard='mean_wave_direction_at_specified_frequency'; desc_name="alpha1 is the mean wave direction, in degrees from true North, for each spectrum frequency bin of the sea surface"}
                                if(grepl("Alpha2", std_name)){var_standard='principal_wave_direction_at_specified_frequency'; desc_name="alpha2 is the principal wave direction, in degrees from true North, for each spectrum frequency bin of the sea surface. alpha2 has ambiguous results in using the arctangent function with the Fourier Coefficients (b2,a2). When necessary, NDBC adds 180 degrees to alpha2 in order to minimize the difference between alpha1 and alpha2"}
                                if(grepl("R1", std_name)){var_standard='first_normalized_polar_coordinate_of_the_Fourier_coefficients'; desc_name="r1 is the nondimensional first normalized polar coordinates of the Fourier coefficients"}
                                if(grepl("R2", std_name)){var_standard='second_normalized_polar_coordinate_of_the_Fourier_coefficients'; desc_name="r2 is the nondimensional second normalized polar coordinates of the Fourier coefficients"}
                          }
                          # overwrite std name if a flag
                          if(grepl("Flag", std_name)){var_standard = "quality_flag"; desc_name= paste0(std_name,"QualityFlag. ",flag_descrip)}

                          # add standard_name
                          ncatt_put(con, gsub("var_","",var_df), 'standard_name', var_standard)
                          rm(var_standard)
                          
                          # add if present (i.e. listed above)
                          if(exists('desc_name')){
                              # add description name
                              ncatt_put(con, gsub("var_","",var_df), 'description_name', desc_name)
                              rm(desc_name)
                          }
                          # add coordinates
                          if(var_df != "var_latitude" & var_df != "var_longitude"){
                              if(grepl("Metadata", var_df)==FALSE){
                                  ncatt_put(con, gsub("var_","",var_df), 'coordinates', 'latitude longitude')
                              }
                          }
                          # add data values to nc file
                          df_name <- gsub("var_", "",var_df)
                          if(grepl("Flag", df_name)){df_name = gsub("Flag","_flag",df_name)}
                          df <- get(paste0("df_",df_name))
                          if(grepl("_flag", df_name)){df_name = gsub("_flag","Flag",df_name)}
                          # print(paste0("add data: ",df_name))
                          if(grepl("Frequencies", df_name)){
                              if(grepl("Flag", df_name)){
                                  ncvar_put(con, df_name, df[,1])
                              }else{
                                  ncvar_put(con, df_name, df)
                              }
                          }else{
                              ncvar_put(con, df_name, df[,1])
                          }
                          rm(df_name, df)
                          if(exists("var_standard")){rm(var_standard)}
                          if(exists("desc_name")){rm(desc_name)}
                      }
                      rm(var_df)
                      
                      # housekeeping
                      rm(list = ls(pattern = "var_"))
                      rm(list = ls(pattern = "df_"))
                      
                      #----------------------------------------------------------------------------------------
                      # add global attributes data for netcdf file
                      #----------------------------------------------------------------------------------------
                      
                      ncatt_put(nc=con,varid=0, attname="id", attval=as.character(buoy), prec="int") 
                      ncatt_put(nc=con,varid=0, attname="naming_authority", attval = "WMO", prec="char") 
                      ncatt_put(nc=con,varid=0, attname="ioos_id", attval = paste0("urn:ioos:station:wmo:",as.character(buoy)), prec="char")
                      ncatt_put(nc=con,varid=0, attname="wmo_id", attval = as.character(buoy), prec="char")
                      ncatt_put(nc=con,varid=0, attname="institution", attval = "National Data Buoy Center", prec="char")
                      ncatt_put(nc=con,varid=0, attname="institution_abbreviation", attval = "NDBC", prec="char")
                      ncatt_put(nc=con,varid=0, attname="title", attval = "NDBC description: Meteorological and Oceanographic Data Collected from the National Data Buoy Center\'s Weather Buoys", prec="char")
                      ncatt_put(nc=con,varid=0, attname="summary", attval = "NDBC description: Over 100 moored weather buoys have been deployed in U.S. coastal and offshore waters. Weather buoy data typically include barometric pressure, wind direction, speed and gust, air temperature, sea water temperature, waves, and relative humidity. Weather buoys also measure wave energy spectra from which significant wave height, dominant wave period, average wave period and mean wave direction are derived.", prec="char")
                      if(length(station_name)>0){ncatt_put(nc=con,varid=0, attname="station_name", attval = station_name, prec="char")}
                      ncatt_put(nc=con,varid=0, attname="history", attval = 'The data were collected by the National Data Buoy Center (NDBC) and archived on their website (https://www.ndbc.noaa.gov/) and in the official National Oceanic and Atmospheric Administation (NOAA) archive at National Center for Environmental Information (NCEI; https://www.ncei.noaa.gov/access/marine-environmental-buoy-database/). Each data source has their own idiosyncrasies (Hall and Jensen, 2021, http://dx.doi.org/10.21079/11681/40059) that need to be accounted for to accurately use these NBDC data within U.S. Army Corps of Engineers (USACE) Engineers and Research Development Center (ERDC) products. USACE sponsored a Coastal Ocean Data Systems (CODS) National Coastal Wave Climate (NCWC) project that developed in-house USACE quality control checks and metadata corrections to develop a best available measurement archive (herewith called the USACE QCC measurement archive). Of note is that integral wave data are imported directly from the NDBC data sources, and are not corrected for calculation errors that occurred during NDBC processing from spectral wave data. The self-described, USACE QCC measurement archive data is stored in netCDF format alongside the USACE Coastal and Hydraulic Laboratory (CHL) Thredds Wave Information Study (WIS) long-term hindcast, accessible to both the USACE and the public.', prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_lat_max", attval = "variable: see latitude data", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_lat_min", attval = "variable: see latitude data", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_lat_units", attval = "degrees", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_lon_max", attval = "variable: see longitude data", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_lon_min", attval = "variable: see longitude data", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_lon_units", attval = "degrees", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_vertial_units", attval = "meters above mean sea level", prec="char")
                      ncatt_put(nc=con,varid=0, attname="geospatial_vertical_datum", attval = "urn:x-noaa:def:datum:noaa::MSL", prec="char")
                      ncatt_put(nc=con,varid=0, attname="qc_manual", attval = "https://www.ndbc.noaa.gov/NDBCHandbookofAutomatedDataQualityControl2009.pdf", prec="char" )
                      ncatt_put(nc=con,varid=0, attname="keywords", attval = "Atmospheric Pressure, Sea level Pressure, Atmospheric Temperature, Surface Temperature, Dewpoint Temperature, Humidity, Surface Winds, Ocean Winds, Ocean Temperature, Sea Surface Temperature, Ocean Waves,  Wave Height, Wave Period, Wave Spectra", prec="char")
                      ncatt_put(nc=con,varid=0, attname="keywords_vocabulary", attval = "GCMD Science Keywords", prec="char")
                      ncatt_put(nc=con,varid=0, attname="restrictions", attval = "There are no restrictions placed on these data.", prec="char")
                      ncatt_put(nc=con,varid=0, attname="scientific_project", attval = "None", prec="char")
                      ncatt_put(nc=con,varid=0, attname="flag_Conventions", attval = "Paris. Intergovernmental Oceanographic Commission of UNESCO. 2013.Ocean Data Standards, Vol.3: Recommendation for a Quality Flag Scheme for the Exchange of Oceanographic and Marine Meteorological Data. (IOC Manuals and Guides, 54, Vol. 3.) 12 pp. (IOC/2013/MG/54-3). http://dx.doi.org/10.25607/OBP-6", prec="char")
                      ncatt_put(nc=con,varid=0, attname="flag_descriptions", attval = flag_descrip, prec="char")
                      ncatt_put(nc=con,varid=0, attname="citation", attval = "The National Data Buoy Center should be cited as the source of these data if used in any publication.", prec="char" )
                      ncatt_put(nc=con,varid=0, attname="distribution_statement", attval = "There are no restrictions placed on these data.", prec="char")
                      ncatt_put(nc=con,varid=0, attname="time_coverage_start", attval = dat_start_date, prec="char")
                      ncatt_put(nc=con,varid=0, attname="time_coverage_end", attval = dat_end_date, prec="char")
                      ncatt_put(nc=con,varid=0, attname="date_created", attval = as.character(Sys.time()), prec="char")
                      ncatt_put(nc=con,varid=0, attname="date_created", attval = as.character(Sys.time()), prec="char")
                      ncatt_put(nc=con,varid=0, attname="processing_level", attval = "0", prec="char")
                      ncatt_put(nc=con,varid=0, attname="publisher_name", attval = "U.S. Army Corps of Engineers (USACE) Engineers and Research Development Center (ERDC) Coastal Ocean Data Systems (CODS) Program", prec="char")
                      ncatt_put(nc=con,varid=0, attname="publisher_email", attval = "candice.hall@usace.army.mil", prec="char")
                      
                      if(as.numeric(substr(buoy, start = 1, stop = 2))==45){
                           if(buoy == "45001"){attval1 = "183 m above mean sea level"}
                           if(buoy == "45002"){attval1 = "176 m above mean sea level"}
                           if(buoy == "45003"){attval1 = "177 m above mean sea level"}
                           if(buoy == "45004"){attval1 = "183 m above mean sea level"}
                           if(buoy == "45005"){attval1 = "174 m above mean sea level"}
                           if(buoy == "45006"){attval1 = "183 m above mean sea level"}
                           if(buoy == "45007"){attval1 = "176 m above mean sea level"}
                           if(buoy == "45008"){attval1 = "177 m above mean sea level"}
                           if(buoy == "45010"){attval1 = "177 m above mean sea level"}
                           if(buoy == "45011"){attval1 = "unknown"}
                           if(buoy == "45012"){attval1 = "74.7 m above mean sea level"}
                           ncatt_put(nc=con,varid=0, attname="site_elevation", attval = attval1, prec="char")
                      }else{ncatt_put(nc=con,varid=0, attname="site_elevation", attval = "sea level", prec="char")}
                      
                      ncatt_put(nc=con,varid=0, attname="standard_name_vocabulary", attval = "Standard Name Table (current version, v78, 21 September 2021); https://cfconventions.org/standard-names.html", prec="char")
                      
                      #----------------------------------------------------------------------------------------
                      # closing nc file
                      #----------------------------------------------------------------------------------------
                      nc_close(con) 
                      rm(con,dat,dat_ls, name_var)
                  }else{
                      print(paste0("no data for ", dateRange,"_",m))
                      rm(m, ncname, dat_ls)
                  }
              } # end month range loop
              rm(list = ls(pattern = paste0("var_")))
              rm(list = ls(pattern = paste0("dat_")))
              rm(list = ls(pattern = paste0("dat_spec_")))
              
          } # end of yearly date range loop
          print(paste0("finished build thredds on buoy: ", buoy))
          rm(list = ls(pattern = paste0("s_",buoy)))
          rm(buoy, data_ls, date_year_end, date_year_start, dateRange, freq_ls, std_name)
          Sys.time()
     } # end of buoy loop
}

              

          
 