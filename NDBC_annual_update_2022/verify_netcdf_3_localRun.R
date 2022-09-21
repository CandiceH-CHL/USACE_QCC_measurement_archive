verify_netcdf_3 <- function(buoys = "list of buoys", data_dir = "data_dir"){
                
        ##----------------------------------------------------------------------------------------
        ## validates netCDF metadata with NDBC google spreadsheet metadata as captured by NDBC, DiNapoli,2020
        ## Hall, Candice
        ##----------------------------------------------------------------------------------------
        
        ## Actions:
        ## 1.  Sets data locations
        ## 2.  Read in NDBC and NCEI data if not already loaded in global environ
        ## 3.  Loads and formats the station specific metadata spreadsheet that was concatenated in step 1a above. 
        ## 4.  Selects the station specific NCEI stdmet data and removes NA and duplicate data
        ## 5.  Concatenates and verifies wave metadata with NDBC metadata spreadsheets
        ## 6.  Concatenates and verifies other dual meteorological sensor metadata with the NDBC metadata spreadsheets
        ## 7.  Performs housekeeping tasks to remove excess sensor information labels that were included within the metadata extraction process.
        ## 8.  Removes excess hull info and verify sensor height information
        ## 9.  Double-checks that NCEI netCDF extracted metadata fields no longer contain 'no available information'. 
        ## 10. Filters for unique date/time data and reorders the datasets by date/time, before reordering the datasets to columns structures that match NDBC stdmet datasets.
        ## 11. The verified stdmet with newly verified metadata, as well as all of the individual spectral wave variable datasets, are saved in an 's_buoy#_ncei_ALL_verified.Rdata' container file within buoy station specific folders

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # library(NCmisc)
        # list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)
        ## libraries required
        # install.packages("tidyverse", lib="/p/home/candice/Rlibs/")
        # install.packages("readxl", lib="/p/home/candice/Rlibs/") # dependent on tidyverse
        # install.packages("lubridate", lib="/p/home/candice/Rlibs/")
        # install.packages("stringr", lib="/p/home/candice/Rlibs/")
        # install.packages("data.table", lib="/p/home/candice/Rlibs/")
        # install.packages("dplyr", lib="/p/home/candice/Rlibs/")
        # install.packages("tidyr", lib="/p/home/candice/Rlibs/")
        
        # # load libraries (HPC run)
        # library(readxl, lib="/p/home/candice/Rlibs/") # dependent of tidyverse
        # library(backports, lib="/p/home/candice/Rlibs/") # tidyverse
        # library(withr, lib="/p/home/candice/Rlibs") # tidyverse
        # library(cli, lib="/p/home/candice/Rlibs") # tidyverse
        # library(tzdb, lib="/p/home/candice/Rlibs") # tidyverse
        # library(readr, lib="/p/home/candice/Rlibs") # tidyverse
        # library(rstudioapi, lib="/p/home/candice/Rlibs") # tidyverse
        # library(tidyverse, lib="/p/home/candice/Rlibs/")
        # library(readxl, lib="/p/home/candice/Rlibs/") # dependent on tidyverse
        # library(lubridate, lib="/p/home/candice/Rlibs/")
        # library(stringr, lib="/p/home/candice/Rlibs/")
        # library(data.table, lib="/p/home/candice/Rlibs/")
        # library(crayon, lib="/p/home/candice/Rlibs/") # dplyr
        # library(pillar, lib="/p/home/candice/Rlibs/") # dplyr
        # library(dplyr, lib="/p/home/candice/Rlibs/")
        # library(tidyr, lib="/p/home/candice/Rlibs/")
        # # library(stats, lib="/p/home/candice/Rlibs/")   
     
        # load libraries (local run)
        library(tidyverse)
        library(readxl) # dependent on tidyverse
        library(lubridate)
        library(stringr)
        library(data.table)
        library(dplyr)
        library(tidyr)
        library(stats)

        ##----------------------------------------------------------------------------------------
        ## set paths
        ##----------------------------------------------------------------------------------------
        drive <- "G:/Candice/"
        # drive <- "/p/work/candice/"
        
        data_dir <- paste0(drive, "projects/WaveTrends/data/")
        setwd(data_dir)
        
        # set input directories
        input_dir <- paste0(data_dir,"concat_data/ncei/")
        
        # metadata sheets
        metadata_dir <- paste0(data_dir,"NDBC_metadata_sheets/")
        # if (!file.exists(metadata_dir)) {dir.create(metadata_dir)}
        
        ##----------------------------------------------------------------------------------------
        ## set buoy stations for downloading (for stand-alone use)
        ##----------------------------------------------------------------------------------------
        list_ndbc <- read.csv("NDBC_buoys.csv",header = TRUE)
        list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
        list_ndbc_buoy <- as.character(list_ndbc$station)
        buoys <- list_ndbc_buoy
        rm(list_ndbc, list_ndbc_buoy)
        print(buoys)

        ##----------------------------------------------------------------------------------------
        ## set sensor heights
        ##----------------------------------------------------------------------------------------
        ## NDBC published sensor heights (August 3, 2016): https://www.ndbc.noaa.gov/bht.shtml
        ## At what heights are the sensors located on moored buoys?
        ## Meteorological sensors are normally located at the ten meter level for the 10-meter 
        ## and the 12-meter discus buoys and are at a nominal height of five meters for the 
        ## 3-meter discus buoys and the 6-meter NOMAD buoys. However, barometers are located 
        ## inside the hull at the water level. 
        ## Sea surface temperature sensors are located at a depth of 1.5 meters for 10-m and 12-m 
        ## buoys and at 1 meter for all 3-m and 6-m moored buoys. Sea surface temperature sensors 
        ## on NDBC buoys are located near one meter below the water line but they vary by hull type. 
        ## Current hull configurations for water temperature sensors are: for 2.4- and 3-meter hulls 
        ## at 0.7 meters; for 6-meter hulls at 0.8 meters; for 10-meter hulls at 1.1 meters; and 
        ## for 12-meter hulls at 0.9 meters. Historically, water temperature sensors for 3-meter 
        ## "foam" hulls were at 0.75 meters and 1.8-meter hulls were at 0.35 meters. 
        
        # wind                  # sst           # ref
        wind_12m <- 10;         sst_12m <- 0.9    # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        wind_10m <- 10;         sst_10m <- 1.1    # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        wind_6m <- 5;           sst_6m <- 0.8     # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        wind_3m <- 5;           sst_3m <- 0.7     # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        wind_elb <- 5.2;        sst_elb <- 1.8    # 1985 Smith
        wind_lnb <- 10;         sst_lnb <- 0.9    # Large navigational buoy: NDBC, 1992, ftp://ftp.library.noaa.gov/noaa_documents.lib/NWS/National_Data_Buoy_Center/technical_bulletin/June-1992_vol-18.pdf
        wind_2_8m <- NA;        sst_2_8m <- NA    # No information, Rodney Riley, NDBC Engineer (pers. comms. 03/02/2021)
        wind_2_6m <- NA;        sst_2_6m <- NA    # No information, Rodney Riley, NDBC Engineer (pers. comms. 03/02/2021)
        wind_2_4m <- 3.3;       sst_2_4m <- 0.7   # NDBC 'BUOY COMPARISONS Final - with sensor heights.pdf' 
        wind_2_3m <- 3.2;       sst_2_3m <- 1.3   # 2017 Bouchard et al., NDBC 'BUOY COMPARISONS Final - with sensor heights.pdf'
        wind_2_1m <- 3.2;       sst_2_1m <- 1.1   # NDBC, 2021, https://www.ndbc.noaa.gov/station_page.php?station=45001
        wind_1_8m <- 2.1;       sst_1_8m <- 0.4   # 2008 Crout et al.
        
        for(buoy in buoys){ # 
             # buoy <- buoys[27]

             # start writing to an output file
             print(paste0("Starting on ",buoy))
             
             # # # start writing to an output file
             # sink(paste0(data_dir,"3_ncei_metadata_verification_",buoy,"_",Sys.Date(),".txt"))
             # print(paste0("Starting on... ",buoy))
             
             #----------------------------------------------------------------------------------------
             ## read in data if not loaded in global environ
             #----------------------------------------------------------------------------------------
             
             if(file.exists(paste0(input_dir,buoy,"/s_",buoy,"_ncei_ALL.RData"))){
                     
                     print("Loading datasets")
                     load(paste0(input_dir,buoy,"/s_",buoy,"_ncei_ALL.RData"))
                     
                     # load spreadsheet metadata
                     # /p/work/candice/projects/WaveTrends/data/NDBC_metadata_sheets
                     met_dat <- list.files(metadata_dir,full.names = TRUE); met_dat <- met_dat[grepl("-Meta-Data-",met_dat)]
                     met_dat <- met_dat[grepl(buoy,met_dat)]
                     metadata <- read_excel(met_dat, col_names = TRUE, skip = 2, na = c("?","N/A"))
                     if("AIO Met Sensor" %in% names(metadata)){
                             metadata <- dplyr::select(metadata, `Date Start`,Dep:`AIO Met Sensor`)
                     }else{metadata <- dplyr::select(metadata, `Date Start`,Dep:Sensor); metadata$`AIO Met Sensor` <- NA}
                     # remove column name spaces and caps, replace wave NA, merge wave metadata columns and remove redundant columns
                     metadata <- dplyr::rename(metadata, Date_Start = `Date Start`,depth = Dep,mooring = Mooring,hull = Hull,payload = Payload, waveSys = WaveSys,waveSensor = Sensor, windSensor = `AIO Met Sensor`)
                     for(rn in 1:nrow(metadata)){if(is.na(metadata$waveSys[rn])){metadata$waveSys[rn] <- "unknown"};if(is.na(metadata$waveSensor[rn])){metadata$waveSensor[rn] <- "unknown"}}
                     metadata$waveSys_Sensor <- paste0(metadata$waveSys,"; ",metadata$waveSensor)
                     # reformat date
                     metadata$DateTime <- ymd_hms(paste0(metadata$Date_Start," 00:00:00"))
                     metadata$waveSys <- NULL; metadata$sensor <- NULL; metadata$Date_Start <- NULL
                     # fill in missing data
                     metadata <- fill(metadata, depth,mooring,hull,payload, .direction = "up")
                     # reorder - spreadsheets don't have the same column order (?)
                     metadata <- dplyr::select(metadata, DateTime,depth,hull,mooring,payload,waveSys_Sensor)
                     # handle unknowns
                     index_hull <- which(metadata$hull=="unknown"); if(length(index_hull)>0){metadata$hull[index_hull] <- metadata$hull[index_hull+1]}
                     
                     # set as data.table for date merge
                     metadata <- data.table(metadata)
                     setkey(metadata, DateTime)
                     
                     # find df's to update
                     stdmet_ls <- ls(pattern = "ncei_stdmet")
                     stdmet_ls
                     
                     for(df in stdmet_ls){
                          # df <- stdmet_ls[1]
                          dat <- get(df)
                          
                          print(paste0("working on ",df))
                          print(Sys.time())
                          
                          # removing any character NA's
                          dat[, 2:ncol(dat)][dat[, 2:ncol(dat)]=="NA"] <- NA

                          # remove duplicate wave data
                          if("significant_wave_height_1" %in% names(dat) & "significant_wave_height_2" %in% names(dat)){
                               print("working on redundant wave data")
                               wave <- dplyr::select(dat, DateTime, contains("wave"))

                               for(wn in 1:nrow(dat)){
                                       # removing redundant values
                                       if(round(!is.na(dat$significant_wave_height_1[wn]),4) == round(!is.na(dat$significant_wave_height_2[wn]),4)){dat$significant_wave_height_2[wn] <- NA; dat$significant_wave_height_metadata_2[wn] <- NA}
                                       if(round(!is.na(dat$dominant_wave_period_1[wn]),4) == round(!is.na(dat$dominant_wave_period_2[wn]),4)){dat$dominant_wave_period_2[wn] <- NA}
                                       if(round(!is.na(dat$average_wave_period_1[wn]),4) == round(!is.na(dat$average_wave_period_2[wn]),4)){dat$average_wave_period_2[wn] <- NA}
                                       if("mean_wave_direction_1" %in% names(wave)){if(!is.na(dat$mean_wave_direction_1[wn]) == !is.na(dat$mean_wave_direction_2[wn])){dat$mean_wave_direction_2[wn] <- NA}}
                                       # replacing na primary data with secondary data
                                       if(is.na(dat$significant_wave_height_1[wn]) & !is.na(dat$significant_wave_height_2[wn])){
                                               dat$significant_wave_height_1[wn] <- dat$significant_wave_height_2[wn]; dat$significant_wave_height_metadata_1[wn] <- dat$significant_wave_height_metadata_2[wn]
                                               dat$significant_wave_height_2[wn] <- NA; dat$significant_wave_height_metadata_2[wn] <- NA}
                                       if(is.na(dat$dominant_wave_period_1[wn]) & !is.na(dat$dominant_wave_period_2[wn])){
                                               dat$dominant_wave_period_1[wn] <- dat$dominant_wave_period_2[wn]; dat$dominant_wave_period_metadata_1[wn] <- dat$dominant_wave_period_metadata_2[wn]
                                               dat$dominant_wave_period_2[wn] <- NA; dat$dominant_wave_period_metadata_2[wn] <- NA}
                                       if(is.na(dat$average_wave_period_1[wn]) & !is.na(dat$average_wave_period_2[wn])){
                                               dat$average_wave_period_1[wn] <- dat$average_wave_period_2[wn]; dat$average_wave_period_metadata_1[wn] <- dat$average_wave_period_metadata_2[wn]
                                               dat$average_wave_period_2[wn] <- NA; dat$average_wave_period_metadata_2[wn] <- NA}
                                       if("mean_wave_direction_1" %in% names(wave)){
                                               if(is.na(dat$mean_wave_direction_1[wn]) & !is.na(dat$mean_wave_direction_2[wn])){
                                                       dat$mean_wave_direction_1[wn] <- dat$mean_wave_direction_2[wn]; dat$mean_wave_direction_metadata_1[wn] <- dat$mean_wave_direction_metadata_2[wn]
                                                       dat$mean_wave_direction_2[wn] <- NA; dat$mean_wave_direction_metadata_2[wn] <- NA}
                                       }
                               }
                               if(sum(as.numeric(dat$significant_wave_height_2), na.rm = TRUE)==0){dat$significant_wave_height_2 <- NULL; dat$significant_wave_height_metadata_2 <- NULL; 
                               dat <- dplyr::rename(dat,significant_wave_height = significant_wave_height_1, significant_wave_height_metadata = significant_wave_height_metadata_1) ;print("removing redundant wave height data")}
                               if(sum(as.numeric(dat$dominant_wave_period_2), na.rm = TRUE)==0){dat$dominant_wave_period_2 <- NULL; dat$dominant_wave_period_metadata_2 <- NULL; 
                               dat <- dplyr::rename(dat,dominant_wave_period = dominant_wave_period_1, dominant_wave_period_metadata = dominant_wave_period_metadata_1);print("removing redundant wave dom period data")}
                               if(sum(as.numeric(dat$average_wave_period_2), na.rm = TRUE)==0){dat$average_wave_period_2 <- NULL; dat$average_wave_period_metadata_2 <- NULL; 
                               dat <- dplyr::rename(dat,average_wave_period = average_wave_period_1, average_wave_period_metadata = average_wave_period_metadata_1);print("removing redundant wave avg period data")}
                               if("mean_wave_direction_1" %in% names(wave)){
                                     if(sum(as.numeric(dat$mean_wave_direction_2), na.rm = TRUE)==0){dat$mean_wave_direction_2 <- NULL; dat$mean_wave_direction_metadata_2 <- NULL; 
                                     dat <- dplyr::rename(dat,mean_wave_direction = mean_wave_direction_1, mean_wave_direction_metadata = mean_wave_direction_metadata_1);print("removing redundant wave dir data")}
                               }
                               rm(wn)
                          }
                          
                          print(Sys.time())
                          
                          # verify wave metadata with NDBC spreadsheets
                          print("verify wave metadata with NDBC spreadsheets")
                          dat_sub <- dplyr::select(dat, DateTime, significant_wave_height_metadata)
                          dat_sub <- dat_sub[complete.cases(dat_sub),]
                          # sep column
                          setDT(dat_sub)[, paste0("wave_payload_info", 1:9) := tstrsplit(significant_wave_height_metadata, "__")]
                          # remove original payload
                          dat_sub <- data.frame(dat_sub, stringsAsFactors = FALSE)
                          dat_sub <- dat_sub[, -c(2,4:ncol(dat_sub))]
                          dat_sub$wave_payload_info1 <- gsub("_significant_wave_height","",dat_sub$wave_payload_info1)
                          # set as d.table and merge date/time
                          dat_sub <- data.table(dat_sub)
                          setkey(dat_sub, DateTime)
                          dat_merge <- metadata[dat_sub, roll = "nearest"]
                          dat_merge_wave <- setkey(data.table(dplyr::rename(dplyr::select(dat_merge,DateTime,wave_payload_info1,waveSys_Sensor),wave_payload_info = wave_payload_info1)), DateTime)
                          dat_merge_wave$wave_payload_info_sensor <- paste0(dat_merge_wave$wave_payload_info,"__",dat_merge_wave$waveSys_Sensor)
                          dat_merge_wave$wave_payload_info <- NULL; dat_merge_wave$waveSys_Sensor <- NULL
                          dat_merge_hull <- setkey(data.table(dplyr::select(dat_merge,DateTime,depth,mooring,hull,payload)), DateTime)
                          # merge with original data
                          dat <- data.table(dat)
                          setkey(dat, DateTime)
                          dat <- dat_merge_hull[dat, roll = "nearest"]
                          dat$significant_wave_height_metadata <- NULL; dat$dominant_wave_period_metadata <- NULL; dat$average_wave_period_metadata <- NULL; dat$mean_wave_direction_metadata <- NULL
                          dat <- left_join(dat,dat_merge_wave, by = "DateTime")
                          rm(dat_merge, dat_merge_hull, dat_merge_wave, dat_sub)
                          # set wave metadata
                          dat$significant_wave_height_metadata <- dat$wave_payload_info_sensor
                          dat$dominant_wave_period_metadata <- NA; dat$average_wave_period_metadata <- NA; dat$mean_wave_direction_metadata <- NA
                          dat$wave_payload_info_sensor <- NULL

                          # concat other sensor metadata
                          print("concatenating other sensors metadata")
                          # wind data
                          col_ls <- names(dat)[grep("wind_speed_metadata",names(dat))]
                          if(length(col_ls) == 2){
                               dat$wind_direction_metadata_1 <- NA; dat$wind_gust_metadata_1 <- NA
                               dat$wind_direction_metadata_2 <- NA; dat$wind_gust_metadata_2 <- NA 
                          }else{dat$wind_direction_metadata <- NA; dat$wind_gust_metadata <- NA}
                          
                          # remove excess sensor info
                          print("removing excess sensor info")
                          print(Sys.time())
                          if(grepl("ncei",df,ignore.case = TRUE)){
                               if("wind_speed_metadata_1" %in% names(dat)){
                                     dat$wind_speed_metadata_1 <- gsub("_wind_speed","",dat$wind_speed_metadata_1); dat$wind_speed_metadata_2 <- gsub("_wind_speed","",dat$wind_speed_metadata_2)
                               }else{dat$wind_speed_metadata <- gsub("_wind_speed","",dat$wind_speed_metadata)}
                                  
                               if("air_pressure_at_sea_level_metadata_1" %in% names(dat)){
                                     dat$air_pressure_at_sea_level_metadata_1 <- gsub("_air_pressure_at_sea_level","",dat$air_pressure_at_sea_level_metadata_1);dat$air_pressure_at_sea_level_metadata_2 <- gsub("_air_pressure_at_sea_level","",dat$air_pressure_at_sea_level_metadata_2) 
                               }else{dat$air_pressure_at_sea_level_metadata <- gsub("_air_pressure_at_sea_level","",dat$air_pressure_at_sea_level_metadata)}
                                  
                               if("air_temperature_metadata_1" %in% names(dat)){
                                     dat$air_temperature_metadata_1 <- gsub("air_temperature__","__",dat$air_temperature_metadata_1);dat$air_temperature_metadata_2 <- gsub("air_temperature__","__",dat$air_temperature_metadata_2) 
                                     dat$air_temperature_metadata_1 <- gsub("___","__",dat$air_temperature_metadata_1);dat$air_temperature_metadata_2 <- gsub("___","__",dat$air_temperature_metadata_2) 
                                     dat$air_temperature_metadata_1 <- gsub("___","__",dat$air_temperature_metadata_1);dat$air_temperature_metadata_2 <- gsub("___","__",dat$air_temperature_metadata_2) 
                                     dat$air_temperature_metadata_1 <- gsub("_air_temperature$","",dat$air_temperature_metadata_1);dat$air_temperature_metadata_2 <- gsub("_air_temperature$","",dat$air_temperature_metadata_2)
                               }else{
                                     dat$air_temperature_metadata <- gsub("air_temperature__","__",dat$air_temperature_metadata)
                                     dat$air_temperature_metadata <- gsub("___","__",dat$air_temperature_metadata)
                                     dat$air_temperature_metadata <- gsub("___","__",dat$air_temperature_metadata)
                                     dat$air_temperature_metadata <- gsub("_air_temperature$","",dat$air_temperature_metadata)
                               }
                               if("dew_point_temperature_metadata_1" %in% names(dat)){
                                     dat$dew_point_temperature_metadata_1 <- gsub("_dew_point_temperature","",dat$dew_point_temperature_metadata_1);dat$dew_point_temperature_metadata_2 <- gsub("_dew_point_temperature","",dat$dew_point_temperature_metadata_2)
                               }else{dat$dew_point_temperature_metadata <- gsub("_dew_point_temperature","",dat$dew_point_temperature_metadata)}
                               
                               if("sea_surface_temperature_metadata_1" %in% names(dat)){
                                     dat$sea_surface_temperature_metadata_1 <- gsub("_sea_surface_temperature","",dat$sea_surface_temperature_metadata_1);dat$sea_surface_temperature_metadata_2 <- gsub("_sea_surface_temperature","",dat$sea_surface_temperature_metadata_2)
                               }else{dat$sea_surface_temperature_metadata <- gsub("_sea_surface_temperature","",dat$sea_surface_temperature_metadata)}
                                  
                          }
                 
                          # remove excess hull info and verify sensor height
                          print("removing excess hull info and verify sensor height")
                          for(rn in 1:nrow(dat)){
                               # print(dat$DateTime[rn])
                
                               # verify hull
                               rm(hull_id, hull_type)
                               hull_id <- dat$hull[rn]
                               if(str_detect(hull_id, "D")){hull_type <- stringr::str_extract(hull_id, ".{0,3}D"); hull_type <- gsub("D","",hull_type)}
                               if(str_detect(hull_id, "N")){hull_type <- stringr::str_extract(hull_id, ".{0,3}N"); hull_type <- gsub("N","",hull_type)}
                               if(str_detect(hull_id, "B")){hull_type <- stringr::str_extract(hull_id, ".{0,3}B")}# ; hull_type <- gsub("D","",hull_type)}
                               if(hull_type == 12){wind_height <- wind_12m; sst_height <- sst_12m};    if(hull_type == 10){wind_height <- wind_10m; sst_height <- sst_10m}
                               if(hull_type == 6){wind_height <- wind_6m; sst_height <- sst_6m};       if(hull_type == 3){wind_height <- wind_3m; sst_height <- sst_3m}
                               if(hull_type == "ELB"){wind_height <- wind_elb; sst_height <- sst_elb}; if(hull_type == 2.8){wind_height <- wind_2_8m; sst_height <- sst_2_8m}
                               if(hull_type == "LNB"){wind_height <- wind_lnb; sst_height <- sst_lnb}; if(hull_type == 2.8){wind_height <- wind_2_8m; sst_height <- sst_2_8m}
                               if(hull_type == 2.6){wind_height <- wind_2_6m; sst_height <- sst_2_6m}; if(hull_type == 2.4){wind_height <- wind_2_4m; sst_height <- sst_2_4m}
                               if(hull_type == 2.3){wind_height <- wind_2_3m; sst_height <- sst_2_3m}; if(hull_type == 1.8){wind_height <- wind_1_8m; sst_height <- sst_1_8m}
                               if(hull_type == 2.1){wind_height <- wind_2_1m; sst_height <- sst_2_1m}
                               
                               # air pressure
                               if("air_pressure_at_sea_level_metadata_1" %in% names(dat)){
                                     if(!is.na(dat$air_pressure_at_sea_level_metadata_1[rn])){
                                         rm_pattern <- stringr::str_extract(dat$air_pressure_at_sea_level_metadata_1[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$air_pressure_at_sea_level_metadata_1[rn] <- gsub(rm_pattern,"",dat$air_pressure_at_sea_level_metadata_1[rn])}
                                         rm(rm_pattern)
                                     }
                               }
                               if("air_pressure_at_sea_level_metadata_2" %in% names(dat)){
                                    if(!is.na(dat$air_pressure_at_sea_level_metadata_2[rn])){
                                         rm_pattern <- stringr::str_extract(dat$air_pressure_at_sea_level_metadata_2[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$air_pressure_at_sea_level_metadata_2[rn] <- gsub(rm_pattern,"",dat$air_pressure_at_sea_level_metadata_2[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("air_pressure_at_sea_level_metadata" %in% names(dat)){
                                    if(!is.na(dat$air_pressure_at_sea_level_metadata[rn])){
                                         rm_pattern <- stringr::str_extract(dat$air_pressure_at_sea_level_metadata[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$air_pressure_at_sea_level_metadata[rn] <- gsub(rm_pattern,"",dat$air_pressure_at_sea_level_metadata[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               # air temp
                               if("air_temperature_metadata_1" %in% names(dat)){
                                    if(!is.na(dat$air_temperature_metadata_1[rn])){
                                         rm_pattern <- stringr::str_extract(dat$air_temperature_metadata_1[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$air_temperature_metadata_1[rn] <- gsub(rm_pattern,"",dat$air_temperature_metadata_1[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("air_temperature_metadata_2" %in% names(dat)){
                                    if(!is.na(dat$air_temperature_metadata_2[rn])){
                                         rm_pattern <- stringr::str_extract(dat$air_temperature_metadata_2[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$air_temperature_metadata_2[rn] <- gsub(rm_pattern,"",dat$air_temperature_metadata_2[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("air_temperature_metadata" %in% names(dat)){
                                    if(!is.na(dat$air_temperature_metadata[rn])){
                                         rm_pattern <- stringr::str_extract(dat$air_temperature_metadata[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$air_temperature_metadata[rn] <- gsub(rm_pattern,"",dat$air_temperature_metadata[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               # sea temp
                               if("sea_surface_temperature_metadata_1" %in% names(dat)){
                                    if(!is.na(dat$sea_surface_temperature_metadata_1[rn])){
                                         rm_pattern <- stringr::str_extract(dat$sea_surface_temperature_metadata_1[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$sea_surface_temperature_metadata_1[rn] <- gsub(rm_pattern,"",dat$sea_surface_temperature_metadata_1[rn])}
                                         rm(rm_pattern)
                                         dat$sea_surface_temperature_metadata_1[rn] <- gsub("no available height data", paste0(sst_height,"m"), dat$sea_surface_temperature_metadata_1[rn])
                                    }
                               }
                               if("sea_surface_temperature_metadata_2" %in% names(dat)){
                                    if(!is.na(dat$sea_surface_temperature_metadata_2[rn])){
                                         rm_pattern <- stringr::str_extract(dat$sea_surface_temperature_metadata_2[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$sea_surface_temperature_metadata_2[rn] <- gsub(rm_pattern,"",dat$sea_surface_temperature_metadata_2[rn])}
                                         rm(rm_pattern)
                                         dat$sea_surface_temperature_metadata_2[rn] <- gsub("no available height data", paste0(sst_height,"m"), dat$sea_surface_temperature_metadata_2[rn])
                                    }
                               }
                               if("sea_surface_temperature_metadata" %in% names(dat)){
                                    if(!is.na(dat$sea_surface_temperature_metadata[rn])){
                                         rm_pattern <- stringr::str_extract(dat$sea_surface_temperature_metadata[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$sea_surface_temperature_metadata[rn] <- gsub(rm_pattern,"",dat$sea_surface_temperature_metadata[rn])}
                                         rm(rm_pattern)
                                         dat$sea_surface_temperature_metadata[rn] <- gsub("no available height data", paste0(sst_height,"m"), dat$sea_surface_temperature_metadata[rn])
                                    }
                               }
                               # dew pt temp
                               if("dew_point_temperature_metadata_1" %in% names(dat)){
                                    if(!is.na(dat$dew_point_temperature_metadata_1[rn])){
                                         rm_pattern <- stringr::str_extract(dat$dew_point_temperature_metadata_1[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$dew_point_temperature_metadata_1[rn] <- gsub(rm_pattern,"",dat$dew_point_temperature_metadata_1[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("dew_point_temperature_metadata_2" %in% names(dat)){
                                    if(!is.na(dat$dew_point_temperature_metadata_2[rn])){
                                         rm_pattern <- stringr::str_extract(dat$dew_point_temperature_metadata_2[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$dew_point_temperature_metadata_2[rn] <- gsub(rm_pattern,"",dat$dew_point_temperature_metadata_2[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("dew_point_temperature_metadata" %in% names(dat)){
                                    if(!is.na(dat$dew_point_temperature_metadata[rn])){
                                         rm_pattern <- stringr::str_extract(dat$dew_point_temperature_metadata[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$dew_point_temperature_metadata[rn] <- gsub(rm_pattern,"",dat$dew_point_temperature_metadata[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               # wind sensor metadata
                               if("wind_speed_metadata_1" %in% names(dat)){
                                    if(!is.na(dat$wind_speed_metadata_1[rn])){
                                         rm_pattern <- stringr::str_extract(dat$wind_speed_metadata_1[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$wind_speed_metadata_1[rn] <- gsub(rm_pattern,"",dat$wind_speed_metadata_1[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("wind_speed_metadata_2" %in% names(dat)){
                                    if(!is.na(dat$wind_speed_metadata_2[rn])){
                                         rm_pattern <- stringr::str_extract(dat$wind_speed_metadata_2[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$wind_speed_metadata_2[rn] <- gsub(rm_pattern,"",dat$wind_speed_metadata_2[rn])}
                                         rm(rm_pattern)
                                    }
                               }
                               if("wind_speed_metadata" %in% names(dat)){
                                    if(!is.na(dat$wind_speed_metadata[rn])){
                                         rm_pattern <- stringr::str_extract(dat$wind_speed_metadata[rn], paste0(".{0,2}",hull_id,".{0,7}"))
                                         if(!is.na(rm_pattern)){dat$wind_speed_metadata[rn] <- gsub(rm_pattern,"",dat$wind_speed_metadata[rn])}
                                         rm(rm_pattern)
                                    }
                               }

                               # remove unknown from redundant sensor heights
                               # find sensor and associated heights
                               rm(wind_s1, wind_s1_height, wind_s2, wind_s2_height)
                               if("wind_speed_metadata_1" %in% names(dat)){
                                    if(!is.na(dat$wind_speed_metadata_1[rn])){
                                         wind_sensor <- unlist(strsplit(dat$wind_speed_metadata_1[rn], "__")); wind_s1 <- wind_sensor[length(wind_sensor)-1];if(length(wind_s1)==0){wind_s1 <- dat$wind_speed_metadata_1[rn]}
                                         wind_sensor_height <- unlist(strsplit(dat$wind_speed_metadata_1[rn], "__")); #wind_s1_height <- wind_sensor_height[length(wind_sensor_height)]
                                         if(wind_sensor_height != wind_s1){wind_s1_height <- wind_sensor_height[length(wind_sensor_height)]}else{wind_s1 <- NA; wind_s1_height <- NA}
                                         if(!is.na(wind_s1_height) & !is.na(wind_s1)){
                                                 if(wind_s1_height == "no available height data" & wind_s1 == "no available manufacturer data"){wind_s1_height <- wind_height;dat$wind_speed_metadata_1[rn] <- gsub("no available height data",paste0(wind_s1_height,"m"), dat$wind_speed_metadata_1[rn])}
                                         }
                                         rm(wind_sensor, wind_sensor_height)
                                    }else{wind_s1 <- NA; wind_s1_height <- NA}
                               }
                               if("wind_speed_metadata_2" %in% names(dat)){
                                    if(!is.na(dat$wind_speed_metadata_2[rn])){
                                         wind_sensor <- unlist(strsplit(dat$wind_speed_metadata_2[rn], "__")); wind_s2 <- wind_sensor[length(wind_sensor)-1];if(length(wind_s2)==0){wind_s2 <- dat$wind_speed_metadata_2[rn]}
                                         wind_sensor_height <- unlist(strsplit(dat$wind_speed_metadata_2[rn], "__"))
                                         if(wind_sensor_height != wind_s2){wind_s2_height <- wind_sensor_height[length(wind_sensor_height)]}else{wind_s2 <- NA; wind_s2_height <- NA}
                                         if(!is.na(wind_s2_height) & !is.na(wind_s2)){
                                                 if(wind_s2_height == "no available height data" & wind_s2 == "no available manufacturer data"){wind_s2_height <- wind_height;dat$wind_speed_metadata_2[rn] <- gsub("no available height data",paste0(wind_s2_height,"m"), dat$wind_speed_metadata_2[rn])}
                                         }
                                         rm(wind_sensor, wind_sensor_height)
                                    }else{wind_s2 <- NA; wind_s2_height <- NA}
                               }
                               if("wind_speed_metadata" %in% names(dat)){
                                    if(!is.na(dat$wind_speed_metadata[rn])){
                                         wind_sensor <- unlist(strsplit(dat$wind_speed_metadata[rn], "__")); wind_s1 <- wind_sensor[length(wind_sensor)-1];if(length(wind_s1)==0){wind_s1 <- dat$wind_speed_metadata[rn]}
                                         wind_sensor_height <- unlist(strsplit(dat$wind_speed_metadata[rn], "__"))
                                         if(wind_sensor_height != wind_s1){wind_s1_height <- wind_sensor_height[length(wind_sensor_height)]}else{wind_s1 <- NA; wind_s1_height <- NA}
                                         if(!is.na(wind_s1_height) & !is.na(wind_s1)){
                                                 if(wind_s1_height == "no available height data" & wind_s1 == "no available manufacturer data"){wind_s1_height <- wind_height;dat$wind_speed_metadata[rn] <- gsub("no available height data",paste0(wind_s1_height,"m"), dat$wind_speed_metadata[rn])}
                                         }
                                         rm(wind_sensor, wind_sensor_height)
                                    }else{wind_s1 <- NA; wind_s1_height <- NA}
                               }
                               
                               # cycle through wind sensor if present
                               if(exists("wind_s1")){ # mainly for NCEI data
                                    if(!is.na(wind_s1)){
                                         
                                         # air pressure
                                         if("air_pressure_at_sea_level_metadata_1" %in% names(dat)){
                                              # sensor 1
                                              if(!is.na(dat$air_pressure_at_sea_level_metadata_1[rn])){
                                                   AP_1 <- unlist(strsplit(dat$air_pressure_at_sea_level_metadata_1[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                   if(grepl(wind_s1, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_pressure_at_sea_level_metadata_1[rn] <- gsub("no available height data",wind_s1_height,dat$air_pressure_at_sea_level_metadata_1[rn])}
                                              }
                                              # sensor 2
                                              if(!is.na(dat$air_pressure_at_sea_level_metadata_2[rn])){
                                                   AP_2 <- unlist(strsplit(dat$air_pressure_at_sea_level_metadata_2[rn], "__"));AP_s2 <- AP_2[length(AP_2)-1]
                                                   if(grepl(wind_s1, AP_s2, ignore.case = TRUE, fixed = FALSE)){dat$air_pressure_at_sea_level_metadata_2[rn] <- gsub("no available height data",wind_s1_height,dat$air_pressure_at_sea_level_metadata_2[rn])}
                                              }
                                         }
                                         if("air_pressure_at_sea_level_metadata" %in% names(dat)){
                                              # sensor 1
                                              if(!is.na(dat$air_pressure_at_sea_level_metadata[rn])){
                                                   AP_1 <- unlist(strsplit(dat$air_pressure_at_sea_level_metadata[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                   if(exists("wind_s1")){if(grepl(wind_s1, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_pressure_at_sea_level_metadata[rn] <- gsub("no available height data",wind_s1_height,dat$air_pressure_at_sea_level_metadata[rn])}}
                                              }
                                         }
                                         
                                         # air temperature
                                         if("air_temperature_metadata_1" %in% names(dat)){
                                              # sensor 1
                                              if(!is.na(dat$air_temperature_metadata_1[rn])){
                                                   AP_1 <- unlist(strsplit(dat$air_temperature_metadata_1[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                   if(grepl(wind_s1, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_temperature_metadata_1[rn] <- gsub("no available height data",wind_s1_height,dat$air_temperature_metadata_1[rn])}
                                              }
                                              # sensor 2
                                              if(!is.na(dat$air_temperature_metadata_2[rn])){
                                                   AP_2 <- unlist(strsplit(dat$air_temperature_metadata_2[rn], "__"));AP_s2 <- AP_2[length(AP_2)-1]
                                                   if(grepl(wind_s1, AP_s2, ignore.case = TRUE, fixed = FALSE)){dat$air_temperature_metadata_2[rn] <- gsub("no available height data",wind_s1_height,dat$air_temperature_metadata_2[rn])}
                                              }
                                              if(buoy == 41001 & year(dat$DateTime[rn])==2019){
                                                   if(grepl("no available description data__no available manufacturer data__no available height data",dat$air_temperature_metadata_1[rn])){
                                                        if(is.na(dat$air_temperature_metadata_2[rn]) & !is.na(dat$air_pressure_at_sea_level_2[rn])){
                                                             dat$air_temperature_metadata_1[rn] <- dat$air_pressure_at_sea_level_metadata_2[rn]
                                                             dat$air_temperature_metadata_1[rn] <- gsub("barometer","air_temperature_sensor", dat$air_temperature_metadata_1[rn])
                                                        }
                                                   }
                                              }
                                         }
                                         if("air_temperature_metadata" %in% names(dat)){
                                              # sensor 1
                                              if(!is.na(dat$air_temperature_metadata[rn])){
                                                   AP_1 <- unlist(strsplit(dat$air_temperature_metadata[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                   if(exists("wind_s1")){if(grepl(wind_s1, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_temperature_metadata[rn] <- gsub("no available height data",wind_s1_height,dat$air_temperature_metadata[rn])}}
                                              }
                                         }
                                         
                                         # dew point temperature
                                         if("dew_point_temperature_metadata_1" %in% names(dat)){
                                              # sensor 1
                                              if(!is.na(dat$dew_point_temperature_metadata_1[rn])){
                                                   AP_1 <- unlist(strsplit(dat$dew_point_temperature_metadata_1[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                   if(grepl(wind_s1, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$dew_point_temperature_metadata_1[rn] <- gsub("no available height data",wind_s1_height,dat$dew_point_temperature_metadata_1[rn])}
                                              }
                                              # sensor 2
                                              if(!is.na(dat$dew_point_temperature_metadata_2[rn])){
                                                   AP_2 <- unlist(strsplit(dat$dew_point_temperature_metadata_2[rn], "__"));AP_s2 <- AP_2[length(AP_2)-1]
                                                   if(grepl(wind_s1, AP_s2, ignore.case = TRUE, fixed = FALSE)){dat$dew_point_temperature_metadata_2[rn] <- gsub("no available height data",wind_s1_height,dat$dew_point_temperature_metadata_2[rn])}
                                              }
                                              if(buoy == 41001 & year(dat$DateTime[rn])==2019){
                                                   if(grepl("no available description data__no available manufacturer data__no available height data",dat$dew_point_temperature_metadata_1[rn])){
                                                        if(is.na(dat$dew_point_temperature_metadata_2[rn]) & !is.na(dat$air_pressure_at_sea_level_2[rn])){
                                                             dat$dew_point_temperature_metadata_1[rn] <- dat$air_temperature_metadata_1[rn]
                                                             dat$dew_point_temperature_metadata_1[rn] <- gsub("barometer","air_temperature_sensor", dat$dew_point_temperature_metadata_1[rn])
                                                        }
                                                   }
                                              }
                                         }
                                         if("dew_point_temperature_metadata" %in% names(dat)){
                                              # sensor 1
                                              if(!is.na(dat$dew_point_temperature_metadata[rn])){
                                                   AP_1 <- unlist(strsplit(dat$dew_point_temperature_metadata[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                   if(exists("wind_s1")){if(grepl(wind_s1, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$dew_point_temperature_metadata[rn] <- gsub("no available height data",wind_s1_height,dat$dew_point_temperature_metadata[rn])}}
                                              }
                                         }
                                    }
                                    if(exists("wind_s2")){
                                         if(!is.na(wind_s2)){
                                              # air pressure
                                              if("air_pressure_at_sea_level_metadata_1" %in% names(dat)){
                                                   # sensor 1
                                                   if(!is.na(dat$air_pressure_at_sea_level_metadata_1[rn])){
                                                        AP_1 <- unlist(strsplit(dat$air_pressure_at_sea_level_metadata_1[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                        if(grepl(wind_s2, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_pressure_at_sea_level_metadata_1[rn] <- gsub("no available height data",wind_s2_height,dat$air_pressure_at_sea_level_metadata_1[rn])}
                                                   }
                                                   # sensor 2
                                                   if(!is.na(dat$air_pressure_at_sea_level_metadata_2[rn])){
                                                        AP_2 <- unlist(strsplit(dat$air_pressure_at_sea_level_metadata_2[rn], "__"));AP_s2 <- AP_2[length(AP_2)-1]
                                                        if(grepl(wind_s2, AP_s2, ignore.case = TRUE, fixed = FALSE)){dat$air_pressure_at_sea_level_metadata_2[rn] <- gsub("no available height data",wind_s2_height,dat$air_pressure_at_sea_level_metadata_2[rn])}
                                                   }
                                              }
                                              if("air_pressure_at_sea_level_metadata" %in% names(dat)){
                                                   # sensor 1
                                                   if(!is.na(dat$air_pressure_at_sea_level_metadata[rn])){
                                                        AP_1 <- unlist(strsplit(dat$air_pressure_at_sea_level_metadata[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                        if(exists("wind_s2")){if(grepl(wind_s2, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_pressure_at_sea_level_metadata[rn] <- gsub("no available height data",wind_s2_height,dat$air_pressure_at_sea_level_metadata[rn])}}
                                                   }
                                              }
                                              
                                              # air temperature
                                              if("air_temperature_metadata_1" %in% names(dat)){
                                                   # sensor 1
                                                   if(!is.na(dat$air_temperature_metadata_1[rn])){
                                                        AP_1 <- unlist(strsplit(dat$air_temperature_metadata_1[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                        if(grepl(wind_s2, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_temperature_metadata_1[rn] <- gsub("no available height data",wind_s2_height,dat$air_temperature_metadata_1[rn])}
                                                   }
                                                   # sensor 2
                                                   if(!is.na(dat$air_temperature_metadata_2[rn])){
                                                        AP_2 <- unlist(strsplit(dat$air_temperature_metadata_2[rn], "__"));AP_s2 <- AP_2[length(AP_2)-1]
                                                        if(grepl(wind_s2, AP_s2, ignore.case = TRUE, fixed = FALSE)){dat$air_temperature_metadata_2[rn] <- gsub("no available height data",wind_s2_height,dat$air_temperature_metadata_2[rn])}
                                                   }
                                              }
                                              if("air_temperature_metadata" %in% names(dat)){
                                                   # sensor 1
                                                   if(!is.na(dat$air_temperature_metadata[rn])){
                                                        AP_1 <- unlist(strsplit(dat$air_temperature_metadata[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                        if(exists("wind_s2")){if(grepl(wind_s2, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$air_temperature_metadata[rn] <- gsub("no available height data",wind_s2_height,dat$air_temperature_metadata[rn])}}
                                                   }
                                              }
                                              
                                              # dew point temperature
                                              if("dew_point_temperature_metadata_1" %in% names(dat)){
                                                   # sensor 1
                                                   if(!is.na(dat$dew_point_temperature_metadata_1[rn])){
                                                        AP_1 <- unlist(strsplit(dat$dew_point_temperature_metadata_1[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                        if(grepl(wind_s2, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$dew_point_temperature_metadata_1[rn] <- gsub("no available height data",wind_s2_height,dat$dew_point_temperature_metadata_1[rn])}
                                                   }
                                                   # sensor 2
                                                   if(!is.na(dat$dew_point_temperature_metadata_2[rn])){
                                                        AP_2 <- unlist(strsplit(dat$dew_point_temperature_metadata_2[rn], "__"));AP_s2 <- AP_2[length(AP_2)-1]
                                                        if(grepl(wind_s2, AP_s2, ignore.case = TRUE, fixed = FALSE)){dat$dew_point_temperature_metadata_2[rn] <- gsub("no available height data",wind_s2_height,dat$dew_point_temperature_metadata_2[rn])}
                                                   }
                                              }
                                              if("dew_point_temperature_metadata" %in% names(dat)){
                                                   # sensor 1
                                                   if(!is.na(dat$dew_point_temperature_metadata[rn])){
                                                        AP_1 <- unlist(strsplit(dat$dew_point_temperature_metadata[rn], "__"));AP_s1 <- AP_1[length(AP_1)-1]
                                                        if(exists("wind_s2")){if(grepl(wind_s2, AP_s1, ignore.case = TRUE, fixed = FALSE)){dat$dew_point_temperature_metadata[rn] <- gsub("no available height data",wind_s2_height,dat$dew_point_temperature_metadata[rn])}}
                                                   }
                                              }
                                         }
                                    }
                               }
                               
                               # cycle through one more time looking for 'no available height data'
                               # wind
                               if("wind_speed_metadata_1" %in% names(dat)){
                                       if(!is.na(dat$wind_speed_metadata_1[rn])){
                                               if(grepl("no available height data", dat$wind_speed_metadata_1[rn])==TRUE){
                                                       dat$wind_speed_metadata_1[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$wind_speed_metadata_1[rn])
                                               }
                                       }
                                       if(!is.na(dat$wind_speed_metadata_2[rn])){
                                               if(grepl("no available height data", dat$wind_speed_metadata_2[rn])==TRUE){
                                                       dat$wind_speed_metadata_2[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$wind_speed_metadata_2[rn])
                                               }
                                       }
                               }
                               if("wind_speed_metadata" %in% names(dat)){
                                       if(!is.na(dat$wind_speed_metadata[rn])){
                                               if(grepl("no available height data", dat$wind_speed_metadata[rn])==TRUE){
                                                       dat$wind_speed_metadata[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$wind_speed_metadata[rn])
                                               }
                                       }
                               }
                               # air pressure
                               if("air_pressure_at_sea_level_metadata_1" %in% names(dat)){
                                       if(!is.na(dat$air_pressure_at_sea_level_metadata_1[rn])){
                                               if(grepl("no available height data", dat$air_pressure_at_sea_level_metadata_1[rn])==TRUE){
                                                       dat$air_pressure_at_sea_level_metadata_1[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$air_pressure_at_sea_level_metadata_1[rn])
                                               }
                                       }
                                       if(!is.na(dat$air_pressure_at_sea_level_metadata_2[rn])){
                                               if(grepl("no available height data", dat$air_pressure_at_sea_level_metadata_2[rn])==TRUE){
                                                       dat$air_pressure_at_sea_level_metadata_2[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$air_pressure_at_sea_level_metadata_2[rn])
                                               }
                                       }
                               }
                               if("air_pressure_at_sea_level_metadata" %in% names(dat)){
                                       if(!is.na(dat$air_pressure_at_sea_level_metadata[rn])){
                                               if(grepl("no available height data", dat$air_pressure_at_sea_level_metadata[rn])==TRUE){
                                                       dat$air_pressure_at_sea_level_metadata[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$air_pressure_at_sea_level_metadata[rn])
                                               }
                                       }
                               }
                               # air temperature
                               if("air_temperature_metadata_1" %in% names(dat)){
                                       if(!is.na(dat$air_temperature_metadata_1[rn])){
                                               if(grepl("no available height data", dat$air_temperature_metadata_1[rn])==TRUE){
                                                       dat$air_temperature_metadata_1[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$air_temperature_metadata_1[rn])
                                               }
                                       }
                                       if(!is.na(dat$air_temperature_metadata_2[rn])){
                                               if(grepl("no available height data", dat$air_temperature_metadata_2[rn])==TRUE){
                                                       dat$air_temperature_metadata_2[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$air_temperature_metadata_2[rn])
                                               }
                                       }
                               }
                               if("air_temperature_metadata" %in% names(dat)){
                                       if(!is.na(dat$air_temperature_metadata[rn])){
                                               if(grepl("no available height data", dat$air_temperature_metadata[rn])==TRUE){
                                                       dat$air_temperature_metadata[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$air_temperature_metadata[rn])
                                               }
                                       }
                               }
                               # dew point temperature
                               if("dew_point_temperature_metadata_1" %in% names(dat)){
                                       if(!is.na(dat$dew_point_temperature_metadata_1[rn])){
                                               if(grepl("no available height data", dat$dew_point_temperature_metadata_1[rn])==TRUE){
                                                       dat$dew_point_temperature_metadata_1[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$dew_point_temperature_metadata_1[rn])
                                               }
                                       }
                                       if(!is.na(dat$dew_point_temperature_metadata_2[rn])){
                                               if(grepl("no available height data", dat$dew_point_temperature_metadata_2[rn])==TRUE){
                                                       dat$dew_point_temperature_metadata_2[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$dew_point_temperature_metadata_2[rn])
                                               }
                                       }
                               }
                               if("dew_point_temperature_metadata" %in% names(dat)){
                                       if(!is.na(dat$dew_point_temperature_metadata[rn])){
                                               if(grepl("no available height data", dat$dew_point_temperature_metadata[rn])==TRUE){
                                                       dat$dew_point_temperature_metadata[rn] <- gsub("no available height data", paste0(wind_height,"m"), dat$dew_point_temperature_metadata[rn])
                                               }
                                       }
                               }
                               # copy metadata across wave variables
                               if(!is.na(dat$dominant_wave_period[rn])){dat$dominant_wave_period_metadata[rn] <- dat$significant_wave_height_metadata[rn]}
                               if(!is.na(dat$average_wave_period[rn])){dat$average_wave_period_metadata[rn] <- dat$significant_wave_height_metadata[rn]}
                               if(!is.na(dat$mean_wave_direction[rn])){dat$mean_wave_direction_metadata[rn] <- dat$significant_wave_height_metadata[rn]}
                               if("wind_direction_1" %in% names(dat)){
                                       if(!is.na(dat$wind_speed_metadata_1[rn])){dat$wind_direction_metadata_1[rn] <- dat$wind_speed_metadata_1[rn]; dat$wind_gust_metadata_1[rn] <- dat$wind_speed_metadata_1[rn]}
                                       if(!is.na(dat$wind_speed_metadata_2[rn])){dat$wind_direction_metadata_2[rn] <- dat$wind_speed_metadata_2[rn]; dat$wind_gust_metadata_2[rn] <- dat$wind_speed_metadata_2[rn]}
                               }
                          } # end of row checks
                          
                          # fix potential wind issues
                          if("wind_direction_metadata" %in% names(dat) & "wind_direction_metadata_1" %in% names(dat)& "wind_direction_metadata_2" %in% names(dat)){
                              dat$wind_direction_1 <- dat$wind_direction; dat$wind_direction_metadata_1 <- dat$wind_speed_metadata_1
                              dat$wind_direction_2 <- NA; dat$wind_direction_metadata_2 <- dat$wind_speed_metadata_2
                              dat$wind_direction <- NULL; dat$wind_direction_metadata <- NULL
                          }
                          
                          # ordering the df by date and selecting unique values only
                          dat <- dat[order(dat$DateTime),]
                          # rename the rows to reflect unique data
                          row.names(dat) <- 1:nrow(dat)
                          
                          if(ncol(dat)== 45){
                                  print("ncei data format - dual sst")
                                  dat <- dplyr::select(dat, DateTime, depth, mooring, hull, payload, lat, lat_metadata, lon, lon_metadata,
                                                wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                                wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                                wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                                significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                                average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                                air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                                air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                                sea_surface_temperature_1, sea_surface_temperature_metadata_1,sea_surface_temperature_2, sea_surface_temperature_metadata_2,
                                                dew_point_temperature_1, dew_point_temperature_metadata_1, dew_point_temperature_2, dew_point_temperature_metadata_2)
                          }else if(ncol(dat)== 43){
                                  print("ncei data format - single sst or single dew point temperature")
                                  if("sea_surface_temperature" %in% names(dat)){
                                          dat <- dplyr::select(dat, DateTime, depth, mooring, hull, payload, lat, lat_metadata, lon, lon_metadata,
                                                               wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                                               wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                                               wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                                               significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                                               average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                                               air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                                               air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                                               sea_surface_temperature, sea_surface_temperature_metadata,
                                                               dew_point_temperature_1, dew_point_temperature_metadata_1, dew_point_temperature_2, dew_point_temperature_metadata_2)
                                  }else if("dew_point_temperature" %in% names(dat)){
                                          dat <- dplyr::select(dat, DateTime, depth, mooring, hull, payload, lat, lat_metadata, lon, lon_metadata,
                                                               wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                                               wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                                               wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                                               significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                                               average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                                               air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                                               air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                                               sea_surface_temperature_1, sea_surface_temperature_metadata_1,sea_surface_temperature_2, sea_surface_temperature_metadata_2,
                                                               dew_point_temperature, dew_point_temperature_metadata)
                                  }else{print("error: dim = 43 but dat names don't match")}
                          }else if(ncol(dat)== 41){
                                  print("ncei data format - single dew point")
                                  dat <- dplyr::select(dat, DateTime, depth, mooring, hull, payload, lat, lat_metadata, lon, lon_metadata,
                                                wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                                wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                                wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                                significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                                average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                                air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                                air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                                sea_surface_temperature, sea_surface_temperature_metadata,
                                                dew_point_temperature, dew_point_temperature_metadata)
                          }else if(ncol(dat)== 31){
                                  print("ncei data format - single sensors for all")
                                  dat <- dplyr::select(dat, DateTime, depth, mooring, hull, payload, lat, lat_metadata, lon, lon_metadata,
                                                wind_direction, wind_direction_metadata, wind_speed, wind_speed_metadata, wind_gust, wind_gust_metadata, 
                                                significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                                average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                                air_pressure_at_sea_level, air_pressure_at_sea_level_metadata, air_temperature, air_temperature_metadata,
                                                sea_surface_temperature, sea_surface_temperature_metadata,
                                                dew_point_temperature, dew_point_temperature_metadata)
                          }else if(ncol(dat)== 39){
                              print("ncei data format - dual sensors for wind and air pressure")
                              dat <- dplyr::select(dat, DateTime, depth, mooring, hull, payload, lat, lat_metadata, lon, lon_metadata,
                                                   wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                                   wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                                   wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                                   significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                                   average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                                   air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                                   air_temperature, air_temperature_metadata, 
                                                   sea_surface_temperature, sea_surface_temperature_metadata,
                                                   dew_point_temperature, dew_point_temperature_metadata)
                          }
                          
                          # save data
                          dfv <- paste0(df,"_verified")
                          assign(dfv,dat)
                          rm(dat,AP_1, AP_2, AP_s1, AP_s2, hull_type, index_hull,wind_s1, wind_s1_height, wind_s2, wind_s2_height, metadata)
                     }
                     
                     #----------------------------------------------------------------------------------------
                     # exporting GeoCleaned datasets
                     #----------------------------------------------------------------------------------------
                     
                     print("Exporting new verified data")
                     ncei_list <- ls(pattern = "verified")
                     print(paste0("NCEI datasets :", ncei_list))
                     
                     # export and save verified dataset
                     for(g in ncei_list){
                          if(grepl("_verified",g)==TRUE){
                               print(g)
                               # write.table(get(g), paste0(input_dir,buoy,"/",g,".csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                               # saveRDS(get(g), paste0(input_dir,buoy,"/",g,".rds"))
                          }
                     }
                     # export to RData
                     ncei_list <- ls(pattern = buoy)
                     save(list = ncei_list, file = paste0(input_dir,buoy,"/s_",buoy,"_ncei_ALL_verified.RData"))
                     rm(list = ncei_list)
                     rm(df, dfv,g,rn,stdmet_ls,ncei_list, col_ls,met_dat)
                     
                     # # Stop writing to the file
                     # sink()
                     print("test complete")
        
                     print(paste0("finishing metadata verification for buoy ",buoy))
             }else{print("no new data for this buoy")}
        }
}