verify_netcdf_3 <- function(buoys = "list of buoys", data_dir = "data_dir"){
                
        ##----------------------------------------------------------------------------------------
        ## validates netCDF metadata with NDBC google spreadsheet metadata as captured by NDBC, DiNapoli,2020
        ## Hall, Candice
        ##----------------------------------------------------------------------------------------
        
        ## Actions:
        ## 1.  Sets data locations
        ## 2.  Read in NDBC and NCEI data if not already loaded in global environ
        ## 3.  Loads and formats the station specific metadata spreadsheet that was concatenated in step 1a above. 
        ## 4.  Selects the station specific NCEI stdmet data and removes NA data
        ## 5.  Concatenates and verifies wave metadata with NDBC metadata spreadsheets
        ## 6.  Concatenates and verifies other dual meteorological sensor metadata with the NDBC metadata spreadsheets
        ## 7.  Applied netCDF QC flags to data
        ## 8.  Filters for unique date/time data and reorders the datasets by date/time
        ## 9.  The verified stdmet with newly verified metadata are saved in an 's_buoy#_ncei_ALL_verified.Rdata' container file within buoy station specific folders

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
        drive <- "D:/Candice/"
        # drive <- "/p/work/candice/"
        
        data_dir <- paste0(drive, "projects/WaveTrends/NDBC_2024/data/")
        setwd(data_dir)
        
        # set input directories
        input_dir <- paste0(data_dir,"concat_data/ncei/")
        
        # metadata sheets
        metadata_dir <- paste0(data_dir,"NDBC_metadata_sheets/")
        meta_nc_dir <- paste0(data_dir,'raw_data/ncei/metadata/')
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
        
        ## no longer needed after 2023
        # # wind                  # sst           # ref
        # wind_12m <- 10;         sst_12m <- 0.9    # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        # wind_10m <- 10;         sst_10m <- 1.1    # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        # wind_6m <- 5;           sst_6m <- 0.8     # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        # wind_3m <- 5;           sst_3m <- 0.7     # NDBC, 2016, https://www.ndbc.noaa.gov/bht.shtml
        # wind_elb <- 5.2;        sst_elb <- 1.8    # 1985 Smith
        # wind_lnb <- 10;         sst_lnb <- 0.9    # Large navigational buoy: NDBC, 1992, ftp://ftp.library.noaa.gov/noaa_documents.lib/NWS/National_Data_Buoy_Center/technical_bulletin/June-1992_vol-18.pdf
        # wind_2_8m <- NA;        sst_2_8m <- NA    # No information, Rodney Riley, NDBC Engineer (pers. comms. 03/02/2021)
        # wind_2_6m <- NA;        sst_2_6m <- NA    # No information, Rodney Riley, NDBC Engineer (pers. comms. 03/02/2021)
        # wind_2_4m <- 3.3;       sst_2_4m <- 0.7   # NDBC 'BUOY COMPARISONS Final - with sensor heights.pdf' 
        # wind_2_3m <- 3.2;       sst_2_3m <- 1.3   # 2017 Bouchard et al., NDBC 'BUOY COMPARISONS Final - with sensor heights.pdf'
        # wind_2_1m <- 3.2;       sst_2_1m <- 1.1   # NDBC, 2021, https://www.ndbc.noaa.gov/station_page.php?station=45001
        # wind_1_8m <- 2.1;       sst_1_8m <- 0.4   # 2008 Crout et al.
        
        for(buoy in buoys){ # 
             # buoy <- buoys[1]

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
                     
                     # load nc metadata
                     meta_nc <- read.csv(paste0(meta_nc_dir, buoy, '_metadata_ALL.csv'))
                     
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
                     
                     # remove bad gps from datasets
                     stdspec <- ls(pattern = buoy)
                     print("remove bad gps from datasets")
                     for(d in stdspec){
                         # d <- stdspec[1]
                         dat <- get(d)
                         dat <- dat %>% filter(!is.na(lat))
                         dat <- dat %>% filter(!is.na(lon))
                         # ordering the df by date and selecting unique values only
                         dat <- dat[order(dat$DateTime),]
                         dat <- unique(dat)
                         # rename the rows to reflect unique data
                         row.names(dat) <- 1:nrow(dat)
                         assign(d,dat)
                     }
                     
                     # find stdmet df to update
                     stdmet_ls <- ls(pattern = "ncei_stdmet")
                     for(df in stdmet_ls){
                          print("working on stdmet")
                          # df <- stdmet_ls[1]
                          dat <- get(df)
                          dat$DateTime <- ymd_hms(dat$DateTime)
                          
                          print(paste0("working on ",df))
                          print(Sys.time())
                          
                          # removing any character NA's
                          dat[, 2:ncol(dat)][dat[, 2:ncol(dat)]=="NA"] <- NA
                          # merge metadata with NDBC spreadsheets
                          dat <- data.table(dat)
                          setkey(dat, DateTime)
                          dat <- metadata[dat, roll = "nearest"]
                          dat <- data.frame(dat)
                          
                          # merge with netcdf metadata
                          print("merging metadata")
                          meta_nc$var <- paste0(gsub('.nc','_',meta_nc$file_name),meta_nc$variable)
                          meta_nc$met_meta <- paste0(meta_nc$description,"_",meta_nc$manufacturer,"_",meta_nc$part_number,"_",meta_nc$installed_height,'_',meta_nc$units)
                          meta_nc$wave_meta <- paste0(meta_nc$description,"_",meta_nc$manufacturer,"_",meta_nc$part_number,"_",
                                                      meta_nc$units,"_",meta_nc$sampling_period,"_",meta_nc$type,"_",meta_nc$wave_processing,"_",meta_nc$software_revision)
                          for(r in 1:nrow(meta_nc)){if(!is.na(meta_nc$wave_processing[r])){meta_nc$met_meta[r]<-NA}}; rm(r)
                          for(r in 1:nrow(meta_nc)){if(is.na(meta_nc$met_meta[r])){meta_nc$met_meta[r] <- meta_nc$wave_meta[r]}}
                          meta_nc$wave_meta <- NULL
                          
                          # merge meta with datasets
                          for(r in 1:nrow(meta_nc)){
                              val <- meta_nc$var[r]
                              met <- meta_nc$met_meta[r]
                              dat[2:ncol(dat)] <- lapply(dat[2:ncol(dat)], function(x) {x[grepl(val, x, fixed = TRUE)] <- met; x})
                          }
                          rm(r, val,met)
                          
                          # check col names
                          print('check ncei col names')
                          ncei_names <- names(dat)
                          ncei_qc <- ncei_names[grepl('_qc_flag',ncei_names)]
                          ncei_qc <- gsub('_qc_flag','',ncei_qc)
                          ncei_names_diff <- setdiff(ncei_qc,ncei_names)
                          if(length(ncei_names_diff)>0){for(n in ncei_names_diff){names(dat)[names(dat) == gsub('_primary_sensor','',n)] <- n}}
                          
                          # # apply netcdf qc flags
                          # print('applying netcdf qc flags')
                          # # :flag_values = "1, 2, 3, 4, 9";
                          # # :flag_meanings = "pass, quality_not_evaluated, suspect_or_high_interest, failed, missing";
                          # ncei_names <- names(dat); ncei_names <- ncei_names[grepl('_qc_flag',ncei_names)]
                          # ncei_names <- ncei_names[!grepl('lat',ncei_names)]; ncei_names <- ncei_names[!grepl('lon',ncei_names)]
                          # for(p in ncei_names){for(r in 1:nrow(dat)){if(!is.na(dat[r,grep(p, colnames(dat))])){if(dat[r,grep(p, colnames(dat))]>3){dat[r,grep(gsub('_qc_flag','$',p), colnames(dat))]<-NA}}}}
                          # 
                          # save data
                          dfv <- paste0(df,"_verified")
                          assign(dfv,dat)
                          rm(dat,metadata, meta_nc)
                     }

                     #----------------------------------------------------------------------------------------
                     # exporting GeoCleaned datasets
                     #----------------------------------------------------------------------------------------
                     
                     print("Exporting new verified data")
                     ncei_list <- ls(pattern = "verified")
                     print(paste0("NCEI datasets :", ncei_list))
                     
                     # export to RData
                     ncei_list <- ls(pattern = buoy)
                     ncei_list <- ncei_list[!grepl('stdmet$',ncei_list)]
                     save(list = ncei_list, file = paste0(input_dir,buoy,"/s_",buoy,"_ncei_ALL_verified.RData"))
                     ncei_list <- ls(pattern = buoy)
                     rm(list = ncei_list)
                     rm(df, dfv,g,rn,stdmet_ls,ncei_list, col_ls,met_dat)
                     
                     # # Stop writing to the file
                     # sink()
                     print("test complete")
        
                     print(paste0("finishing metadata verification for buoy ",buoy))
             }else{print("no new data for this buoy")}
        }
}