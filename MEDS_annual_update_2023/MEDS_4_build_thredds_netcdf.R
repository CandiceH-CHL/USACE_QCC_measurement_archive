MEDS_build_thredds_netcdf_4 <- function(buoys = "list of buoys", start_year = 'start year'){
     
     start_year <- 2022
   
     ##----------------------------------------------------------------------------------------
     ## script to create netCDF data from MEDS web files 
     ## Hall, Candice 
     ##----------------------------------------------------------------------------------------
    
     ## netCDF structure is cf compliant: 
     ## https://www.nodc.noaa.gov/data/formats/netcdf/v2.0/point.cdl
     ## cf compliant standard names: https://cfconventions.org/Data/cf-standard-names/78/build/cf-standard-name-table.html
     
     ## Actions:
     ## 1.  Sets data locations
     ## 2.  This step creates monthly netCDF MEDS data files that contains the best available standard met. (stdmet) and spectral data and metadata 
     ##     variables that were selected in the step above.
     ## 3.  The 'MEDS_build_thredds_netcdf_3.R' function requires a buoy list and a data directory. The script needs access to  
     ##     the buoy bulk, meta and spec_formatted folders in the /data/raw_data directory.
     ## 4.  NetCDF files are subset to create individual station files for each month and year (year_month)
     ## 5.  NetCDF structure is cf compliant as per netCDF NCEI point reference: https://www.nodc.noaa.gov/data/formats/netcdf/v2.0/point.cdl
     ## 6.  NetCDF standard names are cf compliant as per: https://cfconventions.org/Data/cf-standard-names/78/build/cf-standard-name-table.html
     ## 7.  Flag conventions are consistent with:  IGOSS quality codes: https://www.iode.org/index.php?option=com_oe&task=viewDocumentRecord&docID=865
     
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
     
     ##----------------------------------------------------------------------------------------
     ## set paths
     ##----------------------------------------------------------------------------------------
     drive <- "D:/"
     data_dir <- paste0(drive, "Candice/projects/WaveTrends/MEDS_2024/data/")
     setwd(data_dir)
     
     raw_dir <- paste0(data_dir,"raw_data/")
     bulk_met_dir <- paste0(data_dir,"concat_data/")
     
     ## set new output directories for raw and zipped datasets
     # MEDS 
     spec_format_dir <- paste0(raw_dir,"spec_formatted/")
     
     # set new output directories for datasets
     if (!file.exists(paste0(data_dir,"netCDF_year_mth/"))){dir.create((paste0(data_dir,"netCDF_year_mth/")))}
     # best_netCDF_dir <- paste0(data_dir,"best_netCDF_north_east/")
     netCDF_year_mthdir <- paste0(data_dir,"netCDF_year_mth/")

     # function to perform capitalization
     simpleCap <- function(x) {s <- strsplit(x, " ")[[1]]; paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=" ")}
     lowerFirst <- function(x) {substr(x, 1, 1) <- tolower(substr(x, 1, 1)); x}

     ##----------------------------------------------------------------------------------------
     ## set presets
     ##----------------------------------------------------------------------------------------
     
     # setting flags
     # https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html#Co-Quad
     # IGOSS quality codes: https://www.iode.org/index.php?option=com_oe&task=viewDocumentRecord&docID=865
     # 0 Blank - No quality control (QC) has been performed
     # 1 Good - QC has been performed: record appears correct
     # 3 Doubtful - QC has been performed: record appears doubtful
     # 4 Erroneous - QC has been performed: record appears erroneous
     # 5 Changes - The record has been changed as a result of QC
     # 6 Acceptable - QC has been performed: record seems inconsistent with other records
     # 7 Off Position - There is a problem with the buoy position or mooring. Data may still be useful.
     # 8 Reserved
     # 9 Reserved - indicates missing elements

     flag_descrip = "Good [1] = QC has been performed: record appears correct; Doubtful [3] = QC has been performed: record appears doubtful;
     Erroneous [4] = QC has been performed: record appears erroneous; Changes [5] = The record has been changed as a result of QC;
     Acceptable [6] = QC has been performed: record seems inconsistent with other records; Off Position [7] = There is a problem with the buoy position or mooring. Data may still be useful."
     flag_good = 1; flag_good_desc = "Good - QC has been performed: record appears correct"
     flag_questionableSuspect = 3; flag_questionableSuspect_desc = "Doubtful - QC has been performed: record appears doubtful"
     flag_bad = 4; flag_bad_desc = "Erroneous - QC has been performed: record appears erroneous"
     flag_changed = 5; flag_changed_desc = "Changes - The record has been changed as a result of QC"
     flag_changed = 6; flag_changed_desc = "Acceptable - QC has been performed: record seems inconsistent with other records"
     flag_changed = 7; flag_changed_desc = "Off Position - There is a problem with the buoy position or mooring. Data may still be useful."

     # set precision
     miss_value <- -999.99
     variable_prec_df <- 'double'
     variable_prec_flg <- 'integer'
     variable_prec_metadata <- 'char'
     date_range_increment <- 1
     
     # load buoy stations
     list_MEDS <- read.csv(paste0(data_dir,"MEDS_MasterBuoyList.csv"),header = TRUE)
     list_MEDS_buoy <- as.character(list_MEDS$STID)
     buoys <- list_MEDS_buoy
     rm(list_MEDS)
     print(buoys)
     
     # load wave instrument type dictionary
     dict_waveInstrument_MEDS <- read.csv(paste0(data_dir,"MEDS_wave_instrument_type_codes.csv"),header = TRUE)
     dict_waveInstrument_MEDS$Wave_Instrument_Type_Codes <- as.character(dict_waveInstrument_MEDS$Wave_Instrument_Type_Codes)
     
     # load station information dictionary
     dict_station_info_MEDS <- read.csv(paste0(data_dir,"MEDS_station_names_wave_instr.csv"),header = TRUE)

     #----------------------------------------------------------------------------------------
     # Creating netCDF files and add flags
     #----------------------------------------------------------------------------------------

     for(buoy in buoys){
          # buoy <- buoys[2]
          Sys.time()

          print(paste0("Starting on MEDS... ",buoy))
          
          #----------------------------------------------------------------------------------------
          
          # selecting files
          file_list <- list.files(path = paste0(bulk_met_dir,buoy), pattern = "stdmet_QC_met.rds", full.names = TRUE)

          if(length(file_list)>0){
               
               # # start writing to an output file
               # sink(paste0(data_dir,"2_MEDS_concat_",buoy,"_",Sys.Date(),".txt"))
               print(paste0("starting build thredds on MEDS buoy: ", buoy))
               print(file_list)
               
               # read in files
               stdmet_all <- readRDS(file_list)
               
               # remove periods in column names
               names(stdmet_all) <- gsub("\\.", "", names(stdmet_all))
               
               # replace missing data flag (10000) with NA
               is.na(stdmet_all[,2:ncol(stdmet_all)]) <- stdmet_all[,2:ncol(stdmet_all)] == 10000
               
               # perform some obvious qc on the data
               if("WDIR" %in% names(stdmet_all)){is.na(stdmet_all$WDIR) <- stdmet_all$WDIR > 360; is.na(stdmet_all$WDIR) <- stdmet_all$WDIR < 0}
               if("WDIR1" %in% names(stdmet_all)){is.na(stdmet_all$WDIR1) <- stdmet_all$WDIR1 > 360; is.na(stdmet_all$WDIR1) <- stdmet_all$WDIR1 < 0}
               for(r in 1:nrow(stdmet_all)){
                    # r <- 168451
                    if("WDIR" %in% names(stdmet_all)){if(!is.na(stdmet_all$WDIR[r]) & !is.na(stdmet_all$WSPD[r])){if(stdmet_all$WDIR[r] == 0 & stdmet_all$WSPD[r] == 0){stdmet_all$WDIR[r] <- NA; stdmet_all$WSPD[r] <- NA; stdmet_all$GSPD[r] <- NA}}}
                    if("WDIR1" %in% names(stdmet_all)){if(!is.na(stdmet_all$WDIR1[r]) & !is.na(stdmet_all$WSPD1[r]) ){if(stdmet_all$WDIR1[r] == 0 & stdmet_all$WSPD1[r] == 0){stdmet_all$WDIR1[r] <- NA; stdmet_all$WSPD1[r] <- NA; stdmet_all$GSPD1[r] <- NA}}}
               }
               
               # Latitude/Longitude: Positions are presented in decimal degrees with latitudes north of the equator represented as positive values and 
               # longitudes west of the Prime Meridian represented as positive values.
               # https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html
               
               # longitudes converted to negative values in the west.
               stdmet_all$longitude <- stdmet_all$longitude*-1

               #----------------------------------------------------------------------------------------
               # subset data for time periods and create netcdf files
               #----------------------------------------------------------------------------------------
               
               # find date range
               # load stdmet data
               date_year_start <- year(stdmet_all$DateTime[1])
               date_year_end <- year(stdmet_all$DateTime[nrow(stdmet_all)])
               date_range <- seq(date_year_start,date_year_end,date_range_increment)
               date_range <- date_range[date_range>=start_year]
               print(date_range)
               
               if(length(date_range)>0){
                    
                    for(dateRange in date_range){
                         # dateRange <- date_range[1]
                         print(dateRange)
                         
                         ## create buoy specific concat folder
                         if (!file.exists(paste0(netCDF_year_mthdir,buoy,"/"))) {dir.create((paste0(netCDF_year_mthdir,buoy,"/")))}
                         
                         # isolate backup station info, in case raw data is incomplete
                         station_information <- dict_station_info_MEDS[dict_station_info_MEDS$buoy == buoy, ]
                         station_information <- station_information[station_information$station_year == dateRange, ]
                         if(dim(station_information)[1]==0){stop("no station info for this year")}
                         
                         #----------------------------------------------------------------------------------------
                         # load relevant spec data for dateRange and apply qc flags
                         #----------------------------------------------------------------------------------------
                         # selecting files
                         spec_format_dir <- paste0(raw_dir,"spec_formatted/")
                         file_list <- list.files(path = paste0(spec_format_dir,buoy), pattern = paste0(dateRange,"_spec_rows_format.rds"), full.names = TRUE)
                         file_list
                         
                         if(length(file_list)>0){
                              # read in files
                              spec_all <- readRDS(file_list[1])
                              spec_all[,5:ncol(spec_all)] <- lapply(spec_all[,5:ncol(spec_all)], function(x) as.numeric(as.character(x)))
                              
                              # isolate buoy description
                              station_name = unique(spec_all$station_name)
                              
                              #----------------------------------------------------------------------------------------
                              # qc data
                              # https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html#Co-Quad
                              # 0 Blank - No quality control (QC) has been performed
                              # 1 Good - QC has been performed: record appears correct
                              # 3 Doubtful - QC has been performed: record appears doubtful
                              # 4 Erroneous - QC has been performed: record appears erroneous
                              # 5 Changes - The record has been changed as a result of QC
                              # 6 Acceptable - QC has been performed: record seems inconsistent with other records
                              # 7 Off Position - There is a problem with the buoy position or mooring. Data may still be useful.
                              # 8 Reserved
                              # 9 Reserved - indicates missing elements
                              #----------------------------------------------------------------------------------------
                              if(dim(spec_all)[1]>0){
                                   spec_all <- spec_all[spec_all$Quality_Code != 3, ]
                                   spec_all <- spec_all[spec_all$Quality_Code != 4, ]
                                   spec_all <- spec_all[spec_all$Quality_Code != 7, ]
                              }
                              
                              if(dim(spec_all)[1]>0){
                                   # convert wave instrument type code to description
                                   spec_all$station_type <- as.character(spec_all$station_type)
                                   wave_instr <- unique(spec_all$station_type)
                                   
                                   if(length(wave_instr)==1){
                                        wave_instr_index <- which(dict_waveInstrument_MEDS$Wave_Instrument_Type_Codes == wave_instr)
                                        spec_all$wave_instrument <- dict_waveInstrument_MEDS$Wave_Instrument_Type_Description[wave_instr_index]
                                        wave_instr <- dict_waveInstrument_MEDS$Wave_Instrument_Type_Description[wave_instr_index]
                                   }else{
                                        spec_all$wave_instrument <- NA
                                        wave_instr <- "multiple wave instrument types, please see wave instrument type variable"
                                        for(r in 1:nrow(spec_all)){
                                             # r <- 1
                                             wave_instr_index <- which(dict_waveInstrument_MEDS$Wave_Instrument_Type_Codes == spec_all$station_type[r])
                                             spec_all$wave_instrument[r] <- dict_waveInstrument_MEDS$Wave_Instrument_Type_Description[wave_instr_index]
                                        }
                                   }
                                   dataframe_wave_instr <- dplyr::select(spec_all, DateTime, wave_instrument)
                              }else{rm(spec_all)}
                         }
                         
                         #----------------------------------------------------------------------------------------
                         # isolate relevant data for year_month
                         #----------------------------------------------------------------------------------------
                         # subset per month
                         monthRange <- 1:12
                         
                         for(m in monthRange){
                              # m <- 12
                              print(m)
                              m = sprintf("%02d", as.numeric(m))
                              
                              #----------------------------------------------------------------------------------------
                              # load relevant stdmet and metadata for dateRange
                              #----------------------------------------------------------------------------------------
                              dat <-dplyr::filter(stdmet_all, year(DateTime) == as.numeric(dateRange))
                              dat <-dplyr::filter(dat, month(DateTime)== as.numeric(m))
                              new_name <- paste0("data_",as.numeric(dateRange),"_",m)
                              if(dim(dat)[1] > 0){assign(new_name, dat)}
                              if(dim(dat)[1] == 0){rm(dat)}
                              rm(new_name)
                              
                              if(exists("dat")){
                                   dat_date <- dat[,1:2]; dat_date$STN_ID <- NULL
                                   rm(dat)
                                   #----------------------------------------------------------------------------------------
                                   # load relevant spec data for dateRange
                                   #----------------------------------------------------------------------------------------
                                   if(exists("spec_all")){
                                        dat <-dplyr::filter(spec_all, year(DateTime) == as.numeric(dateRange))
                                        dat <-dplyr::filter(dat, month(DateTime)== as.numeric(m))
                                        
                                        # reformat spec data to isolate frequencies and remove unnecessary columns
                                        if(dim(dat)[1]>0){
                                             
                                             # match dates between stdmet and spectral datasets
                                             dat <- dplyr::left_join(dat_date,dat, by = "DateTime")
                                             rm(dat_date)
                                             
                                             # removing unnecessary columns
                                             dat[,c('length_recording', 'additional_parameters', 'Number_Wave_Heights', 'Number_Wave_Periods', 'Sampling_Frequency')] <- list(NULL)
                                             
                                             #isolate number of spectral frequency bands
                                             freq_num <- unique(dat$Number_Spectral_Estimates)
                                             if(NA %in% freq_num){freq_num <- freq_num[!is.na(freq_num)]}
                                             if(length(freq_num)==0){freq_num <- "-51"}
                                             
                                             if(freq_num != "-51"){
                                                  if(length(freq_num) == 1){
                                                       # isolate freq
                                                       library(dplyr)
                                                       df_freq <- dplyr::select(dat, matches("freq"))
                                                       # remove empty columns
                                                       emptycols <- sapply(df_freq, function (k) all(is.na(k)))
                                                       df_freq <- df_freq[!emptycols]; rm(emptycols)
                                                       # finding unique rows
                                                       if(dim(df_freq)[1]>1){df_freq <- as.data.frame(apply(df_freq, 2, as.numeric))}
                                                       df_freq <- df_freq[!apply(is.na(df_freq) | df_freq == "" | df_freq == 0, 1, all),]
                                                       df_freq_unique <- unique(df_freq)
                                                       # remove erroneous data
                                                       df_freq_unique <- df_freq_unique[df_freq_unique$freq_1 < 10,]
                                                       
                                                       if(dim(df_freq_unique)[1]==1){
                                                            # isolate densities
                                                            df_c11 <- dplyr::select(dat, matches("density"))
                                                            colnames(df_c11) <- df_freq[1,]
                                                            # add to main df
                                                            df_main <- dat[,1:grep("Number_Spectral_Estimates", colnames(dat))]
                                                            df_main <- cbind(df_main, df_c11)
                                                            # assign new name
                                                            new_name <- paste0("spec_",as.numeric(dateRange),"_",m)
                                                            assign(new_name, df_main)
                                                            rm(df_c11, new_name,df_main)
                                                            unique_freqs <- str_pad(df_freq_unique, width=6, side="right", pad="0")
                                                       }
                                                       if(dim(df_freq_unique)[1]>1){
                                                            # isolate densities
                                                            df_c11 <- dplyr::select(dat, DateTime,matches("density"))
                                                            # remove empty columns
                                                            emptycols <- sapply(df_c11, function (k) all(is.na(k)))
                                                            df_c11 <- df_c11[!emptycols]; rm(emptycols)
                                                            # isolate frequencies
                                                            if(sd(df_freq_unique[,ncol(df_freq_unique)])!=0){
                                                                 best_freq <- data.frame(unlist(table(df_freq[,ncol(df_freq)])),stringsAsFactors = FALSE)
                                                                 max_freq <- as.character(best_freq$Var1[which(best_freq$Freq == max(best_freq$Freq))])
                                                                 library(stringr)
                                                                 max_freq <- str_pad(max_freq, width=6, side="right", pad="0")
                                                            }
                                                            df_freq <- unique(dplyr::filter(df_freq, df_freq[,ncol(df_freq)]==as.numeric(max_freq)))
                                                            library(stringr)
                                                            unique_freqs <- str_pad(df_freq, width=6, side="right", pad="0")
                                                            rm(best_freq,max_freq)
                                                            
                                                            if(dim(df_freq)[1]==1){
                                                                 # rename cols
                                                                 colnames(df_c11) <- c("DateTime",unique_freqs)
                                                                 # add to main df
                                                                 df_main <- dat[,1:grep("Number_Spectral_Estimates", colnames(dat))]
                                                                 df_main <- dplyr::full_join(df_main, df_c11, by = "DateTime")
                                                                 # assign name
                                                                 new_name <- paste0("spec_",as.numeric(dateRange),"_",m)
                                                                 assign(new_name, df_main)
                                                                 rm(df_main,df_c11, new_name)
                                                            }else{stop('too many unique spectral frequencies...')}
                                                       }
                                                  }else{
                                                       library(dplyr)
                                                       df_freq <- dplyr::select(dat, DateTime,Number_Spectral_Estimates,matches("freq"))
                                                       # remove empty columns
                                                       emptycols <- sapply(df_freq, function (k) all(is.na(k)))
                                                       df_freq <- df_freq[!emptycols]; rm(emptycols)
                                                       
                                                       # subset for each frequency
                                                       for(n in freq_num){
                                                            # n <- freq_num[1]
                                                            df_spec <- dplyr::filter(df_freq,Number_Spectral_Estimates == n)
                                                            # convert to numeric and subset for freq
                                                            if(dim(df_spec)[1]>1){df_spec[2:ncol(df_spec)] <- as.data.frame(apply(df_spec[2:ncol(df_spec)], 2, as.numeric))}
                                                            df_freq_unique <- df_spec[,2:ncol(df_spec)]
                                                            # finding unique rows
                                                            df_freq_unique <- df_freq_unique[rowSums(is.na(df_freq_unique)) != ncol(df_freq_unique),]
                                                            df_freq_unique <- unique(df_freq_unique)
                                                            # remove empty columns
                                                            emptycols <- sapply(df_freq_unique, function (k) all(is.na(k)))
                                                            df_freq_unique <- df_freq_unique[!emptycols]; rm(emptycols)
                                                            df_freq_unique$Number_Spectral_Estimates <- NULL
                                                            # remove erroneous data
                                                            df_freq_unique <- df_freq_unique[df_freq_unique$freq_1 < 10,]
                                                            
                                                            # isolate data
                                                            if(dim(df_freq_unique)[1]==1){
                                                                 # isolate dates
                                                                 freq_date <- df_spec[,1:2]
                                                                 # isolate densities
                                                                 df_c11 <- dplyr::select(dat, DateTime,matches("density"))
                                                                 # subset c11
                                                                 df_c11 <- dplyr::left_join(freq_date, df_c11, by = "DateTime")
                                                                 # remove empty columns
                                                                 emptycols <- sapply(df_c11, function (k) all(is.na(k)))
                                                                 df_c11 <- df_c11[!emptycols]
                                                                 # rename cols
                                                                 unique_freqs <- str_pad(df_freq_unique[1,], width=6, side="right", pad="0")
                                                                 colnames(df_c11) <- c("DateTime","Number_Spectral_Estimates",unique_freqs)
                                                                 df_c11$Number_Spectral_Estimates <- NULL
                                                                 # add to main df
                                                                 df_main <- dat[,1:grep("Number_Spectral_Estimates", colnames(dat))]
                                                                 df_main <- dplyr::full_join(df_main, df_c11, by = "DateTime")
                                                                 # remove extra spec numbers
                                                                 df_main <- df_main %>% mutate(Number_Spectral_Estimates = if_else(Number_Spectral_Estimates == n, n, NULL))
                                                                 # assign name
                                                                 new_name <- paste0("spec_",as.numeric(dateRange),"_",m,"_freq_",n)
                                                                 assign(new_name, df_main)
                                                                 rm(df_main,df_c11, freq_date,emptycols, new_name)
                                                            }
                                                            if(dim(df_freq_unique)[1]>1){
                                                                 # isolate dates
                                                                 freq_date <- df_spec[,1:2]
                                                                 # isolate densities
                                                                 df_c11 <- dplyr::select(dat, DateTime,matches("density"))
                                                                 # subset c11
                                                                 df_c11 <- dplyr::left_join(freq_date, df_c11, by = "DateTime")
                                                                 # remove empty columns
                                                                 emptycols <- sapply(df_c11, function (k) all(is.na(k)))
                                                                 df_c11 <- df_c11[!emptycols]; rm(emptycols)
                                                                 # isolate frequencies
                                                                 if(sd(df_freq_unique[,ncol(df_freq_unique)])!=0){
                                                                      best_freq <- data.frame(unlist(table(df_spec[,ncol(df_spec)])),stringsAsFactors = FALSE)
                                                                      max_freq <- as.character(best_freq$Var1[which(best_freq$Freq == max(best_freq$Freq))])
                                                                      library(stringr)
                                                                      max_freq <- str_pad(max_freq, width=6, side="right", pad="0")
                                                                 }
                                                                 df_freq_unique <- dplyr::filter(df_freq_unique, df_freq_unique[,ncol(df_freq_unique)]==as.numeric(max_freq))
                                                                 library(stringr)
                                                                 unique_freqs <- str_pad(df_freq_unique, width=6, side="right", pad="0")
                                                                 rm(best_freq,max_freq)
                                                                 
                                                                 if(dim(df_freq_unique)[1]==1){
                                                                      # rename cols
                                                                      colnames(df_c11) <- c("DateTime","Number_Spectral_Estimates",unique_freqs)
                                                                      df_c11$Number_Spectral_Estimates <- NULL
                                                                      # add to main df
                                                                      df_main <- dat[,1:grep("Number_Spectral_Estimates", colnames(dat))]
                                                                      df_main <- dplyr::full_join(df_main, df_c11, by = "DateTime")
                                                                      # remove extra spec numbers
                                                                      df_main <- df_main %>% mutate(Number_Spectral_Estimates = if_else(Number_Spectral_Estimates == n, n, NULL))
                                                                      # assign name
                                                                      new_name <- paste0("spec_",as.numeric(dateRange),"_",m,"_freq_",n)
                                                                      assign(new_name, df_main)
                                                                      rm(df_main,df_c11, freq_date,new_name)
                                                                 }else{stop('too many unique spectral frequencies...')}
                                                            }
                                                       }
                                                  }
                                             }
                                        }
                                        dat_spec <- dat
                                        rm(df_freq, df_spec,dat,n)
                                   }
                                   
                                   #----------------------------------------------------------------------------------------
                                   
                                   # subset relevant datasets to populate netCDF file
                                   dat_ls <- ls(pattern = paste0("_",dateRange,"_"))
                                   # delete list if no stdmet data present
                                   if(sum(str_count(dat_ls, "data_"))==0){
                                        rm(list = ls(pattern = paste0(dateRange,"_",m)))
                                        dat_ls <- vector()
                                   }
                                   print(dat_ls)
                                   
                                   #----------------------------------------------------------------------------------------
                                   # merge spec metadata with stdmet data
                                   #----------------------------------------------------------------------------------------
                                   if(length(dat_ls)>1){
                                        # load data
                                        stdmet <- get(dat_ls[grep("data_", dat_ls)])
                                        spectral <- dat_spec
                                        
                                        # match stdmet and spec DateTimes
                                        temp_date <- dplyr::select(stdmet, DateTime, STN_ID); temp_date$STN_ID <- NULL
                                        spectral <- left_join(temp_date,spectral, by = "DateTime")
                                        rm(temp_date)
                                        # join variables
                                        temp_spec <- dplyr::select(spectral,c("DateTime","station_type", "station_name"))
                                        stdmet <- dplyr::left_join(stdmet,temp_spec, by = "DateTime")
                                        # save
                                        new_name <- dat_ls[grep("data_", dat_ls)]; if(dim(stdmet)[1] > 0){assign(new_name, stdmet)}
                                        rm(stdmet, spectral, temp_spec)
                                   }
                                   # remove unnecessary spec columns
                                   spec_ls <- dat_ls[grep("spec_", dat_ls)]
                                   if(length(spec_ls)>0){
                                        for(d in spec_ls){
                                             # d <- spec_ls[2]
                                             df <- get(d)
                                             # remove duplicated fields and save
                                             df[,c('station_id', 'station_type', 'station_name', 'Latitude', 'Longitude', 'depth','Quality_Code')] <- list(NULL)
                                             if(dim(df)[1] > 0){assign(d, df)}
                                             rm(df)
                                        }
                                        rm(d)
                                   }else{rm(spec_ls)}
                                   
                                   #----------------------------------------------------------------------------------------
                                   # load relevant datasets for dateRange
                                   #----------------------------------------------------------------------------------------
                                   if(length(dat_ls)>0){
                                        
                                        # Create and write a netCDF file
                                        
                                        # set new output directories for datasets
                                        if (!file.exists(paste0(netCDF_year_mthdir,buoy,"/"))){dir.create((paste0(netCDF_year_mthdir,buoy,"/")))}
                                        if (!file.exists(paste0(netCDF_year_mthdir,buoy,"/"))){dir.create((paste0(netCDF_year_mthdir,buoy,"/")))}
                                        if (!file.exists(paste0(netCDF_year_mthdir,buoy,"/",dateRange,"/"))){dir.create((paste0(netCDF_year_mthdir,buoy,"/",dateRange,"/")))}
                                        year_mth_dir <- paste0(netCDF_year_mthdir,buoy,"/",dateRange,"/")
                                        buoy_dir <- paste0(netCDF_year_mthdir,buoy,"/")
                                        
                                        ncname <- paste0("C",buoy,"_MEDS_", as.numeric(dateRange),"_",m)
                                        print(ncname)
                                        ncfname <- paste0(year_mth_dir, ncname, ".nc")
                                        print(ncfname)
                                        miss_values <- miss_value
                                        
                                        # load datasets
                                        dat <- get(dat_ls[dat_ls %like% 'data_'])
                                        if(exists("spec_ls")){
                                             if(length(spec_ls)==1){
                                                  dat_spec <- get(dat_ls[dat_ls %like% 'spec_'])
                                                  # isolate wave instrument for global attributes
                                                  wave_instr_glob <- wave_instr
                                             }else{
                                                  for(d in spec_ls){
                                                       # d <- spec_ls[1]
                                                       dat_spec <- get(d)
                                                       freq_count <- unlist(strsplit(d,"_")); freq_count <- freq_count[length(freq_count)]
                                                       new_name <- paste0("dat_spec_",freq_count)
                                                       assign(new_name,dat_spec)
                                                       rm(dat_spec,freq_count)
                                                  }
                                                  # isolate wave instrument for global attributes
                                                  wave_instr_glob <- wave_instr
                                                  rm(d)
                                             }
                                        }
                                        
                                        # isolate depth, lat and lon for global attributes
                                        # wave_instr_glob <- wave_instr
                                        depth_glob <- unique(dat$depth, na.rm = TRUE)
                                        lat_max_glob <- max(dat$latitude, na.rm = TRUE)
                                        lat_min_glob <- min(dat$latitude, na.rm = TRUE)
                                        lon_max_glob <- max(dat$longitude, na.rm = TRUE)
                                        lon_min_glob <- min(dat$longitude, na.rm = TRUE)
                                        
                                        # subset time variable for spec matching
                                        dat_time <- dplyr::select(dat, DateTime, latitude,longitude)
                                        dat_start_date <- as.character(dat_time$DateTime[1])
                                        if(nchar(dat_start_date)<11){dat_start_date <- paste0(dat_start_date," 00:00:00")}
                                        dat_end_date <- as.character(dat_time$DateTime[nrow(dat_time)])
                                        print(paste0(dat_start_date," - ",dat_end_date))
                                        
                                        # add wave instrument information to stdmet data
                                        if(exists("dataframe_wave_instr")){dat <- dplyr::left_join(dat,dataframe_wave_instr, by = "DateTime")}
                                        
                                        # remove empty columns to ensure no empty variables are captured in netcdf file
                                        emptycols <- sapply(dat, function (k) all(is.na(k)))
                                        dat <- dat[!emptycols]; rm(emptycols)
                                        if('hull_ID' %in% colnames(dat)==FALSE){dat$hull_ID <- "no information available"}
                                        if('wave_instrument' %in% colnames(dat)==FALSE){dat$wave_instrument <- "no information available"}
                                        
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
                                        var_ls <- var_ls[!(var_ls %in% c("DateTime","STN_ID"))]
                                        
                                        #----------------------------------------------------------------------------------------
                                        # prep data to netcdf file
                                        #----------------------------------------------------------------------------------------
                                        # collate time from the various old and new freq
                                        if(exists("spec_ls")){
                                             if(length(spec_ls)>0){
                                                  if(length(spec_ls)==1){
                                                       # select wave frequency bands for spec data
                                                       wave_wpm <- as.numeric(unique_freqs)
                                                       wave_freq_length <- length(wave_wpm)
                                                       dimwave_wpm <- ncdim_def("waveFrequency","Hz",as.double(wave_wpm))
                                                       # select variables
                                                       var_ls <- c(var_ls,"spec")
                                                  }else{
                                                       for(d in spec_ls){
                                                            # d <- spec_ls[1]
                                                            # select wave frequency bands for spec data
                                                            df <- get(d); df[,c('DateTime', 'Number_Spectral_Estimates')] <- list(NULL)
                                                            freq_count <- unlist(strsplit(d,"_")); freq_count <- freq_count[length(freq_count)]
                                                            wave_wpm <- as.numeric(names(df)); new_name <- paste0("wave_wpm_",freq_count); assign(new_name,wave_wpm)
                                                            wave_freq_length <- length(wave_wpm); new_name <- paste0("wave_freq_length_",freq_count); assign(new_name,wave_freq_length)
                                                            dimwave_wpm <- ncdim_def(paste0("waveFrequency_",as.character(length(wave_wpm))),"Hz",as.double(wave_wpm))
                                                            new_name <- paste0("dimwave_wpm_",freq_count); assign(new_name,dimwave_wpm)
                                                            # select variables
                                                            var_ls <- c(var_ls,paste0("spec_",freq_count))
                                                            rm(wave_wpm, wave_freq_length,dimwave_wpm,new_name, freq_count)
                                                       }
                                                       rm(d,df)
                                                  }
                                             }else(print(paste0("no freq data for ", dateRange, "_", m)))
                                        }
                                        
                                        # create variable dimensions for the netcdf file
                                        for(var_df in var_ls){
                                             # var_df <- var_ls[23]
                                             # print(var_df)
                                             
                                             miss_values <- miss_value
                                             variable_prec <- variable_prec_df
                                             
                                             ## set variable constants
                                             # https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html
                                             # common variables
                                             if(var_df == "latitude") {var_name = "latitude"; var_units = 'degrees_north'; var_longname='latitude'}#; variable_prec= variable_prec_df}
                                             if(var_df == "longitude") {var_name = "longitude"; var_units = 'degrees_east'; var_longname='longitude'}#; variable_prec= variable_prec_df}
                                             if(var_df == "depth"){var_name = "depth"; var_units = 'm'; var_longname = "sea_floor_depth_below_sea_level"; variable_prec = variable_prec_flg}
                                             if(var_df == "Q_FLAG") {var_name = "quality_flag"; var_units = ''; var_longname='quality_flag'; variable_prec= variable_prec_flg}
                                             if(var_df == "hull_ID"){var_name = "hull_id"; variable_prec = variable_prec_metadata; var_units = ''; var_longname = "MEDS Hull ID"}
                                             if(var_df == "station_type"){var_name = "station_type"; variable_prec = variable_prec_metadata; var_units = ''; var_longname = "MEDS Wave Instrument Type Codes"}
                                             if(var_df == "station_name"){var_name = "station_name"; variable_prec = variable_prec_metadata; var_units = ''; var_longname = "MEDS Station Name"}
                                             if(var_df == "wave_instrument"){var_name = "wave_instrument"; variable_prec = variable_prec_metadata; var_units = ''; var_longname = "MEDS Wave Instrument Type"}
                                             
                                             # stdmet variables units and precision
                                             if(any(grepl("VCAR",var_df))|any(grepl("VWH",var_df))|any(grepl("VCMX",var_df))|
                                                any(grepl("SHE1",var_df))|any(grepl("SWHT",var_df))|any(grepl("VAV1",var_df))|
                                                any(grepl("VMNL",var_df))|any(grepl("VMX",var_df))|any(grepl("VST1",var_df))){var_units = 'm'}
                                             if(any(grepl("VTP",var_df))|any(grepl("SEP1",var_df))|any(grepl("SWPR",var_df))|
                                                any(grepl("VTD1",var_df))|any(grepl("VTZA",var_df))|any(grepl("VZA1",var_df))){var_units = 's'}
                                             if(any(grepl("WDIR",var_df))){var_units = 'degT'; variable_prec = variable_prec_flg}
                                             if(any(grepl("WSPD",var_df))|any(grepl("WSS",var_df))|any(grepl("GSPD",var_df))){var_units <- 'm/s'}
                                             if(any(grepl("DRYT",var_df))|any(grepl("SSTP",var_df))|any(grepl("SST1",var_df))|any(grepl("HAT",var_df))){var_units <- 'degrees_celsius'}
                                             if(any(grepl("SLEV",var_df))){var_units = 'not listed'}
                                             if(any(grepl("ATMS",var_df))){var_units = 'mbar'}
                                             
                                             # spec variables
                                             if(any(grepl("spec",var_df))){
                                                  if(length(spec_ls)==1){var_units <- 'm2/Hz'; var_name="waveEnergyDensity"; variable_prec=variable_prec_df;var_longname = "sea_surface_wave_variance_spectral_density"
                                                  }else{freq_count <- unlist(strsplit(var_df,"_"))[2]; var_units <- 'm2/Hz'; var_name=paste0("waveEnergyDensity_",freq_count,"Frequencies"); variable_prec=variable_prec_df;var_longname = "sea_surface_wave_variance_spectral_density"; rm(freq_count)
                                                  }
                                             }
                                             
                                             # stdmet variables var_name and var_longname
                                             # wave heights
                                             if(var_df == "VCAR"){var_name = "waveHs"; var_longname='characteristic_significant_wave_height(calculated by MEDS)'}
                                             if(var_df == "VCAR1"){var_name = "waveHs1"; var_longname='characteristic_significant_wave_height(calculated by MEDS)'}
                                             if(var_df == "VWH"){var_name = "waveHsBuoy"; var_longname='characteristic_significant_wave_height(reported by the buoy)'}
                                             if(var_df == "VWH1"){var_name = "waveHsBuoy1"; var_longname='characteristic_significant_wave_height(reported by the buoy)'}
                                             
                                             if(var_df == "VCMX"){var_name = "waveHMaxZeroCrossing"; var_longname='maximum_zero_crossing_wave_height(reported by the buoy)'}
                                             if(var_df == "VCMX1"){var_name = "waveHMaxZeroCrossing1"; var_longname='maximum_zero_crossing_wave_height(reported by the buoy)'}
                                             # if(var_df == "SHE1"){var_name = "waveSeaHeightWES"; var_longname='WES_sea_height'}
                                             # if(var_df == "SWHT"){var_name = "waveSwellHeight"; var_longname='swell_height'}
                                             # if(var_df == "VAV1"){var_name = "waveAverageHeave"; var_longname='average_heave_from_the_non-synoptic_part_of_WRIPS_buoy_data'}
                                             # if(var_df == "VMNL"){var_name = "waveDepthDeepestTrough"; var_longname='depth_of_the_deepest_trough'}
                                             # if(var_df == "VMXL"){var_name = "waveHeightHighestCrest"; var_longname='height_of_the_highest_crest'}
                                             # if(var_df == "VMX1"){var_name = "waveHMaxZeroCrossingWRIPS"; var_longname='maximum_zero_crossing_wave_height_from_the_non-synoptic_part_of_WRIPS_buoy_data'}
                                             # if(var_df == "VST1"){var_name = "waveMaxSteepness"; var_longname='maximum_wave_steepness'}
                                             
                                             # wave periods
                                             if(var_df == "VTPK"){var_name = "waveTp"; var_longname='wave_spectrum_peak_period(calculated by MEDS)'}
                                             if(var_df == "VTPK1"){var_name = "waveTp1"; var_longname='wave_spectrum_peak_period(calculated by MEDS)'}
                                             if(var_df == "VTP"){var_name = "waveTpBuoy"; var_longname='wave_spectrum_peak_period(reported by the buoy)'}
                                             if(var_df == "VTP1"){var_name = "waveTpBuoy1"; var_longname='wave_spectrum_peak_period(reported by the buoy)'}
                                             # if(var_df == "SEP1"){var_name = "waveSeaPeriodWES"; var_longname='WES_sea_period'}
                                             # if(var_df == "SWPR"){var_name = "waveSwellPeriod"; var_longname='swell_period'}
                                             # if(var_df == "VTD1"){var_name = "waveTpDominant"; var_longname='dominant_period'}
                                             # if(var_df == "VTZA"){var_name = "waveTmZeroCrossing"; var_longname='average_zero_crossing_wave_period'}
                                             # if(var_df == "VZA1"){var_name = "waveTmmZeroCrossingWRIPS"; var_longname='Average_zero_crossing_period_from_the_non-synoptic_part_of_WRIPS_buoy_data'}
                                             
                                             # meteorological & oceanographic Codes
                                             if(var_df == "WDIR"){var_name = "windDirection"; var_longname='direction_from_which_the_wind_is_blowing'}
                                             if(var_df == "WDIR1"){var_name = "windDirection1"; var_longname='direction_from_which_the_wind_is_blowing'}
                                             if(var_df == "WSPD"){var_name = "windSpeed"; var_longname='horizontal_wind_speed'}
                                             if(var_df == "WSPD1"){var_name = "windSpeed1"; var_longname='horizontal_wind_speed'}
                                             if(var_df == "WSS"){var_name = "windSpeedScalar"; var_longname='horizontal_scalar_wind_speed'}
                                             if(var_df == "WSS1"){var_name = "windSpeedScalar1"; var_longname='horizontal_scalar_wind_speed'}
                                             if(var_df == "GSPD"){var_name = "windGust"; var_longname='gust_wind_speed'}
                                             if(var_df == "GSPD1"){var_name = "windGust1"; var_longname='gust_wind_speed'}
                                             if(var_df == "ATMS"){var_name = "surfaceAirPressure"; var_longname='atmospheric_pressure_at_sea_level'}
                                             if(var_df == "ATMS1"){var_name = "surfaceAirPressure1"; var_longname='atmospheric_pressure_at_sea_level'}
                                             if(var_df == "DRYT"){var_name = "surfaceAirTemperature"; var_longname='dry_bulb_temperature'}
                                             if(var_df == "DRYT1"){var_name = "surfaceAirTemperature1"; var_longname='dry_bulb_temperature'}
                                             if(var_df == "SSTP"){var_name = "surfaceSeaTemperature"; var_longname='sea_surface_temperature'}
                                             if(var_df == "SSTP1"){var_name = "surfaceSeaTemperature1"; var_longname='sea_surface_temperature'}
                                             if(var_df == "SLEV"){var_name = "surfaceObservedSeaLevel"; var_longname='observed_sea_level'}
                                             if(var_df == "SLEV1"){var_name = "surfaceObservedSeaLevel1"; var_longname='observed_sea_level'}
                                             if(var_df == "SST1"){var_name = "surfaceSeaTemperatureWRIPS"; var_longname='average_sea_temperature_from_the_non-synoptic_part_of_WRIPS_buoy_data'}
                                             if(var_df == "SST11"){var_name = "surfaceSeaTemperatureWRIPS1"; var_longname='average_sea_temperature_from_the_non-synoptic_part_of_WRIPS_buoy_data'}
                                             if(var_df == "HAT"){var_name = "surfaceSeaTemperatureHA"; var_longname='water_temperature_from_high_accuracy_temperature sensor'}
                                             if(var_df == "HAT1"){var_name = "surfaceSeaTemperatureHA1"; var_longname='water_temperature_from_high_accuracy_temperature sensor'}
                                             
                                             # create matrix
                                             if(any(grepl("spec",var_df))){
                                                  if(length(spec_ls)>1){dat_spec <- get(paste0("dat_",var_df))}
                                                  # format matrix df by removing unnecessary columns
                                                  dat_spec[,c('DateTime','Number_Spectral_Estimates')] <- list(NULL)
                                                  # create matrix
                                                  mat_df <- matrix(t(dat_spec), nrow=nrow(dat_spec),ncol=ncol(dat_spec))
                                                  df_name <- paste0("df_",var_name)
                                                  # assign(df_name, mat_df)
                                                  assign(df_name, mat_df)
                                                  rm(df_name, mat_df,dat_spec)
                                             }else{
                                                  # create df
                                                  df_df <- dplyr::select(dat,all_of(var_df))
                                                  # rename df
                                                  df_name <- paste0("df_",var_name)
                                                  assign(df_name, df_df)
                                                  rm(df_df, df_name)
                                             }
                                             
                                             # Add dimensions and variables which accompany the dimensions (avoided by create_dimvar = FALSE)
                                             if(var_df == "hull_ID"|var_df == "station_type"|var_df == "station_name"|var_df == "wave_instrument"){
                                                  vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime,dimnchar), prec=variable_prec)
                                             }else if(var_df == "latitude" | var_df == "longitude"){
                                                  vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime), missval=miss_values, prec=variable_prec)
                                             }else if(grepl("spec", var_df)){
                                                  if(length(spec_ls)==1){vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimwave_wpm,dimTime), missval=miss_values, prec=variable_prec)
                                                  }else{count_wpm <- unlist(strsplit(var_df,"_"))[2]; vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(get(paste0("dimwave_wpm_",count_wpm)),dimTime), missval=miss_values, prec=variable_prec)
                                                  }
                                             }else{
                                                  vari_df <- ncvar_def(name=var_name, units=var_units, longname=var_longname, dim=list(dimTime), missval=miss_values)
                                             }
                                             df_name <- paste0("var_",var_name)
                                             assign(df_name, vari_df)
                                             rm(df_name, vari_df,var_longname, var_units, var_name, count_wpm)
                                        }
                                        rm(var_df)
                                        rm(list = ls(pattern = "dim_"))
                                        if(exists("dat_spec")){rm(dat_spec)}
                                        rm(list = ls(pattern = "dat_spec_"))
                                        if(exists("wave_freq_length")){rm(wave_freq_length)}
                                        
                                        #----------------------------------------------------------------------------------------
                                        # build netcdf file
                                        #----------------------------------------------------------------------------------------
                                        rm(var_flag, var_df, var_list, var_units)
                                        
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
                                        rm(vars)
                                        
                                        # This variable was implicitly created by the dimension, so just specifying it by name
                                        ncatt_put(con, 'time', 'standard_name', 'time')
                                        ncatt_put(con, var_longitude, 'axis', 'X')
                                        ncatt_put(con, var_latitude, 'axis', 'Y')
                                        ncatt_put(con, 'time', 'axis', 'T')
                                        
                                        # Add data and some extra attributes
                                        for(var_df in var_list){
                                             # var_df <- var_list[12]
                                             # print(var_df)
                                             std_name <- gsub("var_","",var_df)
                                             # print(std_name)
                                             website_blurb <- '(MEDS wave data source, 2019-09-26). https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/data-donnees/index-eng.asp'
                                             metadata_blurb <- '(MEDS format descriptions, 2020-06-30). https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html'
                                             # set standard names
                                             if(std_name == "latitude") {var_standard = "latitude"; desc_name="latitude"}
                                             if(std_name == "longitude") {var_standard = "longitude"; desc_name="longitude"}
                                             if(std_name == "hull_id"){var_standard = "hull_id"; desc_name=website_blurb}
                                             if(std_name == "depth"){var_standard = "sea_floor_depth_below_mean_sea_level"; desc_name=website_blurb}
                                             if(std_name == "quality_flag"){var_standard = "quality_flag"; desc_name='(MEDS QC flag descriptions, 2020-06-30). https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html'}
                                             if(std_name == "station_type"){var_standard = "platform_type"; desc_name=website_blurb}
                                             if(std_name == "station_name"){var_standard = "platform_name"; desc_name=website_blurb}
                                             if(std_name == "wave_instrument"){var_standard = "instrument_type"; desc_name=website_blurb}
                                             
                                             # wave heights
                                             if(grepl("waveHs", std_name)) {var_standard = 'sea_surface_significant_wave_height'; desc_name=paste0('characteristic_significant_wave_height-calculated by MEDS ',website_blurb)}
                                             if(grepl("waveHsBuoy", std_name)) {var_standard = 'sea_surface_significant_wave_height'; desc_name=paste0('characteristic_significant_wave_height-reported by the buoy ',website_blurb)}
                                             if(grepl("waveHMaxZeroCrossing", std_name)){var_standard = "sea_surface_wave_zero_upcrossing_height"; desc_name=paste0('maximum_zero_crossing_wave_height-reported by the buoy ',website_blurb)}
                                             # if(std_name == "waveSeaHeightWES"){var_standard = "sea_surface_primary_wind_wave_significant_height"; desc_name=paste0('WES_sea_height ',website_blurb)}
                                             # if(std_name == "waveSwellHeight"){var_standard = "sea_surface_swell_wave_significant_height"; desc_name=paste0('swell_height ',website_blurb)}
                                             # if(std_name == "waveAverageHeave"){var_standard = "platform_heave"; desc_name=paste0('average_heave_from_the_non-synoptic_part_of_WRIPS_buoy_data ',website_blurb)}
                                             # if(std_name == "waveDepthDeepestTrough"){var_standard = "sea_surface_wave_maximum_trough_depth"; desc_name=paste0('depth_of_the_deepest_trough ',website_blurb)}
                                             # if(std_name == "waveHeightHighestCrest"){var_standard = "sea_surface_wave_maximum_crest_height"; desc_name=paste0('height_of_the_highest_crest ',website_blurb)}
                                             # if(std_name == "waveHMaxZeroCrossingWRIPS"){var_standard = "sea_surface_wave_zero_upcrossing_height"; desc_name=paste0('maximum_zero_crossing_wave_height_from_the_non-synoptic_part_of_WRIPS_buoy_data ',website_blurb)}
                                             # if(std_name == "waveMaxSteepness"){var_standard = "sea_surface_wave_maximum_steepness"; desc_name=paste0('maximum_wave_steepness ',website_blurb)}
                                             
                                             # wave periods
                                             if(grepl("waveTp", std_name)) {var_standard = 'sea_surface_wave_period_at_variance_spectral_density_maximum'; desc_name=paste0('wave_spectrum_peak_period-calculated by MEDS ',website_blurb)}
                                             if(grepl("waveTpBuoy", std_name)) {var_standard = 'sea_surface_wave_period_at_variance_spectral_density_maximum'; desc_name=paste0('wave_spectrum_peak_period-reported by the buoy ',website_blurb)}
                                             if(var_df == "waveSeaPeriodWES"){var_standard = "sea_surface_wind_wave_period"; desc_name=paste0('WES_sea_period ',website_blurb)}
                                             if(var_df == "waveSwellPeriod"){var_standard = "sea_surface_swell_wave_period"; desc_name=paste0('swell_period ',website_blurb)}
                                             if(var_df == "waveTpDominant"){var_standard = "sea_surface_wave_period_at_variance_spectral_density_maximum"; desc_name=paste0('dominant_period ',website_blurb)}
                                             # if(var_df == "waveTmZeroCrossing"){var_standard = "sea_surface_wave_mean_period"; desc_name=paste0('average_zero_crossing_wave_period ',website_blurb)}
                                             # if(var_df == "waveTmmZeroCrossingWRIPS"){var_standard = "sea_surface_wave_mean_period"; desc_name=paste0('Average_zero_crossing_period_from_the_non-synoptic_part_of_WRIPS_buoy_data ',website_blurb)}
                                             # set freq standard names
                                             if(grepl("EnergyDensity", std_name)){var_standard = 'sea_surface_wave_variance_spectral_density'; desc_name="Energy density, displacement in m2/Hz, for each frequency bin"}
                                             
                                             # stdmet variables var_name and var_longname
                                             # meteorological & oceanographic Codes
                                             if(grepl("windDirection", std_name)){var_standard = "wind_from_direction"; desc_name=paste0('direction_from_which_the_wind_is_blowing ',website_blurb)}
                                             if(grepl("windSpeed", std_name)){var_standard = "wind_speed"; desc_name=paste0('horizontal_wind_speed ',website_blurb)}
                                             if(grepl("windSpeedScalar", std_name)){var_standard = "wind_speed"; desc_name=paste0('horizontal_scalar_wind_speed ',website_blurb)}
                                             if(grepl("windGust", std_name)){var_standard = "wind_speed_of_gust"; desc_name=paste0('gust_wind_speed ',website_blurb)}
                                             if(grepl("surfaceAirPressure", std_name)){var_standard = "air_pressure"; desc_name=paste0('atmospheric_pressure_at_sea_level ',website_blurb)}
                                             if(grepl("surfaceAirTemperature", std_name)){var_standard = "air_temperature"; desc_name=paste0('dry_bulb_temperature ',website_blurb)}
                                             if(grepl("surfaceSeaTemperature", std_name)){var_standard = "sea_surface_temperature"; desc_name=paste0('sea_surface_temperature ',website_blurb)}
                                             if(grepl("surfaceObservedSeaLevel", std_name)){var_standard = "sea_surface_height_above_mean_sea_level"; desc_name=paste0('observed_sea_level ',website_blurb)}
                                             if(grepl("surfaceSeaTemperatureWRIPS", std_name)){var_standard = "sea_surface_temperature"; desc_name=paste0('average_sea_temperature_from_the_non-synoptic_part_of_WRIPS_buoy_data ',website_blurb)}
                                             if(grepl("surfaceAirTemperatureHA", std_name)){var_standard = "sea_surface_temperature"; desc_name=paste0('water_temperature_from_high_accuracy_temperature sensor ',website_blurb)}
                                             
                                             # add standard_name
                                             ncatt_put(con, gsub("var_","",var_df), 'standard_name', var_standard)
                                             rm(var_standard)
                                             
                                             # add description name if present (i.e. listed above)
                                             if(exists('desc_name')){ncatt_put(con, gsub("var_","",var_df), 'description_name', desc_name); rm(desc_name)}
                                             # add coordinates
                                             if(var_df != "var_latitude" & var_df != "var_longitude"){ncatt_put(con, gsub("var_","",var_df), 'coordinates', 'latitude longitude')}
                                             
                                             # add data values to nc file
                                             df_name <- gsub("var_", "",var_df)
                                             df <- get(paste0("df_",df_name))
                                             if(grepl("EnergyDensity", df_name)){ncvar_put(con, df_name, df)
                                             }else{ncvar_put(con, df_name, df[,1])}
                                             
                                             # house keeping
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
                                        ncatt_put(nc=con,varid=0, attname="MEDS_id", attval = as.character(paste0("C",buoy)), prec="char")
                                        ncatt_put(nc=con,varid=0, attname="institution", attval = "Department of Fisheries and Oceans Canada - Marine Environmental Data Section", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="institution_abbreviation", attval = "DFO MEDS", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="title", attval = "DFO Marine Environmental Data Section Archive", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="summary", attval = "The Marine Environmental Data Section of Fisheries and Oceans Canada manages and archives certain types of oceanographic data. These data are collected by our department and acquired through programs conducted in ocean areas near Canada. This Archive includes real-time/near real-time and historical ocean monitoring data. These Canadian wave data include Wave data from buoys.", prec="char")
                                        if(exists("station_name")){ncatt_put(nc=con,varid=0, attname="station_name", attval = station_name, prec="char")
                                        }else{ncatt_put(nc=con,varid=0, attname="station_name", attval = as.character(station_information$station_name), prec="char")}
                                        ncatt_put(nc=con,varid=0, attname="history", attval = 'This MEDS Archive includes real-time/near real-time and historical ocean monitoring data. Canadian wave data include Wave data from buoy observations, and are archived on their website (https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/data-donnees/index-eng.asp). These data have been reformatted for use within U.S. Army Corps of Engineers (USACE) Engineers and Research Development Center (ERDC) products. USACE sponsored a Coastal Ocean Data Systems (CODS) National Coastal Wave Climate (NCWC) work unit that developed an in-house USACE measurement archive (called the USACE QCC measurement archive). Of note is that integral wave data are imported directly from the MEDS data sources, and are not corrected for calculation errors that occurred during MEDS processing from spectral wave data. The self-described, USACE QCC measurement archive data is stored in netCDF format alongside the USACE Coastal and Hydraulic Laboratory (CHL) Thredds Wave Information Study (WIS) long-term hindcast, accessible to both the USACE and the public.', prec="char")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_lat_max", attval = lat_max_glob, prec="double")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_lat_min", attval = lat_min_glob, prec="double")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_lat_units", attval = "degrees", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_lon_max", attval = lon_max_glob, prec="double")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_lon_min", attval = lon_min_glob, prec="double")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_lon_units", attval = "degrees", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_vertical_units", attval = "meters above mean sea level", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="geospatial_vertical_datum", attval = "urn:x-noaa:def:datum:noaa::MSL", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="depth", attval = depth_glob, prec="double")
                                        if(exists("wave_instr_glob")){ncatt_put(nc=con,varid=0, attname="wave_instrument", attval = wave_instr_glob, prec="char")
                                        }else{ncatt_put(nc=con,varid=0, attname="wave_instrument", attval = as.character(station_information$wave_instrument), prec="char")}
                                        ncatt_put(nc=con,varid=0, attname="qc_codes", attval = "https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html#Par", prec="char" )
                                        ncatt_put(nc=con,varid=0, attname="geospatial_conventions", attval = "Positions are presented in decimal degrees with latitudes north of the equator represented as positive values and longitudes west of the Prime Meridian represented as negative values - converted from MEDS format", prec="char" )
                                        ncatt_put(nc=con,varid=0, attname="keywords", attval = "Atmospheric Pressure, Sea level Pressure, Atmospheric Temperature, Surface Temperature, Dewpoint Temperature, Surface Winds, Ocean Winds, Ocean Temperature, Sea Surface Temperature, Ocean Waves,  Wave Height, Wave Period, Wave Spectra", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="keywords_vocabulary", attval = "Global Change Master Directory (GCMD) Science Keywords", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="restrictions", attval = "DFO Privacy information: https://www.canada.ca/en/transparency/privacy.html", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="scientific_project", attval = "None", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="flag_Conventions", attval = "IGOSS quality codes (IOC Manuals and guides No. 01, 1st Ed., Guide to IGOSS data archives and exchange; BATHY and TESAC. https://www.iode.org/index.php?option=com_oe&task=viewDocumentRecord&docID=865", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="flag_descriptions", attval = flag_descrip, prec="char")
                                        ncatt_put(nc=con,varid=0, attname="station_types", attval = "See details of Wave Instrument Type Codes in MEDS FormatB for Non-Directional Spectral Wave Data: https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html#WI", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="citation", attval = "To cite Marine environmental data section: DFO (download year). Marine Environmental Data Section Archive, https://meds-sdmm.dfo-mpo.gc.ca, Ecosystem and Oceans Science, Department of Fisheries and Oceans Canada. Data obtained on YYYY/MM/DD (citation date).", prec="char" )
                                        ncatt_put(nc=con,varid=0, attname="distribution_statement", attval = "DFO Privacy information: https://www.canada.ca/en/transparency/privacy.html", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="time_coverage_start", attval = dat_start_date, prec="char")
                                        ncatt_put(nc=con,varid=0, attname="time_coverage_end", attval = dat_end_date, prec="char")
                                        ncatt_put(nc=con,varid=0, attname="date_created", attval = as.character(Sys.time()), prec="char")
                                        ncatt_put(nc=con,varid=0, attname="date_created", attval = as.character(Sys.time()), prec="char")
                                        ncatt_put(nc=con,varid=0, attname="processing_level", attval = "0", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="publisher_name", attval = "U.S. Army Corps of Engineers (USACE) Engineers and Research Development Center (ERDC) Coastal Ocean Data Systems (CODS) Program", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="publisher_email", attval = "candice.hall@usace.army.mil", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="creator_name", attval = "DFO Canada - Marine Environmental Data Section", prec="char")
                                        ncatt_put(nc=con,varid=0, attname="creator_url", attval = "https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/index-eng.htm", prec="char")
                                        
                                        if(as.numeric(substr(buoy, start = 1, stop = 2))==45){
                                             # "45132","45135","45136","45137","45139","45142","45143","45147","45149","45154","45159"
                                             # if(buoy == "45132"){attval1 = "183 m above mean sea level"}
                                             attval1 = "unknown meters above mean sea level"
                                             ncatt_put(nc=con,varid=0, attname="site_elevation", attval = attval1, prec="char")
                                        }else{ncatt_put(nc=con,varid=0, attname="site_elevation", attval = "sea level", prec="char")}
                                        
                                        ncatt_put(nc=con,varid=0, attname="standard_name_vocabulary", attval = "Standard Name Table (current version, V 79, 19 March 2022); https://cfconventions.org/standard-names.html", prec="char")
                                        
                                        #----------------------------------------------------------------------------------------
                                        # closing nc file
                                        #----------------------------------------------------------------------------------------
                                        nc_close(con)
                                        
                                        # # copy file for Scientific Data folder structure
                                        # file.copy(from=ncfname, to=paste0(best_netCDF_dir,buoy,"/", ncname,".nc"),
                                        #           overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
                                        rm(list = ls(pattern = paste0("data_",dateRange,"_",m)))
                                        rm(list = ls(pattern = paste0("spec_",dateRange,"_",m)))
                                        rm(list = ls(pattern = "dimwave_wpm"))
                                        rm(list = ls(pattern = "wave_freq_length"))
                                        rm(list = ls(pattern = "wave_wmp_"))
                                        
                                        rm(con,dat,dat_ls, name_var, depth_glob, lat_max_glob, lat_min_glob, lon_max_glob, lon_min_glob, ncfname, ncname,
                                           dimTime,dimnchar, dat_time, wave_instr_index, wave_wpm, freq_num, r, attval1, unique_freqs)
                                   }else{
                                        print(paste0("no spectral data for ", dateRange,"_",m))
                                        rm(m, ncname, dat_ls, dimnchar, dimwave_wpm_new, dimwave_wpm_old)
                                   }
                              }else{print(paste0("no data for ", dateRange,"_",m))}
                         } # end month range loop
                         rm(list = ls(pattern = paste0("var_")))
                         rm(list = ls(pattern = paste0("dat_")))
                         rm(list = ls(pattern = paste0("dat_spec_")))
                         rm(list = ls(pattern = paste0("spec_")))
                         
                    } # end of yearly date range loop
               }

               print(paste0("finished build thredds on buoy: ", buoy))
               rm(list = ls(pattern = paste0("s_",buoy)))
               rm(buoy, data_ls, date_year_end, date_year_start, dateRange, freq_ls, std_name,dataframe_wave_instr, stdmet_all, station_name,wave_instr_glob, wave_instr, station_information)
               Sys.time()
               
          } # end of individual buoy run
          
     } # end of buoy loop
     
}

              

          
 