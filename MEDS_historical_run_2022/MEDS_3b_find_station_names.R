MEDS_build_thredds_netcdf_4 <- function(buoys = "list of buoys"){
   
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
     drive <- "E:/"
     data_dir <- paste0(drive, "Candice/projects/WaveTrends/MEDS/data/")
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
     
     # MEDS station name
     df_station <- data.frame()
     
     #----------------------------------------------------------------------------------------
     # Creating netCDF files and add flags
     #----------------------------------------------------------------------------------------

     for(buoy in buoys){
          # buoy <- buoys[1]
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
               
               ## create buoy specific concat folder
               if (!file.exists(paste0(netCDF_year_mthdir,buoy,"/"))) {dir.create((paste0(netCDF_year_mthdir,buoy,"/")))}
               
               # read in files
               stdmet_all <- readRDS(file_list)
               
               # remove periods in column names
               names(stdmet_all) <- gsub("\\.", "", names(stdmet_all))

               #----------------------------------------------------------------------------------------
               # subset data for time periods and create netcdf files
               #----------------------------------------------------------------------------------------
               
               # find date range
               # load stdmet data
               date_year_start <- year(stdmet_all$DateTime[1])
               date_year_end <- year(stdmet_all$DateTime[nrow(stdmet_all)])
               date_range <- seq(date_year_start,date_year_end,date_range_increment)
               print(date_range)
               
               for(dateRange in date_range){
                    # dateRange <- date_range[1]
                    print(dateRange)
                    
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
                         station_id = unique(spec_all$station_id)
                         station_type = unique(spec_all$station_type)
                         station_year = dateRange
                         # build df
                         df_sta <- data.frame(buoy, station_id, station_name,station_type,station_year)
                         df_station <- rbind(df_station,df_sta)
                         rm(station_id, station_type, df_sta)
                         
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

               } # end of yearly date range loop
               rm(wave_instr)

               print(paste0("finished build thredds on buoy: ", buoy))
               rm(list = ls(pattern = paste0("s_",buoy)))
               rm(buoy, data_ls, date_year_end, date_year_start, dateRange, freq_ls, std_name,dataframe_wave_instr, spec_all,stdmet_all, station_name,wave_instr_glob)
               Sys.time()
               
          } # end of individual buoy run
          
     } # end of buoy loop
     
     # format station name df
     df_station <- unique(df_station)
     df_station$wave_instrument <- NA
     
     for(i in 1:nrow(df_station)){
          # i <- 1
          wave_instr_index <- which(dict_waveInstrument_MEDS$Wave_Instrument_Type_Codes == df_station$station_type[i])
          df_station$wave_instrument[i] <- dict_waveInstrument_MEDS$Wave_Instrument_Type_Description[wave_instr_index]
     }
     
     # export
     write.csv(df_station, file = paste0(data_dir, 'MEDS_station_names_wave_instr.csv'), row.names = FALSE)

     # format and export again
     df_station[,c('station_type', 'wave_instrument','station_year')] <- list(NULL)
     df_station <- unique(df_station)
     write.csv(df_station, file = paste0(data_dir, 'MEDS_station_names.csv'), row.names = FALSE)
     
}

              

          
 