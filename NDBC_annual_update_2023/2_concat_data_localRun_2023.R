concat_data_2 <- function(buoys = "list of buoys", start_year = "2020", data_dir = "data_dir"){
        
        start_year <- 2023  
        ##----------------------------------------------------------------------------------------
        ## concat NDBC web files and NCEI netCDF files
        ## Hall, Candice
        ##----------------------------------------------------------------------------------------
        
        ## Actions:
        ## 1. Sets data locations
        
        ## To handle the downloaded NDBC website data, this step allows for the management of: 
        ## a. Differing yearly file formats and spectral frequencies; 
        ## b. Concatenates multiple date and time columns into one field; 
        ## c. Removes redundant dates, as well as visibility and tide columns in stdmet data; 
        ## d. If necessary (not needed for NDBC data but code provision is in place), allocates spectral data 
	      ##    into appropriate 38 frequencies (old wave sensors), and 47 frequencies (new wave sensors); and
        ## e. Converts NDBC r1 and r2 values to their correct values (NDBC stored data are scaled by 100 to 
	      ##    reduce storage requirements, so data are multiplied by 0.01).
     
        ## To handle the NCEI data, this step: 
        ## a. Selects only relevant data for concatenation; 
        ## b. Concatenates stdmet data to match NDBC website data nomenclature; 
        ## c. Applies NDBC netCDF QC flags to the extracted data; 
        ## d. Converts air, water and dew point temperatures from Kelvin to degree Celsius to match NDBC data; 
        ## e. Removes zero ('0') wind gust values when no wind speed is present (data that weren't flagged in file). 
        ## f. Pre-2023 needs: To handle the erroneous netCDF spectral frequency data, this part of the code steps through each 
	      ##    row of spectral data and attempt to match the available spectral frequency data to the appropriate 
	      ##    38 frequencies (old wave sensors) or 47 frequencies (new wave sensors). 
        ## g. Removes redundant netCDF data points that are ~5-10 seconds apart.

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # library(NCmisc)
        # list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)
        # load libraries (local run)
        library(lubridate)
        library(plyr)
        library(dplyr)
        library(data.table)
        library(naniar)
        library(qpcR)
        library(stats)
        library(utils)
        
        # # load libraries (HPC run)
        # library(lubridate, lib="/p/home/candice/Rlibs/")
        # library(plyr, lib="/p/home/candice/Rlibs/")
        # library(crayon, lib="/p/home/candice/Rlibs/") # dplyr
        # library(pillar, lib="/p/home/candice/Rlibs/") # dplyr
        # library(dplyr, lib="/p/home/candice/Rlibs/")
        # library(data.table, lib="/p/home/candice/Rlibs/")
        # library(naniar, lib="/p/home/candice/Rlibs/")
        # library(MASS, lib="/p/home/candice/Rlibs/") # qpcR
        # library(minpack.lm, lib="/p/home/candice/Rlibs/") # qpcR
        # library(xfun, lib="/p/home/candice/Rlibs/") # qpcR
        # library(knitr, lib="/p/home/candice/Rlibs/") # qpcR
        # library(rgl, lib="/p/home/candice/Rlibs/") # qpcR
        # library(DEoptimR, lib="/p/home/candice/Rlibs/") # qpcR
        # library(robustbase, lib="/p/home/candice/Rlibs/") # qpcR
        # library(Matrix, lib="/p/home/candice/Rlibs/") # qpcR
        # library(qpcR, lib="/p/home/candice/Rlibs/")
        # library(stats, lib="/p/home/candice/Rlibs/")
        # library(utils, lib="/p/home/candice/Rlibs/")
     
     
        ##----------------------------------------------------------------------------------------
        ## set paths
        ##----------------------------------------------------------------------------------------
        drive <- "D:/Candice/"
        # drive <- "/p/work/candice/"
        data_dir <- paste0(drive, "projects/WaveTrends/NDBC_2024/data/")
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
        
        ##----------------------------------------------------------------------------------------
        ## set buoy stations for stand-alone use 
        ##----------------------------------------------------------------------------------------
        
        list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
        list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
        list_ndbc_buoy <- as.character(list_ndbc$station)
        buoys <- buoy_ls <-list_ndbc_buoy
        print(buoys)
        rm(list_ndbc)

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        
        ## ndbc web data files

        #----------------------------------------------------------------------------------------

        # looping through the listed buoy ID's
        for (buoy in buoys){
                # buoy <- buoys[1]
                print(paste0("Starting on ndbc... ",buoy))

                #----------------------------------------------------------------------------------------

                # selecting files to concatenate, then concatenating
                file_list <- list.files(path = paste0(unzip_ndbc_dir,buoy), pattern = ".txt", full.names = TRUE)

                if(length(file_list)>0){

                        # # start writing to an output file
                        # sink(paste0(data_dir,"2_ndbc_concat_",buoy,"_",Sys.Date(),".txt"))

                        print(paste0("Starting on ndbc... ",buoy))
                        print(file_list)

                        ## create buoy specific concat folder
                        if (!file.exists(paste0(ndbc_dir,buoy,"/"))) {dir.create((paste0(ndbc_dir,buoy,"/")))}

                        ## merging yearly and monthly datafiles of each type
                        # run loops
                        for (t in dataType_ab){
                                # t <- dataType_ab[5]
                                print(t)
                                files <- file_list[grep(pattern = paste0("/",buoy,t), file_list)]
                                print(files)

                                # loop for each data type using concat_ndbc.R script
                                if(t == "h"){
                                        source(paste0(data_dir,"concat_ndbc_2023.R"))
                                        dataset <- concat_ndbc_2023(files=files, start_year=start_year, t = "h", buoy = buoy)

                                        # remove columns not required
                                        library(dplyr)
                                        dataset <- dplyr::select(dataset,-"VIS",-"TIDE")
                                        dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset), ]
                                        # remove files with no date
                                        dataset <- dataset[!is.na(dataset$DateTime),]
                                        # remove any duplicate rows
                                        dataset <- unique(dataset)
                                        # rename datasets
                                        if("WDIR" %in% names(dataset)){dataset <- dplyr::rename(dataset, wind_direction=WDIR)}else{dataset$wind_direction <- NA;print("no WDIR present, adding...")}
                                        if("WSPD" %in% names(dataset)){dataset <- dplyr::rename(dataset, wind_speed=WSPD)}else{dataset$wind_speed <- NA;print("no WSPD present, adding...")}
                                        if("GST"  %in% names(dataset)){dataset <- dplyr::rename(dataset, wind_gust=GST)}else{dataset$wind_gust <- NA;print("no GST present, adding...")}
                                        if("WVHT" %in% names(dataset)){dataset <- dplyr::rename(dataset, significant_wave_height=WVHT)}else{dataset$significant_wave_height <- NA;print("no WVHT present, adding...")}
                                        if("DPD"  %in% names(dataset)){dataset <- dplyr::rename(dataset, dominant_wave_period=DPD)}else{dataset$dominant_wave_period <- NA;print("no DPD present, adding...")}
                                        if("APD"  %in% names(dataset)){dataset <- dplyr::rename(dataset, average_wave_period=APD)}else{dataset$average_wave_period <- NA;print("no APD present, adding...")}
                                        if("MWD"  %in% names(dataset)){dataset <- dplyr::rename(dataset, mean_wave_direction=MWD)}else{dataset$mean_wave_direction <- NA;print("no MWD present, adding...")}
                                        if("PRES" %in% names(dataset)){dataset <- dplyr::rename(dataset, air_pressure_at_sea_level=PRES)}else{dataset$air_pressure_at_sea_level <- NA;print("no PRES present, adding...")}
                                        if("ATMP" %in% names(dataset)){dataset <- dplyr::rename(dataset, air_temperature=ATMP)}else{dataset$air_temperature <- NA;print("no ATMP present, adding...")}
                                        if("WTMP" %in% names(dataset)){dataset <- dplyr::rename(dataset, sea_surface_temperature=WTMP)}else{dataset$sea_surface_temperature <- NA;print("no WTMP present, adding...")}
                                        if("DEWP" %in% names(dataset)){dataset <- dplyr::rename(dataset, dew_point_temperature=DEWP)}else{dataset$dew_point_temperature <- NA;print("no DEWP present, adding...")}
                                        # formatting for missing data columns
                                        if("wind_direction" %in% names(dataset)){print("data passed test")}else{dataset$wind_direction <- as.numeric(NA); print(paste0("empty wind_direction column added to NDBC stdmet for ", buoy))}
                                        if("wind_speed" %in% names(dataset)){print("data passed test")}else{dataset$wind_speed <- as.numeric(NA); print(paste0("empty wind_speed column added to NDBC stdmet for ", buoy))}
                                        if("wind_gust" %in% names(dataset)){print("data passed test")}else{dataset$wind_gust <- as.numeric(NA); print(paste0("empty wind_gust column added to NDBC stdmet for ", buoy))}
                                        if("significant_wave_height" %in% names(dataset)){print("data passed test")}else{dataset$significant_wave_height <- as.numeric(NA); print(paste0("empty significant_wave_height column added to NDBC stdmet for ", buoy))}
                                        if("dominant_wave_period" %in% names(dataset)){print("data passed test")}else{dataset$dominant_wave_period <- as.numeric(NA); print(paste0("empty dominant_period column added to NDBC stdmet for ", buoy))}
                                        if("average_wave_period" %in% names(dataset)){print("data passed test")}else{dataset$average_wave_period <- as.numeric(NA); print(paste0("empty average_period column added to NDBC stdmet for ", buoy))}
                                        if("mean_wave_direction" %in% names(dataset)){print("data passed test")}else{dataset$mean_wave_direction <- as.numeric(NA); print(paste0("empty mean_wave_direction column added to NDBC stdmet for ", buoy))}
                                        if("air_pressure_at_sea_level" %in% names(dataset)){print("data passed test")}else{dataset$air_pressure_at_sea_level <- as.numeric(NA); print(paste0("empty air_pressure_at_sea_level column added to NDBC stdmet for ", buoy))}
                                        if("air_temperature" %in% names(dataset)){print("data passed test")}else{dataset$air_temperature <- as.numeric(NA); print(paste0("empty air_temperature column added to NDBC stdmet for ", buoy))}
                                        if("sea_surface_temperature" %in% names(dataset)){print("data passed test")}else{dataset$sea_surface_temperature <- as.numeric(NA); print(paste0("empty sea_surface_temperature column added to NDBC stdmet for ", buoy))}
                                        if("dew_point_temperature" %in% names(dataset)){print("data passed test")}else{dataset$dew_point_temperature <- as.numeric(NA); print(paste0("empty dew_point_temperature column added to NDBC stdmet for ", buoy))}

                                        # reset order of df
                                        dataCols <- c("DateTime", "wind_direction", "wind_speed", "wind_gust", "significant_wave_height", "dominant_wave_period",
                                                      "average_wave_period", "mean_wave_direction", "air_pressure_at_sea_level", "air_temperature", "sea_surface_temperature",
                                                      "dew_point_temperature" , "air_pressure_at_sea_level")
                                        dataset <- dplyr::select(dataset, all_of(dataCols))
                                        rm(dataCols)

                                        # assign name and export file
                                        file_name <- "_stdmet"
                                        data_name <- paste0("s_",buoy,"_ndbc",file_name)
                                        # print(d_list)
                                        assign(data_name, dataset)

                                        # save and export dataset
                                        year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                        write.csv(dataset, paste0(ndbc_dir, buoy, "/",data_name,"_",year_range, ".csv"), row.names=FALSE)
                                        # saveRDS(dataset, file=paste0(ndbc_dir, buoy, "/",data_name,"_",year_range, ".rds"))
                                        rm(dataset, year_range)
                                }else{
                                        source(paste0(data_dir,"concat_ndbc_2023.R"))
                                        concat_ndbc_2023(files=files, start_year=start_year, t = t, buoy = buoy)

                                        # assign name and export file, fix NDBC r1 and r2 * 0.01 (https://www.ndbc.noaa.gov/faq/measdes.shtml)
                                        if(t=="w"){file_name <- "_c11"}
                                        if(t=="d"){file_name <- "_alpha1"}
                                        if(t=="i"){file_name <- "_alpha2"}
                                        if(t=="j"){file_name <- "_r1"; dataset_spec[,2:ncol(dataset_spec)] <- dataset_spec[,2:ncol(dataset_spec)]*0.01}
                                        if(t=="k"){file_name <- "_r2"; dataset_spec[,2:ncol(dataset_spec)] <- dataset_spec[,2:ncol(dataset_spec)]*0.01}

                                        # save and export dataset
                                        dataset <- dataset_spec
                                        year_range1 <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                        data_name <- paste0("s_",buoy,"_ndbc",file_name)
                                        assign(data_name, dataset)
                                        data_name <- paste0("s_",buoy,"_ndbc",file_name,"_freq_",year_range1)
                                        write.csv(dataset, paste0(ndbc_dir,buoy,'/',data_name,"_",nrow(dataset_spec),"_records.csv"), row.names=FALSE)
                                        rm(dataset,dataset_spec)
                                } # end of individual dataType_ab loop

                        } # end of dataType_ab loop

                        # Save multiple objects
                        datasets <- ls(pattern = paste0(buoy,"_ndbc"))
                        print(datasets)
                        save(list = datasets, file = paste0(ndbc_dir, buoy, "/s_",buoy,"_ndbc_ALL.RData"))
                        # housekeeping
                        print(paste0("Completed data concat for ",buoy))
                        # rm(list = ls())
                        rm_ls <- ls(pattern = buoy)
                        rm(list = rm_ls)
                        rm(d, data_list,data_name, file_name, first_line, records, rm_ls, count_cols, t, datasets)

                        print(paste0("Finished ndbc... ",buoy))

                        # # Stop writing to the file
                        # sink()

                }else{print("no new ndbc data for this buoy")}

                #----------------------------------------------------------------------------------------

                print(paste0("Finished ndbc... ",buoy))
        }

        #----------------------------------------------------------------------------------------
        
        ## ncei ndbc netCDF data files

        #----------------------------------------------------------------------------------------
        
        data_types_stdmet <- c('lat','lon',  
                               'air_pressure_at_sea_level_primary_sensor','air_pressure_at_sea_level_secondary_sensor',                                   
                               'air_pressure_primary_sensor','air_pressure_secondary_sensor',                                                     
                               'air_temperature_primary_sensor','air_temperature_secondary_sensor',
                               'dew_point_temperature_primary_sensor','dew_point_temperature_secondary_sensor',
                               'relative_humidity',
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
                             'rhq') # c11 = spectral energy, c11m = uncorrected spectral energy
        

        # selecting files to concatenate, then concatenating
        for(buoy in buoys){
                # buoy <- buoys[1]
                print(paste0("Starting on ncei... ",buoy))

                #----------------------------------------------------------------------------------------

                # selecting files to concatenate, then concatenating
                # find all files
                file_list <- list.files(path = paste0(ascii_ncei_dir,buoy), pattern = ".csv", full.names = TRUE, recursive = TRUE)

                # only execute if buoy data available
                if(length(file_list)>0){

                        # # start writing to an output file
                        # sink(paste0(data_dir,"2_ncei_concat_",buoy,"_",Sys.Date(),".txt"))

                        print(paste0("Starting on ncei... ",buoy))
                        print(file_list)

                        ## create buoy specific concat folder
                        if (!file.exists(paste0(ncei_dir,buoy,"/"))) {dir.create((paste0(ncei_dir,buoy,"/")))}

                        #----------------------------------------------------------------------------------------
                        # met data
                        #----------------------------------------------------------------------------------------

                        print(paste0("Starting stdmet data concat for ",buoy))

                        source(paste0(data_dir,"concat_ncei_2023.R"))
                        print("initializing concat_ncei for stdmet...")
                        concat_ncei_2023(file_list, start_year, "stdmet", drive, buoy)
                        print("finished concat_ncei for stdmet...")

                        library(dplyr)
                        # remove any duplicate rows
                        dataset <- unique(dataset)
                        # rename the rows to reflect unique data
                        row.names(dataset) <- 1:nrow(dataset)

                        # renaming for different data nomenclature
                        df_names <- names(dataset)
                        colnames(dataset) = gsub("sea_surface_wave_mean_period_from_variance_spectral_density_second_frequency_moment", "average_wave_period", colnames(dataset))
                        colnames(dataset) = gsub("sea_surface_wave_period_at_variance_spectral_density_maximum", "dominant_wave_period", colnames(dataset))
                        colnames(dataset) = gsub("sea_surface_wave_significant_height", "significant_wave_height", colnames(dataset))
                        colnames(dataset) = gsub("sea_surface_wave_from_direction", "mean_wave_direction", colnames(dataset))

                        # reset order of df
                        df_names <- names(dataset)
                        dataCols <- c("DateTime","lat","lat_qc_flag","lon","lon_qc_flag")
                        dataCols <- c(dataCols,grep("wind_direction",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("wind_speed",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("wind_gust",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("significant_wave_height",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("dominant_wave_period",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("average_wave_period",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("mean_wave_direction",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("air_pressure_at_sea_level",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("air_temperature",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("sea_surface_temperature",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("dew_point_temperature",df_names, value = TRUE))
                        dataCols <- c(dataCols,grep("air_pressure_at_sea_level",df_names, value = TRUE))
                        # reorder columns
                        dataset <- dplyr::select(dataset, all_of(dataCols))
                        rm(dataCols)

                        # save and export
                        year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                        write.csv(dataset, paste0(ncei_dir,buoy,"/s_",buoy, "_ncei_h_stdmet_",year_range,".csv"), row.names=FALSE)
                        # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/s_",buoy,"_ncei_stdmet_",year_range,".rds"))
                        data_name <- paste0("s_",buoy,"_ncei_stdmet")
                        assign(data_name, dataset)

                        gps_positions <- dplyr::select(dataset, "DateTime", "lat", "lon")
                        rm(dataset, year_range)
                        print(paste0("Completed stdmet data concat for ",buoy))

                        #----------------------------------------------------------------------------------------
                        # spectral data
                        #----------------------------------------------------------------------------------------
                        print(paste0("Starting spectral data concat for ",buoy))

                        for (t in data_types_spec){
                                # t <- data_types_spec[1]
                                print(t)
                                # run concat_ncei function code
                                source(paste0(data_dir,"concat_ncei_2023.R"))
                                print("initializing concat_ncei for spec...")
                                concat_ncei_2023(file_list, start_year, "spec", drive, buoy)
                                print("finishing concat_ncei for spec...")
                                if(t == "sea_surface_wave_variance_spectral_density"){j<-"c11"}
                                else if(t == "sea_surface_wave_variance_spectral_density_uncorrected"){j<-"c11m"}
                                else if(t == "sea_surface_wave_spectral_mean_direction"){j<-"alpha1"}
                                else if(t == "sea_surface_wave_spectral_principal_direction"){j<-"alpha2"}
                                else{j<-t}
                                data_name <- paste0('s_',buoy,'_ncei_',j)
                                year_range <- paste0(year(dataset_spec$DateTime[1]), '_', year(dataset_spec$DateTime[length(dataset_spec)]))
                                assign(data_name,dataset_spec)
                                
                                write.csv(dataset_spec, paste0(ncei_dir,buoy,"/",data_name,"_",year_range,"_",nrow(dataset_spec),"_records.csv"), row.names=FALSE)
                                # saveRDS(dataset_spec, file = paste0(ncei_dir,buoy,"/",data_name,"_",year_range,".rds"))
                                rm(dataset_spec)
                        }

                        # Save multiple objects
                        datasets <- ls(pattern = paste0(buoy,"_ncei"))
                        # return(datasets)
                        print(datasets)

                        save(list = datasets, file = paste0(ncei_dir, buoy, "/s_",buoy,"_ncei_ALL.RData"))
                        # housekeeping
                        rm(data_name,datasets, file_list, t,dataset)
                        print(paste0("Completed spectral data concat for ",buoy))
                        print(paste0("Completed data concat for ",buoy))

                        rm_ls <- ls(pattern = buoy)
                        rm(list = rm_ls)
                        rm(gps_positions)

                        print(paste0("Finished ncei buoy... ",buoy))

                        # # Stop writing to the file
                        # sink()
                        #----------------------------------------------------------------------------------------

                        print(paste0("Finished ncei buoy... ",buoy))
                        
                }else{print(paste0("No new ncei data for buoy ",buoy))}
                
        }
}

