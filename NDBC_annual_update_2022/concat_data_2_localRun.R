concat_data_2 <- function(buoys = "list of buoys", start_year = "2020", data_dir = "data_dir"){
        
        start_year <- 2021  
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
        ## f. To handle the erroneous netCDF spectral frequency data, this part of the code steps through each 
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
        drive <- "G:/Candice/"
        # drive <- "/p/work/candice/"
        data_dir <- paste0(drive, "projects/WaveTrends/data/")
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
                # buoy <- "41001"
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
                                # t <- "h"
                                print(t)
                                files <- file_list[grep(pattern = paste0("/",buoy,t), file_list)]
                                print(files)
                                if(t == "w"){
                                        spec_freq_available_ALL <- data.frame(matrix(NA, nrow = 0, ncol = 48))
                                        for(spec_file in files){
                                                con_1 <- file(spec_file,"r")
                                                first_line <- readLines(con_1,n=3)
                                                close(con_1)
                                                dateString <-unlist(strsplit(first_line[[3]]," "))
                                                df <- paste0(dateString[1],"/",dateString[2],"/",dateString[3])
                                                df <- paste0(df, "  ", first_line[[1]])
                                                df <- t(data.frame(unlist(strsplit(df, "  ")), stringsAsFactors = FALSE))
                                                rownames(df) <- NULL
                                                library(plyr)
                                                spec_freq_available_ALL <- qpcR:::rbind.na(spec_freq_available_ALL, df)
                                        }
                                        write.csv(spec_freq_available_ALL, paste0(unzip_ndbc_dir,buoy,"_spec_freq_available_ALL.csv"), row.names=FALSE, na = "NaN")
                                        rm(df, spec_freq_available_ALL,con_1)
                                }
                                if(t == "j"){
                                        spec_r_type_ALL <- data.frame(matrix(NA, nrow = 0, ncol = 48))
                                        for(spec_file in files){
                                                con_1 <- file(spec_file,"r")
                                                first_line <- readLines(con_1, n = 2)
                                                close(con_1)
                                                first_line <- first_line[2]
                                                dateString <-unlist(strsplit(first_line[[1]]," "))
                                                df <- paste0(dateString[1],"/",dateString[2],"/",dateString[3])
                                                df <- paste0(df, "  ", first_line[[1]])
                                                df <- t(data.frame(unlist(strsplit(df, "  ")), stringsAsFactors = FALSE))
                                                rownames(df) <- NULL
                                                # remove empty columns
                                                df <- data.frame(t(df[, colSums(df != "") != 0]), stringsAsFactors = FALSE)
                                                df$X2 <- NULL
                                                library(plyr)
                                                spec_r_type_ALL <- qpcR:::rbind.na(spec_r_type_ALL, df)
                                        }
                                        write.csv(spec_r_type_ALL, paste0(unzip_ndbc_dir,buoy,"_spec_r_type_ALL.csv"), row.names=FALSE, na = "NaN")
                                        rm(df, spec_r_type_ALL,con_1)
                                }

                                # loop for each data type using concat_ndbc.R script
                                if(t == "h"){
                                        source(paste0(data_dir,"concat_ndbc.R"))
                                        dataset <- concat_ndbc(files=files, start_year=start_year, t = "h", buoy = buoy)

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
                                        source(paste0(data_dir,"concat_ndbc.R"))
                                        concat_ndbc(files=files, start_year=start_year, t = t, buoy = buoy)
                                        data_avail <- ls(pattern = "dataset")

                                        # assign name and export file
                                        if(t=="w"){file_name <- "_c11"}
                                        if(t=="d"){file_name <- "_alpha1"}
                                        if(t=="i"){file_name <- "_alpha2"}
                                        if(t=="j"){file_name <- "_r1"}
                                        if(t=="k"){file_name <- "_r2"}

                                        # saving individual datasets
                                        data_list <- ls(pattern = "dataset_spec_")
                                        print(data_list)

                                        if(length(data_list)>0){
                                                for(d in data_list){
                                                        print(paste0("Saving individual... ",d))
                                                        dataset <- get(d)
                                                        if(dim(dataset)[1]!= 0){
                                                                # save and export dataset
                                                                year_range1 <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                                                records1 <- nrow(dataset)
                                                                if(unlist(strsplit(d,"_"))[3] != "new" & unlist(strsplit(d,"_"))[3] != "old"){
                                                                        count_cols <- ncol(dataset)
                                                                        data_name <- paste0("s_",buoy,"_ndbc",file_name,"_freq_",count_cols,"cols")

                                                                }else{
                                                                        data_name <- paste0("s_",buoy,"_ndbc",file_name,"_freq_",unlist(strsplit(d,"_"))[3])

                                                                }
                                                                # write.csv(dataset, paste0(unlist(strsplit(concat_ind_dir,"s_"))[1],data_name,"_",year_range1,"_",records1,"_records.csv"), row.names=FALSE)
                                                                # saveRDS(dataset, file = paste0(unlist(strsplit(concat_ind_dir,"s_"))[1],data_name,"_",year_range1,".rds"))
                                                                rm(year_range1, records1)
                                                        }
                                                        rm(dataset)
                                                }
                                                rm(d, data_list)
                                        }else{print(paste0("no ",t," data for ", buoy))}

                                        #----------------------------------------------------------------------------------------
                                        # reformat odd data
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "dataset_")
                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                        print(data_list)

                                        if(length(data_list)>0){
                                                for(d in data_list){
                                                        df <- get(d)
                                                        print(d)
                                                        if(dim(df)[1]!= 0){
                                                                # remove rows with no dates
                                                                df <- df[!is.na(df$DateTime), ]
                                                                # remove blank data rows
                                                                df <- df[rowSums(is.na(df)) != ncol(df)-1,]
                                                                if(dim(df)[1] > 0){
                                                                        # remove columns with zero or NA
                                                                        df <- df[, colSums(df != 0, na.rm = TRUE) > 0]
                                                                        # reorder columns numerically
                                                                        library(data.table)
                                                                        df <- setcolorder(df, c(1, order(as.numeric(names(df)[-1])) + 1))
                                                                        # ordering the df by date and selecting unique values only
                                                                        df <- df[order(df$DateTime),]
                                                                        df <- unique(df)
                                                                        # rename the rows to reflect unique data
                                                                        row.names(df) <- 1:nrow(df)
                                                                        # assign data
                                                                        assign(d,df)
                                                                }else{
                                                                        rm(list = ls()[grepl(d, ls())])
                                                                        print("dataset removed: all row NA or zero")
                                                                }
                                                        }
                                                        rm(df)
                                                }
                                        }else{print("no unusual frequency data A")}

                                        #----------------------------------------------------------------------------------------
                                        # testing data merge with old and new frequency dfs
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "dataset_")
                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                        print(data_list)

                                        if(length(data_list)>0){

                                                # dealing with multiple formats for the same frequencies
                                                if("dataset_spec_old" %in% data_avail){
                                                        print("dataset_spec_old already exists")

                                                }else{
                                                        colNames_spec_old <- c("DateTime", "0.0100","0.0200","0.0300","0.0400","0.0500","0.0600","0.0700","0.0800","0.0900","0.1000","0.1100","0.1200",
                                                                               "0.1300","0.1400","0.1500","0.1600","0.1700","0.1800","0.1900","0.2000","0.2100","0.2200",
                                                                               "0.2300","0.2400","0.2500","0.2600","0.2700","0.2800","0.2900","0.3000","0.3100","0.3200",
                                                                               "0.3300","0.3400","0.3500","0.3600","0.3700","0.3800","0.3900","0.4000")
                                                        dataset_spec_old <- data.frame(matrix(NA, nrow = 1, ncol = length(colNames_spec_old)))
                                                        colnames(dataset_spec_old) <- colNames_spec_old
                                                        dataset_spec_old$DateTime <- lubridate::ymd_hms(dataset_spec_old$DateTime)
                                                }

                                                if("dataset_spec_new" %in% data_avail){
                                                        print("dataset_spec_new already exists")
                                                }else{
                                                        colNames_spec_new <- c("DateTime", "0.0200", "0.0325", "0.0375", "0.0425", "0.0475", "0.0525",
                                                                               "0.0575", "0.0625", "0.0675", "0.0725", "0.0775", "0.0825", "0.0875", "0.0925", "0.1000", "0.1100",
                                                                               "0.1200", "0.1300", "0.1400", "0.1500", "0.1600", "0.1700", "0.1800", "0.1900", "0.2000", "0.2100",
                                                                               "0.2200", "0.2300", "0.2400", "0.2500", "0.2600", "0.2700", "0.2800", "0.2900", "0.3000", "0.3100",
                                                                               "0.3200", "0.3300", "0.3400", "0.3500", "0.3650", "0.3850", "0.4050", "0.4250", "0.4450", "0.4650",
                                                                               "0.4850")
                                                        dataset_spec_new <- data.frame(matrix(NA, nrow = 1, ncol = length(colNames_spec_new)))
                                                        colnames(dataset_spec_new) <- colNames_spec_new
                                                        dataset_spec_new$DateTime <- lubridate::ymd_hms(dataset_spec_new$DateTime)
                                                }

                                                # loop through odd frequencies to test if df can be merged with new and odd frequencies
                                                # testing data merge
                                                data_list <- ls(pattern = "dataset_")
                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                print(data_list)

                                                for (l in data_list){
                                                        print(l)
                                                        dat <- get(l)
                                                        spec_list <- ls(pattern = "dataset_spec_")
                                                        spec_list <- spec_list[spec_list %in% c("dataset_spec_new","dataset_spec_old") == TRUE]
                                                        print(spec_list)

                                                        for(spec in spec_list){
                                                                if(exists("dat")){
                                                                        spec_dat <- get(spec)
                                                                        ndbc_freq <- names(spec_dat)
                                                                        dat_names <- names(dat)
                                                                        setdiff(ndbc_freq, dat_names)
                                                                        print(unique(dat_names %in% ndbc_freq))
                                                                        # looping through data
                                                                        if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                                                                print(paste0(spec," didn't match"))
                                                                        }else{
                                                                                tryCatch({
                                                                                        library(plyr)
                                                                                        spec_dat <- rbind.fill(spec_dat,dat)
                                                                                        print(paste0("concat ",spec, " and ",l," match"))
                                                                                        assign(spec,spec_dat)
                                                                                        rm(dat,spec_dat)
                                                                                        rm(list = ls()[grepl(l, ls())])
                                                                                }, error = function(e) {
                                                                                        print("dataset doesn't match")
                                                                                })
                                                                                if(exists("spec_dat")){rm(spec_dat)}
                                                                        }
                                                                        rm(spec_dat,ndbc_freq,dat_names)
                                                                }

                                                        }
                                                        if(exists("dat")){
                                                                assign(l,dat)
                                                                rm(dat)
                                                        }
                                                }
                                        }else{print("no unusual frequency data B")}

                                        #----------------------------------------------------------------------------------------
                                        # if still present, loop through odd frequencies to attempt a merge by row
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "dataset_")
                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                        print(data_list)

                                        if(length(data_list)>0){
                                                # loop through odd frequencies to attempt a merge by row
                                                # testing data merge
                                                data_list <- ls(pattern = "dataset_")
                                                if("dataset_spec_new" %in% data_list){df_freq_new <- dataset_spec_new; rm(dataset_spec_new)}
                                                if("dataset_spec_old" %in% data_list){df_freq_old <- dataset_spec_old; rm(dataset_spec_old)}
                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                print(data_list)

                                                # try per row
                                                for (l in data_list){
                                                        print(l)
                                                        dat <- get(l)
                                                        # loop through each row and try to match to new and old frequencies
                                                        for(i in 1:nrow(dat)) {
                                                                print(i)
                                                                # Extract row and all columns
                                                                dat_row <- dat[i, ]
                                                                # remove cells with zero or NA - selects ANY NA and removes column
                                                                library(dplyr)
                                                                dat_row <- dat_row %>% select_if(~ !any(is.na(.)))
                                                                # match data rows
                                                                spec_list <- ls(pattern = "df_freq_")
                                                                print(spec_list)

                                                                for(spec in spec_list){
                                                                        if(exists("dat_row")){
                                                                                spec_dat <- get(spec)
                                                                                ndbc_freq <- names(spec_dat)
                                                                                dat_names <- names(dat_row)
                                                                                setdiff(ndbc_freq, dat_names)
                                                                                print(unique(dat_names %in% ndbc_freq))
                                                                                # looping through data
                                                                                if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                                                                        print(paste0(spec," didn't match"))
                                                                                }else{
                                                                                        tryCatch({
                                                                                                library(plyr)
                                                                                                spec_dat <- rbind.fill(spec_dat,dat_row)
                                                                                                print(paste0("concat ",spec, " and row ",i," match"))
                                                                                                assign(spec,spec_dat)
                                                                                                rm(dat_row,spec_dat)
                                                                                        }, error = function(e) {
                                                                                                print("dataset doesn't match")
                                                                                        })
                                                                                        if(exists("spec_dat")){rm(spec_dat)}
                                                                                }
                                                                        }

                                                                }
                                                                if(exists("dat_row")){
                                                                        count <- length(spec_list)+1
                                                                        df_freq <- data.frame(matrix(NA, nrow = 0, ncol = dim(dat_row)[2]))
                                                                        df_freq<-rbind(df_freq, dat_row)
                                                                        new_name <- paste0("df_freq_",count)
                                                                        assign(new_name, df_freq)
                                                                        print(paste0("data added to NEW DF:: ",new_name))
                                                                        rm(dat_row, df_freq,new_name)
                                                                }

                                                        }# end of row loop
                                                        rm(dat,i,count)
                                                        rm(list = ls()[grepl(l, ls())])
                                                }
                                        }else{print("no usual frequency data C")}

                                        #----------------------------------------------------------------------------------------
                                        # renaming frequency datasets
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "df_freq_")
                                        if("df_freq_new" %in% data_list){dataset_spec_new <- df_freq_new; rm(df_freq_new)}
                                        if("df_freq_old" %in% data_list){dataset_spec_old <- df_freq_old; rm(df_freq_old)}
                                        data_list <- ls(pattern = "df_freq_")
                                        if(length(data_list)>0){
                                                for(d in data_list){
                                                        dat_name <- gsub("df_freq_","dataset_spec_",d)
                                                        print(dat_name)
                                                        assign(dat_name,get(d))
                                                        rm(list = ls()[grepl(d, ls())])
                                                        rm(d)
                                                }
                                        }else{print("no usual frequency data D")}

                                        #----------------------------------------------------------------------------------------
                                        # trying to merge remaining odd frequencies
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "dataset_")
                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                        print(data_list)
                                        # n <- 0
                                        # if there are any odd data left
                                        if(length(data_list)>0){
                                                # test loop three times
                                                for (n in 1:3){
                                                        print(n)
                                                        data_list <- ls(pattern = "dataset_")
                                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                        print(data_list)
                                                        if(length(data_list)>1){
                                                                df_count <- data.frame(matrix(NA, nrow = 0, ncol = 2))
                                                                colnames(df_count) <- c("Name", "Count")
                                                                # calculate and save column dimensions
                                                                for(c in data_list){
                                                                        df <- data.frame(matrix(NA, nrow = 0, ncol = 2))
                                                                        colnames(df) <- c("Name", "Count")
                                                                        df[1,] <- c(as.character(c), dim(get(c))[2])
                                                                        df_count <- rbind(df_count, df)
                                                                        rm(df)
                                                                }
                                                                # find the df with the most columns
                                                                df_longest <- df_count[match(max(df_count$Count, na.rm = TRUE), df_count$Count),1]
                                                                print(df_longest)

                                                                # set main with which to merge shorter datasets
                                                                df_orig <- get(df_longest)
                                                                rm(list = ls()[grepl(df_longest, ls())])

                                                                # search for remaining odd frequency datasets
                                                                data_list <- ls(pattern = "dataset_")
                                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                                print(data_list)

                                                                for (l in data_list){
                                                                        print(l)
                                                                        dat <- get(l)
                                                                        if(exists("dat")){
                                                                                ndbc_freq <- names(df_orig)
                                                                                dat_names <- names(dat)
                                                                                setdiff(ndbc_freq, dat_names)
                                                                                print(unique(dat_names %in% ndbc_freq))
                                                                                # looping through data
                                                                                if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                                                                        print(paste0(spec," didn't match"))
                                                                                }else{
                                                                                        tryCatch({
                                                                                                library(plyr)
                                                                                                df_orig <- rbind.fill(df_orig,dat)
                                                                                                print(paste0("concat ",df_longest, " and ",l," match"))
                                                                                                dat_name <- paste0("df_freq_", unlist(strsplit(df_longest,"_"))[3])
                                                                                                assign(dat_name,df_orig)
                                                                                                rm(dat)
                                                                                                rm(list = ls()[grepl(l, ls())])
                                                                                        }, error = function(e) {
                                                                                                print("dataset doesn't match")
                                                                                        })
                                                                                }
                                                                                rm(ndbc_freq,dat_names)
                                                                        }
                                                                        if(exists("dat")){
                                                                                assign(l,dat)
                                                                                rm(dat)
                                                                        }
                                                                }
                                                                rm(df_count, df_orig)

                                                        }else{print("only one odd frequency data left")}
                                                }
                                        }else{print("no remaining odd frequency data E")}

                                        #----------------------------------------------------------------------------------------
                                        # renaming frequency datasets
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "df_freq_")
                                        print(data_list)

                                        if(length(data_list)>0){
                                                for(d in data_list){
                                                        dat_name <- gsub("df_freq_","dataset_spec_",d)
                                                        print(dat_name)
                                                        assign(dat_name,get(d))
                                                        rm(list = ls()[grepl(d, ls())])
                                                        rm(d)
                                                }
                                        }else{print("no usual frequency data F")}

                                        #----------------------------------------------------------------------------------------
                                        # formatting and exporting the final, merged data
                                        #----------------------------------------------------------------------------------------
                                        data_list <- ls(pattern = "dataset_")
                                        print(data_list)

                                        if(length(data_list)>0){
                                                for(d in data_list){
                                                        # d <- data_list[1]
                                                        dataset <- get(d)
                                                        if(dim(dataset)[1]!= 0){
                                                                # correct for NDBC storage format - "The R1 and R2 values in the monthly and yearly
                                                                # historical data files are scaled by 100, a carryover from how the data are transported
                                                                # to the archive centers. The units are hundredths, so the R1 and R2 values in those
                                                                # files should be multiplied by 0.01. ref: https://www.ndbc.noaa.gov/measdes.shtml"
                                                                if(t == "j" | t == "k"){
                                                                        dataset[,2:ncol(dataset)] <- dataset[,2:ncol(dataset)]*0.01
                                                                }
                                                                # remove empty rows - accounting for datasets with no lat/lon
                                                                dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset)-1,]
                                                                column_names <- names(dataset)
                                                                # remove columns with zero or NA in old freq
                                                                if("0.0100" %in% column_names){
                                                                        if(sum(dataset$`0.0100`, na.rm = TRUE)==0){dataset$`0.0100` <- NULL}
                                                                        if(sum(dataset$`0.0200`, na.rm = TRUE)==0){dataset$`0.0200` <- NULL}
                                                                        print("removing 0.0100 and 0.0200 frequency from dataset")
                                                                }else{print("no 0.0100 frequency in dataset")}
                                                                # ordering the dataset by date and selecting unique values only
                                                                dataset <- dataset[order(dataset$DateTime),]
                                                                dataset <- unique(dataset)
                                                                # rename the rows to reflect unique data
                                                                row.names(dataset) <- 1:nrow(dataset)
                                                                # save and export dataset
                                                                year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                                                records <- nrow(dataset)
                                                                if(unlist(strsplit(d,"_"))[3] != "new" & unlist(strsplit(d,"_"))[3] != "old"){
                                                                        count_cols <- ncol(dataset)-3
                                                                        data_name <- paste0("s_",buoy,"_ndbc",file_name,"_freq_",count_cols,"cols")
                                                                }else{
                                                                        data_name <- paste0("s_",buoy,"_ndbc",file_name,"_freq_",unlist(strsplit(d,"_"))[3])
                                                                }
                                                                # export
                                                                write.csv(dataset, paste0(ndbc_dir,buoy,"/",data_name,"_",year_range,"_",records,"_records.csv"), row.names=FALSE)
                                                                # saveRDS(dataset, file = paste0(ndbc_dir,buoy,"/",data_name,"_",year_range,".rds"))
                                                                # assign(data_name, dataset, envir=globalenv())
                                                                assign(data_name, dataset)
                                                                rm(year_range, records)
                                                                rm(dataset)
                                                        }else{
                                                                rm(dataset)
                                                                rm(list = ls()[grepl(d, ls())])
                                                        }

                                                }
                                        }else{print("no data")}
                                        # housekeeping
                                        rm_ls <- ls(pattern = "dataset_")
                                        rm(list = rm_ls)
                                        rm(rm_ls,data_name,d,data_list)

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

        # selecting files to concatenate, then concatenating
        for(buoy in buoys){
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

                        source(paste0(data_dir,"concat_ncei.R"))
                        print("initializing concat_ncei for stdmet...")
                        concat_ncei(file_list, start_year, "stdmet", drive, buoy, file_list)
                        print("finished concat_ncei for stdmet...")

                        library(dplyr)
                        # remove any duplicate rows
                        dataset <- unique(dataset)
                        # rename the rows to reflect unique data
                        row.names(dataset) <- 1:nrow(dataset)

                        # checking for different data nomenclature
                        df_names <- names(dataset)
                        if(length(grep("dominant_period",df_names, value = TRUE))>0){colnames(dataset) = gsub("dominant_period", "dominant_wave_period", colnames(dataset))}
                        if(length(grep("average_period",df_names, value = TRUE))>0){colnames(dataset) = gsub("average_period", "average_wave_period", colnames(dataset))}
                        if(length(grep("_pressure",df_names, value = TRUE))>0){colnames(dataset) = gsub("_pressure", "_pressure_at_sea_level", colnames(dataset))}
                        # if("air_pressure_2" %in% names(dataset)){dataset <- dplyr::rename(dataset, air_pressure_at_sea_level_2 = air_pressure_2, air_pressure_at_sea_level_metadata_2 = air_pressure_metadata_2); print(paste0("renamed air_pressure_at_sea_level_2 column for ", buoy))}

                        # formatting for missing data columns
                        df_names <- names(dataset)
                        if(length(grep("wind_direction",df_names, value = TRUE))>0){print("wind_direction data present")}else{dataset$wind_direction <- as.numeric(NA); dataset$wind_direction_metadata <- as.numeric(NA); print(paste0("empty wind_direction column added to NCEI stdmet for ", buoy))}
                        if(length(grep("wind_speed",df_names, value = TRUE))>0){print("wind_speed data present")}else{dataset$wind_speed <- as.numeric(NA); dataset$wind_speed_metadata <- as.numeric(NA); print(paste0("empty wind_speed column added to NCEI stdmet for ", buoy))}
                        if(length(grep("wind_gust",df_names, value = TRUE))>0){print("wind_gust data present")}else{dataset$wind_gust <- as.numeric(NA); dataset$wind_gust_metadata <- as.numeric(NA); print(paste0("empty wind_gust column added to NCEI stdmet for ", buoy))}
                        if(length(grep("significant_wave_height",df_names, value = TRUE))>0){print("significant_wave_height data present")}else{dataset$significant_wave_height <- as.numeric(NA); dataset$significant_wave_height_metadata <- as.numeric(NA); print(paste0("empty significant_wave_height column added to NCEI stdmet for ", buoy))}
                        if(length(grep("dominant_wave_period",df_names, value = TRUE))>0){print("dominant_wave_period data present")}else{dataset$dominant_wave_period <- as.numeric(NA); dataset$dominant_wave_period_metadata <- as.numeric(NA); print(paste0("empty dominant_wave_period column added to NCEI stdmet for ", buoy))}
                        if(length(grep("average_wave_period",df_names, value = TRUE))>0){print("average_wave_period data present")}else{dataset$average_wave_period <- as.numeric(NA); dataset$average_wave_period_metadata <- as.numeric(NA); print(paste0("empty average_wave_period column added to NCEI stdmet for ", buoy))}
                        if(length(grep("mean_wave_direction",df_names, value = TRUE))>0){print("mean_wave_direction data present")}else{dataset$mean_wave_direction <- as.numeric(NA); dataset$mean_wave_direction_metadata <- as.numeric(NA); print(paste0("empty mean_wave_direction column added to NCEI stdmet for ", buoy))}
                        if(length(grep("air_pressure_at_sea_level",df_names, value = TRUE))>0){print("air_pressure_at_sea_level data present")}else{dataset$air_pressure_at_sea_level <- as.numeric(NA); dataset$air_pressure_at_sea_level_metadata <- as.numeric(NA); print(paste0("empty air_pressure_at_sea_level column added to NCEI stdmet for ", buoy))}
                        if(length(grep("air_temperature",df_names, value = TRUE))>0){print("air_temperature data present")}else{dataset$air_temperature <- as.numeric(NA); dataset$air_temperature_metadata <- as.numeric(NA); print(paste0("empty air_temperature column added to NCEI stdmet for ", buoy))}
                        if(length(grep("sea_surface_temperature",df_names, value = TRUE))>0){print("sea_surface_temperature data present")}else{dataset$sea_surface_temperature <- as.numeric(NA); dataset$sea_surface_temperature_metadata <- as.numeric(NA); print(paste0("empty sea_surface_temperature column added to NCEI stdmet for ", buoy))}
                        if(length(grep("dew_point_temperature",df_names, value = TRUE))>0){print("dew_point_temperature data present")}else{dataset$dew_point_temperature <- as.numeric(NA); dataset$dew_point_temperature_metadata <- as.numeric(NA); print(paste0("empty dew_point_temperature column added to NCEI stdmet for ", buoy))}

                        # deal with skipped GPS positions not lasting more than 4 hours
                        if("lat" %in% names(dataset)){
                                for(c in match(c("lat","lon"),names(dataset))){
                                        for(loops in 1:3){
                                                find_NA <- which(is.na(dataset[,c]))
                                                NA_count <- length(find_NA)
                                                print(paste0("Pre count of NA: ",NA_count, ", run: ",loops))
                                                for(p in find_NA){
                                                        tryCatch({if(!is.na(dataset[p-1,c]) & !is.na(dataset[p+1,c])){dataset[p,c] <- dataset[p-1,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                        tryCatch({if(!is.na(dataset[p-1,c]) & !is.na(dataset[p+2,c])){dataset[p,c] <- dataset[p-1,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                        tryCatch({if(!is.na(dataset[p-2,c]) & !is.na(dataset[p+2,c])){dataset[p,c] <- dataset[p-2,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                        tryCatch({if(!is.na(dataset[p-1,c]) & !is.na(dataset[p+2,c])){dataset[p,c] <- dataset[p-2,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                }
                                                find_NA <- which(is.na(dataset[,c]))
                                                NA_count <- length(find_NA)
                                                print(paste0("Post count of NA: ",NA_count, ", run: ",loops))
                                        }
                                }
                        }else{
                                dataset <- left_join(dataset, gps_positions, by = "DateTime")
                                print(paste0("adding gps positions to ",d))
                        }

                        # reset order of df
                        df_names <- names(dataset)
                        dataCols <- c("DateTime","lat","lat_metadata","lon","lon_metadata")
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
                                print(t)
                                # run concat_ncei function code
                                source(paste0(data_dir,"concat_ncei.R"))

                                print("initializing concat_ncei for spec...")
                                concat_ncei(file_list, start_year, "spec", drive, buoy, file_list, t)
                                print("finishing concat_ncei for spec...")

                                data_avail <- ls(pattern = "dataset")

                                if(length(data_avail)>0){
                                        if(t == "sensor_output"){
                                                # ordering the dataset by date and selecting unique values only
                                                dataset <- dataset[order(dataset$DateTime),]
                                                dataset <- unique(dataset)
                                                # rename the rows to reflect unique data
                                                row.names(dataset) <- 1:nrow(dataset)
                                                # save and export dataset
                                                year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                                records <- nrow(dataset)
                                                data_name <- paste0("s_",buoy,"_ncei_",t)
                                                print("exporting dataset")
                                                # write.csv(dataset, paste0(ncei_dir,buoy,"/",data_name,"_",year_range,"_",records,"_records.csv"), row.names=FALSE)
                                                # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/",data_name,"_",year_range,".rds"))
                                                assign(data_name, dataset)
                                                rm(dataset, year_range)
                                        }else{
                                                # formatting the data
                                                data_list <- ls(pattern = "dataset_")
                                                for(d in data_list){
                                                        print(paste0("Saving individual... ",d))
                                                        dataset <- get(d)
                                                        if(dim(dataset)[1]!= 0){
                                                                # save and export dataset
                                                                year_range1 <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                                                records1 <- nrow(dataset)
                                                                if(unlist(strsplit(d,"_"))[3] != "new" & unlist(strsplit(d,"_"))[3] != "old"){
                                                                        count_cols <- ncol(dataset)
                                                                        data_name <- paste0("s_",buoy,"_ncei_",t,"_freq_",count_cols,"cols")

                                                                }else{
                                                                        data_name <- paste0("s_",buoy,"_ncei_",t,"_freq_",unlist(strsplit(d,"_"))[3])

                                                                }
                                                                # write.csv(dataset, paste0(unlist(strsplit(concat_ind_dir,"s_"))[1],data_name,"_",year_range1,"_",records1,"_records.csv"), row.names=FALSE)
                                                                # saveRDS(dataset, file = paste0(unlist(strsplit(concat_ind_dir,"s_"))[1],data_name,"_",year_range1,".rds"))
                                                                rm(year_range1, records1)
                                                        }
                                                        rm(dataset)
                                                }
                                                rm(d)
                                                #----------------------------------------------------------------------------------------
                                                # reformat odd data
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "dataset_")
                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                print(data_list)

                                                if(length(data_list)>0){
                                                        for(d in data_list){
                                                                df <- get(d)
                                                                print(d)
                                                                if(dim(df)[1]!= 0){
                                                                        # remove rows with no dates
                                                                        df <- df[!is.na(df$DateTime), ]
                                                                        # remove blank data rows
                                                                        df <- df[rowSums(is.na(df)) != ncol(df)-3,]
                                                                        if(dim(df)[2]>3){   # to handle two column data
                                                                                df$count <- rowSums(df[4:ncol(df)], na.rm = TRUE)
                                                                                df <- dplyr::filter(df, count != 0)
                                                                                df$count <- NULL
                                                                        }
                                                                        if(dim(df)[1] > 0){
                                                                                # format data
                                                                                df1 <- dplyr::select(df, DateTime, lat,lon)
                                                                                df2 <- df
                                                                                df2$lat <- NULL; df2$lon <- NULL
                                                                                # remove columns with zero or NA
                                                                                df2 <- df2[, colSums(df2 != 0, na.rm = TRUE) > 0]
                                                                                # reorder columns numerically
                                                                                library(data.table)
                                                                                df2 <- setcolorder(df2, c(1, order(as.numeric(names(df2)[-1])) + 1))
                                                                                df <- full_join(df1,df2, by = "DateTime")
                                                                                rm(df1,df2)
                                                                                # ordering the df by date and selecting unique values only
                                                                                df <- df[order(df$DateTime),]
                                                                                df <- unique(df)
                                                                                # rename the rows to reflect unique data
                                                                                row.names(df) <- 1:nrow(df)
                                                                                # assign data
                                                                                assign(d,df)
                                                                        }else{
                                                                                rm(list = ls()[grepl(d, ls())])
                                                                                print("dataset removed: all row NA or zero")
                                                                        }
                                                                }
                                                                rm(df)
                                                        }
                                                }else{print("no unusual data A")}

                                                #----------------------------------------------------------------------------------------
                                                # testing data merge with old and new frequency dfs
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "dataset_")
                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                print(data_list)

                                                if(length(data_list)>0){

                                                        # dealing with multiple formats for the same frequencies
                                                        if("dataset_spec_old" %in% data_avail){
                                                                print("dataset_spec_old already exists")

                                                        }else{
                                                                colNames_spec_old <- c("DateTime", "lat", "lon", "0.0100","0.0200","0.0300","0.0400","0.0500","0.0600","0.0700","0.0800","0.0900","0.1000","0.1100","0.1200",
                                                                                       "0.1300","0.1400","0.1500","0.1600","0.1700","0.1800","0.1900","0.2000","0.2100","0.2200",
                                                                                       "0.2300","0.2400","0.2500","0.2600","0.2700","0.2800","0.2900","0.3000","0.3100","0.3200",
                                                                                       "0.3300","0.3400","0.3500","0.3600","0.3700","0.3800","0.3900","0.4000")
                                                                dataset_spec_old <- data.frame(matrix(NA, nrow = 1, ncol = length(colNames_spec_old)))
                                                                colnames(dataset_spec_old) <- colNames_spec_old
                                                                dataset_spec_old$DateTime <- lubridate::ymd_hms(dataset_spec_old$DateTime)
                                                        }

                                                        if("dataset_spec_new" %in% data_avail){
                                                                print("dataset_spec_new already exists")
                                                        }else{
                                                                colNames_spec_new <- c("DateTime", "lat", "lon", "0.0200", "0.0325", "0.0375", "0.0425", "0.0475", "0.0525",
                                                                                       "0.0575", "0.0625", "0.0675", "0.0725", "0.0775", "0.0825", "0.0875", "0.0925", "0.1000", "0.1100",
                                                                                       "0.1200", "0.1300", "0.1400", "0.1500", "0.1600", "0.1700", "0.1800", "0.1900", "0.2000", "0.2100",
                                                                                       "0.2200", "0.2300", "0.2400", "0.2500", "0.2600", "0.2700", "0.2800", "0.2900", "0.3000", "0.3100",
                                                                                       "0.3200", "0.3300", "0.3400", "0.3500", "0.3650", "0.3850", "0.4050", "0.4250", "0.4450", "0.4650",
                                                                                       "0.4850")
                                                                dataset_spec_new <- data.frame(matrix(NA, nrow = 1, ncol = length(colNames_spec_new)))
                                                                colnames(dataset_spec_new) <- colNames_spec_new
                                                                dataset_spec_new$DateTime <- lubridate::ymd_hms(dataset_spec_new$DateTime)
                                                        }

                                                        # loop through odd frequencies to test if df can be merged with new and odd frequencies
                                                        # testing data merge
                                                        data_list <- ls(pattern = "dataset_")
                                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                        print(data_list)

                                                        for (l in data_list){
                                                                print(l)
                                                                dat <- get(l)
                                                                spec_list <- ls(pattern = "dataset_spec_")
                                                                spec_list <- spec_list[spec_list %in% c("dataset_spec_new","dataset_spec_old") == TRUE]
                                                                print(spec_list)

                                                                for(spec in spec_list){
                                                                        if(exists("dat")){
                                                                                spec_dat <- get(spec)
                                                                                ndbc_freq <- names(spec_dat)
                                                                                dat_names <- names(dat)
                                                                                setdiff(ndbc_freq, dat_names)
                                                                                print(unique(dat_names %in% ndbc_freq))
                                                                                # looping through data
                                                                                if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                                                                        print(paste0(spec," didn't match"))
                                                                                }else{
                                                                                        tryCatch({
                                                                                                library(plyr)
                                                                                                spec_dat <- rbind.fill(spec_dat,dat)
                                                                                                print(paste0("concat ",spec, " and ",l," match"))
                                                                                                assign(spec,spec_dat)
                                                                                                rm(dat,spec_dat)
                                                                                                rm(list = ls()[grepl(l, ls())])
                                                                                        }, error = function(e) {
                                                                                                print("dataset doesn't match")
                                                                                        })
                                                                                        if(exists("spec_dat")){rm(spec_dat)}
                                                                                }
                                                                                rm(spec_dat,ndbc_freq,dat_names)
                                                                        }

                                                                }
                                                                if(exists("dat")){
                                                                        assign(l,dat)
                                                                        rm(dat)
                                                                }
                                                        }
                                                }else{print("no unusual frequency data B")}

                                                #----------------------------------------------------------------------------------------
                                                # if still present, loop through odd frequencies to attempt a merge by row
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "dataset_")
                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                print(data_list)

                                                if(length(data_list)>0){
                                                        # loop through odd frequencies to attempt a merge by row
                                                        # testing data merge
                                                        data_list <- ls(pattern = "dataset_")
                                                        if("dataset_spec_new" %in% data_list){df_freq_new <- dataset_spec_new; rm(dataset_spec_new)}
                                                        if("dataset_spec_old" %in% data_list){df_freq_old <- dataset_spec_old; rm(dataset_spec_old)}
                                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                        print(data_list)

                                                        # try per row
                                                        for (l in data_list){
                                                                print(l)
                                                                dat <- get(l)
                                                                # loop through each row and try to match to new and old frequencies
                                                                for(i in 1:nrow(dat)) {
                                                                        print(i)
                                                                        # Extract row and all columns
                                                                        dat_row <- dat[i, ]
                                                                        # remove cells with zero or NA - selects ANY NA and removes column
                                                                        library(dplyr)
                                                                        dat_row <- dat_row %>% select_if(~ !any(is.na(.)))
                                                                        # match data rows
                                                                        spec_list <- ls(pattern = "df_freq_")
                                                                        # spec_list <- spec_list[spec_list %in% c("df_freq_new","df_freq_old") == TRUE]
                                                                        print(spec_list)

                                                                        for(spec in spec_list){
                                                                                if(exists("dat_row")){
                                                                                        spec_dat <- get(spec)
                                                                                        ndbc_freq <- names(spec_dat)
                                                                                        dat_names <- names(dat_row)
                                                                                        setdiff(ndbc_freq, dat_names)
                                                                                        print(unique(dat_names %in% ndbc_freq))
                                                                                        # looping through data
                                                                                        if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                                                                                print(paste0(spec," didn't match"))
                                                                                        }else{
                                                                                                tryCatch({
                                                                                                        library(plyr)
                                                                                                        spec_dat <- rbind.fill(spec_dat,dat_row)
                                                                                                        print(paste0("concat ",spec, " and row ",i," match"))
                                                                                                        assign(spec,spec_dat)
                                                                                                        rm(dat_row,spec_dat)
                                                                                                        # rm(list = ls()[grepl(d, ls())])
                                                                                                }, error = function(e) {
                                                                                                        print("dataset doesn't match")
                                                                                                })
                                                                                                if(exists("spec_dat")){rm(spec_dat)}
                                                                                        }
                                                                                 }

                                                                        }
                                                                        if(exists("dat_row")){
                                                                                count <- length(spec_list)+1
                                                                                df_freq <- data.frame(matrix(NA, nrow = 0, ncol = dim(dat_row)[2]))
                                                                                df_freq<-rbind(df_freq, dat_row)
                                                                                new_name <- paste0("df_freq_",count)
                                                                                assign(new_name, df_freq)
                                                                                print(paste0("data added to NEW DF:: ",new_name))
                                                                                rm(dat_row, df_freq,new_name)
                                                                        }

                                                                }# end of row loop
                                                                rm(dat,i,count)
                                                                rm(list = ls()[grepl(l, ls())])
                                                        }
                                                }else{print("no usual frequency data C")}

                                                #----------------------------------------------------------------------------------------
                                                # renaming frequency datasets
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "df_freq_")
                                                if("df_freq_new" %in% data_list){dataset_spec_new <- df_freq_new; rm(df_freq_new)}
                                                if("df_freq_old" %in% data_list){dataset_spec_old <- df_freq_old; rm(df_freq_old)}
                                                data_list <- ls(pattern = "df_freq_")
                                                if(length(data_list)>0){
                                                        for(d in data_list){
                                                                dat_name <- gsub("df_freq_","dataset_spec_",d)
                                                                print(dat_name)
                                                                assign(dat_name,get(d))
                                                                rm(list = ls()[grepl(d, ls())])
                                                                rm(d)
                                                        }
                                                }else{print("no usual frequency data D")}

                                                #----------------------------------------------------------------------------------------
                                                # trying to merge remaining odd frequencies within themselves
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "dataset_")
                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                print(data_list)
                                                # if there are any odd data left
                                                if(length(data_list)>0){
                                                        # test loop three times
                                                        for (n in 1:3){
                                                                print(n)
                                                                data_list <- ls(pattern = "dataset_")
                                                                data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                                print(data_list)
                                                                if(length(data_list)>1){
                                                                        df_count <- data.frame(matrix(NA, nrow = 0, ncol = 2))
                                                                        colnames(df_count) <- c("Name", "Count")
                                                                        # calculate and save column dimensions
                                                                        for(c in data_list){
                                                                                df <- data.frame(matrix(NA, nrow = 0, ncol = 2))
                                                                                colnames(df) <- c("Name", "Count")
                                                                                df[1,] <- c(as.character(c), dim(get(c))[2])
                                                                                df_count <- rbind(df_count, df)
                                                                                rm(df)
                                                                        }
                                                                        # find the df with the most columns
                                                                        df_longest <- df_count[match(max(df_count$Count, na.rm = TRUE), df_count$Count),1]
                                                                        print(df_longest)

                                                                        # set main with which to merge shorter datasets
                                                                        df_orig <- get(df_longest)
                                                                        rm(list = ls()[grepl(df_longest, ls())])

                                                                        # search for remaining odd frequency datasets
                                                                        data_list <- ls(pattern = "dataset_")
                                                                        data_list <- data_list[data_list %in% c("dataset_spec_new","dataset_spec_old") == FALSE]
                                                                        print(data_list)

                                                                        for (l in data_list){
                                                                                print(l)
                                                                                dat <- get(l)
                                                                                if(exists("dat")){
                                                                                        ndbc_freq <- names(df_orig)
                                                                                        dat_names <- names(dat)
                                                                                        setdiff(ndbc_freq, dat_names)
                                                                                        print(unique(dat_names %in% ndbc_freq))
                                                                                        # looping through data
                                                                                        if(any(unique(dat_names %in% ndbc_freq)==FALSE)){
                                                                                                print(paste0(spec," didn't match"))
                                                                                        }else{
                                                                                                tryCatch({
                                                                                                        library(plyr)
                                                                                                        df_orig <- rbind.fill(df_orig,dat)
                                                                                                        print(paste0("concat ",df_longest, " and ",l," match"))
                                                                                                        dat_name <- paste0("df_freq_", unlist(strsplit(df_longest,"_"))[3])
                                                                                                        assign(dat_name,df_orig)
                                                                                                        rm(dat)
                                                                                                        rm(list = ls()[grepl(l, ls())])
                                                                                                }, error = function(e) {
                                                                                                        print("dataset doesn't match")
                                                                                                })
                                                                                                #if(exists("spec_dat")){rm(spec_dat)}
                                                                                        }
                                                                                        rm(ndbc_freq,dat_names)
                                                                                }
                                                                                if(exists("dat")){
                                                                                        assign(l,dat)
                                                                                        rm(dat)
                                                                                }
                                                                        }
                                                                        rm(df_count, df_orig)

                                                                }else{print("only one odd frequency data left")}
                                                        }
                                                }else{print("no remaining odd frequency data E")}

                                                #----------------------------------------------------------------------------------------
                                                # renaming frequency datasets after trying to merge odd frequencies with each other
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "df_freq_")
                                                print(data_list)

                                                if(length(data_list)>0){
                                                        for(d in data_list){
                                                                dat_name <- gsub("df_freq_","dataset_spec_",d)
                                                                print(dat_name)
                                                                assign(dat_name,get(d))
                                                                rm(list = ls()[grepl(d, ls())])
                                                                rm(d)
                                                        }
                                                }else{print("no usual frequency data F")}

                                                #----------------------------------------------------------------------------------------
                                                # formatting the merged data
                                                #----------------------------------------------------------------------------------------
                                                data_list <- ls(pattern = "dataset_")
                                                print(data_list)

                                                if(length(data_list)>0){
                                                        for(d in data_list){
                                                                print(paste0("Saving concat... ",d))
                                                                dataset <- get(d)
                                                                # remove empty rows across df
                                                                dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset),]
                                                                # remove rows with no datetimes
                                                                dataset <- dataset[!is.na(dataset$DateTime),]

                                                                if(dim(dataset)[1]!= 0){
                                                                        # remove empty rows - accounting for datasets with no lat/lon
                                                                        column_names <- names(dataset)
                                                                        if("lat" %in% column_names){
                                                                                dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset)-3,]
                                                                                dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset)-1,]
                                                                        }else {dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset)-1,]}
                                                                        # remove columns with zero or NA in old freq
                                                                        if("0.0100" %in% column_names){
                                                                                if(sum(dataset$`0.0100`, na.rm = TRUE)==0){dataset$`0.0100` <- NULL}
                                                                                if(sum(dataset$`0.0200`, na.rm = TRUE)==0){dataset$`0.0200` <- NULL}

                                                                        }
                                                                        if(dim(dataset)[1]!= 0){
                                                                                # deal with skipped GPS positions not lasting more than 4 hours
                                                                                if("lat" %in% names(dataset)){
                                                                                        for(c in match(c("lat","lon"),names(dataset))){
                                                                                                for(loops in 1:3){
                                                                                                        # c <- 3
                                                                                                        find_NA <- which(is.na(dataset[,c]))
                                                                                                        NA_count <- length(find_NA)
                                                                                                        print(paste0("Pre count of NA: ",NA_count, ", run: ",loops))
                                                                                                        for(p in find_NA){
                                                                                                                # p <- find_NA[1]
                                                                                                                # print(p)
                                                                                                                tryCatch({if(!is.na(dataset[p-1,c]) & !is.na(dataset[p+1,c])){dataset[p,c] <- dataset[p-1,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                                                                                tryCatch({if(!is.na(dataset[p-1,c]) & !is.na(dataset[p+2,c])){dataset[p,c] <- dataset[p-1,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                                                                                tryCatch({if(!is.na(dataset[p-2,c]) & !is.na(dataset[p+2,c])){dataset[p,c] <- dataset[p-2,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                                                                                tryCatch({if(!is.na(dataset[p-1,c]) & !is.na(dataset[p+2,c])){dataset[p,c] <- dataset[p-2,c]}}, error = function(cond){print("no lat/lon inserted")})
                                                                                                        }
                                                                                                        find_NA <- which(is.na(dataset[,c]))
                                                                                                        NA_count <- length(find_NA)
                                                                                                        print(paste0("Post count of NA: ",NA_count, ", run: ",loops))
                                                                                                }
                                                                                        }
                                                                                }else{
                                                                                        dataset <- left_join(dataset, gps_positions, by = "DateTime")
                                                                                        print(paste0("adding gps positions to ",d))
                                                                                }
                                                                        }else{
                                                                                print(paste0("empty dataframe ",d))
                                                                                rm(dataset)
                                                                        }

                                                                        # ordering the dataset by date and selecting unique values only
                                                                        dataset <- dataset[order(dataset$DateTime),]
                                                                        dataset <- unique(dataset)
                                                                        # rename the rows to reflect unique data
                                                                        row.names(dataset) <- 1:nrow(dataset)
                                                                        # save and export dataset
                                                                        year_range <- paste0(year(min(dataset$DateTime,na.rm = TRUE)),"_", year(max(dataset$DateTime, na.rm = TRUE)))
                                                                        records <- nrow(dataset)
                                                                        if(unlist(strsplit(d,"_"))[3] != "new" & unlist(strsplit(d,"_"))[3] != "old"){
                                                                                count_cols <- ncol(dataset)-3
                                                                                data_name <- paste0("s_",buoy,"_ncei_",t,"_freq_",count_cols,"cols")
                                                                        }else{
                                                                                data_name <- paste0("s_",buoy,"_ncei_",t,"_freq_",unlist(strsplit(d,"_"))[3])
                                                                        }

                                                                        print("exporting datasets")
                                                                        write.csv(dataset, paste0(ncei_dir,buoy,"/",data_name,"_",year_range,"_",records,"_records.csv"), row.names=FALSE)
                                                                        # saveRDS(dataset, file = paste0(ncei_dir,buoy,"/",data_name,"_",year_range,".rds"))
                                                                        # assign(data_name, dataset, envir=parent.frame())
                                                                        assign(data_name, dataset)
                                                                        rm(year_range, records)
                                                                        rm(dataset)
                                                                }else{
                                                                        rm(dataset)
                                                                        rm(list = ls()[grepl(d, ls())])
                                                                }                                               }
                                                }else{print("no data to merge")}

                                                rm_ls <- ls(pattern = "dataset_")
                                                rm(list = rm_ls)
                                                rm(rm_ls,data_name,d,data_list)
                                        } # end of no sensor loop

                                }else{print(paste0("no dataset:",t))} # end of individual t

                        } # end of all t loop

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

                }else{
                        print(paste0("No new ncei data for buoy ",buoy))
                }
        }

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # clean glob environ
        # rm(list = ls())
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
}

