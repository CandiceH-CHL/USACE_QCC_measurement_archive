download_data_1 <- function(buoys = "list of buoys", start_year = "start year", data_dir = "data_dir"){

        ##----------------------------------------------------------------------------------------
        ## script to download NDBC web files and NCEI netcdf files
        ## Hall, Candice
        ##----------------------------------------------------------------------------------------
        
        ## Actions:
        ## 1.   Sets data locations
        ## 2.   Lists buoy ID's for downloading
        ## 3.   Finds current end date for monthly and yearly data downloads
        ## 4.   Downloads NDBC website historical data from the NDBC website 
        ## 5.   Unzips NDBC files
        ## 6.   Downloads NDBC NetCDF data from the NCEI website
        ## 7.   Calls 'read_NetCDF_post_2011.R and read_NetCDF_pre_2011.R' to extract NetCDF files to ASCII
        ## 8.   Attaches datetime, lat and lon to all datasets - including gps/latitude and gps/longitude = repeated to confirm concat accuracy
        ## 9.   Calls 'read_NetCDF_metadata.R' script to extract all netCDF metadata from global and variable attributes

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        
        # load libraries (local run)
        library(lubridate)
        library(R.utils)
        # library("ncdf4")
        library(qpcR)
        library(plyr)
        library(dplyr)
        library(ncdf4)
        
        ## load libraries (HPC run)
        # library(lubridate, lib="/p/home/candice/Rlibs/")
        # library(R.utils, lib="/p/home/candice/Rlibs/")
        # library("ncdf4", lib="/p/home/candice/Rlibs/")
        # library(qpcR, lib="/p/home/candice/Rlibs/")
        # library(plyr, lib="/p/home/candice/Rlibs/")
        # library(dplyr, lib="/p/home/candice/Rlibs/")
        # library(ncdf4, lib="/p/home/candice/Rlibs/")
        
        ##----------------------------------------------------------------------------------------
        ## set paths
        ##----------------------------------------------------------------------------------------
        setwd(data_dir)
        # create new folders for data
        if (!file.exists(paste0(data_dir,"raw_data/"))) {dir.create((paste0(data_dir,"raw_data/")))}
        input_dir <- paste0(data_dir,"raw_data/")
        ## set new output directories for raw and zipped datasets
        # ndbc 
        if (!file.exists(paste0(input_dir,"ndbc/"))) {dir.create((paste0(input_dir,"ndbc/")))}
        ndbc_dir <- paste0(input_dir,"ndbc/")
        if (!file.exists(paste0(ndbc_dir,"zipped/"))) {dir.create((paste0(ndbc_dir,"zipped/")))}
        zip_ndbc_dir <- paste0(ndbc_dir,"zipped/")
        if (!file.exists(paste0(ndbc_dir,"unzipped/"))) {dir.create((paste0(ndbc_dir,"unzipped/")))}
        unzip_ndbc_dir <- paste0(ndbc_dir,"unzipped/")
        # ncei
        if (!file.exists(paste0(input_dir,"ncei/"))) {dir.create((paste0(input_dir,"ncei/")))}
        ncei_dir <- paste0(input_dir,"ncei/")
        if (!file.exists(paste0(ncei_dir,"netCDF/"))) {dir.create((paste0(ncei_dir,"netCDF/")))}
        netCDF_ncei_dir <- paste0(ncei_dir,"netCDF/")
        if (!file.exists(paste0(ncei_dir,"ascii/"))) {dir.create((paste0(ncei_dir,"ascii/")))}
        ascii_ncei_dir <- paste0(ncei_dir,"ascii/")
        if (!file.exists(paste0(ncei_dir,"metadata/"))) {dir.create((paste0(ncei_dir,"metadata/")))}
        metadata_ncei_dir <- paste0(ncei_dir,"metadata/")
        
        ##----------------------------------------------------------------------------------------
        ## set buoy stations for downloading (for stand-alone use)
        ##----------------------------------------------------------------------------------------
        
        # list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
        # list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
        # list_ndbc_buoy <- as.character(list_ndbc$station)
        # 
        # buoys <- list_ndbc_buoy
        # rm(list_ndbc)

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        
        # downloading NDBC website historical data
        
        #----------------------------------------------------------------------------------------
        ## sample web addresses for all data related to each buoy station
        
        ## yearly Standard meteorological data:                 https://www.ndbc.noaa.gov/data/historical/stdmet/44014h1990.txt.gz
        ## yearly Spectral wave density data:                   https://www.ndbc.noaa.gov/data/historical/swden/44014w1996.txt.gz
        ## yearly Spectral wave (alpha1) direction data:        https://www.ndbc.noaa.gov/data/historical/swdir/44014d1996.txt.gz
        ## yearly Spectral wave (alpha2) direction data:        https://www.ndbc.noaa.gov/data/historical/swdir2/44014i1998.txt.gz
        ## yearly Spectral wave (r1) direction data:            https://www.ndbc.noaa.gov/data/historical/swr1/44014j1998.txt.gz
        ## yearly Spectral wave (r2) direction data:            https://www.ndbc.noaa.gov/data/historical/swr2/44014k1998.txt.gz
        
        ## monthly Standard meteorological data:                https://www.ndbc.noaa.gov/data/stdmet/Jan/4401412019.txt.gz
        ## monthly Spectral wave density data:                  https://www.ndbc.noaa.gov/data/swden/Jan/4401412019.txt.gz
        ## monthly Spectral wave (alpha1) direction data:       https://www.ndbc.noaa.gov/data/swdir/Jan/4401412019.txt.gz
        ## monthly Spectral wave (alpha2) direction data:       https://www.ndbc.noaa.gov/data/swdir2/Jan/4401412019.txt.gz
        ## monthly Spectral wave (r1) direction data:           https://www.ndbc.noaa.gov/data/swr1/Jan/4401412019.txt.gz
        ## monthly Spectral wave (r2) direction data:           https://www.ndbc.noaa.gov/data/swr2/Jan/4401412019.txt.gz
        
        ##----------------------------------------------------------------------------------------
        ## set lists of downloading parameters
        ##----------------------------------------------------------------------------------------
        
        # end date for monthly and yearly data downloads
        library(lubridate)
        if(month(Sys.Date())== 1 | month(Sys.Date())== 2){
                current_year <- as.numeric(unlist(strsplit(as.character(Sys.Date()),"-"))[1])-1
        }else{
                current_year <- as.numeric(unlist(strsplit(as.character(Sys.Date()),"-"))[1])
        }
        # yearly files
        start_year <- as.numeric(start_year)
        years <- seq(start_year,current_year,1)
        month <- month.abb[c(1:12)]
        
        # dataset types (Standard meteorological, Spectral wave density, Spectral wave (alpha1) direction,
        #                Spectral wave (alpha2) direction, Spectral wave (r1) direction, Spectral wave (r2) direction)
        dataTypes <- c("stdmet", "swden", "swdir", "swdir2", "swr1","swr2")
        dataType_ab <- c("h", "w", "d", "i","j","k")
        
        ##----------------------------------------------------------------------------------------
        ## download code
        ##----------------------------------------------------------------------------------------

        # looping through the listed buoy ID's
        for (buoy in buoys){
                print(paste0("Starting NDBC download: ",buoy))

                # # start writing to an output file
                # sink(paste0(data_dir,"0_ndbc_download_buoy_",buoy,"_",Sys.Date(),".txt"))
                # print(paste0("Starting NDBC download: ",buoy))

                ## create buoy specific download folder
                if (!file.exists(paste0(zip_ndbc_dir,buoy,"/"))) {dir.create((paste0(zip_ndbc_dir,buoy,"/")))}
                ## for each dataset type, set the data type
                for (dataT in dataTypes){
                        # dataT <- dataTypes[1]
                        ab <- dataType_ab[match(dataT,dataTypes)]
                        ## downloading yearly files
                        ## https://www.ndbc.noaa.gov/data/historical/stdmet/44014h1990.txt.gz
                        ## https://www.ndbc.noaa.gov/data/historical/swden/44014wb1997.txt.gz
                        for (year in years){
                                # year <- years[8]
                                # URL website
                                fileUrl1 <- paste0("https://www.ndbc.noaa.gov/data/historical/",dataT,"/",buoy,ab,year,".txt.gz")
                                fileUrl2 <- paste0("https://www.ndbc.noaa.gov/data/historical/",dataT,"/",buoy,ab,"b",year,".txt.gz")
                                #https://www.ndbc.noaa.gov/data/historical/stdmet/44014h2001.txt.gz
                                print(fileUrl1)
                                 # set download location and file name
                                destfile = paste0(zip_ndbc_dir,buoy,"/",buoy,ab,year,"_",dataT,".txt.gz")
                                destfile2 = paste0(zip_ndbc_dir,buoy,"/",buoy,ab,"b",year,"_",dataT,".txt.gz")
                                # Download data from the website
                                if (!file.exists(destfile)) {
                                        try(download.file(fileUrl1, mode = "wb", destfile = destfile))
                                        print(destfile)
                                } else {print("Data are already downloaded: yearly")}
                                if (!file.exists(destfile2)) {
                                        try(download.file(fileUrl2, mode = "wb", destfile = destfile2))
                                        print(destfile2)
                                } else {print("Data are already downloaded: yearly")}
                        }
                        library(lubridate)
                        if(month(Sys.Date()) == 1) {
                                year1 <- current_year-1
                                ## downloading monthly files of the current year
                                ## https://www.ndbc.noaa.gov/data/stdmet/Jan/4401412019.txt.gz
                                for(m in month){
                                        # m <- month[11]
                                        num_mth <- match(m, month.abb)
                                        print(m)
                                        # URL website
                                        fileUrl2 <- paste0("https://www.ndbc.noaa.gov/data/",dataT,"/",m,"/",buoy,num_mth,year1,".txt.gz") # normal monthly NDBC file name
                                        fileUrl3 <- paste0("https://www.ndbc.noaa.gov/data/",dataT,"/",m,"/",buoy,"a",year1,".txt.gz") # new name??
                                        fileUrl4 <- paste0("https://www.ndbc.noaa.gov/data/",dataT,"/",m,"/",buoy,".txt") # another new name???
                                        print(fileUrl2)
                                        # set download location and file name
                                        # stdmet/Jan/4401412019.txt.gz
                                        destfile2 = paste0(zip_ndbc_dir,buoy,"/",buoy,ab,year1,"_",match(m,month.abb),"_",dataT,".txt.gz")
                                        # Download data from the website
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl2, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Data are already downloaded: fileUrl2")}
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl3, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Data are already downloaded: fileUrl3")}
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl4, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Data are already downloaded: fileUrl4")}
                                }
                        }else{  year1 <- current_year
                                ## downloading monthly files of the current year
                                ## https://www.ndbc.noaa.gov/data/stdmet/Jan/4401412019.txt.gz
                                for(m in month){
                                        # m <- month[11]
                                        num_mth <- match(m, month.abb)
                                        print(m)
                                        # URL website
                                        fileUrl2 <- paste0("https://www.ndbc.noaa.gov/data/",dataT,"/",m,"/",buoy,num_mth,year1,".txt.gz") # normal monthly NDBC file name
                                        fileUrl3 <- paste0("https://www.ndbc.noaa.gov/data/",dataT,"/",m,"/",buoy,"a",year1,".txt.gz") # new name??
                                        fileUrl4 <- paste0("https://www.ndbc.noaa.gov/data/",dataT,"/",m,"/",buoy,".txt") # another new name???
                                        print(fileUrl2)
                                        # set download location and file name
                                        # stdmet/Jan/4401412019.txt.gz
                                        destfile2 = paste0(zip_ndbc_dir,buoy,"/",buoy,ab,year1,"_",match(m,month.abb),"_",dataT,".txt.gz")
                                        # Download data from the website
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl2, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Data are already downloaded: fileUrl2")}
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl3, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Data are already downloaded: fileUrl3")}
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl4, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Data are already downloaded: fileUrl4")}

                                }
                        }
                }
                print(paste0("Finished NDBC download: ",buoy))
                # end writing to an output file
                # sink()
                # print(paste0("Finished NDBC download: ",buoy))
        }

        ## Different NDBC formats examples...
        # https://www.ndbc.noaa.gov/data/stdmet/Oct/44014a2019.txt.gz
        # https://www.ndbc.noaa.gov/data/stdmet/Nov/44014.txt
        # https://www.ndbc.noaa.gov/data/stdmet/Oct/44014a2019.txt.gz
        # https://www.ndbc.noaa.gov/data/swr2/Oct/44014a2019.txt.gz
        
        ##----------------------------------------------------------------------------------------
        ## copy and unzip datafiles code
        ##----------------------------------------------------------------------------------------
        
        library(R.utils)
        # copy files to new folder and unzip
        for (buoy in buoys){
                print(paste0("Starting NDBC unzip: ",buoy))

                # copy files to new folder for unzipping
                list.zip <- list.files(path = paste0(zip_ndbc_dir,buoy,"/"), pattern = ".gz", full.names = TRUE)

                if(length(list.zip)> 0){

                        # start writing to an output file
                        # sink(paste0(data_dir,"0_ndbc_unzip_buoy_",buoy,"_",Sys.Date(),".txt"))
                        # print(paste0("Starting NDBC unzip: ",buoy))

                        ## create buoy specific unzip folder
                        if (!file.exists(paste0(unzip_ndbc_dir,buoy,"/"))) {dir.create((paste0(unzip_ndbc_dir,buoy,"/")))}

                        # copy files to new folder for unzipping
                        list.zip <- list.files(path = paste0(zip_ndbc_dir,buoy,"/"), pattern = ".gz", full.names = TRUE)
                        list.unzipped <- list.files(path = paste0(unzip_ndbc_dir,buoy,"/"), pattern = ".txt", full.names = FALSE)
                        # copy new files over
                        for (file in list.zip){
                                file_present <- gsub(".gz","",unlist(strsplit(file,paste0(zip_ndbc_dir,buoy,"/")))[2])
                                if(file_present %in% list.unzipped){
                                        print("file already exists")
                                }else{
                                        file.copy(file, paste0(unzip_ndbc_dir,buoy,"/"))
                                }
                        }
                        # unzip files in unzipped folder
                        list.unzip <- list.files(path = paste0(unzip_ndbc_dir,buoy,"/"), pattern = ".gz", full.names = TRUE)
                        for(i in list.unzip){
                                print(i)
                                tryCatch({
                                        gunzip(i, remove = TRUE)
                                }, error = function(e){
                                        print("already unzipped")
                                        unlink(i)
                                })
                        }

                        print(paste0("Finished NDBC upzip: ",buoy))
                        # end writing to an output file
                        # sink()
                }else{print("no ndbc data to unzip")}

                print(paste0("Finished NDBC unzip: ",buoy))

        }

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        
        # downloading NCEI NDBC NetCDF data

        #----------------------------------------------------------------------------------------

        # ncei website

        ## sample web addresses for all data related to each buoy station

        ## ftp://ftp.nodc.noaa.gov/pub/f291/200301/44014_200301.Z
        ## https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2011/01/NDBC_44014_201101_D1_v00.nc
        ## https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2019/01/NDBC_44014_201901_D5_v00.nc

        ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2020/03/NDBC_41002_202003_D7_v00.nc
        ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2020/04/NDBC_41001_202004_D4_v00.nc

        ## NCEI NODC THREDDS NDBC CMANWX server
        #https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2006/12/44014_200612.nc

        ##----------------------------------------------------------------------------------------
        ## download code
        ##----------------------------------------------------------------------------------------
        library(lubridate)

        # looping through the listed buoy ID's
        for (buoy in buoys){

                print(paste0("Starting NCEI download: ",buoy))

                # # start writing to an output file
                # sink(paste0(data_dir,"0_ncei_download_buoy_",buoy,"_",Sys.Date(),".txt"))
                # print(paste0("Starting NCEI download: ",buoy))

                ## create buoy specific download folder
                if (!file.exists(paste0(netCDF_ncei_dir,buoy,"/"))) {dir.create((paste0(netCDF_ncei_dir,buoy,"/")))}

                ## downloading netCDF year_month files
                ## old format for pre and post 2011 files - as of Apr 2020
                ## https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2006/12/44014_200612.nc
                ## https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2011/01/NDBC_44014_201101_D1_v00.nc

                ## new format for pre and post 2011 files - as of Apr 2020
                ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/1980/01/41001_198001.nc
                ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2020/04/NDBC_41001_202004_D4_v00.nc

                for(year in years){
                        # year <- 2020
                        for(m in month){
                                # m <- month[7]
                                num_mth <- match(m, month.abb)
                                print(m)
                                ##----------------------------------------------------------------------------------------
                                # # for pre-2011 F291 file formats (no longer available but retaining code for historical value)
                                # if(year <2011){
                                #         # URL website
                                #         # ftp://ftp.nodc.noaa.gov/pub/f291/201001/44014_201001.Z
                                #         fileUrl1 <- paste0("ftp://ftp.nodc.noaa.gov/pub/f291/",year,sprintf("%02d", num_mth),"/",buoy,"_",year,sprintf("%02d", num_mth),".Z") # pre-2011
                                #         print(fileUrl1)
                                #         # set download location and file name
                                #         destfile1 = paste0(netCDF_ncei_dir,buoy,"/",buoy,"_",year,sprintf("%02d", num_mth),".Z")
                                #
                                #         # Download data from the website
                                #         if (!file.exists(destfile1)) {
                                #                 try(download.file(fileUrl1, mode = "wb", destfile = destfile1))
                                #                 print(destfile1)
                                #         } else {print("Data are already downloaded: fileUrl4")}
                                # }
                                ##----------------------------------------------------------------------------------------
                                # # post 2011
                                if(year >= 2011){
                                        for(d in 1:20){
                                                # print(d)
                                                # fileUrl1 <- paste0("https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/",year,"/",sprintf("%02d", num_mth),"/NDBC_",buoy,"_",year,sprintf("%02d", num_mth),"_D",d,"_v00.nc") # post 2011
                                                fileUrl1 <- paste0("https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/",year,"/",sprintf("%02d", num_mth),"/NDBC_",buoy,"_",year,sprintf("%02d", num_mth),"_D",d,"_v00.nc") # post 2011
                                                print(fileUrl1)
                                                destfile1 = paste0(netCDF_ncei_dir,buoy,"/",buoy,"_",year,sprintf("%02d", num_mth),"_D",d,".nc")
                                                print(destfile1)
                                                # Download data from the website
                                                if (!file.exists(destfile1)) {
                                                        try(download.file(fileUrl1, mode = "wb", destfile = destfile1))
                                                        print(destfile1)
                                                } else {print("Post 2011 data are already downloaded")}
                                        }
                                }
                                # pre 2011
                                if(year < 2011){
                                        # fileUrl2 <- paste0("https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/",year,"/",sprintf("%02d", num_mth),"/",buoy,"_",year,sprintf("%02d", num_mth),".nc") # post 2011
                                        fileUrl2 <- paste0("https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/",year,"/",sprintf("%02d", num_mth),"/",buoy,"_",year,sprintf("%02d", num_mth),".nc") # post 2011
                                        destfile2 = paste0(netCDF_ncei_dir,buoy,"/",buoy,"_",year,sprintf("%02d", num_mth),".nc")
                                        print(destfile2)
                                        if (!file.exists(destfile2)) {
                                                try(download.file(fileUrl2, mode = "wb", destfile = destfile2))
                                                print(destfile2)
                                        } else {print("Pre 2011 data are already downloaded")}
                                }
                        }
                }
                print(paste0("Finished NCEI download: ",buoy))
                # # end writing to an output file
                # sink()
                # print(paste0("Finished NCEI download: ",buoy))
        }
        

        ##----------------------------------------------------------------------------------------
        ## call 'read_NetCDF_pre_2011.R' and 'read_NetCDF_post_2011.R' scripts and extract NetCDF files to ASCII
        ##----------------------------------------------------------------------------------------

        # copy and extract files to new folder
        print(buoys)

        for (buoy in buoys){
                print(paste0("Starting NCEI read_NetCDF: ",buoy))

                # # find spectral frequencies used over time
                list.zip <- list.files(path = paste0(netCDF_ncei_dir,buoy,"/"), full.names = TRUE)

                if(length(list.zip) > 0){
                        # # start writing to an output file
                        # sink(paste0(data_dir,"0_ncei_readNetCDF_buoy_",buoy,"_",Sys.Date(),".txt"))
                        # print(paste0("Starting NCEI read_NetCDF: ",buoy))

                        library("ncdf4")
                        library(qpcR)
                        # concat all spectral wave frequencies used over time
                        wave_wpm_available_ALL <- data.frame(matrix(NA, nrow = 0, ncol = 48))
                        spec_r_type_ALL <- data.frame(matrix(NA, nrow = 0, ncol = 48))

                        for(file in list.zip){
                                # file <- list.zip[1]
                                tryCatch({
                                        nc <- nc_open(file)                         # Reading the netcdf data
                                        print(nc$filename)
                                        # extract data
                                        time <- data.frame(ncvar_get(nc, "time"), stringsAsFactors = FALSE)
                                        # wave wpm check
                                        tryCatch({
                                                wave_wpm <- ncvar_get(nc, "wave_wpm")
                                                # human readable time
                                                time[,1] <- as.POSIXct(time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                                                colnames(time) <- c("time")
                                                # save wave spectral frequency bands
                                                wave_wpm_available <- t(data.frame(c(as.character(time[1,1]),signif(wave_wpm,6)), stringsAsFactors = FALSE))
                                                library(plyr)
                                                wave_wpm_available_ALL <- qpcR:::rbind.na(wave_wpm_available_ALL, wave_wpm_available)
                                                rownames(wave_wpm_available_ALL) <- NULL
                                                rm(wave_wpm_available)
                                        }, error = function(e){print("no wave_wmp")}
                                        )

                                        # r data check
                                        dat_list <- NULL
                                        
                                        for (i in 1:nc$nvars){
                                                j<-nc$var[[i]]$name
                                                # print(j)
                                                dat_list <- c(dat_list,j)
                                        }
                                        # find r1 data
                                        if(any(grepl("*/r1$",dat_list))){
                                                r_num <- grep("*/r1$",dat_list)[1]
                                                r_dat <- data.frame(t(ncvar_get(nc, dat_list[r_num])), stringsAsFactors = FALSE)
                                                # human readable time
                                                time[,1] <- as.POSIXct(time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                                                colnames(time) <- c("time")
                                                # save wave spectral frequency bands
                                                r_available <- data.frame(c(as.character(time[1,1]), r_dat[1,]), stringsAsFactors = FALSE)
                                                library(plyr)
                                                spec_r_type_ALL <- qpcR:::rbind.na(spec_r_type_ALL, r_available)
                                                rownames(spec_r_type_ALL) <- NULL
                                                rm(r_available)
                                        }else{print(paste0("has no R: ",file))}
                                        nc_close(nc)
                                        rm(nc,time,dat_list, j)
                                }, error = function(e){print(paste0("can't open: ",file))}
                                )
                        }
                        write.csv(wave_wpm_available_ALL, paste0(ascii_ncei_dir,buoy,"_spec_freq_available_ALL.csv"), row.names=FALSE, na = "NaN")
                        rm(wave_wpm_available_ALL)
                        write.csv(spec_r_type_ALL, paste0(ascii_ncei_dir,buoy,"_spec_r_type_ALL.csv"), row.names=FALSE, na = "NaN")
                        rm(spec_r_type_ALL)

                        ## 2011 onward NetCDF format

                        ## create buoy specific unzip folder
                        if (!file.exists(paste0(ascii_ncei_dir,buoy,"/"))) {dir.create((paste0(ascii_ncei_dir,buoy,"/")))}
                        print(paste0("working on post-2011: ", file))

                        # copy files to new folder for unzipping
                        list.zip <- list.files(path = paste0(netCDF_ncei_dir,buoy,"/"), pattern = "_D", full.names = TRUE)

                        if(length(list.zip)>0){
                                
                                list.extracted <- list.files(path = paste0(ascii_ncei_dir,buoy,"/"), full.names = FALSE)

                                # copy new files over
                                for (file in list.zip){
                                        # file <- list.zip[115]
                                        file_present <- gsub(".nc","",unlist(strsplit(file,paste0(netCDF_ncei_dir,buoy,"/")))[2])
                                        if(file_present %in% list.extracted){
                                                print("file already exists")
                                        }else{
                                                file.copy(file, paste0(ascii_ncei_dir,buoy,"/"))
                                                print(paste0("copying file over: ", file))
                                        }
                                }
                                rm(file_present)

                                # Use read_NetCDF_post_2011.R script and extract NetCDF files to ASCII
                                list.zip <- list.files(path = paste0(ascii_ncei_dir,buoy,"/"), pattern = ".nc", full.names = TRUE)
                                source(paste0(data_dir,"read_NetCDF_post_2011.R"))
                                for(file in list.zip){
                                        # file <- list.zip[1] # watch for missing time_10second, release flags for 10 second data that doesn't
                                        file
                                        read_NetCDF_post_2011(file, buoy, ascii_ncei_dir)
                                }

                                # remove files if extracted
                                ext_files <- list.files(path = paste0(ascii_ncei_dir,buoy,"/"), pattern = ".nc", full.names = FALSE)
                                for (e in ext_files){
                                        # e <- ext_files[1]
                                        e_full <- paste0(ascii_ncei_dir,buoy,"/",e)
                                        destfile <- paste0(ascii_ncei_dir,buoy,"/",gsub(".nc","",e))
                                        if(file.exists(destfile)){file.remove(e_full)}
                                }
                               print(paste0("finished post-2011: ", file))

                        }else{print("no available post-2011 files")}


                        ## pre-2011 NetCDF format

                        # reset directory
                        setwd(data_dir)
                        print(paste0("working on pre-2011: ", file))

                        # # copy files to new folder for unzipping
                        # list.zip <- list.files(path = paste0(netCDF_ncei_dir,buoy,"/"), pattern = buoy, full.names = TRUE)
                        list.netcdf <- list.files(path = paste0(netCDF_ncei_dir,buoy,"/"), full.names = TRUE)
                        list.ascii <-  list.files(path = paste0(ascii_ncei_dir,buoy,"/"), full.names = TRUE)

                        if (start_year <= 2010){
                                dates <- c(1970:2010)
                                ## find the files for this data setup in netcdf folders
                                files <- list()
                                for (i in 1:length(dates)){
                                        files[[i]] <- list.netcdf[grepl(paste0("_",dates[i]),list.netcdf)]
                                }
                                list.netcdf <- unlist(files)
                                ## find the files for this data setup in ascii folders
                                files <- list()
                                for (i in 1:length(dates)){
                                        files[[i]] <- list.ascii[grepl(paste0("_",dates[i]),list.ascii)]
                                }
                                list.ascii <- unlist(files)
                                # look for extracted files
                                list.extracted <- NULL
                                for(l in list.ascii){
                                        # l <- list.ascii[1]
                                        stripped <- unlist(strsplit(l, paste0("/",buoy,"/")))[2]
                                        list.extracted <- c(list.extracted,stripped)
                                }
                                # copy new files over
                                for (file in list.netcdf){
                                        # file <- list.netcdf[1]
                                        file_present <- gsub(".nc","",unlist(strsplit(file,paste0(netCDF_ncei_dir,buoy,"/")))[2])
                                        if(file_present %in% list.extracted){
                                                print("file already exists")
                                        }else{
                                                file.copy(file, paste0(ascii_ncei_dir,buoy,"/"))
                                                print(paste0("copying file over: ", file))
                                        }
                                }

                                # Use 'read_NetCDF_pre_2011.R' script and extract NetCDF files to ASCII
                                source(paste0(data_dir,"read_NetCDF_pre_2011.R"))
                                files <-  list.files(path = paste0(ascii_ncei_dir,buoy,"/"), pattern = ".nc", full.names = TRUE)

                                for(file in files){
                                        # file <- files[202]
                                        file
                                        read_NetCDF_pre_2011(file, buoy, ncei_dir, ascii_ncei_dir)
                                }
                                # remove files if extracted
                                ext_files <- list.files(path = paste0(ascii_ncei_dir,buoy,"/"), pattern = ".nc", full.names = FALSE)
                                for (e in ext_files){
                                        # e <- ext_files[1]
                                        e_full <- paste0(ascii_ncei_dir,buoy,"/",e)
                                        destfile <- paste0(ascii_ncei_dir,buoy,"/", unlist(strsplit(e, ".nc"))[1])
                                        destfile2 <- paste0(ascii_ncei_dir,buoy,"/", unlist(strsplit(e, "_D"))[1])
                                        if(file.exists(destfile)){try(file.remove(e_full))}
                                        if(file.exists(destfile2)){try(file.remove(e_full))}
                                }
                                print(paste0("finished pre-2011: ", file))

                                # buoy <- buoys[1]
                                print(paste0("Finished NCEI read_NetCDF: ",buoy))
                        }
                        # end writing to an output file
                        # sink()
                        print(paste0("Finished NCEI read_NetCDF: ",buoy))
                }else{print("empty folder - no data")}
                # # end writing to an output file
                print(paste0("Finished NCEI read_NetCDF: ",buoy))
        }
        rm(r_dat)
        
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        #
        # Extract all metadata
        #
        #----------------------------------------------------------------------------------------
        # buoys with no post-2011 data:
        # buoys <- c(41006, 41018, 41021, 41022, 41023, 42007, 42038, 42041, 42042, 42053, 42054, 42080,
        #            44004, 44028, 44070, 45011, 46003, 46023, 46030, 46045, 46051, 46062, 46063, 46079,
        #            46106, 46107, 48011, 51026, 51028)
        
        for (buoy in buoys){
                print(paste0("Starting NCEI metadata dump: ",buoy))
                time_pause <- 18 # seconds 

                # look for data
                list.zip <- list.files(path = paste0(netCDF_ncei_dir,buoy,"/"), pattern = "_D", full.names = TRUE)

                if(length(list.zip)>0){
                        # # start writing to an output file
                        # sink(paste0(data_dir,"0_ncei_metadata_buoy_",buoy,"_",Sys.Date(),".txt"))
                        # print(paste0("Starting NCEI read_NetCDF: ",buoy))
                        
                        # create buoy folder
                        if (!file.exists(paste0(metadata_ncei_dir,buoy,"/"))) {dir.create((paste0(metadata_ncei_dir,buoy,"/")))}
                        
                        source(paste0(data_dir,"read_NetCDF_metadata.R"))
                        read_NetCDF_metadata(buoy, list.zip, netCDF_ncei_dir, metadata_ncei_dir, time_pause)
                        
                        # # # end writing to an output file
                        # sink()
                        # print(paste0("Finished metadata: ",buoy))
                }else{
                        print("no netCDF data available for download")
                        # sink()
                        print(paste0("Finished metadata: ",buoy))
                }
                # # preventative
                # sink()
        }
        

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # clean glob environ
        rm(list = ls())
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
}

