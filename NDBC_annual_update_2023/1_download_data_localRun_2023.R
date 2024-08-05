download_data_0 <- function(buoys = "list of buoys", start_year = "start year", data_dir = "data_dir"){
        
        start_year <- 2023
        
        ##----------------------------------------------------------------------------------------
        ## script to download NDBC web files and NCEI netcdf files
        ## Hall 12/28/2019
        ##----------------------------------------------------------------------------------------
        
        ## Actions:
        ## 1.   Sets data locations
        ## 2.   Lists buoy ID's for downloading
        ## 3.   Finds current end date for monthly and yearly data downloads
        ## 4.   Downloads NDBC website historical data from the NDBC website 
        ##      - download and unzip files
        ## 5.   Downloads NDBC NetCDF data from the NCEI website
        ##      - Use R to call 'read_NetCDF_post_xxxx.R and read_NetCDF_pre_xxxx.R' to extract NetCDF files to ASCII
        ##      - Attaches datetime, lat and lon to all datasets - including gps/latitude and gps/longitude = repeated to confirm concat accuracy
        ##      - Extract data and metadata

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # load libraries
        library(lubridate)
        library(R.utils)
        # library("ncdf4")
        library(qpcR)
        library(plyr)
        library(dplyr)
        library(ncdf4)
        
        # # load libraries
        # library(lubridate, lib="/p/home/candice/Rlibs/")
        # library(R.utils, lib="/p/home/candice/Rlibs/")
        # library("ncdf4", lib="/p/home/candice/Rlibs/")
        # library(qpcR, lib="/p/home/candice/Rlibs/")
        # library(plyr, lib="/p/home/candice/Rlibs/")
        # library(dplyr, lib="/p/home/candice/Rlibs/")
        # library(ncdf4, lib="/p/home/candice/Rlibs/")
        
        # D:\Candice\projects\WaveTrends\data\raw_data
        drive <- "G:/"
        proj_dir <- paste0(drive, "Candice/projects/WaveTrends/")
        data_dir <- paste0(proj_dir, "data/")
        setwd(data_dir)
        
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
        ## set buoy stations for downloading 
        ##----------------------------------------------------------------------------------------
        
        list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
        list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
        list_ndbc_buoy <- as.character(list_ndbc$station)
         
        buoys <- list_ndbc_buoy
        rm(list_ndbc)
        #buoy <- buoy_ls[142]
         
        # test buoys
        #buoys <- buoy_ls
        print(buoys)
        
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # install.packages("R.utils")
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
                # buoy <- buoy_ls[26]
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

        ## Different NDBC formats...
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
                # buoy <- buoys[1]
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
        # buoys <- c("44014","46029") # y: 1990, 1984

        ## sample web addresses for all data related to each buoy station

        ## ftp://ftp.nodc.noaa.gov/pub/f291/200301/44014_200301.Z
        ## https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2011/01/NDBC_44014_201101_D1_v00.nc
        ## https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2019/01/NDBC_44014_201901_D5_v00.nc

        ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2020/03/NDBC_41002_202003_D7_v00.nc
        ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2020/04/NDBC_41001_202004_D4_v00.nc
        ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2023/01/NDBC_41001_202301_D6_v00.nc

        ## NCEI NODC THREDDS NDBC CMANWX server
        #https://data.nodc.noaa.gov/thredds/fileServer/ndbc/cmanwx/2006/12/44014_200612.nc

        ##----------------------------------------------------------------------------------------
        ## download code
        ##----------------------------------------------------------------------------------------
        library(lubridate)
        
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

        # looping through the listed buoy ID's
        for (buoy in buoys){

                # buoy <- "51000"
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
                ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2023/06/NDBC_46082_202306_D11_v00.nc
                ## https://www.ncei.noaa.gov/data/oceans/ndbc/cmanwx/2023/06/NDBC_51000_202306_D6_v00.nc
          

                for(year in years){
                        # year <- 2023
                        for(m in month){
                                # m <- month[6]
                                num_mth <- match(m, month.abb)
                                print(m)
                                ##----------------------------------------------------------------------------------------
                                # # for pre-2011 F291 file formats
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
        ## call 'read_NetCDF_pre_2011.R' and 'read_NetCDF_post_2011.R'/'Read_NetCDF_ch.m' scripts and extract NetCDF files to ASCII
        ##----------------------------------------------------------------------------------------
        # buoys <- c("46029","45001", "44014")
        ##----------------------------------------------------------------------------------------

        # copy and extract files to new folder
        print(buoys)

        for (buoy in buoys){
                # buoy <- buoys[115]
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
                        wave_freq_available_ALL <- data.frame(matrix(NA, nrow = 0, ncol = 48))
                        spec_r_type_ALL <- data.frame(matrix(NA, nrow = 0, ncol = 48))

                        for(file in list.zip){
                                # file <- list.zip[1]
                                tryCatch({
                                        nc <- nc_open(file)                         # Reading the netcdf data
                                        print(nc$filename)
                                        # extract data
                                        time <- data.frame(ncvar_get(nc, "time_waves"), stringsAsFactors = FALSE)
                                        # wave wpm check
                                        tryCatch({
                                                wave_freq <- ncvar_get(nc, "wave_frequency")
                                                # human readable time
                                                time[,1] <- as.POSIXct(time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                                                colnames(time) <- c("time")
                                                # save wave spectral frequency bands
                                                wave_freq_available <- t(data.frame(c(as.character(time[1,1]),signif(wave_freq,6)), stringsAsFactors = FALSE))
                                                library(plyr)
                                                wave_freq_available_ALL <- qpcR:::rbind.na(wave_freq_available_ALL, wave_freq_available)
                                                rownames(wave_freq_available_ALL) <- NULL
                                                rm(wave_freq_available)
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
                                        if(any(grepl("r1",dat_list))){
                                                r_num <- grep("r1",dat_list)[1]
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
                        if(exists('wave_freq_available_ALL')){
                            write.csv(wave_freq_available_ALL, paste0(ascii_ncei_dir,buoy,"_spec_freq_available_ALL.csv"), row.names=FALSE, na = "NaN")
                            rm(wave_freq_available_ALL)
                            write.csv(spec_r_type_ALL, paste0(ascii_ncei_dir,buoy,"_spec_r_type_ALL.csv"), row.names=FALSE, na = "NaN")
                            rm(spec_r_type_ALL, r_dat)
                        }

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
                                        # file <- list.zip[1]
                                        file_present <- gsub(".nc","",unlist(strsplit(file,paste0(netCDF_ncei_dir,buoy,"/")))[2])
                                        if(file_present %in% list.extracted){
                                                print("file already exists")
                                        }else{
                                                file.copy(file, paste0(ascii_ncei_dir,buoy,"/"))
                                                print(paste0("copying file over: ", file))
                                        }
                                }
                                rm(file_present)
                                
                                if(start_year >= 2023){
                                      # Use read_NetCDF_post_2023.R script and extract NetCDF files to ASCII
                                      list.zip <- list.files(path = paste0(ascii_ncei_dir,buoy,"/"), pattern = ".nc", full.names = TRUE)
                                      source(paste0(data_dir,"read_NetCDF_post_2023.R"))
                                      for(file in list.zip){
                                          # file <- list.zip[1] 
                                          file
                                          read_NetCDF_post_2023(file, buoy, ascii_ncei_dir)
                                      }
                                }else if(start_year >= 2011 && start_year <= 2022){
                                    # Use read_NetCDF_post_2011.R script and extract NetCDF files to ASCII
                                    list.zip <- list.files(path = paste0(ascii_ncei_dir,buoy,"/"), pattern = ".nc", full.names = TRUE)
                                    source(paste0(data_dir,"read_NetCDF_post_2011.R"))
                                    for(file in list.zip){
                                        # file <- list.zip[1] # watch for missing time_10second, release flags for 10 second data that doesn't
                                        file
                                        read_NetCDF_post_2011(file, buoy, ascii_ncei_dir)
                                    }
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
                
                # buoy <- buoys[128]
                print(paste0("Starting NCEI metadata dump: ",buoy))

                # look for data
                list.zip <- list.files(path = paste0(netCDF_ncei_dir,buoy,"/"), pattern = "_D", full.names = TRUE)

                if(length(list.zip)>0){
                        # # start writing to an output file
                        # sink(paste0(data_dir,"0_ncei_metadata_buoy_",buoy,"_",Sys.Date(),".txt"))
                        # print(paste0("Starting NCEI read_NetCDF: ",buoy))
                        
                        # create buoy folder
                        if (!file.exists(paste0(metadata_ncei_dir,buoy,"/"))) {dir.create((paste0(metadata_ncei_dir,buoy,"/")))}
                        
                        list.extracted <- list.files(path = paste0(metadata_ncei_dir,buoy,"/"), full.names = FALSE)
                        # copy new files over
                        for (file in list.zip){
                                # file <- list.zip[1]
                                file_present <- unlist(strsplit(file,paste0(netCDF_ncei_dir,buoy,"/")))[2]
                                if(file_present %in% list.extracted){
                                        print("file already exists")
                                }else{
                                        file.copy(file, paste0(metadata_ncei_dir,buoy,"/"))
                                        print(paste0("copying file over: ", file))
                                }
                        }
                        rm(file_present,list.extracted)
                        
                        setwd(paste0(metadata_ncei_dir,buoy,"/"))
                        list.zip <- list.files(path = paste0(metadata_ncei_dir,buoy,"/"), pattern = ".nc", full.names = TRUE)
                        
                        library(dplyr)
                        library(ncdf4)
        
                        # attributes of specific global variable
                        colNames1 <- c('file_name','variable','platform_wmo_id','sea_floor_depth_below_sea_level','latitude','longitude','geospatial_vertical_units',
                                       'hull_id','units','payload_id','description','manufacturer','part_number','installed_height','sampling_period',
                                       'type','wave_processing','software_revision')
                        metadat_master <- data.frame(matrix(data = NA, nrow = 0, ncol = length(colNames1)), stringsAsFactors = FALSE)
                        colnames(metadat_master) <- colNames1
        
                        for(file in list.zip){
                                # file <- list.zip[1]
                                print(file)
                                # Open a connection to the file
                                my.nc <- nc_open(file)
                                output <- paste0(gsub("\\.nc","",file),"_dump")
                                sink(paste0(output, ".txt"))
                                print(my.nc)
                                sink()
        
                                # extract metadata
                                pay_ls <- attributes(my.nc$var)$names
                                
                                for (payloads in pay_ls){
                                        # payloads <- pay_ls[1]
                                        print(payloads)
                                        file_name <- unlist(strsplit(file,"/"))[length(unlist(strsplit(file,"/")))]
                                        # # global attributes
                                        if(!is.null(ncatt_get(my.nc, payloads)$payload_id)){
                                            if(grepl('wave',payloads)){
                                                  metadat_global <- data.frame(file_name, payloads,
                                                                               ncatt_get(my.nc, 0)$platform_wmo_id, 
                                                                               ncatt_get(my.nc, 0)$sea_floor_depth_below_sea_level, 
                                                                               ncatt_get(my.nc, 0)$nominal_latitude,
                                                                               ncatt_get(my.nc, 0)$nominal_longitude,
                                                                               if(!is.null(ncatt_get(my.nc, 0)$geospatial_vertical_units)){ncatt_get(my.nc, 0)$geospatial_vertical_units}else{"no geospatial_vertical_units available"})
                                                  
                                                  # group attributes
                                                  metadat_group <- data.frame('no hull_id available',
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$units)){ncatt_get(my.nc, payloads)$units}else{"no units available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$payload_id)){ncatt_get(my.nc, payloads)$payload_id}else{"no payload_id available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$description)){ncatt_get(my.nc, payloads)$description}else{"no description available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$manufacturer)){ncatt_get(my.nc, payloads)$manufacturer}else{"no manufacturer available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$part_number)){ncatt_get(my.nc, payloads)$part_number}else{"no part_number available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$installed_height)){ncatt_get(my.nc, payloads)$installed_height}else{"no installed_height available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$sampling_period)){ncatt_get(my.nc, payloads)$sampling_period}else{"no sampling_period available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$type)){ncatt_get(my.nc, payloads)$type}else{"no type available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$wave_processing)){ncatt_get(my.nc, payloads)$wave_processing}else{"no wave_processing available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$software_revision)){ncatt_get(my.nc, payloads)$software_revision}else{"no software available"})
                                                  # create table
                                                  metadat <- cbind(metadat_global, metadat_group)
                                                  colnames(metadat) <- c('file_name','variable','platform_wmo_id','sea_floor_depth_below_sea_level','latitude','longitude','geospatial_vertical_units',
                                                                         'hull_id','units','payload_id','description','manufacturer','part_number','installed_height','sampling_period',
                                                                         'type','wave_processing','software_revision')
                                                  
                                            }else{
                                                  metadat_global <- data.frame(file_name,payloads, 
                                                                               ncatt_get(my.nc, 0)$platform_wmo_id, 
                                                                               ncatt_get(my.nc, 0)$sea_floor_depth_below_sea_level, 
                                                                               ncatt_get(my.nc, 0)$nominal_latitude,
                                                                               ncatt_get(my.nc, 0)$nominal_longitude,
                                                                               if(!is.null(ncatt_get(my.nc, 0)$geospatial_vertical_units)){ncatt_get(my.nc, 0)$geospatial_vertical_units}else{"no geospatial_vertical_units available"})
                                                  # group attributes
                                                  metadat_group <- data.frame('no hull_id available',
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$units)){ncatt_get(my.nc, payloads)$units}else{"no units available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$payload_id)){ncatt_get(my.nc, payloads)$payload_id}else{"no payload_id available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$description)){ncatt_get(my.nc, payloads)$description}else{"no description available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$manufacturer)){ncatt_get(my.nc, payloads)$manufacturer}else{"no manufacturer available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$part_number)){ncatt_get(my.nc, payloads)$part_number}else{"no part_number available"},
                                                                              if(!is.null(ncatt_get(my.nc, payloads)$installed_height)){ncatt_get(my.nc, payloads)$installed_height}else{"no installed_height available"})
                                                  
                                                  # create table
                                                  metadat <- cbind(metadat_global, metadat_group)
                                                  colnames(metadat) <- c('file_name','variable','platform_wmo_id','sea_floor_depth_below_sea_level','latitude','longitude','geospatial_vertical_units',
                                                                         'hull_id','units','payload_id','description','manufacturer','part_number','installed_height')
                                            }
                                            # join datasets
                                            metadat_master <- rbind.fill(metadat_master, metadat)
                                            rm(metadat)
                                            rm(metadat_global)
                                            rm(metadat_group)
                                        }
                                }
                                nc_close(my.nc)
                                rm(my.nc)
                        } # end of buoy files list
        
                        # remove any duplicate rows
                        metadat_master <- unique(metadat_master)
                        # rename the rows to reflect unique data
                        row.names(metadat_master) <- 1:nrow(metadat_master)
                        # export
                        write.csv(metadat_master, paste0(metadata_ncei_dir,buoy,"_metadata_ALL.csv"), row.names=FALSE)
                        saveRDS(metadat_master, file = paste0(metadata_ncei_dir,buoy,"_metadata_ALL.rds"))
        
                        # remove files if extracted
                        ext_files <- list.files(path = paste0(metadata_ncei_dir,buoy,"/"), pattern = ".nc", full.names = FALSE)
                        for (e in ext_files){
                                # e <- ext_files[1]
                                e_full <- paste0(metadata_ncei_dir,buoy,"/",e)
                                file.remove(e_full)
                        }
                        print("removed .nc files from folder")
                        # remove directory
                        setwd(metadata_ncei_dir)
        
                        unlink(paste0(metadata_ncei_dir, buoy), recursive = TRUE)
                        rm(metadat_master)
        
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

