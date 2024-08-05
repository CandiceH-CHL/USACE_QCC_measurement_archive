MEDS_download_data_1 <- function(buoys = "list of buoys", start_year = "start year", data_dir = "data_dir"){
        
        start_year <- 2022
        
        ##----------------------------------------------------------------------------------------
        ## script to download MEDS files
        ## Hall 06/17/2022
        ##----------------------------------------------------------------------------------------
        
        ## Actions:
        ## 1.   Sets data locations
        ## 2.   Lists buoy ID's for downloading
        ## 3.   Finds current end date for monthly and yearly data downloads
        ## 4.   Downloads MEDS website historical data from the MEDS website 
        ##      - download and unzip files
        
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
        library(RCurl)
        
        drive <- "D:/"
        data_dir <- paste0(drive, "Candice/projects/WaveTrends/MEDS_2024/data/")
        setwd(data_dir)
        
        if (!file.exists(paste0(data_dir,"raw_data/"))) {dir.create((paste0(data_dir,"raw_data/")))}
        input_dir <- paste0(data_dir,"raw_data/")
        ## set new output directories for raw and zipped datasets
        # MEDS 
        if (!file.exists(paste0(input_dir,"bulk/"))) {dir.create((paste0(input_dir,"bulk/")))}
        bulk_dir <- paste0(input_dir,"bulk/")
        if (!file.exists(paste0(input_dir,"meta/"))) {dir.create((paste0(input_dir,"meta/")))}
        meta_dir <- paste0(input_dir,"meta/")
        if (!file.exists(paste0(input_dir,"spec/"))) {dir.create((paste0(input_dir,"spec/")))}
        spec_dir <- paste0(input_dir,"spec/")
        
        ##----------------------------------------------------------------------------------------
        ## set buoy stations for downloading 
        ##----------------------------------------------------------------------------------------
        
        list_MEDS <- read.csv(paste0(data_dir,"MEDS_MasterBuoyList.csv"),header = TRUE)
        list_MEDS_buoy <- as.character(list_MEDS$STID)
        buoys <- list_MEDS_buoy
        rm(list_MEDS)
        print(buoys)
        
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # install.packages("R.utils")
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        
        # downloading MEDS website historical data
        
        #----------------------------------------------------------------------------------------
        ## sample web addresses for all data related to each buoy station
        
        ## All standard meteorological data:      https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44131_csv.zip
        ## yearly meta data:                      https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/metaData/meta_c44131.csv
        ## yearly Spectral data:                  https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/fbyears/C44131/c44131_1991.zip
        ## year to date spectral data:            https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/fbyear2date/c45144_y2d.zip

        ##----------------------------------------------------------------------------------------
        ## set lists of downloading parameters
        ##----------------------------------------------------------------------------------------
        
        # end date for monthly and yearly data downloads
        library(lubridate)

        # yearly files
        start_year <- as.numeric(start_year)
        current_year <- as.numeric(unlist(strsplit(as.character(Sys.Date()),"-"))[1])
        years <- seq(start_year,current_year,1)
        # month <- month.abb[c(1:12)]
        
        ##----------------------------------------------------------------------------------------
        ## download code
        ##----------------------------------------------------------------------------------------

        # looping through the listed buoy ID's
        for (buoy in buoys){
             # buoy <- buoys[2]
             print(paste0("Starting MEDS download: ",buoy))

             # # start writing to an output file
             # sink(paste0(data_dir,"0_MEDS_download_buoy_",buoy,"_",Sys.Date(),".txt"))
             # print(paste0("Starting MEDS download: ",buoy))

             ## downloading All standard meteorological data:
             ## https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44131_csv.zip
             fileUrl1 <- paste0("https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c",buoy,"_csv.zip")
             print(fileUrl1)
             # set download location and file name
             destfile = paste0(bulk_dir,buoy,"/c",buoy,".csv")
             # Download data from the website
             if (!file.exists(destfile)) {
                  library(data.table)
                  dat <- fread(paste0("curl ",fileUrl1," | funzip"))
                  if(dim(dat)[1]==0){rm(dat); print("no csv data")}
                  if(exists("dat")){
                       if (!file.exists(paste0(bulk_dir,buoy,"/"))) {dir.create((paste0(bulk_dir,buoy,"/")))}
                       write.csv(dat, file = destfile, row.names = FALSE)
                       rm(dat)
                  }
                  rm(dat)
             } else {print("Data are already downloaded: all csv")}

             ## downloading meta data:
             ## https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/metaData/meta_c44131.csv
             fileUrl1 <- paste0("https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/metaData/meta_c",buoy,".csv")
             print(fileUrl1)
             # set download location and file name
             destfile = paste0(meta_dir,buoy,"/c",buoy,"_meta.csv")
             # Download data from the website
             if (!file.exists(destfile)) {
                  ## create buoy specific download folder
                  if (!file.exists(paste0(meta_dir,buoy,"/"))) {dir.create((paste0(meta_dir,buoy,"/")))}
                  try(download.file(fileUrl1, mode = "wb", destfile = destfile)); print(destfile)
             } else {print("Data are already downloaded: meta csv")}
             
             ## downloading Year to date spectral data:      
             ## https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/fbyear2date/c45144_y2d.zip
             fileUrl1 <- paste0("https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/fbyear2date/c",buoy,"_y2d.zip")
             print(fileUrl1)
             # set download location and file name
             destfile = paste0(spec_dir,buoy,"/c",buoy,"_Y2D_spec")
             temp_dir = paste0(spec_dir,"temp")
             temp_dest = paste0(spec_dir,"temp/c",buoy,"_Y2D.zip")
             
             # Download data from the website
             if (!file.exists(temp_dest)) {
                  try(download.file(fileUrl1, mode = "wb", destfile = temp_dest))
                  try(unzip(temp_dest, exdir = temp_dir))
                  print(temp_dest)
             } else {print("Data are already downloaded: yearly spec")}
             
             tryCatch(
                  {
                       dat <- read.table(gsub("Y2D.zip","y2d.fb",temp_dest), header = FALSE, fill = TRUE)
                       if(dim(dat)[1]==0){rm(dat); print("no spec data")}
                       if(exists("dat")){
                            if(!file.exists(paste0(spec_dir,buoy,"/"))) {dir.create((paste0(spec_dir,buoy,"/")))}
                            write.table(dat, paste0(destfile,".txt"), append = FALSE, sep = " ", dec = ".",row.names = FALSE, col.names = FALSE)
                            write.csv(dat, paste0(destfile,".csv"), row.names = FALSE)
                            rm(dat)
                       }
                       
                  },
                  error=function(cond) {
                       message(paste("URL does not seem to exist:", destfile))
                  })
             try(do.call(file.remove, list(list.files(temp_dir, full.names = TRUE))))
             
             
             # # Download data from the website - OLD Y2D CODE....
             # # if (!file.exists(destfile)) {
             #      library(data.table)
             #      dat <- fread(paste0("curl ",fileUrl1," | funzip"), fill = TRUE)
             #      if(dim(dat)[1]==0){rm(dat); print("no csv data")}
             #      if(exists("dat")){
             #           if (!file.exists(paste0(spec_dir,buoy,"/"))) {dir.create((paste0(spec_dir,buoy,"/")))}
             #           write.csv(dat, file = destfile, row.names = FALSE)
             #           rm(dat)
             #      }
             # # } else {print("Data are already downloaded: Y2D csv")}

             ## downloading spectral data:
             ## https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/fbyears/C44131/c44131_1991.zip
             for (year in years){
                 # year <- years[23]
                 # URL website
                 fileUrl1 <- paste0("https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/fbyears/C",buoy,"/c",buoy,"_",year,".zip")
                 print(fileUrl1)

                 # set download location and file name
                 destfile = paste0(spec_dir,buoy,"/c",buoy,"_",year)
                 temp_dir = paste0(spec_dir,"temp")
                 temp_dest = paste0(spec_dir,"temp/c",buoy,"_",year,".zip")

                 # Download data from the website
                 if(!file.exists(temp_dest)){
                         try(download.file(fileUrl1, mode = "wb", destfile = temp_dest))
                         try(unzip(temp_dest, exdir = temp_dir))
                         print(temp_dest)
                 }else{print("Data are already downloaded: yearly spec")}

                 tryCatch(
                      {
                           dat <- read.table(gsub(".zip",".fb",temp_dest), header = FALSE, fill = TRUE)
                           if(dim(dat)[1]==0){rm(dat); print("no spec data")}
                           if(exists("dat")){
                                if(!file.exists(paste0(spec_dir,buoy,"/"))) {dir.create((paste0(spec_dir,buoy,"/")))}
                                write.table(dat, paste0(destfile,"_spec.txt"), append = FALSE, sep = " ", dec = ".",row.names = FALSE, col.names = FALSE)
                                write.csv(dat, paste0(destfile,"_spec.csv"), row.names = FALSE)
                                rm(dat)
                          }

                      },
                      error=function(cond) {
                           message(paste("URL does not seem to exist:", destfile))
                      })
                 try(do.call(file.remove, list(list.files(temp_dir, full.names = TRUE))))
             }
              
              print(paste0("Finished MEDS download: ",buoy))
              # end writing to an output file
              # sink()
              # print(paste0("Finished NDBC download: ",buoy))
        }

        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
        # clean glob environ
        rm(list = ls())
        #----------------------------------------------------------------------------------------
        #----------------------------------------------------------------------------------------
}

