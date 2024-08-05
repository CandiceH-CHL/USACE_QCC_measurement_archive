create_best_data_5 <- function(buoys = "list of buoys", data_dir = "data dir"){
                        
      ##----------------------------------------------------------------------------------------
      ## script to select the best available data from the compared, geographically quality controlled and self-describing datasets 
      ## Hall, Candice
      ##----------------------------------------------------------------------------------------
      
      ## Actions:
      ## The 'create_best_data_5.R' script loads the 's_buoy#_ndbc/ncei_comp_geoClean.Rdata' container files created in the previous step. 
      ## The script tests for any pre-NDBC data that may have been included within the NCEI data sources and reduces any multiple sensor data to a single, best value for each variable. 
      ## The script then selects all available NDBC data files, and any non-common NCEI data files for inclusion in the best available dataset. 
      ## The selected datasets with self-describing metadata are saved in 's_buoy#_best_data.Rdata' container file within buoy station specific folders.
     
      #----------------------------------------------------------------------------------------
      #----------------------------------------------------------------------------------------
      # library(NCmisc)
      # list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)
     
      ## load libraries (local run)
      library(lubridate)
      library(dplyr)
      library(gridExtra)
      library(data.table)
      library(oce)
      library(naniar)
      library(tidyverse)
      library(broom)
      library(openair)
      library(graphics)
      library(utils)

      # # load libraries (HPC run)
      # library(lubridate, lib="/p/home/candice/Rlibs/")
      # library(crayon, lib="/p/home/candice/Rlibs/") # dplyr
      # library(pillar, lib="/p/home/candice/Rlibs/") # dplyr
      # library(dplyr, lib="/p/home/candice/Rlibs/")
      # library(gridExtra, lib="/p/home/candice/Rlibs/")
      # library(data.table, lib="/p/home/candice/Rlibs/")
      # # library(oce, lib="/p/home/candice/Rlibs/")
      # library(naniar, lib="/p/home/candice/Rlibs/")
      # library(readxl, lib="/p/home/candice/Rlibs/") # dependent of tidyverse
      # library(backports, lib="/p/home/candice/Rlibs/") # tidyverse
      # library(withr, lib="/p/home/candice/Rlibs") # tidyverse
      # library(cli, lib="/p/home/candice/Rlibs") # tidyverse
      # library(tzdb, lib="/p/home/candice/Rlibs") # tidyverse
      # library(readr, lib="/p/home/candice/Rlibs") # tidyverse
      # library(rstudioapi, lib="/p/home/candice/Rlibs") # tidyverse
      # library(tidyverse, lib="/p/home/candice/Rlibs/")
      # library(broom, lib="/p/home/candice/Rlibs/")
      # library(openair, lib="/p/home/candice/Rlibs/")
      # library(plotly, lib="/p/home/candice/Rlibs/")
      # # library(graphics, lib="/p/home/candice/Rlibs/")
      # # library(utils, lib="/p/home/candice/Rlibs/")
      
      ##----------------------------------------------------------------------------------------
      ## set paths
      ##----------------------------------------------------------------------------------------
      drive <- "D:/Candice/"
      # drive <- "/p/work/candice/"
      data_dir <- paste0(drive, "projects/WaveTrends/NDBC_2024/data/")
      
      setwd(data_dir)
      
      # set input directories
      geoClean_dir <- paste0(data_dir,"geoClean_data/data/")

      # set new output directories for datasets, stats and figures
      if (!file.exists(paste0(data_dir,"best_data/"))) {dir.create((paste0(data_dir,"best_data/")))}
      best_data_dir <- paste0(data_dir,"best_data/")
      
      # if (!file.exists(paste0(best_data_dir,"data/"))) {dir.create((paste0(best_data_dir,"data/")))}
      # best_dir <- paste0(best_data_dir,"data/")
      
      list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
      list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
      list_ndbc_buoy <- as.character(list_ndbc$station)

      buoys <- list_ndbc_buoy
      rm(list_ndbc, list_ndbc_buoy)
      print(buoys)

      ##----------------------------------------------------------------------------------------
      ## Create single, corrected data set from NDBC and NCEI data sources
      ##----------------------------------------------------------------------------------------
      list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
      list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
      list_ndbc_buoy <- as.character(list_ndbc$station)
      
      buoys <- list_ndbc_buoy
      rm(list_ndbc, list_ndbc_buoy)
      print(buoys)
      
      for(buoy in buoys){
           # buoy <- buoys[1]
        
           # if(buoy == 41046 | buoy == 51001){buffer_lim <- 3}else{buffer_lim <- 1}

           # start writing to an output file
           print(paste0("Starting on ",buoy))
         
           if(file.exists(paste0(geoClean_dir,buoy,"/s_",buoy,"_ndbc_comp_geoClean.RData"))){

                 # sink(paste0(data_dir,"5_create_best_data_",buoy,"_",Sys.Date(),".txt"))
                 # print(paste0("Starting on ",buoy))
                 
                 #----------------------------------------------------------------------------------------
                 ## read in data if not loaded in global environ
                 #------------------------------------------------------------ ----------------------------
                 
                 # Load data
                 load(paste0(geoClean_dir,buoy,"/s_",buoy,"_ndbc_comp_geoClean.RData"))
                 load(paste0(geoClean_dir,buoy,"/s_",buoy,"_ncei_comp_geoClean.RData"))

                 #----------------------------------------------------------------------------------------
                 ## build best available dataset
                 #----------------------------------------------------------------------------------------
                 # list ndbc data
                 ndbc_ls <- ls(pattern = paste0("s_",buoy,"_ndbc_"))
                 ndbc_ls <- ndbc_ls[ndbc_ls %in% grep(paste0("_geoClean", collapse = "|"), ndbc_ls, value = T)]
                 print(ndbc_ls)
                 # find common datasets
                 ndbc_ncei_common <- gsub("_ndbc_", "_ncei_", ndbc_ls)
                 print(ndbc_ncei_common)
                 # find ncei datasets
                 ncei_ls <- ls(pattern = paste0("s_",buoy,"_ncei_"))
                 print(ncei_ls)
                 # remove data already in ndbc list from ncei list
                 ncei_ls <- ncei_ls[!ncei_ls %in% grep(paste0(ndbc_ncei_common, collapse = "|"), ncei_ls, value = T)]
                 print(ncei_ls)
                 # select only geoClean data
                 ncei_ls <- ncei_ls[ncei_ls %in% grep(paste0("_geoClean", collapse = "|"), ncei_ls, value = T)]
                 print(ncei_ls)
                 # add station metadata and sensor output data
                 ncei_ls_met <- ls(pattern = "station_metadata_geoClean")
                 ncei_ls_sensor <- ls(pattern = "sensor_output_geoClean")
                 
                 best_ls <- unique(c(ndbc_ls, ncei_ls, ncei_ls_met,ncei_ls_sensor))
                 print(best_ls)
                 
                 if((length(ndbc_ls) + length(ncei_ls))-length(best_ls) == 0){
                      print("Successful best data collection")
                 }else{
                      print("WARNING: check best data collection")
                 }

                 #----------------------------------------------------------------------------------------
                 # exporting best datasets
                 #----------------------------------------------------------------------------------------
                 
                 # export and save geoClean dataset
                 print("exporting...")
                 if (!file.exists(paste0(best_data_dir,buoy,"/"))) {dir.create((paste0(best_data_dir,buoy,"/")))}
                 for(g in best_ls){
                      print(g)
                      if (!file.exists(paste0(best_data_dir,buoy,"/"))) {dir.create(paste0(best_data_dir,buoy,"/"))}
                      write.table(get(g), paste0(best_data_dir,buoy,"/",g,".csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                      saveRDS(get(g), paste0(best_data_dir,buoy,"/",g,".rds"))
                 }
                 # export to RData
                 
                 save(list = best_ls, file = paste0(best_data_dir,buoy,"/s_",buoy,"_best_data.RData"))
                 rm_ls <- ls(pattern = buoy)
                 rm(list = rm_ls)
      
                 #----------------------------------------------------------------------------------------
                 
                 print(paste0("Finished with Best data: ",buoy))
                 # sink()
                 # print(paste0("Finished with Best data: ",buoy))
                 
           }else{print('no new buoy data')}
           
      }
}
#----------------------------------------------------------------------------------------











