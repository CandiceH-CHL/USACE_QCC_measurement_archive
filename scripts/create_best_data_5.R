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

      ## load libraries (HPC run)
      # library(lubridate, lib="/p/home/candice/Rlibs/")
      # library(dplyr, lib="/p/home/candice/Rlibs/")
      # library(gridExtra, lib="/p/home/candice/Rlibs/")
      # library(data.table, lib="/p/home/candice/Rlibs/")
      # # library(oce, lib="/p/home/candice/Rlibs/")
      # library(naniar, lib="/p/home/candice/Rlibs/")
      # library(tidyverse, lib="/p/home/candice/Rlibs/")
      # library(broom, lib="/p/home/candice/Rlibs/")
      # library(openair, lib="/p/home/candice/Rlibs/") 

      ##----------------------------------------------------------------------------------------
      ## set paths
      ##----------------------------------------------------------------------------------------
      # drive <- "E:/Candice/"
      # drive <- "/p/work/candice/"
      
      setwd(data_dir)
      
      # set input directories
      geoClean_dir <- paste0(data_dir,"geoClean_data/data/")

      # set new output directories for datasets, stats and figures
      if (!file.exists(paste0(data_dir,"best_data/"))) {dir.create((paste0(data_dir,"best_data/")))}
      best_data_dir <- paste0(data_dir,"best_data/")
      
      if (!file.exists(paste0(best_data_dir,"data/"))) {dir.create((paste0(best_data_dir,"data/")))}
      best_dir <- paste0(best_data_dir,"data/")

      ##----------------------------------------------------------------------------------------
      ## set set parameters common to all plots
      ##----------------------------------------------------------------------------------------
      xlab = "Date"   # label for x axis
      width = 2000    # width of exported plot
      height = 1500   # height of exported plot
      res = 300
      # set plot parameters
      width1 = 1000
      height1 = 700
      par1 = c(5,5,4,4)
      
      # colors for each buoy
      color_sd <- "red"
      color_ref <- "black"
      color_test <- "blue"
      # plot type, symbol, size
      type = "p"
      pch = 20
      lwd = 1
      cex = 0.5
      # set parameters
      Delta <- '\U0394'
      degree <- '\U00B0'
      # my.grid function formats
      my.format <- "%m-%d-%Y" # "%m-%Y" (long datasets) or "%m-%d-%Y" (short datasets)
      my.period <- "weeks" # "months" (long datasets) or "weeks (short datasets)
      # my grid function for plots
      my.grid <-function(dataset, my.period = "year", my.format = "%Y"){
           grid(nx=NA, ny=NULL)
           abline(v=axis.POSIXct(1, at=seq(min(dataset[1,1]), max(dataset[nrow(dataset),1]), 
                                           by= my.period), format=my.format),
                  col = "lightgray", lty = "dotted", lwd = par("lwd"))
      }
      # function to capitalize string
      simpleCap <- function(x) {
           s <- strsplit(x, " ")[[1]]
           paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" ")
      }
      
      ##----------------------------------------------------------------------------------------
      ## Create single, corrected data set from NDBC and NCEI data sources
      ##----------------------------------------------------------------------------------------

      print(buoys)
      
      for(buoy in buoys){

           # start writing to an output file
           print(paste0("Starting on ",buoy))
         
           if(file.exists(paste0(geoClean_dir,buoy,"/s_",buoy,"_ndbc_comp_geoClean.RData"))){

                 sink(paste0(data_dir,"4_create_best_data_",buoy,"_",Sys.Date(),".txt"))
                 print(paste0("Starting on ",buoy))
                 
                 #----------------------------------------------------------------------------------------
                 ## read in data if not loaded in global environ
                 #----------------------------------------------------------------------------------------
                 
                 # Load data
                 load(paste0(geoClean_dir,buoy,"/s_",buoy,"_ndbc_comp_geoClean.RData"))
                 load(paste0(geoClean_dir,buoy,"/s_",buoy,"_ncei_comp_geoClean.RData"))
                 
                 #----------------------------------------------------------------------------------------
                 ## reduce multiple sensors in pre_NDBC NCEI stdmet data to best single sensor values
                 #----------------------------------------------------------------------------------------
                 
                 # load stdmet data
                 preNDBC_ls <- ls(pattern = "_ncei_stdmet_preNDBC_")
                 if(length(preNDBC_ls)>0){
                      dat <- get(preNDBC_ls[1])
                      # clear loaded data from input vector
                      col_names <- names(dat)
                      secondary_sensor_ls <- col_names[col_names %like% '_2']; secondary_sensor_ls <- secondary_sensor_ls[!grepl('_metadata_', secondary_sensor_ls)]
                      if("wind_direction_2" %in% col_names){if(sum(is.na(dat$wind_direction_2)) == nrow(dat)){dat$wind_direction_2 <-NULL; dat$wind_direction_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_direction=wind_direction_1, wind_direction_metadata=wind_direction_metadata_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$wind_direction_1[i]) & !is.na(dat$wind_direction_2[i])){dat$wind_direction_1[i]=dat$wind_direction_2[i]; dat$wind_direction_metadata_1[i]=dat$wind_direction_metadata_2[i]
                      dat$wind_direction_2 <-NULL; dat$wind_direction_metadata_2 <- NULL;dat <- dplyr::rename(dat, wind_direction=wind_direction_1, wind_direction_metadata=wind_direction_metadata_1)}}}
                      }
                      if("wind_speed_2" %in% col_names){if(sum(is.na(dat$wind_speed_2)) == nrow(dat)){dat$wind_speed_2 <-NULL; dat$wind_speed_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_speed=wind_speed_1, wind_speed_metadata=wind_speed_metadata_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$wind_speed_1[i]) & !is.na(dat$wind_speed_2[i])){dat$wind_speed_1[i]=dat$wind_speed_2[i]; dat$wind_speed_metadata_1[i]=dat$wind_speed_metadata_2[i]
                      dat$wind_speed_2 <-NULL; dat$wind_speed_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_speed=wind_speed_1, wind_speed_metadata=wind_speed_metadata_1)}}}
                      }
                      if("wind_gust_2" %in% col_names){if(sum(is.na(dat$wind_gust_2)) == nrow(dat)){dat$wind_gust_2 <-NULL; dat$wind_gust_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_gust=wind_gust_1, wind_gust_metadata=wind_gust_metadata_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$wind_gust_1[i]) & !is.na(dat$wind_gust_2[i])){dat$wind_gust_1[i]=dat$wind_gust_2[i]; dat$wind_gust_metadata_1[i]=dat$wind_gust_metadata_2[i]
                      dat$wind_gust_2 <-NULL; dat$wind_gust_metadata_2 <- NULL; dat <- dplyr::rename(dat, wind_gust=wind_gust_1, wind_gust_metadata=wind_gust_metadata_1)}}}
                      }
                      if("air_pressure_at_sea_level_2" %in% col_names){if(sum(is.na(dat$air_pressure_at_sea_level_2)) == nrow(dat)){dat$air_pressure_at_sea_level_2 <-NULL; dat$air_pressure_at_sea_level_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_pressure_at_sea_level=air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata=air_pressure_at_sea_level_metadata_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$air_pressure_at_sea_level_1[i]) & !is.na(dat$air_pressure_at_sea_level_2[i])){dat$air_pressure_at_sea_level_1[i]=dat$air_pressure_at_sea_level_2[i]; dat$air_pressure_at_sea_level_metadata_1[i]=dat$air_pressure_at_sea_level_metadata_2[i]
                      dat$air_pressure_at_sea_level_2 <-NULL; dat$air_pressure_at_sea_level_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_pressure_at_sea_level=air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata=air_pressure_at_sea_level_metadata_1)}}}
                      }
                      if("air_temperature_2" %in% col_names){if(sum(is.na(dat$air_temperature_2)) == nrow(dat)){dat$air_temperature_2 <-NULL; dat$air_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_temperature=air_temperature_1, air_temperature_metadata=air_temperature_metadata_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$air_temperature_1[i]) & !is.na(dat$air_temperature_2[i])){dat$air_temperature_1[i]=dat$air_temperature_2[i]; dat$air_temperature_metadata_1[i]=dat$air_temperature_metadata_2[i]
                      dat$air_temperature_2 <-NULL; dat$air_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, air_temperature=air_temperature_1, air_temperature_metadata=air_temperature_metadata_1)}}}
                      }
                      if("dew_point_temperature_2" %in% col_names){if(sum(is.na(dat$dew_point_temperature_2)) == nrow(dat)){dat$dew_point_temperature_2 <-NULL; dat$dew_point_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, dew_point_temperature=dew_point_temperature_1, dew_point_temperature_metadata=dew_point_temperature_metadata_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$dew_point_temperature_1[i]) & !is.na(dat$dew_point_temperature_2[i])){dat$dew_point_temperature_1[i]=dat$dew_point_temperature_2[i]; dat$dew_point_temperature_metadata_1[i]=dat$dew_point_temperature_metadata_2[i]
                      dat$dew_point_temperature_2 <-NULL; dat$dew_point_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, dew_point_temperature=dew_point_temperature_1, dew_point_temperature_metadata=dew_point_temperature_metadata_1)}}}
                      }
                      if("sea_surface_temperature_2" %in% col_names){if(sum(is.na(dat$sea_surface_temperature_2)) == nrow(dat)){dat$sea_surface_temperature_2 <-NULL; dat$sea_surface_temperature_metadata_2 <- NULL; dat <- dplyr::rename(dat, sea_surface_temperature=sea_surface_temperature_1, sea_surface_temperature_metadata=sea_surface_temperature_1)
                      }else{for(i in 1:nrow(dat)){if(is.na(dat$sea_surface_temperature_1[i]) & !is.na(dat$sea_surface_temperature_2[i])){dat$sea_surface_temperature_1[i]=dat$sea_surface_temperature_2[i]; dat$sea_surface_temperature_metadata_1[i]=dat$sea_surface_temperature_metadata_2[i]
                      dat$sea_surface_temperature_2 <-NULL; dat$sea_surface_temperature_2 <- NULL; dat <- dplyr::rename(dat, sea_surface_temperature=sea_surface_temperature_1, sea_surface_temperature_metadata=sea_surface_temperature_metadata_1)}}}
                      }
                 }
      
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
                      print("Succesful best data collection")
                 }else{
                      print("WARNING: check best data collection")
                 }
                 
                 #----------------------------------------------------------------------------------------
                 # exporting best datasets
                 #----------------------------------------------------------------------------------------
                 
                 # export and save geoClean dataset
                 if (!file.exists(paste0(best_dir,buoy,"/"))) {dir.create((paste0(best_dir,buoy,"/")))}
                 for(g in best_ls){
                      print(g)
                      write.table(get(g), paste0(best_dir,buoy,"/",g,".csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                      saveRDS(get(g), paste0(best_dir,buoy,"/",g,".rds"))
                 }
                 # export to RData
                 save(list = best_ls, file = paste0(best_dir,buoy,"/s_",buoy,"_best_data.RData"))
                 rm_ls <- ls(pattern = buoy)
                 rm(list = rm_ls)
      
                 #----------------------------------------------------------------------------------------
                 
                 print(paste0("Finished with Best data: ",buoy))
                 sink()
                 print(paste0("Finished with Best data: ",buoy))
                 
           }else{print('no new buoy data')}
           
      }
}
#----------------------------------------------------------------------------------------











