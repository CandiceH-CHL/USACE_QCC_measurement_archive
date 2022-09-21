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
      drive <- "G:/Candice/"
      # drive <- "/p/work/candice/"
      data_dir <- paste0(drive, "projects/WaveTrends/data/")
      
      setwd(data_dir)
      
      # set input directories
      geoClean_dir <- paste0(data_dir,"geoClean_data/data/")

      # set new output directories for datasets, stats and figures
      if (!file.exists(paste0(data_dir,"best_data/"))) {dir.create((paste0(data_dir,"best_data/")))}
      best_data_dir <- paste0(data_dir,"best_data/")
      
      if (!file.exists(paste0(best_data_dir,"data/"))) {dir.create((paste0(best_data_dir,"data/")))}
      best_dir <- paste0(best_data_dir,"data/")
      
      list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
      list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
      list_ndbc_buoy <- as.character(list_ndbc$station)

      buoys <- list_ndbc_buoy
      rm(list_ndbc, list_ndbc_buoy)
      print(buoys)

      ##----------------------------------------------------------------------------------------
      ## set set parameters common to all plots
      ##----------------------------------------------------------------------------------------
      buffer_lim <- 1
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
      list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
      list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
      list_ndbc_buoy <- as.character(list_ndbc$station)
      
      buoys <- list_ndbc_buoy
      rm(list_ndbc, list_ndbc_buoy)

      # buoys <- c("41001", "41002", "41004", "41006", "41008", "41009", "41012", "41013",
      #            "41018", "41021", "41022", "41023", "41025", "41035", "41036", "41040", "41041", "41043",
      #            "41044", "41046", "41047", "41048", "41049", "42001", "42002", "42003", "42007", "42012",
      #            "42019", "42020", "42035", "42036", "42038", "42039", "42040", "42041", "42042", "42053",
      #            "42054", "42055", "42056", "42057", "42058", "42059", "42060", "42065", "42080", "44004", 
      #            "44005", "44007", "44008", "44009", "44011", "44013", "44014", "44017", "44018", "44020",
      #            "44025", "44027", "44028", "44065", "44066", "44070", "45001", "45002", "45003", "45004", 
      #            "45005", "45006", "45007", "45008", "45011", "45012", "46001", "46002", "46003",
      #            "46005", "46006", "46011", "46012", "46013",# "46014", 
      #            "46015", "46022", "46023", "46025", 
      #            "46026", "46027", "46028", "46029", "46030", "46035", "46041", "46042", "46045", "46047", 
      #            "46050", "46051", #"46053", 
      #            "46054", "46059", "46060", "46061", "46062", "46063", "46066", 
      #            "46069", "46070", "46071", "46072", "46073", "46075", "46076", "46077", "46078", "46079", 
      #            "46080", "46081", "46082", "46083", "46084", "46085", "46086", "46087", "46088", "46089",
      #            "46105", "46106", "46107", "48011", "51000", "51001", "51002", "51003", "51004", "51026",
      #            "51028", "51100", "51101", "52009")
      print(buoys)
      
      for(buoy in buoys){
           # buoy <- buoys[3]
        
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
                      print("Successful best data collection")
                 }else{
                      print("WARNING: check best data collection")
                 }
                 
                 #----------------------------------------------------------------------------------------
                 # re-filter for GPS positions
                 #----------------------------------------------------------------------------------------
                 print("geoCleaning...")
                 ndbc_pos <- get(ls(pattern = "ndbc_stdmet_geoClean"))
                 ndbc_pos <- dplyr::select(ndbc_pos, DateTime, lat, lon)
                 # using a sorted table of value occurrences to find most common lat/lon
                 GPS_buffer <- buffer_lim
                 lat_tail <- tail(names(sort(table(ndbc_pos$lat))),1)
                 lon_tail <- tail(names(sort(table(ndbc_pos$lon))),1)
                 
                 for(g in best_ls){
                      # g <- best_ls[12]
                      print(g)
                      df <- get(g)
                      if("lat" %in% names(df)){
                           # using a sorted table of value occurrences to filter lat/lon
                           df <- dplyr::filter(df, lat >= as.numeric(lat_tail)-as.numeric(GPS_buffer) & lat <= as.numeric(lat_tail)+as.numeric(GPS_buffer))
                           df <- dplyr::filter(df, lon >= as.numeric(lon_tail)-as.numeric(GPS_buffer) & lon <= as.numeric(lon_tail)+as.numeric(GPS_buffer))
                           if(dim(df)[1] == 0 & grepl("cols_", g)){
                             rm(df)
                             best_ls <- best_ls[!best_ls %in% g]
                           }else if(dim(df)[1] == 0 & grepl("_preNDBC_", g)){
                             rm(df)
                             best_ls <- best_ls[!best_ls %in% g]
                           }else{
                             # ordering the df by date and selecting unique values only
                             df <- df[order(df$DateTime),]
                             df <- unique(df)
                             # rename the rows to reflect unique data
                             row.names(df) <- 1:nrow(df)
                             assign(g,df)
                           }
                      }
                      rm(df)
                 }
                 rm(ndbc_pos, GPS_buffer, lat_tail, lon_tail, g)
                 
                 #----------------------------------------------------------------------------------------
                 # exporting best datasets
                 #----------------------------------------------------------------------------------------
                 
                 # export and save geoClean dataset
                 print("exporting...")
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
                 # sink()
                 # print(paste0("Finished with Best data: ",buoy))
                 
           }else{print('no new buoy data')}
           
      }
}
#----------------------------------------------------------------------------------------











