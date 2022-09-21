geoClean_data_4 <- function(buoys = "list of buoys", data_dir = "data_dir"){
      
      ##----------------------------------------------------------------------------------------
      ## Compares and geographically cleans data before verifying metadata by combining NDBC web files and NCEI netcdf files
      ## Hall, Candice
      ##----------------------------------------------------------------------------------------
      
      ## Actions:
      ## 1.  Sets data locations
      ## 2.  Read in NDBC and NCEI data if not already loaded in global environ
      ## 3.  Compares the NDBC and NCEI sourced data by matching the datasets on 'nearest' date and time. 
      ## 4.  This step geographically quality controls each dataset by removing GPS positions and 
      ##     associated data that are not within a pre-selected radius of the NDBC station watch circles. 
      ## 5.  This step assigns verified metadata to the NDBC stdmet datasets.
      ## 6.  If desired, the final section of code within the 'geoClean_data_4.R' script produces statistical 
      ##     comparisons and plots of the NDBC and NCEI datasets. A 'switch_set' turns this functionality 
      ##     on and off during stand-alone use of this script. To activate this functionality in HPC runs, the 
      ##     'geoClean_data_4.R' will require and additional 'switch-set' input value to function. 

      #----------------------------------------------------------------------------------------
      #----------------------------------------------------------------------------------------
      # library(NCmisc)
      # list.functions.in.file(rstudioapi::getSourceEditorContext()$path, alphabetic = TRUE)
     
      ## load libraries (local run)
      library(lubridate)
      library(plyr)
      library(dplyr)
      library(gridExtra)
      library(data.table)
      # library(oce)
      library(naniar)
      library(tidyverse)
      library(broom)
      library(openair) # polarplots
      library(plotly)
      library(magrittr)
      library(tidyr)
      # library(grid)
      # library(devtools)
      library(lsr)
      library(stringr)
      library(RColorBrewer)
      library(viridis)
      library(colorRamps)
      library(ggplot2)
      library(ggmap)
      library(maps)
      library(mapdata)
      library(modeest)
      library(tibble)
      library(graphics)
      library(grDevices)
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
      # library(gridExtra, lib="/p/home/candice/Rlibs/")
      # library(broom, lib="/p/home/candice/Rlibs/")
      # library(R.methodsS3, lib="/p/home/candice/Rlibs/")
      # library(R.oo, lib="/p/home/candice/Rlibs/")
      # library(R.utils, lib="/p/home/candice/Rlibs/")
      # library(openair, lib="/p/home/candice/Rlibs/") # polarplots *
      # library(magrittr, lib="/p/home/candice/Rlibs/")
      # library(lsr, lib="/p/home/candice/Rlibs/")
      # library(RColorBrewer, lib="/p/home/candice/Rlibs/")
      # library(viridisLite, lib="/p/home/candice/Rlibs") # viridis
      # library(viridis, lib="/p/home/candice/Rlibs/")
      # library(colorRamps, lib="/p/home/candice/Rlibs/")
      # library(ggplot2, lib="/p/home/candice/Rlibs/")
      # library(sp, lib="/p/home/candice/Rlibs/") # ggmap
      # library(RgoogleMaps, lib="/p/home/candice/Rlibs") # ggmap
      # library(bitops, lib="/p/home/candice/Rlibs") # ggmap
      # library(rjson, lib="/p/home/candice/Rlibs") # ggmap
      # library(ggmap, lib="/p/home/candice/Rlibs/")
      # library(maps, lib="/p/home/candice/Rlibs/")
      # library(mapdata, lib="/p/home/candice/Rlibs/")
      # library(tibble, lib="/p/home/candice/Rlibs/")
      # library(scriptName, lib="/p/home/candice/Rlibs/")
      # library(timeDate, lib="/p/home/candice/Rlibs/") # modeest
      # library(timeSeries, lib="/p/home/candice/Rlibs/") # modeest
      # library(fBasics, lib="/p/home/candice/Rlibs/") # modeest
      # library(rmutil, lib="/p/home/candice/Rlibs/") # modeest
      # library(stable, lib="/p/home/candice/Rlibs/") # modeest
      # library(stabledist, lib="/p/home/candice/Rlibs/") # modeest
      # library(clue, lib="/p/home/candice/Rlibs/") # modeest
      # library(statip, lib="/p/home/candice/Rlibs/") # modeest
      # library(modeest, lib="/p/home/candice/Rlibs/") #*
      # library(plotly, lib="/p/home/candice/Rlibs/")
      # library(graphics, lib="/p/home/candice/Rlibs/")
      # library(grDevices, lib="/p/home/candice/Rlibs/")
      # library(stats, lib="/p/home/candice/Rlibs/")
      # library(utils, lib="/p/home/candice/Rlibs/")
      
      #----------------------------------------------------------------------------------------
      #----------------------------------------------------------------------------------------
      
      # set switch for script actions
      # 1 == plot/stats for geoClean dataset creation, 2 == don't plot/stats
      # switch_set <- 1
      switch_set_a <- 2
      # GPS buffer
      GPS_buffer <- 3
      
      ##----------------------------------------------------------------------------------------
      ## set paths
      ##----------------------------------------------------------------------------------------
      drive <- "G:/Candice/"
      # drive <- "/p/work/candice/"
      # data_dir <- paste0(drive, "projects/WaveTrends/annual_runs/data/")
      data_dir <- paste0(drive, "projects/WaveTrends/data/")
      
      setwd(data_dir)
      
      # set input directories
      input_dir <- paste0(data_dir,"concat_data/")
      # ndbc 
      ndbc_dir <- paste0(input_dir,"ndbc/")
      # ncei 
      ncei_dir <- paste0(input_dir,"ncei/")

      # set new output directories for datasets, stats and figures
      if (!file.exists(paste0(data_dir,"geoClean_data/"))) {dir.create((paste0(data_dir,"geoClean_data/")))}
      clean_data_dir <- paste0(data_dir,"geoClean_data/")
      
      # set new output directories for datasets, stats and figures
      if (!file.exists(paste0(clean_data_dir,"results/"))) {dir.create((paste0(clean_data_dir,"results/")))}
      out_dir <- paste0(clean_data_dir,"results/")
      if (!file.exists(paste0(clean_data_dir,"data/"))) {dir.create((paste0(clean_data_dir,"data/")))}
      clean_dir <- paste0(clean_data_dir,"data/")
      if (!file.exists(paste0(out_dir,"stats/"))) {dir.create((paste0(out_dir,"stats/")))}
      stats_dir <- paste0(out_dir,"stats/")
      if (!file.exists(paste0(out_dir,"figures/"))) {dir.create((paste0(out_dir,"figures/")))}
      fig_dir <- paste0(out_dir,"figures/")
      if (!file.exists(paste0(out_dir,"GPS_plots/"))) {dir.create((paste0(out_dir,"GPS_plots/")))}
      gps_dir <- paste0(out_dir,"GPS_plots/")
      
      ##----------------------------------------------------------------------------------------
      ## set set parameters common to all plots
      ##----------------------------------------------------------------------------------------
      xlab = "Date"   # label for x axis
      width = 2000+2000    # width of exported plot
      height = 1500+1500   # height of exported plot
      res = 300
      # res2 = 500
      # set plot parameters
      width1 = 1000
      height1 = 700
      par1 = c(5,5,4,4)
      
      # # colors for each buoy
      plot_colors <- viridis(n=6)
      color_ndbc_raw <- plot_colors[1]
      color_ndbc_orig <- plot_colors[5]    #"#440154FF" # purple
      color_ndbc_recalc <- plot_colors[3] # "#3B528BFF"  #"5 = #FDE725FF" # yellow
      color_chl_calc <- plot_colors[4]  # "#21908CFF"   #"#21908CFF" # green
      color_WIS <- "red"     # "#5DC863FF"
      
      # wind/wave polar plots
      colour1 <- viridis(n=4)
      cols1 <- c(colour1[4], colour1[3], colour1[2], colour1[1])
      colour2 <- viridis(n=5)
      cols2 <- c(colour2[5], colour2[4], colour2[3], colour2[2], colour2[1])
      type = "l"
      pch = "."
      lwd = 0.5
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
      
      # The choice of significance level at which you reject H0 is arbitrary. 
      sig <- 0.01
      
      ##----------------------------------------------------------------------------------------
      ## set buoy stations for downloading (for stand-alone use)
      ##----------------------------------------------------------------------------------------
      
      list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
      list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
      list_ndbc_buoy <- as.character(list_ndbc$station)

      buoys <- list_ndbc_buoy
      rm(list_ndbc, list_ndbc_buoy)
      print(buoys)
      ##----------------------------------------------------------------------------------------

      for(buoy in buoys){
          # buoy <- buoys[2]
        
          # start writing to an output file
          print(paste0("Starting on ",buoy))
          
          #----------------------------------------------------------------------------------------
          ## read in data if not loaded in global environ
          #----------------------------------------------------------------------------------------
          
          if(file.exists(paste0(ndbc_dir,buoy,"/s_",buoy,"_ndbc_ALL.RData"))){
            
                # start writing to an output file
                # sink(paste0(data_dir,"4_geoClean_data_",buoy,"_",Sys.Date(),".txt"))
                print(paste0("Starting on... ",buoy))
            
                print("Loading datasets")
            
                load(paste0(ndbc_dir,buoy,"/s_",buoy,"_ndbc_ALL.RData"))
                load(paste0(ncei_dir,buoy,"/s_",buoy,"_ncei_ALL_verified.RData"))
                
                #-------------------------------------------------
                # check to remove empty dataframes, if any
                #-------------------------------------------------
                print("checking for empty dataframes")
                dat_list <- ls(pattern = buoy)
                print(dat_list)
                for(d in dat_list){
                    dat <- get(d)
                    dat <- data.frame(dat, stringsAsFactors = FALSE)
                    if(dim(dat)[1]<= 1){
                        rm(list = ls()[grepl(d, ls())])
                        print(paste0("removing empty df: ",d))
                    }else{print(paste0(d, " is not empty"))}
                    rm(dat)
                }
                
                #-------------------------------------------------
                # isolating common metadata from verified ncei df
                #-------------------------------------------------
                print("Handling verified ncei metadata")
                # rename non verified data
                ncei_list <- ls(pattern = "_ncei_stdmet")
                ncei_list <- ncei_list[!ncei_list %in% grep(paste0("_verified", collapse = "|"), ncei_list, value = T)]
                print(ncei_list) 
                dat <- get(ncei_list)
                name_net <- paste0("s_",buoy,"_ncei_sdmet_netcdf")
                assign(name_net, dat)
                
                print("isolating common metadata from verified ncei df")
                metadata <- ls(pattern = "_verified")
                print(metadata) 
                df <- data.frame(get(metadata), stringsAsFactors = FALSE)
                drop_cols <- c("DateTime", "lat", "lon","depth", "mooring", "hull", "payload")
                meta_df <- dplyr::select(df,all_of(drop_cols))
                df <- df[, !names(df) %in% c("depth", "mooring", "hull", "payload")]
                # save data
                assign(ncei_list, df)
                metadata <- gsub("_stdmet_verified", "_station_metadata", metadata)
                assign(metadata, meta_df)
                # remove original data
                rm(list = ls(pattern = "_verified"))
                rm(ncei_list,dat,df, meta_df, metadata, drop_cols)
            
                #----------------------------------------------------------------------------------------
                ## create data count table
                #----------------------------------------------------------------------------------------
                data_count <- t(data.frame("Original_count", "Original_start", "Original_end",
                                           "Comparison_count", "Comparison_start",  "Comparison_end",
                                           "GeoCleaned_count", "GeoCleaned_start", "GeoCleaned_end"))
                
                # rm(Original_count, Original_start, Original_end,Comparison_count,Comparison_start,Comparison_end,GeoCleaned_count,GeoCleaned_start,GeoCleaned_end)
                data_count <- data.frame(data_count, stringsAsFactors = FALSE)
                
                #----------------------------------------------------------------------------------------
                ## geoclean matching data
                #----------------------------------------------------------------------------------------
                print("GeoCleaning matching datasets")
                ncei_list <- ls(pattern = "_ncei_")
                ncei_list <- ncei_list[!ncei_list %in% grep(paste0("sensor_output", collapse = "|"), ncei_list, value = T)]
                print(ncei_list)    
                colNames <- c("DateTime", "lat", "lon")
                ncei_positions <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames)))
                colnames(ncei_positions) <- colNames
                ncei_positions$DateTime <- lubridate::ymd_hms(ncei_positions$DateTime)
                
                # selection matching positions
                for(n in ncei_list){
                      print(n)
                      dat <- get(n)
                      column_names <- names(dat)
                      if("lat" %in% column_names){
                          dat <- dplyr::select(dat,DateTime,lat,lon)
                          dat$lat <- as.numeric(dat$lat)
                          dat$lon <- as.numeric(dat$lon)
                          ncei_positions <- rbind(ncei_positions,dat)
                      }else{print(paste0("no GPS data in ", n))}
                      rm(dat)
                }
                # find unique
                ncei_positions <- unique(ncei_positions)
                # ordering the df by date and selecting unique values only
                ncei_positions <- ncei_positions[order(ncei_positions$DateTime),]
                ncei_positions <- unique(ncei_positions)
                # rename the rows to reflect unique data
                row.names(ncei_positions) <- 1:nrow(ncei_positions)
                # remove bad data
                ncei_positions <- ncei_positions[complete.cases(ncei_positions),]
                
                # remove bad netcdf GPS positions and correct ncei datafiles
                library(modeest)
                # using a sorted table of value occurrences to find most common lat/lon
                lat_tail <- tail(names(sort(table(ncei_positions$lat))),1)
                lon_tail <- tail(names(sort(table(ncei_positions$lon))),1)
                print(paste0("sorted table method - lat: ", lat_tail, "; lon: ", lon_tail))
                # check positions in ndbc bulk
                if(abs(range(ncei_positions$lat, na.rm = TRUE)[1] - range(ncei_positions$lat, na.rm = TRUE)[2]) > GPS_buffer | abs(range(ncei_positions$lon, na.rm = TRUE)[1] - range(ncei_positions$lon, na.rm = TRUE)[2]) > GPS_buffer){
                    print(paste0("range - lat: ", range(ncei_positions$lat, na.rm = TRUE)[1],"; ", range(ncei_positions$lat, na.rm = TRUE)[2]))
                    print(paste0("range - lon: ", range(ncei_positions$lon, na.rm = TRUE)[1],"; ", range(ncei_positions$lon, na.rm = TRUE)[2]))
                    ncei_positions <- dplyr::filter(ncei_positions, lat >= as.numeric(lat_tail)-as.numeric(GPS_buffer) & lat <= as.numeric(lat_tail)+as.numeric(GPS_buffer))
                    ncei_positions <- dplyr::filter(ncei_positions, lon >= as.numeric(lon_tail)-as.numeric(GPS_buffer) & lon <= as.numeric(lon_tail)+as.numeric(GPS_buffer))
                    # ordering the df by date and selecting unique values only
                    ncei_positions <- ncei_positions[order(ncei_positions$DateTime),]
                    ncei_positions <- unique(ncei_positions)
                    # rename the rows to reflect unique data
                    row.names(ncei_positions) <- 1:nrow(ncei_positions)
                    # remove bad data
                    ncei_positions <- ncei_positions[complete.cases(ncei_positions),]
                    
                    # apply corrected positions for ncei data
                    for(n in ncei_list){
                        print(n)
                        dat <- get(n)
                        column_names <- names(dat)
                        if("lat" %in% column_names){
                            dat$lat <- NULL; dat$lon <- NULL
                            dat <- left_join(dat, ncei_positions, by = "DateTime")
                            dat <- dplyr::select(dat, all_of(column_names))
                            assign(n,dat)
                        }else{print(paste0("no GPS data in ", n))}
                        rm(dat)
                    }
                }
      
                # export as csv and rds
                print("saving original GPS data")
                if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                # write.table(ncei_positions, paste0(clean_dir,buoy,"/s_",buoy,"_GPS_ALL.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                # saveRDS(ncei_positions, file = paste0(clean_dir,buoy,"/s_",buoy,"_GPS_ALL.rds"))
                # rm(ncei_positions)
            
                # list out the NDBC and NCEI data
                ndbc_list <- ls(pattern = paste0("s_",buoy,"_ndbc_"))
                print(ndbc_list)
                ndbc_ls <- vector()
                remainder <- vector()
                
                # load individual datasets and geoclean pairs
                for(n in ndbc_list){
                    # n <- ndbc_list[6]
                    name_export <- n
                    print(name_export)
                    
                    # find matching datasets
                    ndbc <- get(n)
                    ncei_name <- gsub("ndbc_", "ncei_",n)
                    tryCatch({dat <- get(ncei_name)},error=function(cond){print(paste0("no matching ncei data for ", n))})
                    if(exists("dat")==TRUE){
                          if(grepl("cols", ncei_name, fixed = TRUE)){
                              if("lat" %in% names(ndbc)){
                                    ncei <- get(ls(pattern = gsub("ndbc_", "ncei_",n)))
                              }else{
                                  ncei_count <- as.numeric(gsub("cols","",unlist(strsplit(n,"_freq_"))[2]))+2
                                  ncei_name2 <- gsub("ndbc_", "ncei_",paste0(unlist(strsplit(n,"_freq_"))[1],"_freq_",ncei_count,"cols"))
                                  # check if ncei data exists
                                  if(exists(ncei_name2)){
                                      ncei <- get(ls(pattern = ncei_name2))
                                  }else{
                                      print("no matching ncei data")
                                      ndbc_ls <- c(ndbc_ls,n)
                                  }
                                  rm(ncei_count, ncei_name2)
                              }
                          }else{
                              ncei <- get(ls(pattern = gsub("ndbc_", "ncei_",n)))
                          }
                          rm(ncei_name)
                    }else {remainder <- c(remainder,n)}
                    
                    # handling ndbc data that has no matching ncei data
                    if(exists("ncei")){
                          # remove blank rows
                          ndbc <- ndbc[rowSums(is.na(ndbc)) != ncol(ndbc)-1,]
                          ncei <- ncei[rowSums(is.na(ncei)) != ncol(ncei)-1,]
                          positions <- dplyr::select(ncei, DateTime, lat,lon)
                          # original dfs
                          ncei_orig <- ncei #<- ncei_orig
                          ndbc_orig <- ndbc #<- ndbc_orig
                          ncei_orig_count <- nrow(ncei_orig); ncei_orig_start <- min(ncei_orig$DateTime, na.rm = TRUE); ncei_orig_end <- max(ncei_orig$DateTime, na.rm = TRUE)
                          ndbc_orig_count <- nrow(ndbc_orig); ndbc_orig_start <- min(ndbc_orig$DateTime, na.rm = TRUE); ndbc_orig_end <- max(ndbc_orig$DateTime, na.rm = TRUE)
      
                          # checking geographical positions for service visits and buoy adrift, i.e. not in watch circle
                          # removing multiple significant places that skew the sorted table results
                          positions$lat <- as.numeric(positions$lat); positions$lon <- as.numeric(positions$lon)
                          positions$lat <- signif(positions$lat,3)
                          positions$lon <- signif(positions$lon,3)
      
                          # two methods of finding mode
                          library(modeest)
                          lat_mean <- mfv(positions$lat, na_rm = TRUE)
                          lon_mean <- mfv(positions$lon, na_rm = TRUE)
                          print(paste0("mean method - lat: ", lat_mean, "; lon: ", lon_mean))
                          # using a sorted table of value occurrences to find most common lat/lon
                          lat_tail <- tail(names(sort(table(positions$lat))),1)
                          lon_tail <- tail(names(sort(table(positions$lon))),1)
                          print(paste0("sorted table method - lat: ", lat_tail, "; lon: ", lon_tail))
                          # check positions in ndbc bulk
                          print(paste0("range - lat: ", range(positions$lat, na.rm = TRUE)[1],"; ", range(positions$lat, na.rm = TRUE)[2]))
                          print(paste0("range - lon: ", range(positions$lon, na.rm = TRUE)[1],"; ", range(positions$lon, na.rm = TRUE)[2]))
                          rm(positions)
      
                          # matching time setkeys to only include geoClean data times from NDBC website
                          # set as data.tables
                          library(data.table)
                          ncei_dt <- data.table(dplyr::select(ncei,DateTime,lat,lon))
                          ndbc_dt <- data.table(dplyr::select(ndbc,DateTime))
                          setkey(ncei_dt,DateTime)
                          setkey(ndbc_dt,DateTime)
      
                          # manipulate and subset to include common data, using NDBC as ref dataset
                          ncei_dt$date <- ncei_dt$DateTime
                          match_dt <- ncei_dt[ndbc_dt, roll = "nearest" ]
                          # removing rows with no dates
                          match_dt <- match_dt[!with(match_dt,is.na(DateTime)| is.na(date)),]
                          match_dt$DateTime <- match_dt$date
                          match_dt <- match_dt[,1:3]
                          # subset
                          ncei2 <- data.table(ncei)
                          ndbc2 <- data.table(ndbc)
                          setkey(ncei2,DateTime)
                          setkey(ndbc2,DateTime)
                          ncei <- data.frame(ncei2[match_dt, roll = "nearest" ], stringsAsFactors = FALSE)
                          ndbc <- data.frame(ndbc2[match_dt, roll = "nearest" ], stringsAsFactors = FALSE)
                          # remove extra lat/lon in ndbc data
                          ncei <- ncei[ , -which(names(ncei) %in% c("i.lat", "i.lon"))]
                          # remove x in column names
                          colnames(ndbc) <- gsub("X", "", colnames(ndbc))
                          colnames(ncei) <- gsub("X", "", colnames(ncei))
                          # remove 0.0100 column if empty
                          if("0.0100" %in% names(ncei)){if(sum(is.na(ncei$`0.0100`))==nrow(ncei)){ncei$`0.0100` <- NULL}}
                          # re-order datasets
                          idcols <- names(ncei)
                          # formatting datasets
                          if(unlist(strsplit(n,"_"))[4]== "stdmet"){
                                ncei_metadata <- ncei
                                idcols <- str_subset(idcols, "_metadata", negate = TRUE)
                                idcols <- str_subset(idcols, "_2", negate = TRUE)
                                idcols <- gsub("_1", "", idcols)
                          }
                          if(grepl("_freq_",name_export)){
                              if("0.0200" %in% names(ndbc)){ndbc <- dplyr::select(ndbc, all_of(idcols))
                              }else{ndbc$`0.0200` <- NA;ndbc <- dplyr::select(ndbc, all_of(idcols))}
                          }
      
                          # save comparable datasets
                          ncei_comp <- ncei; ndbc_comp <- ndbc
                          ncei_comp_count <- nrow(ncei_comp); ncei_comp_start <- min(ncei_comp$DateTime, na.rm = TRUE); ncei_comp_end <- max(ncei_comp$DateTime, na.rm = TRUE)
                          ndbc_comp_count <- nrow(ndbc_comp); ndbc_comp_start <- min(ndbc_comp$DateTime, na.rm = TRUE); ndbc_comp_end <- max(ndbc_comp$DateTime, na.rm = TRUE)
                          name_comp <- paste0(name_export,"_comp")
                          assign(name_comp,ndbc_comp)
                          name_comp <- gsub("ndbc", "ncei", paste0(name_export,"_comp"))
                          assign(name_comp,ncei_comp)
                          # housekeeping
                          rm(ncei_dt,ndbc_dt,ncei2,ndbc2, match_dt, name_comp)
      
                          # export as csv and rds
                          print(paste0("saving comps ",n))
                          if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                          # write.table(ncei_comp, paste0(clean_dir,buoy,"/",gsub("ndbc_", "ncei_",n),"_comp.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                          # saveRDS(ncei_comp, file = paste0(clean_dir,buoy,"/",gsub("ndbc_", "ncei_",n),"_comp.rds"))
                          # write.table(ndbc_comp, paste0(clean_dir,buoy,"/",n,"_comp.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                          # saveRDS(ndbc_comp, file = paste0(clean_dir,buoy,"/",n,"_comp.rds"))
      
                          # filter for pre-matching data
                          start_date_ncei <- min(ncei_comp$DateTime, na.rm = TRUE)
                          ncei_pre <- dplyr::filter(ncei_orig, ncei_orig$DateTime < start_date_ncei)
                          # quick check = should == 0
                          start_date_ndbc <- min(ndbc_comp$DateTime, na.rm = TRUE)
                          ndbc_pre <- dplyr::filter(ndbc_orig, ndbc_orig$DateTime < start_date_ndbc)
                          # removing empty dfs
                          for(i in c("ndbc_pre","ncei_pre")){
                              dat <- get(i)
                              if(dim(dat)[1]>0){
                                  # print(paste0("Error: pre filter data available in ",n))
                                  print(paste0("Pre filter data available in ",i))
                                  if(i == "ndbc_pre"){
                                        # if pre-ndbc exists, there are no geographical positions that match.
                                        # export and delete
                                        # export as csv and rds
                                        print(paste0("saving pre NDBC data: ",name_export))
                                        if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                                        if (!file.exists(paste0(clean_dir,buoy,"/no_gps/"))) {dir.create((paste0(clean_dir,buoy,"/no_gps/")))}
                                        # write.table(ndbc_pre, paste0(clean_dir,buoy,"/no_gps/",name_export,"_pre_NDBC.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                                        # saveRDS(ndbc_pre, file = paste0(clean_dir,buoy,"/no_gps/",name_export,"_pre_NDBC.rds"))
                                        rm(ndbc_pre)
                                  }
                              }else{
                                  rm(dat)
                                  rm(list = ls()[grepl(i, ls())])
                                  print(paste0("removing empty df: ",i))
                              }
                          }
                          rm(i, ndbc_orig, ncei_orig)
      
                          # filter the data on these geographical conditions
                          dat_ls <- c("ncei_comp", "ncei_pre", "ndbc_comp")
                          for(i in dat_ls){
                              # i <- "ndbc_comp"
                              if(exists(i)){
                                  print(paste0("geoCleaning: ",i))
                                  dat <- get(i)
                                  if(dim(dat)[1]>2){
                                      dat <- dplyr::filter(dat, lat >= as.numeric(lat_tail)-as.numeric(GPS_buffer) & lat <= as.numeric(lat_tail)+as.numeric(GPS_buffer))
                                      dat <- dplyr::filter(dat, lon >= as.numeric(lon_tail)-as.numeric(GPS_buffer) & lon <= as.numeric(lon_tail)+as.numeric(GPS_buffer))

                                      dat <- unique(dat)
      
                                      if(unlist(strsplit(n,"_"))[4]== "stdmet"){
                                          #----------------------------------------------------------------------------------------
                                          ## using a sorted table of value occurrences to find outliers in stdmet data
                                          #----------------------------------------------------------------------------------------
                                          print("Re-ordering matching stdmet columns")
                                          # preserving metadata
                                          met <- names(dat)
                                          if(length(grep("_metadata",met))>0){
                                              dat_metadata <- dplyr::select(dat,c(DateTime, contains("_metadata")))
                                              dat <- dplyr::select(dat,!contains("_metadata"))
                                          }else{
                                              met_2 <- met[!met %in% c("DateTime", "lat", "lon")]
                                              dat_names <- c("DateTime", "lat", "lon", met_2)
                                              dat <- dplyr::select(dat, all_of(dat_names))
                                              rm(met_2, dat_names)
                                          }

                                          # identify and remove outlier
                                          for(c in 4:ncol(dat)){
                                              # c <- 5
                                              print(colnames(dat[c]))
                                              # find outliers
                                              outlier_table <- sort(as.numeric(names(sort(table(dat[,c])))))
                                              if(length(outlier_table)>1){
                                                  if(max(outlier_table, na.rm = TRUE) >= 2*outlier_table[length(outlier_table)-1]){
                                                      index <- which(dat[,c] == max(outlier_table, na.rm = TRUE))
                                                      dat[index,c] <- NA
                                                      print(paste0("outlier of ",max(outlier_table, na.rm = TRUE), " removed from ",colnames(dat[c])))
                                                  }else{print(paste0("no outliers for ",colnames(dat[c])))}
                                              }else{print(paste0("no outliers for ",colnames(dat[c])))}
                                          }
                                          # correcting for directional data outliers
                                          dir_list <- grep("_direction",names(dat))
                                          for(dir_column in dir_list){
                                              print(names(dat)[dir_column])
                                              direction_ls <- which(dat[,dir_column] > 360)
                                              print(paste0("outlier indices: ",direction_ls))
                                              if(length(direction_ls)>0){for(d in direction_ls){dat[d,dir_column] <- NA}}
                                              rm(direction_ls)
                                          }
                                          rm(dir_list)
      
                                          # rejoining ncei stdmet data and metadata
                                          if(length(grep("_metadata",met))>0){
                                              dat <- full_join(dat, dat_metadata, by = "DateTime")
                                              dat <- dplyr::select(dat, all_of(met))
                                          }
                                      }
      
                                      # export as csv and rds
                                      if(i == "ncei_pre"){
                                          name1 <- gsub("ndbc", "ncei", name_export)
                                          name2 <- paste0(name1,"_preNDBC_geoClean")
                                      }else if(i == "ndbc_pre"){
                                          name2 <- paste0(name_export,"_preNDBC_geoClean")
                                      }else if(i == "ncei_comp"){
                                          name1 <- gsub("ndbc", "ncei", name_export)
                                          name2 <- paste0(name1,"_geoClean")
                                      }else{
                                          name2 <- paste0(name_export,"_geoClean")
                                      }
                                      # export as csv and rds
                                      print("")
                                      print(paste0("saving geoCleaned: ",name2))
                                      if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                                      # write.table(dat, paste0(clean_dir,buoy,"/",name2,".csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                                      # saveRDS(dat, file = paste0(clean_dir,buoy,"/",name2,".rds"))
                                      # save for RData later
                                      assign(name2,dat)
                                      # housekeeping
                                      rm(dat, dat_metadata)
                                  }else{print("df dim less than 2 rows")}
                                  rm(dat)
                              }else{print(paste0("no preNDBC data for ",i))}
                              rm(i,dat)
                          }
                          # count
                          ncei_geoClean_count <- nrow(ncei_comp); ncei_geoClean_start <- min(ncei_comp$DateTime, na.rm = TRUE); ncei_geoClean_end <- max(ncei_comp$DateTime, na.rm = TRUE)
                          ndbc_geoClean_count <- nrow(ndbc_comp); ndbc_geoClean_start <- min(ndbc_comp$DateTime, na.rm = TRUE); ndbc_geoClean_end <- max(ndbc_comp$DateTime, na.rm = TRUE)
      
                          # build table
                          ncei_count <- t(data.frame(ncei_orig_count, ncei_orig_start, ncei_orig_end,
                                                     ncei_comp_count, ncei_comp_start, ncei_comp_end,
                                                     ncei_geoClean_count, ncei_geoClean_start, ncei_geoClean_end))
                          colnames(ncei_count) <- gsub("ndbc", "ncei", n)
                          ncei_count <- data.frame(ncei_count, stringsAsFactors = FALSE)
      
                          ndbc_count <- t(data.frame(ndbc_orig_count, ndbc_orig_start, ndbc_orig_end,
                                                     ndbc_comp_count, ndbc_comp_start, ndbc_comp_end,
                                                     ndbc_geoClean_count, ndbc_geoClean_start, ndbc_geoClean_end))
                          colnames(ndbc_count) <- n
                          ndbc_count <- data.frame(ndbc_count, stringsAsFactors = FALSE)
      
                          # add to table
                          data_count <- cbind(data_count,ncei_count)
                          data_count <- cbind(data_count,ndbc_count)
                          # remove unclean data
                          rm(ncei, ndbc, ncei_comp, ndbc_comp, ncei_count, ndbc_count,
                             ncei_orig_count, ncei_comp_count, ncei_geoClean_count,ncei_orig_start, ncei_orig_end, ncei_comp_start, ncei_comp_end,ncei_geoClean_start,ncei_geoClean_end,
                             ndbc_orig_count, ndbc_comp_count, ndbc_geoClean_count, ndbc_orig_start, ndbc_orig_end, ndbc_comp_start, ndbc_comp_end,ndbc_geoClean_start, ndbc_geoClean_end)
                    }
                    if(exists("ndbc")){rm(ndbc)}; if(exists("ncei")){rm(ncei)}
                }
                 
                #----------------------------------------------------------------------------------------
                ## attach stdmet metadata to geoCleaned ndbc stdmet dataset 
                #----------------------------------------------------------------------------------------
                ncei_met <- get(ls(pattern = "ncei_stdmet_geoClean")); ncei_met <- unique(ncei_met)
                ndbc_met <- get(ls(pattern = "ndbc_stdmet_geoClean")); ndbc_met <- unique(ndbc_met)
                
                # deal with significant places that skew alignment
                ndbc_col_num <- grep("wind_direction", colnames(ndbc_met))
                ncei_col_num <- grep("wind_direction", colnames(ncei_met))
                for(n in ndbc_col_num:ncol(ndbc_met)){if(is.numeric(ndbc_met[,n]) == TRUE){ndbc_met[,n] <- round(ndbc_met[n],2)}}
                for(n in ncei_col_num[1]:ncol(ncei_met)){if(is.numeric(ncei_met[,n]) == TRUE){ncei_met[,n] <- round(ncei_met[n],2)}}
            
                # select variables to run
                var_list <- names(ndbc_met) 
                # selecting only main variable per sensor to increase computational efficiency
                var_list_main <- var_list[!var_list %in% c("DateTime", "lat", "lon", "wind_direction", "wind_gust", 
                                                           "dominant_wave_period", "average_wave_period", "mean_wave_direction")]
                print(var_list_main)
                
                print(Sys.time())
                
                for(i in var_list_main){
                     # i <- var_list_main[5]
                    print(paste0("matching ncei metadata to ndbc data for ",i," for ",buoy))
                    print(Sys.time())
                  
                    # subset data
                    library(dplyr)
                    ndbc <- dplyr::select(ndbc_met, DateTime, contains(i))
                    ncei <- dplyr::select(ncei_met, DateTime, contains(i))
                    
                    # if no data for this variable, skip the following...
                    if(sum(is.na(ndbc[i])) != nrow(ndbc)){
                      
                        # single sensor
                        if(dim(ncei)[2]==3){
                              print("handling single sensor")
                              ncei$two <- NA; ncei$met <- NA
                              colnames(ncei) <- c("DateTime", paste0(i,"_1"),paste0(i,"_metadata_1"),paste0(i,"_2"),paste0(i,"_metadata_2"))
                        }
                      
                        # multiple sensors
                        ndbc <- left_join(ndbc,ncei,by = "DateTime")
                        # remove rows with no data 
                        ndbc <- ndbc[!is.na(ndbc[,2]), ]
                        # order and re-index rows
                        ndbc <- ndbc[order(ndbc$DateTime),]
                        ndbc <- unique(ndbc)
                        row.names(ndbc) <- 1:nrow(ndbc)
                        # set up metadata column and select relevant data columns
                        ndbc$metadata <- NA
                        ncei_dat_ls <- names(dplyr::select(ncei, contains(i)))
                        
                        # matching ndbc && ncei primary sensor where secondary data is na 
                        var_index <- which(ndbc[i] == ndbc[ncei_dat_ls[1]] & is.na(ndbc[ncei_dat_ls[3]]))# | is.na(ndbc[ncei_dat_ls[3]]))
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for published duplicate metadata")
                            for(d in var_index){
                                # insert metadata, accounting for NDBC practice of inserting duplicate data
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]#}
                            }
                        }else{print("no published duplicate metadata")}
 
                        # matching ndbc && ncei primary and secondary sensor - to account for ndbc's ncei practice of publishing duplicate data
                        var_index <- which(is.na(ndbc$metadata) & ndbc[i] == ndbc[ncei_dat_ls[1]] & ndbc[i] == ndbc[ncei_dat_ls[3]])# | is.na(ndbc[ncei_dat_ls[3]]))
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for published duplicate metadata")
                            for(d in var_index){
                                # insert metadata, accounting for NDBC practice of inserting duplicate data
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]
                            }
                        }else{print("no published duplicate metadata")}
                        
                        # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                        var_index <- which(is.na(ndbc$metadata) & ndbc[i] != ndbc[ncei_dat_ls[1]] & ndbc[ncei_dat_ls[1]] == ndbc[ncei_dat_ls[3]])
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for non-matching sensor data")
                            for(d in var_index){
                                # insert metadata, accounting for NetCDF build errors
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]
                                ndbc[d,ncei_dat_ls[1]] <- ndbc[d,i]
                            }
                        }else{print("no non-matching sensor data")}
                        
                        # ndbc data that matches ncei secondary but DOESN't match primary sensor - logical secondary sensor usage 
                        var_index <- which(is.na(ndbc$metadata) & ndbc[i] == ndbc[ncei_dat_ls[3]] & ndbc[ncei_dat_ls[1]] != ndbc[ncei_dat_ls[3]])
                        length(var_index)
                        if(length(var_index)>0){
                          print("check for secondary sensor metadata with non-matching primary metadata")
                          for(d in var_index){
                            # insert metadata
                            ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[4]]
                          }
                        }else{print("no only secondary sensor metadata")}
                        
                        # ndbc data that matches ncei primary but DOESN't match secondary sensor - logical primary sensor usage 
                        var_index <- which(is.na(ndbc$metadata) & ndbc[i] == ndbc[ncei_dat_ls[1]] & ndbc[ncei_dat_ls[1]] != ndbc[ncei_dat_ls[3]])
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for primary sensor metadata with non-matching secondary metadata")
                            for(d in var_index){
                                # insert metadata
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]
                            }
                        }else{print("no only primary sensor data")}
                        
                        # Test if exists: ndbc data that DOESN'T match ncei primary or secondary but is still present - possible NDBC data QC 
                        var_index <- which(is.na(ndbc$metadata) & is.na(ndbc[ncei_dat_ls[1]]) & is.na(ndbc[ncei_dat_ls[3]]))
                        length(var_index)
                        if(length(var_index)>0){
                          print("check for remaining primary sensor metadata")
                          for(d in var_index){
                            # insert metadata
                            ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]
                            ndbc[d,ncei_dat_ls[1]] <- ndbc[d,i]
                          }
                        }else{print("no remaining primary sensor metadata")}
                        
                        # Test if exists: ndbc data that DOESN'T match ncei primary but is still present - possible NDBC rounding issues or data QC 
                        var_index <- which(is.na(ndbc$metadata) & !is.na(ndbc[i]) & !is.na(ndbc[ncei_dat_ls[1]]) & !is.na(ndbc[ncei_dat_ls[2]]) & is.na(ndbc[ncei_dat_ls[3]]) & is.na(ndbc[ncei_dat_ls[4]]))
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for remaining primary sensor metadata")
                            for(d in var_index){
                                # insert metadata
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]
                            }
                        }else{print("no remaining primary sensor metadata")}
                    
                        # Test if exists: ndbc data that DOESN'T match ncei secondary but is still present - possible NDBC rounding issues or data QC 
                        var_index <- which(is.na(ndbc$metadata) & !is.na(ndbc[ncei_dat_ls[3]]) & !is.na(ndbc[ncei_dat_ls[4]]) & is.na(ndbc[ncei_dat_ls[1]]) & is.na(ndbc[ncei_dat_ls[2]]))
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for remaining secondary sensor metadata")
                            for(d in var_index){
                                # insert metadata
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[4]]
                            }
                        }else{print("no remaining secondary sensor metadata")}
                        
                        # Test if exists: remaining primary metadata  
                        var_index <- which(is.na(ndbc$metadata) & is.na(ndbc[ncei_dat_ls[1]]) & !is.na(ndbc[ncei_dat_ls[2]]) & is.na(ndbc[ncei_dat_ls[3]]) & is.na(ndbc[ncei_dat_ls[4]]))
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for remaining ndbc blank metadata")
                            for(d in var_index){
                                # insert metadata
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]#}
                            }
                        }else{print("no remaining ndbc blank primary metadata")}
                        
                        # Test if exists: look for remaining secondary metadata  
                        var_index <- which(is.na(ndbc$metadata) & is.na(ndbc[ncei_dat_ls[1]]) & is.na(ndbc[ncei_dat_ls[2]]) & is.na(ndbc[ncei_dat_ls[3]]) & !is.na(ndbc[ncei_dat_ls[4]]))
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for remaining ndbc blank metadata")
                            for(d in var_index){
                                # insert metadata
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[4]]#}
                            }
                        }else{print("no remaining ndbc blank secondary metadata")}
                        
                        # Test if exists: remaining metadata where no data matches... possible shoreside corrections? 
                        var_index <- which(is.na(ndbc$metadata) & !is.na(ndbc[ncei_dat_ls[1]]) & !is.na(ndbc[ncei_dat_ls[2]]) & 
                                             !is.na(ndbc[ncei_dat_ls[3]]) & !is.na(ndbc[ncei_dat_ls[4]]) & ndbc[i] != ndbc[ncei_dat_ls[1]] & 
                                             ndbc[i] != ndbc[ncei_dat_ls[3]] & ndbc[ncei_dat_ls[1]] != ndbc[ncei_dat_ls[3]])
                        length(var_index)
                        if(length(var_index)>0){
                            print("check for remaining ndbc blank metadata")
                            for(d in var_index){
                                # insert metadata
                                ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]#}
                            }
                        }else{print("no remaining blank metadata")}
                        
                        # Test if exists: another metadata checks with no data matches... possible shoreside corrections? 
                        var_index <- which(is.na(ndbc$metadata) & !is.na(ndbc[ncei_dat_ls[1]]) & !is.na(ndbc[ncei_dat_ls[2]]) & 
                                             is.na(ndbc[ncei_dat_ls[3]]))
                        length(var_index)
                        if(length(var_index)>0){
                          print("check for remaining ndbc blank metadata with non-matching ndbc and ncei data")
                          for(d in var_index){
                            # insert metadata
                            ndbc$metadata[d] <- ndbc[d,ncei_dat_ls[2]]#}
                          }
                        }else{print("no remaining blank metadata")}
                        
                        print(paste0("finished adding metadata to ",i))
                        
                        # correct bad ncei data
                        ncei <- ndbc; ncei[,2] <- NULL; ncei[ncol(ncei)] <- NULL
                        # remove ncei metadata if columns now empty
                        if("TRUE" %in% unique(!is.na(ndbc$metadata))){
                              print("unique metadata == TRUE")
                              ndbc[,ncei_dat_ls[1]] <- NULL; ndbc[,ncei_dat_ls[2]] <- NULL
                              ndbc[,ncei_dat_ls[3]] <- NULL; ndbc[,ncei_dat_ls[4]] <- NULL
                        }
                        
                        # *** if the df is not reduced to 3 columns, break
                        if(dim(ndbc)[2] != 3){
                             print("after correcting bad ncei data - df is not reduced to 3 columns, quitting...")
                             quit(save = "no", status = 999)
                        }else{print("after correcting bad ncei data - df IS reduced to 3 columns, NOT quitting...")}
                        
                        if(dim(ndbc)[2]==2){ndbc$metadata <- NA}
                        colnames(ndbc) <- c("DateTime", i, paste0(i,"_metadata"))
                        
                        df <- ndbc
                        # check for old values that matched and resulted in incorrect metadata assignment - only applicable post 2011 with dual sensor deployment
                        # fill in any NA values
                        for(j in 1:nrow(df)){
                            if(is.na(df[j,3])){tryCatch({df[j,3] <- df[j-1,3]}, error = function(cond){print("can't fill in NA")})}
                        }
                        # find first date if not in 2011
                        index <- which(year(df$DateTime)>=2011)
                        if(year(df$DateTime[1])>=2011){index <- index[-c(1,2,3,4,5)]}  # adjusting for data that starts after 2011 (i.e. nothing previous to search on below)
                        # check indices
                        if(length(index) != 0){
                            for(j in index[1]:nrow(df)-4){
                                if(!is.na(df[j,3])){
                                    tryCatch({if(df[j,3] != df[j-1,3] & df[j,3] != df[j+1,3] & df[j-1,3] == df[j+1,3]){df[j,3] <- df[j-1,3]}},error=function(cond){print("")})
                                    tryCatch({if(df[j,3] != df[j-2,3] & df[j,3] != df[j+2,3] & df[j-2,3] == df[j+2,3]){df[j,3] <- df[j-2,3]}},error=function(cond){print("")})
                                    tryCatch({if(df[j,3] != df[j-3,3] & df[j,3] != df[j+3,3] & df[j-3,3] == df[j+3,3]){df[j,3] <- df[j-3,3]}},error=function(cond){print("")})
                                    tryCatch({if(df[j,3] != df[j-4,3] & df[j,3] != df[j+4,3] & df[j-4,3] == df[j+4,3]){df[j,3] <- df[j-4,3]}},error=function(cond){print("")})
                                }#else{print("NA value, skipping...")}
                            }
                        }else{print("no pre-2011 data")}
                        
                        # select only metadata and join to main datasets
                        ndbc <- dplyr::select(df, DateTime, contains("metadata"))
                        rm(df)
                        ndbc_met <- left_join(ndbc_met, ndbc, by = "DateTime")
                        
                        # correcting ncei data for final ndbc transferal
                        if(i != "significant_wave_height" & i != "sea_surface_temperature"){
                            print("repairing ncei data")  
                            df <- ncei_met
                            drop_columns <- unique(c(colnames(df)[grep(i, colnames(df))], colnames(ncei)[grep(i, colnames(ncei))]))
                            print(drop_columns)
                            df[drop_columns] <- NULL
                            ncei_met <- left_join(df, ncei, by = "DateTime")
                            rm(df)
                        }else{print('not repairing ncei data')}

                    }else{
                      
                      print(paste0("no data for ",i))
                      df <- dplyr::select(ndbc_met, DateTime,lat)
                      df$lat <- NA
                      colnames(df) <- c("DateTime", paste0(i,"_metadata"))
                      ndbc_met <- left_join(ndbc_met, df, by = "DateTime")
                      rm(df)
                    }
                    rm(ncei, ndbc)
                }
                # unique
                if(exists("ncei_met")){ncei_met <- unique(ncei_met)}
                if(exists("ndbc_met")){ndbc_met <- unique(ndbc_met)}
                print("end variable met loop")
                print(Sys.time())
                
                #----------------------------------------------------------------------------------------
                
                # transfer ndbc metadata over to variables from single sensors
                var_names <- names(ndbc_met)
                if("wind_speed_metadata" %in% var_names){
                    ndbc_met$wind_direction_metadata <- ndbc_met$wind_speed_metadata
                    ndbc_met$wind_gust_metadata <- ndbc_met$wind_speed_metadata
                }
                if("significant_wave_height_metadata" %in% var_names){
                    ndbc_met$dominant_wave_period_metadata <- ndbc_met$significant_wave_height_metadata
                    ndbc_met$average_wave_period_metadata <- ndbc_met$significant_wave_height_metadata
                    ndbc_met$mean_wave_direction_metadata <- ndbc_met$significant_wave_height_metadata
                }
                
                # re-order ndbc columns
                ndbc_ord <- c("DateTime","lat","lon","wind_direction","wind_direction_metadata","wind_speed","wind_speed_metadata","wind_gust","wind_gust_metadata",
                              "significant_wave_height","significant_wave_height_metadata","dominant_wave_period","dominant_wave_period_metadata","average_wave_period",
                              "average_wave_period_metadata","mean_wave_direction","mean_wave_direction_metadata" ,"air_pressure_at_sea_level","air_pressure_at_sea_level_metadata",
                              "air_temperature","air_temperature_metadata","sea_surface_temperature","sea_surface_temperature_metadata","dew_point_temperature","dew_point_temperature_metadata")    
                ndbc_met <- dplyr::select(ndbc_met,all_of(ndbc_ord))

                #----------------------------------------------------------------------------------------
                
                # correct ncei 
                var_list_rem <- c("wind_direction", "wind_gust")
                print(var_list_rem)
                for(i in var_list_rem){
                      # i <- var_list_rem[2]
                      print(i)
                      if(any(grepl(i, colnames(ncei_met)))==TRUE){
                          # subset data
                          library(dplyr)
                          ndbc <- dplyr::select(ndbc_met, DateTime, contains(i))
                          ncei <- dplyr::select(ncei_met, DateTime, contains(i))
                          if(dim(ndbc)[2]==3 & dim(ncei)[2]==3){
                               colnames(ndbc) <- c("DateTime",paste0(colnames(ndbc[,2:3]),"_ndbc"))
                               colnames(ncei) <- c("DateTime",paste0(colnames(ncei[,2:3]),"_ncei"))
                               # join df
                               merged_df <- merge(ndbc, ncei, by = "DateTime")
                               # select relevant data columns
                               ncei_dat_ls <- names(dplyr::select(ncei, contains(i)))
                               # select var
                               m <- paste0(i, "_ndbc")
                               
                               # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                               var_index <- which(!is.na(merged_df[m]) != !is.na(merged_df[ncei_dat_ls[1]]) & !is.na(merged_df[m]) != !is.na(merged_df[ncei_dat_ls[2]]))
                               length(var_index)
                               if(length(var_index)>0){
                                    print("check for non-matching sensor data")
                                    for(d in var_index){merged_df[d,ncei_dat_ls[1]] <- merged_df[d,m]}
                               }else{print("no non-matching sensor data")}
                               
                               # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                               var_index <- which(merged_df[m] != merged_df[ncei_dat_ls[1]] & merged_df[ncei_dat_ls[1]] == merged_df[ncei_dat_ls[2]])
                               length(var_index)
                               if(length(var_index)>0){
                                    print("check for non-matching sensor data")
                                    for(d in var_index){merged_df[d,ncei_dat_ls[1]] <- merged_df[d,m]}
                               }else{print("no non-matching sensor data")}
                               
                               # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                               var_index <- which(!is.na(merged_df[m]) != merged_df[ncei_dat_ls[1]] & merged_df[ncei_dat_ls[1]] == merged_df[ncei_dat_ls[2]])
                               length(var_index)
                               if(length(var_index)>0){
                                    print("check for non-matching sensor data")
                                    for(d in var_index){merged_df[d,ncei_dat_ls[1]] <- merged_df[d,m]}
                               }else{print("no non-matching sensor data")}
                               
                               # remove ndbc and ncei data
                               merged_df[,2:3] <- NULL
                               # i <- gsub("_ndbc","",i)
                               drop_columns <- grep(i, colnames(ncei_met))
                               ncei_met[drop_columns] <- NULL
                               colnames(merged_df) <- gsub("_ncei","",colnames(merged_df))
                               ncei_met <- left_join(ncei_met, merged_df, by = "DateTime")
                               ncei_met <- unique(ncei_met)
                               rm(merged_df, ndbc, ncei, var_index, drop_columns)
                               
                          }else{
                               # join df
                               merged_df <- merge(ndbc, ncei, by = "DateTime")
                               # select relevant data columns
                               ncei_dat_ls <- names(dplyr::select(ncei, contains(i)))
                               
                               # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                               var_index <- which(!is.na(merged_df[i]) != (!is.na(merged_df[ncei_dat_ls[1]])) & (!is.na(merged_df[i])) != (!is.na(merged_df[ncei_dat_ls[3]])))
                               length(var_index)
                               if(length(var_index)>0){
                                    print("check for non-matching sensor data")
                                    for(d in var_index){merged_df[d,ncei_dat_ls[1]] <- merged_df[d,i]}
                               }else{print("no non-matching sensor data")}
                               
                               # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                               var_index <- which(merged_df[i] != merged_df[ncei_dat_ls[1]] & merged_df[ncei_dat_ls[1]] == merged_df[ncei_dat_ls[3]])
                               length(var_index)
                               if(length(var_index)>0){
                                    print("check for non-matching sensor data")
                                    for(d in var_index){merged_df[d,ncei_dat_ls[1]] <- merged_df[d,i]}
                               }else{print("no non-matching sensor data")}
                               
                               # ndbc data that DOESN't match ncei primary and secondary sensor - to account for possible build errors 
                               var_index <- which(!is.na(merged_df[i]) != merged_df[ncei_dat_ls[1]] & merged_df[ncei_dat_ls[1]] == merged_df[ncei_dat_ls[3]])
                               length(var_index)
                               if(length(var_index)>0){
                                    print("check for non-matching sensor data")
                                    for(d in var_index){merged_df[d,ncei_dat_ls[1]] <- merged_df[d,i]}
                               }else{print("no non-matching sensor data")}
                               
                               # remove ndbc and ncei data
                               merged_df[,2:3] <- NULL
                               drop_columns <- grep(i, colnames(ncei_met))
                               ncei_met[drop_columns] <- NULL
                               ncei_met <- left_join(ncei_met, merged_df, by = "DateTime")
                               ncei_met <- unique(ncei_met)
                               rm(merged_df, ndbc, ncei, var_index, drop_columns)
                          }
                      }
                }
                # remove empty columns
                if("wind_speed_2" %in% names(ncei_met)){
                     if(sum(ncei_met$wind_speed_2, na.rm = TRUE) == 0){ncei_met$wind_speed_2 <- NULL; ncei_met$wind_speed_metadata_2 <- NULL;
                     ncei_met <- dplyr::rename(ncei_met, wind_speed = wind_speed_1, wind_speed_metadata = wind_speed_metadata_1)}
                     ncei_met <- unique(ncei_met)
                     print("removing empty wind_speed_2")
                }
                if("wind_direction_2" %in% names(ncei_met)){
                     if(sum(ncei_met$wind_direction_2, na.rm = TRUE) == 0){ncei_met$wind_direction_2 <- NULL; ncei_met$wind_direction_metadata_2 <- NULL;
                     ncei_met <- dplyr::rename(ncei_met, wind_direction = wind_direction_1, wind_direction_metadata = wind_direction_metadata_1)}
                     ncei_met <- unique(ncei_met)
                     print("removing empty wind_direction_2")
                }
                if("wind_gust_2" %in% names(ncei_met)){
                     if(sum(ncei_met$wind_gust_2, na.rm = TRUE) == 0){ncei_met$wind_gust_2 <- NULL; ncei_met$wind_gust_metadata_2 <- NULL;
                     ncei_met <- dplyr::rename(ncei_met, wind_gust = wind_gust_1, wind_gust_metadata = wind_gust_metadata_1)}
                     ncei_met <- unique(ncei_met)
                     print("removing empty wind_gust_2")
                }
                if("air_pressure_at_sea_level_2" %in% names(ncei_met)){
                     if(sum(ncei_met$air_pressure_at_sea_level_2, na.rm = TRUE) == 0){ncei_met$air_pressure_at_sea_level_2 <- NULL; ncei_met$air_pressure_at_sea_level_metadata_2 <- NULL;
                     ncei_met <- dplyr::rename(ncei_met, air_pressure_at_sea_level = air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata = air_pressure_at_sea_level_metadata_1)}
                     ncei_met <- unique(ncei_met)
                     print("removing empty air_pressure_at_sea_level_2")
                }
                if("air_temperature_2" %in% names(ncei_met)){
                     if(sum(ncei_met$air_temperature_2, na.rm = TRUE) == 0){ncei_met$air_temperature_2 <- NULL; ncei_met$air_temperature_metadata_2 <- NULL;
                     ncei_met <- dplyr::rename(ncei_met, air_temperature = air_temperature_1, air_temperature_metadata = air_temperature_metadata_1)}
                     ncei_met <- unique(ncei_met)
                     print("removing empty air_temperature_2")
                }
                if("dew_point_temperature_2" %in% names(ncei_met)){
                     if(sum(ncei_met$dew_point_temperature_2, na.rm = TRUE) == 0){ncei_met$dew_point_temperature_2 <- NULL; ncei_met$dew_point_temperature_metadata_2 <- NULL;
                     ncei_met <- dplyr::rename(ncei_met, dew_point_temperature = dew_point_temperature_1, dew_point_temperature_metadata = dew_point_temperature_metadata_1)}
                     ncei_met <- unique(ncei_met)
                     print("removing empty dew_point_temperature_2")
                }
                # re-order columns
                if(ncol(ncei_met)== 41){
                    print("ncei data format - dual sst")
                    ncei_met <- dplyr::select(ncei_met, DateTime, lat, lat_metadata, lon, lon_metadata,
                                         wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                         wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                         wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                         significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                         average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                         air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                         air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                         sea_surface_temperature_1, sea_surface_temperature_metadata_1,sea_surface_temperature_2, sea_surface_temperature_metadata_2,
                                         dew_point_temperature_1, dew_point_temperature_metadata_1, dew_point_temperature_2, dew_point_temperature_metadata_2)
                }else if(ncol(ncei_met)== 39){
                    if("sea_surface_temperature" %in% names(ncei_met)){
                      print("ncei data format - single sst")
                      ncei_met <- dplyr::select(ncei_met, DateTime, lat, lat_metadata, lon, lon_metadata,
                                           wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                           wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                           wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                           significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                           average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                           air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                           air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                           sea_surface_temperature, sea_surface_temperature_metadata,
                                           dew_point_temperature_1, dew_point_temperature_metadata_1, dew_point_temperature_2, dew_point_temperature_metadata_2)
                    }else if("dew_point_temperature" %in% names(ncei_met)){
                      print("ncei data format - single dew point temperature")
                         ncei_met <- dplyr::select(ncei_met, DateTime, lat, lat_metadata, lon, lon_metadata,
                                           wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                           wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                           wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                           significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                           average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                           air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                           air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                           sea_surface_temperature_1, sea_surface_temperature_metadata_1,sea_surface_temperature_2, sea_surface_temperature_metadata_2,
                                           dew_point_temperature, dew_point_temperature_metadata)
                    }else{print("error: dim = 39 but dat names don't match")}
                }else if(ncol(ncei_met)== 37){
                    print("ncei data format - single dew point")
                     ncei_met <- dplyr::select(ncei_met, DateTime, lat, lat_metadata, lon, lon_metadata,
                                         wind_direction_1, wind_direction_metadata_1, wind_direction_2, wind_direction_metadata_2,
                                         wind_speed_1, wind_speed_metadata_1, wind_speed_2, wind_speed_metadata_2,
                                         wind_gust_1, wind_gust_metadata_1, wind_gust_2, wind_gust_metadata_2,
                                         significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                         average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                         air_pressure_at_sea_level_1, air_pressure_at_sea_level_metadata_1, air_pressure_at_sea_level_2, air_pressure_at_sea_level_metadata_2,
                                         air_temperature_1, air_temperature_metadata_1, air_temperature_2, air_temperature_metadata_2,
                                         sea_surface_temperature, sea_surface_temperature_metadata,
                                         dew_point_temperature, dew_point_temperature_metadata)
                }else if(ncol(ncei_met)== 27){
                    print("ncei data format - single sensors for all")
                     ncei_met <- dplyr::select(ncei_met, DateTime, lat, lat_metadata, lon, lon_metadata,
                                         wind_direction, wind_direction_metadata, wind_speed, wind_speed_metadata, wind_gust, wind_gust_metadata, 
                                         significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                         average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                         air_pressure_at_sea_level, air_pressure_at_sea_level_metadata, air_temperature, air_temperature_metadata,
                                         sea_surface_temperature, sea_surface_temperature_metadata,
                                         dew_point_temperature, dew_point_temperature_metadata)
                }else if(ncol(ncei_met)== 27){
                     print("ncei data format - single sensors for all")
                     ncei_met <- dplyr::select(ncei_met, DateTime, lat, lat_metadata, lon, lon_metadata,
                                          wind_direction, wind_direction_metadata, wind_speed, wind_speed_metadata, wind_gust, wind_gust_metadata, 
                                          significant_wave_height, significant_wave_height_metadata, dominant_wave_period, dominant_wave_period_metadata,
                                          average_wave_period, average_wave_period_metadata, mean_wave_direction, mean_wave_direction_metadata,
                                          air_pressure_at_sea_level, air_pressure_at_sea_level_metadata, air_temperature, air_temperature_metadata,
                                          sea_surface_temperature, sea_surface_temperature_metadata,
                                          dew_point_temperature, dew_point_temperature_metadata)
                }else{print("no column count match")}

                #----------------------------------------------------------------------------------------
                
                # find unique rows only
                ndbc_met <- unique(ndbc_met)
                ncei_met <- unique(ncei_met)
                # rename the rows to reflect unique data
                row.names(ndbc_met) <- 1:nrow(ndbc_met)
                row.names(ncei_met) <- 1:nrow(ncei_met)
                
                # save file in glob environment
                ndbc_name <- ls(pattern = "ndbc_stdmet_geoClean")
                ncei_name <- ls(pattern = "ncei_stdmet_geoClean")
                
                assign(ndbc_name, ndbc_met)
                assign(ncei_name, ncei_met)
                
                # housekeeping
                rm(ncei_met, ndbc_met, ncei_metadata, ndbc_name, ncei_name)
            
                #----------------------------------------------------------------------------------------
                ## geoclean unique data
                #----------------------------------------------------------------------------------------
                print(paste0("GeoCleaning unique datasets for ", buoy))
                ncei_list <- ls(pattern = "_ncei_")
                remove <- gsub("_ndbc_", "_ncei_", ndbc_list)
                ncei_list <- ncei_list[!grepl("_geoClean", ncei_list)]
                ncei_list <- ncei_list[!grepl("_comp", ncei_list)]
                ncei_list <- ncei_list[!grepl("_sdmet_netcdf", ncei_list)]
                ncei_list <- ncei_list[str_remove_all(ncei_list, paste(remove, collapse = "|"))!= ""]
                # add extra ndbc data with no matching ncei
                if(length(ndbc_ls)>0){ncei_list <- c(ncei_list, ndbc_ls)}
                if(length(remainder)>0){ncei_list <- c(ncei_list, remainder)}
                # remove buoy info
                ncei_list <- gsub(paste0("s_",buoy,"_"), "", ncei_list)
                print(ncei_list)
                
                if("ncei_sensor_output" %in% ncei_list){
                      dat_name <- paste0("s_",buoy,"_ncei_sensor_output")
                      dat <- get(dat_name)
                      dat <- left_join(dat, ncei_positions, by = "DateTime")
                      assign(dat_name,dat)
                      rm(dat)
                }
                
                # load individual datasets and geoclean pairs
                for(n in ncei_list){
                      print(n)
                  
                      dat_source <- unlist(strsplit(n,"_"))[1]
                      name_export <- gsub(paste0(dat_source,"_"),"",n)
                      
                      # find matching datasets
                      dat <- get(paste0("s_",buoy,"_",n))
                
                      # remove blank rows
                      dat <- dat[rowSums(is.na(dat)) != ncol(dat)-1,]
                      dat_orig_count <- nrow(dat); dat_orig_start <- min(dat$DateTime, na.rm = TRUE); dat_orig_end <- max(dat$DateTime, na.rm = TRUE)
            
                      # export as csv and rds
                      print(paste0("saving orig ",n))
                      if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                      # write.table(dat, paste0(clean_dir,buoy,"/","s_",buoy,"_",dat_source,"_",name_export,"_orig.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                      # saveRDS(dat, file = paste0(clean_dir,buoy,"/","s_",buoy,"_",dat_source,"_",name_export,"_orig.rds"))
                      
                      # checking geographical positions for service visits and buoy adrift, i.e. not in watch circle
                      # using a sorted table of value occurrences to find most common lat/lon
                      if("lat" %in% names(dat)){
                            positions <- dplyr::select(dat,DateTime,lat,lon)
                            positions$lat <- as.numeric(positions$lat); positions$lon <- as.numeric(positions$lon)
                            positions$lat <- signif(positions$lat,3)
                            positions$lon <- signif(positions$lon,3)
                      }else{
                            print("no gps data, adding data from master positions data")
                            positions <- ncei_positions
                            ## Use round.Date to round, then format to format
                            positions$DateTime <- format(round(positions$DateTime, units="hours"), format="%Y-%m-%d %H:%M:%S")
                            positions$DateTime <- lubridate::ydm_hms(positions$DateTime)
                            
                            # Join and standardize the data frames
                            positions <- left_join(dat,positions, by = "DateTime")
                            positions <- dplyr::select(positions,DateTime,lat,lon)
                            positions$lat <- as.numeric(positions$lat); positions$lon <- as.numeric(positions$lon)
                            positions$lat <- signif(positions$lat,3); positions$lon <- signif(positions$lon,3)
                            # positions <- unique(positions)
                      }
                      
                      # # two methods of finding mode
                      library(modeest)
                      lat_mean <- mfv(positions$lat, na_rm = TRUE)
                      lon_mean <- mfv(positions$lon, na_rm = TRUE)
                      print(paste0("mean method - lat: ", lat_mean, "; lon: ", lon_mean))
                      # using a sorted table of value occurrences to find most common lat/lon
                      lat_tail <- tail(names(sort(table(positions$lat))),1)
                      lon_tail <- tail(names(sort(table(positions$lon))),1)
                      print(paste0("sorted table method - lat: ", lat_tail, "; lon: ", lon_tail))
                      # check positions in ndbc bulk
                      print(paste0("range - lat: ", range(positions$lat, na.rm = TRUE)[1],"; ", range(positions$lat, na.rm = TRUE)[2]))
                      print(paste0("range - lon: ", range(positions$lon, na.rm = TRUE)[1],"; ", range(positions$lon, na.rm = TRUE)[2]))
                      
                      # filter the data on these geographical conditions
                      if(dat_source == "ndbc"){dat <- left_join(dat, positions, by = "DateTime")}
                      dat <- dplyr::filter(dat, lat >= as.numeric(lat_tail)-as.numeric(GPS_buffer) & lat <= as.numeric(lat_tail)+as.numeric(GPS_buffer))
                      dat <- dplyr::filter(dat, lon >= as.numeric(lon_tail)-as.numeric(GPS_buffer) & lon <= as.numeric(lon_tail)+as.numeric(GPS_buffer))
                      dat_clean <- unique(dat)
                      # count
                      dat_geoClean_count <- nrow(dat_clean)
                      dat_geoClean_count <- nrow(dat_clean); dat_geoClean_start <- min(dat_clean$DateTime, na.rm = TRUE); dat_geoClean_end <- max(dat_clean$DateTime, na.rm = TRUE)
                      
                      # export as csv and rds
                      print(paste0("saving geoCleaned ",n))
                      if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                      # write.table(dat_clean, paste0(clean_dir,buoy,"/","s_",buoy,"_",dat_source,"_",name_export,"_geoclean.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                      # saveRDS(dat_clean, file = paste0(clean_dir,buoy,"/","s_",buoy,"_",dat_source,"_",name_export,"_geoclean.rds"))
                      
                      # build table
                      dat_count <- t(data.frame(dat_orig_count, dat_orig_start, dat_orig_end,
                                                 NA, NA, NA,
                                                 dat_geoClean_count, dat_geoClean_start,dat_geoClean_end))
                      colnames(dat_count) <- paste0("s_", buoy, "_",n)
                      dat_count <- data.frame(dat_count, stringsAsFactors = FALSE)
                      # add to table
                      data_count <- cbind(data_count,dat_count)
                      data_count <- data.frame(data_count, stringsAsFactors = FALSE)
                      
                      # # remove unclean data
                      # rm(list = ls(pattern = name_export))
                      # save for RData later
                      clean_dat <- paste0("s_",buoy,"_",dat_source,"_",name_export,"_geoClean")
                      assign(clean_dat, dat_clean)
                      rm(dat, clean_dat, positions, dat_clean, name_export, dat_count, dat_geoClean_count, dat_orig_count)
                }
            
                #----------------------------------------------------------------------------------------
                ## create station hull count table and save data_count table
                #----------------------------------------------------------------------------------------
                met_ls <- ls(pattern = "station_metadata")
                for(d in met_ls){
                    d_name <- paste0(d, "_count")
                    d_name <- gsub("ncei_","", d_name)
                    dat <- get(d)
                    # create empty df
                    df <- data.frame(matrix(ncol = 0, nrow = 30))
                    df$count <- 1:30
                    # find unique data per variable
                    depth <- data.frame(1:length(unique(dat$depth)),unique(dat$depth), stringsAsFactors = FALSE); colnames(depth) <- c("count","depth")
                    mooring <- data.frame(1:length(unique(dat$mooring)),unique(dat$mooring), stringsAsFactors = FALSE); colnames(mooring) <- c("count","mooring")
                    hull <- data.frame(1:length(unique(dat$hull)),unique(dat$hull), stringsAsFactors = FALSE); colnames(hull) <- c("count","hull")
                    payload <- data.frame(1:length(unique(dat$payload)),unique(dat$payload), stringsAsFactors = FALSE); colnames(payload) <- c("count","payload")
                    # build data frame
                    df <- full_join(df,depth, by = "count")
                    df <- full_join(df,mooring, by = "count")
                    df <- full_join(df,hull, by = "count")
                    df <- full_join(df,payload, by = "count")
                    df$count <- NULL
                    df <- df[rowSums(is.na(df)) != ncol(df),]
                    # save data
                    print("saving Station Count tables")
                    assign(d_name, df)
                    # write.table(df, paste0(clean_dir,buoy,"/",d_name,".csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                    # saveRDS(df, paste0(clean_dir,buoy,"/",d_name,".rds"))
                    # housekeeping
                    rm(d_name,dat,df)
                }
                rm(met_ls)
                
                # Save data count tables
                print("saving Data Count tables")
                colnames(data_count)[1] <- paste0("Station ",buoy)
                # write.table(data_count, paste0(clean_dir,buoy,"/","s_",buoy,"_data_counts.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                data_count_name <- paste0("s_",buoy,"_ndbc_ncei_data_counts")
                assign(data_count_name, data_count)
                
                #----------------------------------------------------------------------------------------
                # exporting GeoCleaned datasets
                #----------------------------------------------------------------------------------------
            
                # if(switch_set == 1){
                    print("Exporting Comp and GeoCleaned data")
                    
                    ndbc_list <- ls(pattern = "_ndbc_"); ncei_list <- ls(pattern = "_ncei_")
                    ndbc_list <- ndbc_list[!grepl("color_", ndbc_list)]
                    ndbc_list <- ndbc_list[grepl("_geoClean", ndbc_list)]
                    print(paste0("NDBC datasets :", ndbc_list))
                    print("")
                    ncei_list <- ncei_list[grepl("_geoClean", ncei_list)]
                    print(paste0("NCEI datasets :", ncei_list))
                    
                    
                    # export and save geoClean dataset
                    if (!file.exists(paste0(clean_dir,buoy,"/"))) {dir.create((paste0(clean_dir,buoy,"/")))}
                    for(g in c(ndbc_list,ncei_list)){
                        if(grepl("_stdmet",g)==TRUE){
                            print(g)
                            write.table(get(g), paste0(clean_dir,buoy,"/",g,".csv"), row.names=FALSE, col.names = TRUE, sep = ",")
                            saveRDS(get(g), paste0(clean_dir,buoy,"/",g,".rds"))
                        }
                    }
                    # export to RData
                    save(list = ndbc_list, file = paste0(clean_dir,buoy,"/s_",buoy,"_ndbc_comp_geoClean.RData"))
                    save(list = ncei_list, file = paste0(clean_dir,buoy,"/s_",buoy,"_ncei_comp_geoClean.RData"))
                # }else{print("switch set to don't export geoClean datasets")}
                
                rm(ndbc_list,ncei_list, ndbc, ncei, dat_metadata, ncei_metadata)
                
                #----------------------------------------------------------------------------------------
                # stats and plotting data
                #----------------------------------------------------------------------------------------
            
                if(switch_set_a == 1){
            
                    # if one or more dataset is present but may have different time periods
                    print("Working on stats and plots...")
            
                    data_lists <- ls(pattern = buoy)
                    # remove preNDBC, station metadata, data counts and original netcdf data
                    data_lists <- data_lists[!grepl("_preNDBC_", data_lists)]
                    data_lists <- data_lists[!grepl("_station_metadata", data_lists)]
                    data_lists <- data_lists[!grepl("_ndbc_ncei_data_counts", data_lists)]
                    data_lists <- data_lists[!grepl("_netcdf", data_lists)]
                    print(data_lists)
                    
                    #----------------------------------------------------------------------------------------
                    # aggregating and plotting
                    #----------------------------------------------------------------------------------------
                    if (!file.exists(paste0(stats_dir,buoy,"/"))) {dir.create((paste0(stats_dir,buoy,"/")))}
                    if (!file.exists(paste0(fig_dir,buoy,"/"))) {dir.create((paste0(fig_dir,buoy,"/")))}
            
                    for(df in data_lists){
                        print(df)
                      
                        if(length(grep("_sensor_output",df))>0){
                              dat_s <- get(df)
                              png(paste0(fig_dir, paste0(buoy,"/",df,".png")), width = width1, height = height1)
                              mainTitle <- paste0("NDBC Station ", buoy," NCEI Sensor Output")
                              plot(dat_s$DateTime, dat_s$sensor_output,  
                                   xlab = "Date", ylab = "1 = Displacement / 2 = Acceleration",
                                   lty = 5, pch = 0, col = "skyblue2", cex = 1, cex.lab = 1.2, 
                                   panel.first=grid(), main = mainTitle)
                              dev.off()
                              print(paste0("printing... ",mainTitle))
                              rm(dat_s)
                        }else if(length(grep("_stdmet",df))>0){
                              # GPS data
                              print("working on GPS plot...")
            
                              # get data
                              dat <- get(df)
            
                              if(dim(dat)[1]>0){
                                
                                  # # formatting datasets
                                  if(length(grep("_stdmet",df))>0){
                                      # library(tidyverse) 
                                      dat <- dat %>% dplyr::select(-contains("_metadata"))
                                      if(grepl("_ncei",df)==TRUE){
                                          # reducing primary and secondary data to one column
                                          var_names <- names(dat[4:ncol(dat)])
                                          var_names <- var_names[grepl("_1", var_names)]
                                          # replace missing primary data with secondary data for plotting and stats
                                          for(r in var_names){
                                              for(q in 1:nrow(dat)){
                                                  if(is.na(dat[q,r]) & !is.na(dat[q,gsub("_1", "_2", r)])){
                                                    dat[q,r] <- dat[q,gsub("_1", "_2", r)]
                                                    dat[q,gsub("_1", "_2", r)] <- NA
                                                  }
                                                  if(!is.na(dat[q,r])){
                                                    dat[q,gsub("_1", "_2", r)] <- NA
                                                  }
                                              }
                                          }
                                        # remove columns if empty
                                        dat <- dat[,colSums(is.na(dat))<nrow(dat)]
                                        # remove _1 from col names
                                        colnames(dat) <- sub("_1","",colnames(dat))
                                      }
                                  }
            
                                  if("lat" %in% names(dat)){
                                      # compute the bounding box
                                      library(ggplot2)
                                      library(ggmap)
                                      library(maps)
                                      library(mapdata)
                                      #----------------------------------------------------------------------------------------
                                      gps <- dplyr::select(dat, lat,lon)
                                      gps <- gps[complete.cases(gps),]
                                    
                                      bc_bbox <- make_bbox(lat = lat, lon = lon, data = gps)
                                      bc_bbox
                                      tryCatch({
                                            # grab the maps from google
                                            bc_big <- get_map(location = bc_bbox, source = "google", maptype = "satellite")
                                            #> Warning: bounding box given to google - spatial extent only approximate.
                                            #> converting bounding box to center/zoom specification. (experimental)
                                            #> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=51.86665,-127.98475&zoom=6&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false
                                            # maptype = c("terrain","terrain-background", "satellite", "roadmap", "hybrid", "toner",
                                            #             "watercolor", "terrain-labels", "terrain-lines", "toner-2010",
                                            #             "toner-2011", "toner-background", "toner-hybrid", "toner-labels",
                                            #             "toner-lines", "toner-lite")
                                            # source = c("google", "osm", "stamen")
                                            
                                            # ggplot code
                                            bp <- ggmap(bc_big) +
                                              geom_point(data = ncei_positions, mapping = aes(x = lon, y = lat, color = color_ndbc_raw), size = 2) +
                                              geom_point(data = dat, mapping = aes(x = lon, y = lat, color = color_ndbc_orig), size = 2)
                                            
                                            # plotting
                                            b1 <- bp +  labs(title = paste0("Station ",buoy," NDBC GPS Positions\n"), x = "Longitude", y = "Latitude", color = "Data Sources") +
                                              scale_color_hue(labels = c("NCEI NetCDF Raw", "NDBC GeoCleaned Data"))#, values = c(color_ndbc_raw, color_ndbc_orig))
                                            # export the plot
                                            tiff(paste0(gps_dir, paste0("s_",buoy,"_Raw_vs_geoClean_NDBC_GPSPositions.tiff")), width = width, height = height, res = res)
                                            b1
                                            dev.off()
                                            # png
                                            png(paste0(gps_dir, paste0(df,"_Raw_vs_geoClean_NDBC_GPSPositions_ggplot.png")), width = width, height = height, res = res)
                                            b1
                                            dev.off()
                                            rm(bp,b1)
                                      }, error = function(e){
                                            print("no map box plot")
                                      })
            
                                      ## plot positions overall
                                      png(paste0(gps_dir, paste0(df,"_Raw_vs_geoClean_NDBC_GPSPositions.png")), width = width, height = height, res = res)
                                      mainTitle <- paste0("Station ",buoy," NDBC GPS Positions")
                                      print(paste0("printing... ",mainTitle))
                                      plot(ncei_positions$lon, ncei_positions$lat, col = color_ndbc_raw,
                                           main = mainTitle, ylab = "Latitude (N)", xlab = "Longitude (W)")
                                      points(dat$lon, dat$lat, col = color_ndbc_orig, pch = 1)
                                      par(xpd=TRUE)
                                      legend("topright", inset=c(0,-0.085),
                                             box.lty = 0, legend=c("NCEI NetCDF data","NDBC GeoClean data"),
                                             col=c(color_ndbc_raw, color_ndbc_orig),
                                             pch = c(1,1),
                                             cex=1)
                                      dev.off()
                                  }else{print(paste0("no gps data in ",df))}
            
                                  # standard meteorological data
                                  print("working on stdmet...")
                                  
                                  # set columns of interest list
                                  dataCols <- c("wind_direction","wind_speed","wind_gust","significant_wave_height","dominant_wave_period",
                                                "average_wave_period","mean_wave_direction","air_pressure_at_sea_level","air_temperature","sea_surface_temperature")
                                  # remove `air_pressure_at_sea_level` in dataCols2
                                  dataCols_2 <- dataCols[-8]
                                  
                                  dat_stats <- dat
                                  dat_stats$lat <-NULL; dat_stats$lon <-NULL; dat_stats$DateTime <- NULL
                                  stats_all <- data.frame(do.call(cbind, lapply(dat_stats, summary)), stringsAsFactors = FALSE)
                                  # adding in a NA row if no NA's present in dataset (and therefore not added during summary, which throws off row names)
                                  if("NA's" %in% rownames(stats_all)==FALSE){stats_all[nrow(stats_all)+1,] <- 0}
                                  stats_all[nrow(stats_all)+1,] <- nrow(dat_stats)
                                  rownames(stats_all) <- c("Min","1st_Qu","Median","Mean","3rd_Qu","Max","NA","Total_Count")
                                  stats_all <- signif(stats_all,3)
                                  stats_all[nrow(stats_all)-1,1:ncol(stats_all)]<- trunc(stats_all[nrow(stats_all)-1,1:ncol(stats_all)])
                                  stats_all[nrow(stats_all),1:ncol(stats_all)]<- trunc(stats_all[nrow(stats_all),1:ncol(stats_all)])
                                  write.csv(stats_all,paste0(stats_dir, buoy, "/",df,"_stats.csv"),row.names=TRUE)
                                  rm(dat_stats, stats_all)
            
                                  #----------------------------------------------------------------------------------------
                                  # plotting and stats
                                  #----------------------------------------------------------------------------------------
                                  for(i in dataCols_2){
                                      # options(warn = -1)
            
                                      print(paste0("Working on... " , i))
            
                                      if(i %in% names(dat)){
                                            # print("plotting")
                                            library(dplyr)
                                            dat_df <- dplyr::select(dat, DateTime, all_of(i))
                                            # checking for empty columns
                                            if(sum(is.na(dat_df[,2])) != nrow(dat_df)){
                                                  # export plot
                                                  source(paste0(data_dir,"plot_stdmet.R"))
                                                  png(paste0(fig_dir, buoy,"/",df,"_",i,".png"), width = width, height = height, res = res)
                                                  plot_stdmet(dat_df, i, buoy)
                                                  dev.off()
                                            }else{print(paste0(i," data are all NA for ",buoy))}
                                            rm(dat_df)
            
                                      }else{print(paste0("no ",i," data for ",buoy)) 
                                            tryCatch({
                                                  dev.off()
                                            },error=function(cond) {
                                              message("")
                                            })
                                      }
                                  }
                                  rm(dat)
                              }
            
                        }else{
                              #----------------------------------------------------------------------------------------
                              # stats
                              #----------------------------------------------------------------------------------------
                              print(paste0("Working on... " , df))
            
                              # print("plotting")
                              dat <- get(df)
                              if(unique(sapply(dat[,2:ncol(dat)], class))=="character"){
                                    dat[,2:ncol(dat)] <- sapply(dat[,2:ncol(dat)],as.numeric)
                              }
                              # plot_stats_name <- gsub("_geoClean", "",df)
                              plot_stats_name <- df
                              
                              if(dim(dat)[1]>0){
                                    # descriptive stats
                                    if("lat" %in% names(dat)){
                                          stats <- as.data.frame.matrix(summary(dat[,4:ncol(dat)]), stringsAsFactors = FALSE)
                                    }else{
                                          stats <- as.data.frame.matrix(summary(dat[,2:ncol(dat)]), stringsAsFactors = FALSE)
                                    }
                                    write.csv(stats,paste0(stats_dir, buoy,"/",plot_stats_name,"_stats.csv"),row.names=FALSE)
                                    rm(stats)
              
                                    #----------------------------------------------------------------------------------------
                                    # aggregating and plotting
                                    #----------------------------------------------------------------------------------------
                                    library(lubridate)
                                    # accounting for single years of comparison data- daily, month or yearly grouping
                                    if(length(unique(year(dat$DateTime))) >= 2){
                                        agg_period <- "annual"
                                        dat$grp <- year(dat$DateTime)
                                    }
                                    if(length(unique(year(dat$DateTime))) < 2){
                                        if(length(unique(month(dat$DateTime))) <= 1){
                                            agg_period <- "daily"
                                            year_plot <- unique(year(dat$DateTime))
                                            dat$grp <- day(dat$DateTime)
                                        }else{
                                            agg_period <- "monthly"
                                            year_plot <- unique(year(dat$DateTime))
                                            dat$grp <- month(dat$DateTime)
                                        }
                                    }
                                    # descriptive stats
                                    if("lat" %in% names(dat)){
                                        dat <- aggregate(dat[, 3:ncol(dat)-1], list(dat$grp), mean, na.rm = TRUE)
                                    }else {
                                        dat <- aggregate(dat[, 2:ncol(dat)-1], list(dat$grp), mean, na.rm = TRUE)
                                    }

                                    if("lat" %in% names(dat)){
                                        dat$lat <- NULL; dat$lon <- NULL
                                    }else{print(paste0("no lat/lon in ",df))}
                                    if("DateTime" %in% names(dat)){
                                      dat$DateTime <- NULL
                                    }else{print(paste0("no DateTime in ",df))}
              
                                    # reformat for plotting purposes
                                    dat1 <- dat
                                    is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
                                    dat1[is.nan.data.frame(dat1)] <- 0
                                    dat1 <- data.frame(t(dat1),stringsAsFactors = FALSE)
                                    names(dat1) <- dat1[1,]
                                    col_name <- names(dat1)
                                    dat1 <- data.frame(dat1[-1,],stringsAsFactors = FALSE)
                                    names(dat1) <- col_name
                                    library(dplyr)
                                    library(tibble)
                                    dat1 <- tibble::rownames_to_column(dat1, "Frequency")
                                    if(agg_period == "daily" | agg_period == "monthly"){
                                        if(agg_period == "monthly"){
                                            colnames(dat1) <- c("Frequency",month.abb[as.numeric(gsub("X","",names(dat1[2:ncol(dat1)])))])
                                        }else{
                                            colnames(dat1) <- c("Frequency",gsub("X","",names(dat1[2:ncol(dat1)])))
                                        }
                                    }
                                    dat1$Frequency <- as.numeric(gsub("X","",dat1$Frequency))
                                    dat1$Frequency <- signif(dat1$Frequency, digits = 6)
                                    if(agg_period != "monthly"){
                                        names_dat1 <- as.numeric(gsub("X","",names(dat1[2:ncol(dat1)])))
                                        colnames(dat1) <- c("Frequency",names_dat1)
                                    }
                                    # export as csv
                                    write.table(dat1, paste0(stats_dir,buoy,"/",plot_stats_name,"_",agg_period,"_mean.csv"), row.names=FALSE, col.names = TRUE, sep = ",")
              
                                    # formatting for plots
                                    if(agg_period == "daily"){year_num <- names(dat1); year_num <- as.numeric(year_num[-1])}
                                    if(agg_period == "monthly"){year_num <- names(dat1);year_num <- year_num[-1]}
                                    if(agg_period == "annual"){year_num <- names(dat1);year_num <- as.numeric(year_num[-1])}
                                    print(year_num)
              
                                    for (year1 in year_num){
                                        source(paste0(data_dir,"plots_spec.R"))
                                        plot_titles <- paste0(plot_stats_name,"_mean")
                                        library(R.utils)
                                        if(unlist(strsplit(plot_titles,"_"))[3]=="ncei") {main_titles <- gsub("Ncei","NCEI",gsub("S ","NDBC Station ",str_to_title(gsub("_"," ",plot_titles))))}
                                        if(unlist(strsplit(plot_titles,"_"))[3]=="ndbc") {main_titles <- gsub("Ndbc","NDBC",gsub("S ","NDBC Station ",str_to_title(gsub("_"," ",plot_titles))))}
                                        if(agg_period == "monthly"){
                                            png(paste0(fig_dir, paste0(buoy,"/",plot_titles,year1,"_",year_plot,".png")), width = width1, height = height1)
                                            mainTitle <- paste0(main_titles," ",year1,", ",year_plot)
                                            plots_spec(dat1, year1, df, mainTitle)
                                            dev.off()
                                            print(paste0("printing... ",mainTitle))
                                        }else if(agg_period == "daily"){
                                            png(paste0(fig_dir, paste0(buoy,"/",plot_titles,year1,"_",year_plot,".png")), width = width1, height = height1)
                                            mainTitle <- paste0(main_titles," ",year1,", ",year_plot)
                                            plots_spec(dat1,year1, df, mainTitle)
                                            dev.off()
                                            print(paste0("printing... ",mainTitle))
                                        }else{
                                            png(paste0(fig_dir, buoy,"/",plot_titles,year1,".png"), width = width1, height = height1)
                                            mainTitle <- paste0(main_titles," ",year1)
                                            plots_spec(dat1,year1, df, mainTitle)
                                            dev.off()
                                            print(paste0("printing... ",mainTitle))
                                        }
                                    }
            
                              }else{print(paste0(df," is an empty dataset"))}
                              rm(dat1)
                        } # end plot individual
                    }
                    #----------------------------------------------------------------------------------------
            
                }else{print("switch set to don't plot this time")}
            
                #----------------------------------------------------------------------------------------
                
                # housekeeping    
                rm(list = ls(pattern = buoy))
                rm(ncei_positions)
                rm(data_count,a,c,d,i,n,g,p,name1,name2,r,var_index,idcols,index,j,lat_mean, lat_tail,lon_mean,lon_tail,dir_column,q,dat_source,t, 
                   dat_geoClean_start,dat_geoClean_end,met,ncei_dat_ls,ncei_ls,ndbc_ls,ndbc_name,ndbc_ord,dat_list,dat_ls,column_names,single_sensor_ls,
                   start_date_ncei,start_date_ndbc,remove,dat_orig_end,dat_orig_start,col_index,col_names,colNames,outlier_table,var_list,depth,hull,
                   mooring, payload, gps, agg_period, bc_bbox, bc_big, col_name, dat_name, data_lists, dataCols, dataCols_2, df, main, main_titles,
                   mainTitle, name_net, names_dat1, ndbc_col_num, ncei_col_num, plot_stats_name, plot_titles, var,var_names, year_num, xlab,year_num, year1)
                if(exists("ncei_pre")){rm(ncei_pre)}
                
                # start writing to an output file
                print(paste0("Finishing with geoClean: ",buoy))
                # sink()
                
          }else{print("no new buoy data")}
          
          print(paste0("Finishing with geoClean: ",buoy))
      }
}
# #----------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------



    
    
    
    
    
    
    
    
    
       

    
    


