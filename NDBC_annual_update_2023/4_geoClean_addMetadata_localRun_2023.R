geoClean_addMetadata_4 <- function(buoys = "list of buoys", data_dir = "data_dir"){
      
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
  
      # remotes::install_github("dkahle/ggmap")

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
      # 1 == plot for geoClean dataset creation, 2 == don't plot/stats
      # switch_set <- 1
      switch_set_a <- 1
      # GPS buffer
      GPS_buffer <- 2
      
      ##----------------------------------------------------------------------------------------
      ## set paths
      ##----------------------------------------------------------------------------------------
      drive <- "D:/Candice/"
      # drive <- "/p/work/candice/"
      # data_dir <- paste0(drive, "projects/WaveTrends/annual_runs/data/")
      data_dir <- paste0(drive, "projects/WaveTrends/NDBC_2024/data/")
      
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
          # buoy <- buoys[14]
        
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
                # rm_ls <- ls(pattern = paste0('s_',buoy,'_ncei_stdmet')); rm_ls <- rm_ls[!grepl("_verified",rm_ls)]
                # rm(list = rm_ls)
                
                #-------------------------------------------------
                # check to remove empty dataframes, if any
                #-------------------------------------------------
                print("checking for empty dataframes")
                dat_list <- ls(pattern = buoy)
                # print(dat_list)
                for(d in dat_list){dat <- get(d);dat <- data.frame(dat, stringsAsFactors = FALSE)
                    if(dim(dat)[1]<= 1){rm(list = ls()[grepl(d, ls())]); print(paste0("removing empty df: ",d))}# else{print(paste0(d, " is not empty"))}
                    rm(dat)
                }
                
                #-------------------------------------------------
                # isolating common metadata from verified ncei df
                #-------------------------------------------------
                print("Handling verified ncei metadata")
                # rename non verified data
                ncei_list <- ls(pattern = "_ncei_stdmet_verified")
                # ncei_list <- ncei_list[!ncei_list %in% grep(paste0("_verified", collapse = "|"), ncei_list, value = T)]
                # print(ncei_list) 
                dat <- get(ncei_list)
                # isolating common metadata from verified ncei df
                drop_cols <- c("DateTime", "lat", "lon","depth", "mooring", "hull", "payload", "waveSys_Sensor")
                meta_df <- dplyr::select(dat,all_of(drop_cols))
                dat <- dat[, !names(dat) %in% c("depth", "mooring", "hull", "payload","waveSys_Sensor", "lat_qc_flag", "lon_qc_flag")]
                name_net <- paste0("s_",buoy,"_ncei_stdmet_verified")
                assign(name_net, dat)
                rm(dat)
                
                #----------------------------------------------------------------------------------------
                ## geoclean matching data
                #----------------------------------------------------------------------------------------
                print("GeoCleaning matching datasets")
                ncei_list <- ls(pattern = "_ncei_")
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
                
                # match ndbc datasets to geoCleaned GPS positions
                for(df in ndbc_list){
                    # df <- ndbc_list[1]
                    dat <- get(df)
                    dat <- dplyr::right_join(ncei_positions,dat, by = "DateTime")
                    if(sum(is.na(dat$lat), na.rm = FALSE) == nrow(dat)){ # where ndbc and ncei time minutes don't match
                        dat$lat <- NULL; dat$lon <- NULL
                        dat <- data.table(dat); ncei_positions_dt <- data.table(ncei_positions)
                        setkey(dat,DateTime); 
                        setkey(ncei_positions_dt, DateTime)
                        dat <- dat[ncei_positions_dt, roll = "nearest" ]
                        dat <- data.frame(dat)
                        # reset order of df
                        dataCols <- c('DateTime','lat','lon',names(dat)[2:48])
                        dat <- dplyr::select(dat, all_of(dataCols))
                        colnames(dat) <- gsub('X','',names(dat))
                        rm(dataCols)
                    }
                    assign(df,dat)
                    rm(dat)
                }
                
                # remove comp rows with no gps position
                print('remove comp rows with no gps position')
                stdspec <- ls(pattern = buoy)
                for(d in stdspec){
                    # d <- stdspec[1]
                    print(d)
                    dat <- get(d)
                    dat <- dat %>% filter(!is.na(lat))
                    dat <- dat %>% filter(!is.na(lon))
                    # ordering the df by date and selecting unique values only
                    dat <- dat[order(dat$DateTime),]
                    dat <- unique(dat)
                    # rename the rows to reflect unique data
                    row.names(dat) <- 1:nrow(dat)
                    assign(d,dat)
                    rm(dat)
                }
                rm(d)
                
                # test ndbc stdmet release against ncei release
                print('test ndbc stdmet release against ncei release')
                ncei <- get(ls(pattern = '_stdmet_verified'))
                ndbc <- get(ls(pattern = '_ndbc_stdmet'))
                
                # test vars
                ndbc_names <- names(ndbc)
                drop_names <- c("DateTime","lat","lon","depth","mooring","hull","payload","waveSys_Sensor")
                ndbc_names <- ndbc_names[!ndbc_names %in% drop_names]
                for(i in ndbc_names){
                    # i <- ndbc_names[4]
                    print(i)
                    ndbc_df <- dplyr::select(ndbc, c("DateTime",contains(i)))
                    ncei_df <- dplyr::select(ncei,c('DateTime',contains(i)))
                    ndbc_df$metadata <- NA
                    dat <- dplyr::left_join(ndbc_df,ncei_df, by = "DateTime")
                    # only primary sensor
                    if(dim(dat)[2]==7){
                        # test primary sensor
                        for(r in 1:nrow(dat)){if(!is.na(signif(as.numeric(dat[r,2]),2)) & !is.na(signif(as.numeric(dat[r,4]),2))){dat$metadata[r] <- dat[r,7]}}
                        # dat <- dplyr::select(dat,c('DateTime','metadata'))
                        # colnames(dat) <- c('DateTime',paste0(i,'_metadata'))
                        # # add to ndbc
                        # ndbc <- dplyr::left_join(ndbc,dat,by = 'DateTime')
                        # rm(ndbc_df, ncei_df)
                    }
                    # test for secondary sensor
                    if(dim(dat)[2]==11){
                      # test primary sensor
                      for(r in 1:nrow(dat)){if(!is.na(signif(as.numeric(dat[r,2]),2)) & !is.na(signif(as.numeric(dat[r,4]),2))){dat$metadata[r] <- dat[r,7]}}
                      # test for missing metadata and fill in with secondary metadata
                      for(r in 1:nrow(dat)){if(is.na(dat$metadata[r])){if(!is.na(signif(as.numeric(dat[r,2]),2)) & !is.na(signif(as.numeric(dat[r,8]),2))){dat$metadata[r] <- dat[r,11]}}else{print('no secondary metadata needed')}}
                    }
                    # fill in missing metadata
                    for(r in 1:nrow(dat)){if(!is.na(dat[r,2]) & is.na(dat$metadata[r])){dat$metadata[r] <- 'no available metadata'}}
                    dat <- dplyr::select(dat,c('DateTime','metadata'))
                    colnames(dat) <- c('DateTime',paste0(i,'_metadata'))
                    # add to ndbc
                    ndbc <- dplyr::left_join(ndbc,dat,by = 'DateTime')
                    rm(ndbc_df, ncei_df,dat)
                }
                rm(i)
                
                # reorder ndbc df
                dataCols <- c("wind_direction", "wind_speed", "wind_gust", "significant_wave_height", "dominant_wave_period",
                              "average_wave_period", "mean_wave_direction", "air_pressure_at_sea_level", "air_temperature", "sea_surface_temperature",
                              "dew_point_temperature" , "air_pressure_at_sea_level")
                ndbc_names <- names(ndbc); var_names <- vector() 
                for(d in dataCols){var_names <- c(var_names,ndbc_names[grepl(d,ndbc_names)])}
                ndbc <- dplyr::select(ndbc, c('DateTime','lat','lon',all_of(var_names)))
                rm(dataCols, ndbc_names,var_names,d)
                
                # check metadata are available
                dat_names <- names(ndbc); drop_names <- c("DateTime","lat","lon")
                dat_names <- dat_names[!dat_names %in% drop_names]; dat_names <- dat_names[!grepl('_metadata',dat_names)]
                for(n in dat_names){
                    # n <- dat_names[1]
                    met_name <- paste0(n,'_metadata')
                    if(met_name %in% names(ndbc)){print(paste0(met_name,' present'))}else{print(peacanpie)}
                }
                
                # add metadata
                print('add common metadata')
                meta_df <- dplyr::select(meta_df, c("DateTime","depth","mooring","hull","payload","waveSys_Sensor"))
                # join df's
                ndbc <- dplyr::left_join(ndbc,meta_df, by = "DateTime")
                for(i in 1:nrow(ndbc)){if(is.na(ndbc$significant_wave_height[i])){ndbc$waveSys_Sensor[i]<- NA}}
                # save ndbc df
                ndbc_ls <- ls(pattern = '_ndbc_stdmet')
                assign(ndbc_ls[1],ndbc)
                rm(ncei,ndbc,meta_df,ndbc_ls,ndbc_df,ncei_df)
                
                # add geoClean to name
                df_ls <- ls(pattern = buoy)
                for(i in df_ls){
                    # i <- df_ls[1]
                    df <- get(i)
                    new_name <- paste0(i,'_geoClean')
                    assign(new_name,df)
                    rm(df)
                }
                rm(df_ls)
                #----------------------------------------------------------------------------------------
                # exporting GeoCleaned datasets
                #----------------------------------------------------------------------------------------

                # if(switch_set == 1){
                print("Exporting Comp and GeoCleaned data")

                ndbc_list <- ls(pattern = "_ndbc_"); ncei_list <- ls(pattern = "_ncei_")
                ndbc_list <- ndbc_list[grepl("_geoClean", ndbc_list)]
                print(paste0("NDBC datasets :", ndbc_list))
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

                rm(ndbc_list,ncei_list, drop_names, drop_cols)

                #----------------------------------------------------------------------------------------
                # stats and plotting data
                #----------------------------------------------------------------------------------------

                if(switch_set_a == 1){
                  
                    # compute the bounding box
                    library(ggplot2)
                    library(ggmap)
                    library(maps)
                    library(mapdata)
                    #----------------------------------------------------------------------------------------
                    gps <- dplyr::select(ncei_positions, lat,lon)
                    gps <- gps[complete.cases(gps),]
                    dat <- get(ls(pattern = 'ndbc_stdmet_geoClean'))
                    
                    ## plot positions overall
                    
                    png(paste0(gps_dir, paste0(buoy,"_geoClean_NDBC_GPSPositions.png")), width = width, height = height, res = res)
                    mainTitle <- paste0("Station ",buoy," NDBC GPS Positions")
                    print(paste0("printing... ",mainTitle))
                    plot(ncei_positions$lon, ncei_positions$lat, col = 'purple',
                         main = mainTitle, ylab = "Latitude (N)", xlab = "Longitude (W)")
                    points(dat$lon, dat$lat, col = 'orange', pch = 1)
                    par(xpd=TRUE)
                    legend("topright", inset=c(0,-0.085),
                           box.lty = 0, legend=c("NCEI data","NDBC data"),
                           col=c('purple', 'orange'),
                           pch = c(1,1),
                           cex=1)
                    dev.off()
                    rm(gps,dat)
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

                # start writing to an output file
                print(paste0("Finishing with geoClean: ",buoy))
                # sink()
                
          }else{print("no new buoy data")}
          
          print(paste0("Finishing with geoClean: ",buoy))
      }
}
# #----------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------



    
    
    
    
    
    
    
    
    
       

    
    


