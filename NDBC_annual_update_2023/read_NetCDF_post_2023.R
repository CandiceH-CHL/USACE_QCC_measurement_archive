read_NetCDF_post_2023 <- function(x = "list of files", buoy = "buoy number", ascii_ncei_dir = "ascii_ncei_dir"){
        # x <- file
        library("ncdf4")
        library("stringr")
        ## create buoy specific extract folder
        # folder_name <- unlist(strsplit(gsub(".nc","",unlist(strsplit(x, paste0("netCDF/",buoy,"/")))[2]), "_D"))[1]
        folder_name <- gsub(".nc","",unlist(strsplit(x, paste0("/",buoy,"/")))[2])
        dir.create((paste0(ascii_ncei_dir,buoy,"/",folder_name)))
        
        tryCatch({
                nc <- nc_open(x)                         # Reading the netcdf data
                print(nc$filename)
                # print(paste("The file has",nc$nvars,"variables"))
                # extract time data
                time <- data.frame(ncvar_get(nc, "time"), stringsAsFactors = FALSE)
                # test for waves time field
                tryCatch({spec_time = data.frame(ncvar_get(nc, "time_waves"), stringsAsFactors = FALSE)
                }, error = function(e) {print("no wave time field")
                })
                # human readable time
                time[,1] <- as.POSIXct(time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                colnames(time) <- c("time")
                time_list <- ls(pattern = "time")
                if("spec_time" %in% time_list){
                        spec_time[,1] <- as.POSIXct(spec_time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                        colnames(spec_time) <- c("time")
                }
                # # identify variables in .nc file 
                # dat_list <- NULL
                # check for empty NetCDF file before continuing.
                if(nc$nvars != 0){
                        # Get a list of the nc variable names.
                        dat_list <- attributes(nc$var)$names
                        # test <- data.frame(ncvar_get(nc, attributes(nc$var)$names[5]), stringsAsFactors = FALSE)
                        # lat and lon
                        if("latitude" %in% dat_list){
                                lat <- cbind(time,data.frame(ncvar_get(nc, "latitude"), stringsAsFactors = FALSE),
                                             data.frame(ncvar_get(nc, "latitude_qc"), stringsAsFactors = FALSE))#,
                                             # data.frame(ncvar_get(nc, "latitude_release"), stringsAsFactors = FALSE))
                        }else{lat <- time;lat$lat <- NA; lat$qc_flag <- NA}#; lat$release_flag <- NA}
                        if("longitude" %in% dat_list){
                                lon <- cbind(time, data.frame(ncvar_get(nc, "longitude"), stringsAsFactors = FALSE),
                                             data.frame(ncvar_get(nc, "longitude_qc"), stringsAsFactors = FALSE))#,
                                             # data.frame(ncvar_get(nc, "longitude_release"), stringsAsFactors = FALSE))
                                colnames(lat) <- c("time", "lat", "qc_flag")#, "release_flag")
                                colnames(lon) <- c("time", "lon","qc_flag")#, "release_flag")
                        }else{lon <- time; lon$lon <- NA; lon$qc_flag <- NA}#; lon$release_flag <- NA}
                        
                        # concat time, lat, lon
                        library(dplyr)
                        time_position <- full_join(lat[,1:2], lon[,1:2], by = "time")
                        # export files
                        write.csv(lat, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_lat.csv"), row.names=FALSE, na = "NaN")
                        write.csv(lon, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_lon.csv"), row.names=FALSE, na = "NaN")
                        # write.csv(time, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time.csv"), row.names=FALSE, na = "NaN")
                        # write.csv(time_position, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time_position.csv"), row.names=FALSE, na = "NaN")

                        if("spec_time" %in% time_list){
                                spec_time_position <- left_join(spec_time, lat[,1:2], by = "time")
                                spec_time_position <- left_join(spec_time_position, lon[,1:2], by = "time")
                                colnames(spec_time_position) <- c("time", "lat", "lon")
                                # fill in missing lat/lon
                                library(tidyr)
                                spec_time_position <- spec_time_position %>% fill(c(lat,lon),.direction = "downup")
                                # write.csv(spec_time, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_spec_time.csv"), row.names=FALSE, na = "NaN")
                                write.csv(spec_time_position, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_spec_time_position.csv"), row.names=FALSE, na = "NaN")
                                rm(spec_time)
                        }else{
                                spec_time_position <- data.frame()
                        }
                        rm(lat,lon, time)
                        # remove qc and release fields from main dat list
                        dat_use <- Filter(function(x) !any(grepl("_qc", x)), dat_list)
                        dat_use <- Filter(function(x) !any(grepl("_release", x)), dat_use)
                        dat_use <- dat_use[!grepl('latitude',dat_use)]; dat_use <- dat_use[!grepl('longitude',dat_use)]
                        
                        a = 1#; g = 1; b = 1; d = 1; w = 1
                        # a_ls <- NULL; g_ls <- NULL; b_ls <- NULL; d_ls <- NULL; w_ls <- NULL
                        # extract data through variable names, attached time and position, and export
                        for(j in dat_use){
                                # j <- dat_use[1]
                                #print(j)
                                dat_name <- j
                                # get data
                                dat <- data.frame(ncvar_get(nc, j), stringsAsFactors = FALSE)
                                # find qc and release code
                                if(paste0(j,"_qc") %in% dat_list){
                                        qc <- data.frame(ncvar_get(nc, paste0(j,"_qc")), stringsAsFactors = FALSE)
                                }
                                if(paste0(j,"_release") %in% dat_list){
                                        rl <- data.frame(ncvar_get(nc, paste0(j,"_release")), stringsAsFactors = FALSE)
                                }
                                
                                # build dataframe
                                if(dim(dat)[1]==dim(time_position)[1]){
                                        if(paste0(j,"_qc") %in% dat_list){
                                                if(paste0(j,"_release") %in% dat_list){
                                                        dat <- cbind(time_position,dat, qc, rl)
                                                        colnames(dat) <- c("time","lat",'lon',dat_name, "qc_flag","release_flag")
                                                }else{
                                                        dat <- cbind(time_position,dat, qc)
                                                        dat$rl <- NA
                                                        colnames(dat) <- c("time","lat",'lon',dat_name, "qc_flag","release_flag")
                                                }
                                        }else{ 
                                              dat <- cbind(time_position,dat)
                                              colnames(dat) <- c("time","lat",'lon',dat_name)
                                        }
                                }else if(dim(dat)[1]!=dim(time_position)[1] & dim(dat)[1]!=dim(spec_time_position)[1] & dim(dat)[2]!=dim(spec_time_position)[1]){
                                        if(exists('qc')){dat <- cbind(dat,qc)}
                                        if(exists('rl')){dat <- cbind(dat,rl)}
                                        colnames(dat) <- c(j,'qc_flag', 'release_flag')
                                }else if(dim(dat)[1]==dim(spec_time_position)[1]){
                                        if(exists('qc') & exists('rl')){
                                            dat <- cbind(spec_time_position,dat, qc, rl)
                                            colnames(dat) <- c("time","lat",'lon',dat_name, "qc_flag","release_flag")
                                        }else if(exists('qc')){
                                          dat <- cbind(spec_time_position,dat, qc)
                                          colnames(dat) <- c("time","lat",'lon',dat_name, "qc_flag")
                                        }
                                }else if(dim(dat)[2]==dim(spec_time_position)[1]){
                                        dat <- data.frame(t(dat))
                                        freq_vals <- ncvar_get(nc, "wave_frequency")
                                        colnames(dat) <- paste0('x_',signif(freq_vals,4))
                                        dat <- cbind(spec_time_position,dat)
                                }
                                # if(grepl('_telemetry',j)){j <- paste0(unlist(strsplit(j,'sensor_'))[1],'sensor')}
                                          
                                write.csv(dat, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_",j,".csv"), row.names=FALSE, na = "NaN")
                                rm(dat)
                                if(exists("qc")){rm(qc)}
                                if(exists("rl")){rm(rl)}
                        }
                        nc_close(nc)
                        rm(nc, time_position, spec_time_position)
                        if("time_wave" %in% time_list){
                                rm(time_wave_position)
                        }
                }else{                
                        print("No data in NetCDF")
                        if(exists("nc")){nc_close(nc)}
                }
        }, error = function(e){print(paste0("can't open: ",file))}
        )
}
  