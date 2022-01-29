read_NetCDF_post_2011 <- function(x = "list of files", buoy = "buoy number", ascii_ncei_dir = "ascii_ncei_dir"){
        library("ncdf4")
        library("stringr")
        ## create buoy specific extract folder
        folder_name <- gsub(".nc","",unlist(strsplit(x, paste0("/",buoy,"/")))[2])
        dir.create((paste0(ascii_ncei_dir,buoy,"/",folder_name)))
        
        tryCatch({
                nc <- nc_open(x)                         # Reading the netcdf data
                print(nc$filename)
                # extract time data
                time <- data.frame(ncvar_get(nc, "time"), stringsAsFactors = FALSE)
                # test for 10 second time field
                tryCatch({time_10min = data.frame(ncvar_get(nc, "time10"), stringsAsFactors = FALSE)
                }, error = function(e) {print("no 10-second time field")
                })
                # time_10min <- data.frame(ncvar_get(nc, "time10"), stringsAsFactors = FALSE)
                # human readable time
                time[,1] <- as.POSIXct(time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                colnames(time) <- c("time")
                time_list <- ls(pattern = "time")
                if("time_10min" %in% time_list){
                        time_10min[,1] <- as.POSIXct(time_10min[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                        colnames(time_10min) <- c("time")
                }
                # check for empty NetCDF file before continuing.
                if(nc$nvars != 0){
                        # Get a list of the nc variable names.
                        dat_list <- attributes(nc$var)$names
                        # lat and lon
                        if("gps_1/latitude" %in% dat_list){
                                lat <- cbind(time,data.frame(ncvar_get(nc, "gps_1/latitude"), stringsAsFactors = FALSE),
                                             data.frame(ncvar_get(nc, "gps_1/latitude_qc"), stringsAsFactors = FALSE),
                                             data.frame(ncvar_get(nc, "gps_1/latitude_release"), stringsAsFactors = FALSE))

                                lon <- cbind(time, data.frame(ncvar_get(nc, "gps_1/longitude"), stringsAsFactors = FALSE),
                                             data.frame(ncvar_get(nc, "gps_1/longitude_qc"), stringsAsFactors = FALSE),
                                             data.frame(ncvar_get(nc, "gps_1/longitude_release"), stringsAsFactors = FALSE))
                                colnames(lat) <- c("time", "lat", "qc_flag", "release_flag")
                                colnames(lon) <- c("time", "lon","qc_flag", "release_flag")
                        }else{
                                lat <- time
                                lat$lat <- NA; lat$qc_flag <- NA; lat$release_flag <- NA
                                lon <- time
                                lon$lon <- NA; lon$qc_flag <- NA; lon$release_flag <- NA
                        }
                        # concat time, lat, lon
                        library(dplyr)
                        time_position <- full_join(lat[,1:2], lon[,1:2], by = "time")
                        # export files
                        write.csv(lat, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_lat.csv"), row.names=FALSE, na = "NaN")
                        write.csv(lon, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_lon.csv"), row.names=FALSE, na = "NaN")
                        write.csv(time, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time.csv"), row.names=FALSE, na = "NaN")
                        write.csv(time_position, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time_position.csv"), row.names=FALSE, na = "NaN")

                        if("time_10min" %in% time_list){
                                time_10min_position <- full_join(time_10min, lat[,1:2], by = "time")
                                time_10min_position <- full_join(time_10min_position, lon[,1:2], by = "time")
                                colnames(time_10min_position) <- c("time", "lat", "lon")
                                # fill in missing lat/lon
                                library(tidyr)
                                time_10min_position <- time_10min_position %>% fill(c(lat,lon),.direction = "downup")
                                write.csv(time_10min, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time_10min.csv"), row.names=FALSE, na = "NaN")
                                write.csv(time_10min_position, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time_10min_position.csv"), row.names=FALSE, na = "NaN")
                                rm(time_10min)
                        }
                        
                        rm(lat,lon, time)
                        # remove qc and release fields from main dat list
                        dat_use <- Filter(function(x) !any(grepl("_qc", x)), dat_list)
                        dat_use <- Filter(function(x) !any(grepl("_release", x)), dat_use)
                        
                        a = 1; g = 1; b = 1; d = 1; w = 1
                        a_ls <- NULL; g_ls <- NULL; b_ls <- NULL; d_ls <- NULL; w_ls <- NULL
                        # extract data through variable names, attached time and position, and export
                        for(j in dat_use){
                                dat <- data.frame(ncvar_get(nc, j), stringsAsFactors = FALSE)
                                # find qc and release code
                                if(paste0(j,"_qc") %in% dat_list){
                                        qc <- data.frame(ncvar_get(nc, paste0(j,"_qc")), stringsAsFactors = FALSE)
                                }
                                if(paste0(j,"_release") %in% dat_list){
                                        rl <- data.frame(ncvar_get(nc, paste0(j,"_release")), stringsAsFactors = FALSE)
                                }
                                # define column name
                                if(!is.na(unlist(strsplit(j, "/"))[2])){
                                        dat_name <- unlist(strsplit(j, "/"))[2]
                                }else{
                                        dat_name <- j
                                }
                                # build dataframe
                                if(dim(dat)[1]==dim(time_position)[1]){
                                        if(paste0(j,"_qc") %in% dat_list){
                                                if(paste0(j,"_release") %in% dat_list){
                                                        dat <- cbind(time_position,dat, qc, rl)
                                                        colnames(dat) <- c("time","lat",'lon',dat_name, "qc_flag","release_flag")
                                                }
                                        }else{
                                                dat <- cbind(time_position,dat)
                                                colnames(dat) <- c("time","lat",'lon',dat_name)
                                                
                                        }
                                }
                                if(dim(dat)[2] == dim(time_position)[1]){                # spectral parameters
                                        if(dim(dat)[1] == 2){
                                                dat <- dat
                                        }else{
                                                dat <- data.frame(t(dat), stringsAsFactors = FALSE)
                                                dat <- cbind(time_position,dat)
                                                tryCatch({
                                                        wave_wpm <- ncvar_get(nc, "wave_wpm")
                                                        colnames(dat) <- c("time","lat",'lon',signif(wave_wpm,4))
                                                }, error = function(e){print("no wave_wpm data")})
                                        }
                                }
                                if("time_10min" %in% time_list){
                                        if(dim(dat)[1]==dim(time_10min_position)[1]){
                                                if(dim(rl)[1] == dim(time_position)[1]){
                                                        rl <- cbind(time_position[1], rl)
                                                        dat <- cbind(time_10min_position,dat, qc)
                                                        time_new <- dplyr::left_join(time_10min_position[1], rl, by = "time")
                                                        dat <- left_join(dat,time_new, by = "time")
                                                }else{
                                                        dat <- cbind(time_10min_position,dat, qc, rl)
                                                }
                                                colnames(dat) <- c("time","lat","lon",dat_name, "qc_flag","release_flag")
                                        }
                                        if(dim(dat)[1] < dim(time_10min_position)[1] & dim(dat)[1] > dim(time_position)[1]){                # spectral parameters
                                                if(j != "time_wpm_40_bnds"){
                                                        if(dim(qc)[1]==dim(rl)[1]){
                                                                dat <- cbind(dat, qc, rl)
                                                                colnames(dat) <- c(dat_name, "qc_flag","release_flag")
                                                        }else{
                                                                dat <- cbind(dat, qc)
                                                                colnames(dat) <- c(dat_name, "qc_flag")
                                                                dat$release_flag <- NA
                                                        }
                                                }else{
                                                        dat <- dat
                                                        colnames(dat) <- dat_name                        }
                                        }
                                } 
                                
                                # define export name and export
                                dat_name <- gsub("/","_",j)
                                find_dat <- unlist(strsplit(dat_name,"_"))[1]
                                payload <- names(unlist(nc$fqgn2Rindex))
                                # print(dat_name)
                                if(find_dat == "gps"){
                                        if(dat_name %in% g_ls){g <- sum(str_count(g_ls, paste0(dat_name,"$")))+1}
                                        g_ls <- c(g_ls,dat_name)
                                        payload_num <- unlist(payload[grepl(find_dat,payload, ignore.case = TRUE)])
                                        payload_num <- unlist(strsplit(payload_num[g],paste0("/",find_dat)))[1]
                                }else if(find_dat == "anemometer"){
                                        if(dat_name %in% a_ls){a <- sum(str_count(a_ls, paste0(dat_name,"$")))+1}
                                        a_ls <- c(a_ls,dat_name)
                                        payload_num <- unlist(payload[grepl(find_dat,payload, ignore.case = TRUE)])
                                        payload_num <- unlist(strsplit(payload_num[a],paste0("/",find_dat)))[1]
                                }else if(find_dat == "barometer"){
                                        if(dat_name %in% b_ls){b <- sum(str_count(b_ls, paste0(dat_name,"$")))+1}
                                        b_ls <- c(b_ls,dat_name)
                                        payload_num <- unlist(payload[grepl(find_dat,payload, ignore.case = TRUE)])
                                        payload_num <- unlist(strsplit(payload_num[b],paste0("/",find_dat)))[1]
                                }else if(find_dat == "air"){
                                        if(dat_name %in% d_ls){d <- sum(str_count(d_ls, paste0(dat_name,"$")))+1}
                                        d_ls <- c(d_ls,dat_name)
                                        payload_num <- unlist(payload[grepl(find_dat,payload, ignore.case = TRUE)])
                                        payload_num <- unlist(strsplit(payload_num[d],paste0("/",find_dat)))[1]
                                }else if(find_dat == "wave"){
                                        if(dat_name %in% w_ls){w <- sum(str_count(w_ls, paste0(dat_name,"$")))+1}
                                        w_ls <- c(w_ls,dat_name)
                                        payload_num <- unlist(payload[grepl(find_dat,payload, ignore.case = TRUE)])
                                        payload_num <- unlist(strsplit(payload_num[w],paste0("/",find_dat)))[1]
                                }else{
                                        payload_num <- unlist(strsplit(payload[grepl(find_dat,payload, ignore.case = TRUE)],"/"))[1]
                                }
                                print(paste0(payload_num,"_",dat_name))
                                write.csv(dat, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_",payload_num,"_",dat_name,".csv"), row.names=FALSE, na = "NaN")
                                rm(dat)
                                if(exists("qc")){rm(qc)}
                                if(exists("rl")){rm(rl)}
                        }
                        nc_close(nc)
                        rm(nc, time_position)
                        if("time_10min" %in% time_list){
                                rm(time_10min_position)
                        }
                }else{                
                        print("No data in NetCDF")
                        if(exists("nc")){nc_close(nc)}
                }
        }, error = function(e){print(paste0("can't open: ",file))}
        )
}
  