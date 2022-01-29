read_NetCDF_pre_2011 <- function(x = "list of files", buoy = "buoy number", ncei_dir = "ncei_dir", ascii_ncei_dir = "ascii_ncei_dir"){
        
        print(x)
        library("ncdf4")
        ## create buoy specific extract folder
        folder_name <- gsub(".nc","",unlist(strsplit(x, paste0("/",buoy,"/")))[2])
        if (!file.exists(paste0(ascii_ncei_dir,buoy,"/",folder_name))) {dir.create((paste0(ascii_ncei_dir,buoy,"/",folder_name)))}
        tryCatch({
                nc <- nc_open(x)                         # Reading the netcdf data
                # identify variables in .nc file 
                dat_list <- NULL
                for (i in 1:nc$nvars){
                        j<-nc$var[[i]]$name
                        # print(j)
                        dat_list <- c(dat_list,j)
                }
                # extract data
                lat <- ncvar_get(nc, "lat")	
                lon <- ncvar_get(nc, "lon")	 
                time <- data.frame(ncvar_get(nc, "time"), stringsAsFactors = FALSE)
                # human readable time
                time[,1] <- as.POSIXct(time[,1],origin = "1970-01-01",tz = "uct") # seconds since 1970-01-01 00:00:00 UTC"
                colnames(time) <- c("time")
                # export lat / lon and time
                lat_t <- merge(time,lat)
                colnames(lat_t) <- c("time", "lat")
                lon_t <- merge(time,lon)
                colnames(lon_t) <- c("time", "lon")
                # concat time, lat, lon
                library(dplyr)
                time_position <- full_join(lat_t, lon_t, by = "time")
                # consistency with post 2011 files
                lat_t$qc_flag <- NA; lat_t$release_flag <- NA
                lon_t$qc_flag <- NA; lon_t$release_flag <- NA
                # export files
                write.csv(lat_t, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_lat.csv"), row.names=FALSE, na = "NaN")
                write.csv(lon_t, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_lon.csv"), row.names=FALSE, na = "NaN")
                write.csv(time, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time.csv"), row.names=FALSE, na = "NaN")
                write.csv(time_position, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_time_position.csv"), row.names=FALSE, na = "NaN")
                rm(lat_t, lon_t, time)
                # extract data through variable names, attached time stamp and export
                for(j in dat_list[5: length(dat_list)]){
                        print(j)
                        dat <- data.frame(ncvar_get(nc, j), stringsAsFactors = FALSE)
                        if(dim(dat)[1]==dim(time_position)[1]){
                                dat <- cbind(time_position,dat)
                                colnames(dat) <- c("time","lat",'lon',j)
                                # to match post 2011 dataset
                                dat$qc_flag <- NA; dat$release_flag <- NA
                                
                        }else if(dim(dat)[2] == dim(time_position)[1]){                # spectral parameters
                                if(dim(dat)[1] == 2){
                                        dat <- dat
                                }else{  dat <- data.frame(t(dat), stringsAsFactors = FALSE)
                                        dat <- cbind(time_position,dat)
                                        if("wave_wpm_bnds" %in% dat_list){
                                                wave_wpm <- ncvar_get(nc, "wave_wpm")
                                                wave_col <- c("time","lat",'lon',signif(wave_wpm,4))
                                                if(dim(dat)[2]==length(wave_col)){
                                                        colnames(dat) <- wave_col
                                                }
                                        }
                                        if("depth_bnds" %in% dat_list){
                                                depth <- ncvar_get(nc, "depth")
                                                sub_col <- c("time","lat",'lon',depth)
                                                if(dim(dat)[2]==length(sub_col)){
                                                        colnames(dat) <- sub_col
                                                }
                                        }
                                 }
                        }else{
                                dat <- dat
                        }
                        write.csv(dat, paste0(ascii_ncei_dir,buoy,"/",folder_name, "/",folder_name,"_",j,".csv"), row.names=FALSE, na = "NaN")
                        rm(dat)
                        if("depth_bnds" %in% dat_list){
                                tryCatch({
                                        rm(depth)
                                }, error = function(e){
                                        print("no depth")
                                })
                        }
                }
                nc_close(nc)
                rm(nc, time_position)
        }, error = function(e){print(paste0("can't open: ",file))}
        )
}
  