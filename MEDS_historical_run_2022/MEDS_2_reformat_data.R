MEDS_reformat_data_2 <- function(buoys = "list of buoys", data_dir = "data_dir"){
        
     ##----------------------------------------------------------------------------------------
     ## concat MEDS web files 
     ## Hall, Candice
     ##----------------------------------------------------------------------------------------
     
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     library(lubridate)
     library(plyr)
     library(dplyr)
     library(data.table)
     library(naniar)
     library(qpcR)
     library(stats)
     library(utils)

     ##----------------------------------------------------------------------------------------
     ## set paths
     ##----------------------------------------------------------------------------------------
     drive <- "E:/"
     data_dir <- paste0(drive, "Candice/projects/WaveTrends/MEDS/data/")
     setwd(data_dir)
     
     input_dir <- paste0(data_dir,"raw_data/")
     output_dir <- paste0(data_dir,"concat_data/")
     ## set new output directories for raw and zipped datasets
     # MEDS 
     bulk_dir <- paste0(input_dir,"bulk/")
     meta_dir <- paste0(input_dir,"meta/")
     spec_dir <- paste0(input_dir,"spec/")
     spec_fixed_dir <- paste0(input_dir,"spec_fixed/")
     spec_format_dir <- paste0(input_dir,"spec_formatted/")
     
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
        
     ## MEDS web stdmet data files
        
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     options(warn=-1)
     # looping through the listed buoy ID's
     for (buoy in buoys){
          # buoy <- buoys[9]

          print(paste0("Starting on MEDS... ",buoy))

          #----------------------------------------------------------------------------------------

          # selecting files to concatenate, then concatenating
          file_list <- list.files(path = paste0(bulk_dir,buoy), pattern = ".csv", full.names = TRUE)
          met_list <- list.files(path = paste0(meta_dir,buoy), pattern = ".csv", full.names = TRUE)

          if(length(file_list)>0){

               # # start writing to an output file
               # sink(paste0(data_dir,"2_MEDS_concat_",buoy,"_",Sys.Date(),".txt"))
               print(paste0("Starting on MEDS... ",buoy))
               print(file_list)

               ## create buoy specific concat folder
               if (!file.exists(paste0(output_dir,buoy,"/"))) {dir.create((paste0(output_dir,buoy,"/")))}

               dat <- read.csv(file_list, header = TRUE, na.strings = c("NA",NA))
               if(length(met_list)>0){meta <- read.csv(met_list, header = FALSE, na.strings = c("NA",NA), skip = 2)}

               #----------------------------------------------------------------------------------------
               # format data
               #----------------------------------------------------------------------------------------
               dat$DATE <- mdy_hm(dat$DATE)
               if("V24" %in% names(dat)){dat$V24 <- NULL}
               # rename columns
               library(data.table)
               setnames(dat, old = c("DATE","LATITUDE","LONGITUDE","DEPTH"),#,"VCAR","VTPK","VWH.","VCMX","VTP.","WDIR","WSPD","WSS.","GSPD",
                                     # "WDIR.1","WSPD.1","WSS..1","GSPD.1","ATMS","ATMS.1","DRYT","SSTP"),
                        new = c("DateTime","latitude","longitude","depth"))#,"significant_wave_height_MEDS","dominant_wave_period_MEDS","significant_wave_height",
                                # "wave_height_zero","dominant_wave_period","wind_direction_1","wind_speed_1","wind_speed_scalar_1","wind_gust_1",
                                # "wind_direction_2","wind_speed_2","wind_speed_scalar_2","wind_gust_2","air_pressure_at_sea_level_1","air_pressure_at_sea_level_2",
                                # "air_temperature_dry","sea_surface_temperature"))

               #----------------------------------------------------------------------------------------
               # format meta
               #----------------------------------------------------------------------------------------
               if(exists('meta')){
                    if(dim(meta)[2]>1){
     
                         meta <- meta[-2,]
                         names(meta) <- meta[1,]
                         meta <- meta[-1,]
     
                         library(lubridate)
                         meta$`Date - Start` <- ymd(meta$`Date - Start`)
                         meta$`Date - End` <- ymd(meta$`Date - End`)
                         write.csv(meta, file = paste0(output_dir, buoy, "/c",buoy,'_metadata.csv'), row.names = FALSE)
                         saveRDS(meta, file = paste0(output_dir, buoy, "/c",buoy,'_metadata.rds'))
     
     
                         #----------------------------------------------------------------------------------------
                         # Add metadata
                         #----------------------------------------------------------------------------------------
                         meta <- meta[,c("Date - Start","Date - End","Hull #")]
     
                         # create hull time series
                         df <- seq(from = meta[1,1], to = meta[1,2], by = "day")
                         hulls <- data.frame(df,meta[1,3], stringsAsFactors = FALSE)
                         colnames(hulls) <- c("Date", "hull_id")
     
                         for(i in 2:nrow(meta)){
                              # i <- 1
                              df <- seq(from = meta[i,1], to = meta[i,2], by = "day")
                              hull_df <- data.frame(df,meta[i,3], stringsAsFactors = FALSE)
                              colnames(hull_df) <- c("Date", "hull_id")
                              hulls <- rbind(hulls,hull_df)
                              rm(df, hull_df)
                         }
     
                         # export
                         write.csv(hulls, file = paste0(output_dir, buoy, "/c",buoy,'_hullId.csv'), row.names = FALSE)
                         saveRDS(hulls, file = paste0(output_dir, buoy, "/c",buoy,'_hullId.rds'))
     
                         # add to data
                         dat$Date <- as.Date(dat$DateTime)
                         dat <-left_join(dat,hulls,by = "Date")
                         dat$Date <- NULL
     
                    }else{print("no meta data")}
               }else{print("no meta data")}

               # export
               write.csv(dat, file = paste0(output_dir, buoy, "/c",buoy,'_stdmet.csv'), row.names = FALSE)
               saveRDS(dat, file = paste0(output_dir, buoy, "/c",buoy,'_stdmet.rds'))

               print(paste0("Finished MEDS stdmet... ",buoy))
               rm(dat,hulls,meta,i)

               ## Stop writing to the file
               # sink()

          }else{print("no new MEDS data for this buoy")}

          #----------------------------------------------------------------------------------------
          print(paste0("Finished MEDS stdmet... ",buoy))
     }
     options(warn=0)

     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     
     ## MEDS web spec single row data files
     
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------

     # looping through the listed buoy ID's
     for (buoy in buoys){
          # buoy <- buoys[2]

          print(paste0("Starting on MEDS spectral single row files... ",buoy))

          #----------------------------------------------------------------------------------------

          # selecting files to concatenate, then concatenating
          file_list <- list.files(path = paste0(spec_dir,buoy), pattern = "spec.txt", full.names = TRUE)

          if(length(file_list)>0){

               # # start writing to an output file
               # sink(paste0(data_dir,"2_MEDS_concat_",buoy,"_",Sys.Date(),".txt"))
               print(paste0("Starting on MEDS... ",buoy))
               print(file_list)

               ## create buoy specific fixed folder
               if (!file.exists(paste0(spec_fixed_dir,buoy,"/"))) {dir.create((paste0(spec_fixed_dir,buoy,"/")))}

               #----------------------------------------------------------------------------------------
               # handle short rows
               #----------------------------------------------------------------------------------------
               for(df in file_list){
                    # df <- file_list[35]
                    file_name <- gsub(paste0(spec_dir,buoy,"/"),"",df)
                    file_name <- gsub("_spec","_spec_rows",file_name)
                    if(grepl("Y2D",file_name)){file_name <- gsub("Y2D",year(Sys.Date()),file_name)}

                    # extracting what is needed
                    # new_lines = c(unlist(strsplit(readLines(df, n=1), " ")[[1]])[1],unlist(strsplit(readLines(df, n=1), " ")[[1]])[2])
                    new_lines <- unlist(strsplit(readLines(df, n=1), " ")[[1]])[2]
                    dat <- readChar(df, file.info(df)$size)
                    dat <- gsub("ATMS","ATMS,",dat)
                    dat <- gsub('""','NA',dat)
                    dat <- gsub("\r\n",",",dat)
                    library(stringr)
                    if(buoy == "44138"){
                         hulls_types <- unique(unlist(str_extract_all(dat, c("\"AE\"", "\"6N\"", "\"3D\"", "\"12\"", "\"AW\"", "\"EN\"", "\"HX\"", "\"KG\"", "\"MI\"", "\"PC\"", "\"ST\"",
                                                                             #"\"SW\"",
                                                                             "\"TG\"", "\"TR\"", "\"WC\"", "\"WD\"", "\"WK\"", "\"WP\"", "\"WR\"")), use.names = FALSE))
                    }else{
                         hulls_types <- unique(unlist(str_extract_all(dat, c("\"AE\"", "\"6N\"", "\"3D\"", "\"12\"", "\"AW\"", "\"EN\"", "\"HX\"", "\"KG\"", "\"MI\"", "\"PC\"", "\"ST\"",
                                                                             "\"SW\"", "\"TG\"", "\"TR\"", "\"WC\"", "\"WD\"", "\"WK\"", "\"WP\"", "\"WR\"")), use.names = FALSE))
                    }

                    for(j in hulls_types){dat <- gsub(j,paste0("\n",j),dat)}
                    # dat <- gsub(new_lines,paste0("\n",new_lines),dat)
                    dat <- gsub("\"",",",dat)
                    dat <- gsub(" ",",",dat)
                    dat <- gsub(',,',',',dat)
                    dat <- gsub(', ,',',',dat)
                    dat <- gsub(',,',',NA,',dat)
                    dat <- gsub("NA,","",dat)

                    library(readr)
                    write_lines(dat, paste0(spec_fixed_dir,buoy,"/",file_name))
                    rm(new_lines, dat)
               }
          }
     }
     
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     
     ## MEDS fixed web spec data files
     
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------

     # looping through the listed buoy ID's
     for (buoy in buoys[32:length(buoys)]){
          # buoy <- buoys[31]

          # print(paste0("Starting on MEDS spectral file fixes... ",buoy))

          #----------------------------------------------------------------------------------------

          # selecting files to concatenate, then concatenating
          file_list <- list.files(path = paste0(spec_fixed_dir,buoy), pattern = "spec_rows.txt", full.names = TRUE)

          if(length(file_list)>0){

               #----------------------------------------------------------------------------------------
               # load and reformat corrected data
               #----------------------------------------------------------------------------------------

               # selecting files to concatenate, then concatenating
               file_list

               for(i in file_list){
                    # i <- file_list[15]
                    print(i)
                    # dat <- fread(i,sep = ",", fill = TRUE)
                    dat <- readLines(i)
                    dat <- strsplit(dat, ",", fixed = TRUE)
                    # rowbind unequal length list, and convert to data.table
                    dat <- data.table(t(sapply(dat, '[', seq(max(lengths(dat))))))
                    dat <- data.frame(dat, stringsAsFactors = FALSE)
                    # remove empty rows
                    dat <- dat[!apply(is.na(dat) | dat == "", 1, all),]
                    drop_rows_all_na <- function(x, pct=0.9) x[!rowSums(is.na(x)) >= ncol(x)*pct,]
                    dat <- drop_rows_all_na(dat)
                    # remove empty first column
                    empty_columns <- sapply(dat, function(x) all(is.na(x) | x == ""))
                    dat <- dat[, !empty_columns]
                    # convert to numeric
                    dat <- type.convert(dat,as.is = T)
                    
                    # file name
                    file_name <- gsub(spec_fixed_dir,"",i)

                    # handle complex station names
                    index_num <- grep(paste0("C",buoy), dat[1,])
                    sta_name <- gsub(", ","_",toString(dat[1,3:index_num-1]))
                    station_id <- cbind(data.frame(dat[,index_num], stringsAsFactors = FALSE),data.frame(dat[,1], stringsAsFactors = FALSE),data.frame(sta_name,stringsAsFactors = FALSE))
                    dat <- dat[, -c(1:index_num)]
                    dat <- cbind(station_id,dat)
                    rm(station_id)

                    # remove columns that are all NA
                    library(dplyr)
                    library(janitor)
                    dat <- dat %>%
                         mutate_all(funs(na_if(., ""))) %>%
                         remove_empty("cols")
                    dat <- dat[!sapply(dat, function (x) all(is.na(x) | x == "" | x == "NA"))]

                    # check date format
                    column_names1 <- c("station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "date1", "date2", "date3", "date4")
                    base <- dat[,1:18]
                    colnames(base)<-column_names1
                    base$DateTime <- NA

                    # empty df
                    column_names2 <- c("DateTime","station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "length_recording",
                                       "Sampling_Frequency", "Quality_Code", "additional_parameters", "Number_Wave_Heights","Number_Wave_Periods","Number_Spectral_Estimates")
                    new_df <- data.frame(matrix(ncol = length(column_names2)))
                    colnames(new_df) <- column_names2
                    library(lubridate)
                    new_df$DateTime <- ymd_hm(new_df$DateTime)
                    print("working on date format...")

                    # manually dealing with changing date formats in single file...
                    for(j in 1:nrow(base)){
                         # j = 2147
                         # print(j)
                         date_df <- base[j,]

                         if(nchar(date_df$date1[1])==8){
                              column_names1 <- c("station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "YMD", "hhmm", "length_recording",
                                                 "Sampling_Frequency", "Quality_Code", "additional_parameters", "Number_Wave_Heights","Number_Wave_Periods","Number_Spectral_Estimates", "DateTime")
                              colnames(date_df)<-column_names1
                              # fix date_time
                              date_df$DateTime <- paste0(date_df$YMD," ",formatC(date_df$hhmm, width = 4, format = "d", flag = "0"))
                              library(lubridate)
                              date_df$DateTime <- ymd_hm(date_df$DateTime)
                              date_df$YMD <- NULL; date_df$hhmm <- NULL; date_df$`NA` <- NULL; date_df$`NA` <- NULL
                              date_df <- date_df[,1:14]
                              # check date format worked
                              if(is.na(date_df$DateTime)){stop("Execution stopped. Check date format")}
                              # join to new_df
                              new_df <- rbind.fill(new_df,date_df)

                         }else if(nchar(date_df$date1[1])==6 & nchar(date_df$date2[1])<=2 & nchar(date_df$date3[1])>=2){
                              column_names1 <- c("station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "year_month", "day","hhmm", "length_recording",
                                                 "Sampling_Frequency", "Quality_Code", "additional_parameters", "Number_Wave_Heights","Number_Wave_Periods","Number_Spectral_Estimates", "DateTime")
                              colnames(date_df)<-column_names1
                              # fix date_time
                              date_df$DateTime <- paste0(date_df$year_month,formatC(date_df$day, width = 2, format = "d", flag = "0")," ",formatC(date_df$hhmm, width = 4, format = "d", flag = "0"))
                              library(lubridate)
                              date_df$DateTime <- ymd_hm(date_df$DateTime)
                              date_df$year_month <- NULL; date_df$day <- NULL; date_df$hhmm <- NULL
                              date_df <- date_df[,1:14]
                              # check date format worked
                              if(is.na(date_df$DateTime)){stop("Execution stopped. Check date format")}
                              # join to new_df
                              new_df <- rbind.fill(new_df,date_df)

                         }else if(nchar(date_df$date1[1])==4 & nchar(date_df$date2[1])<=2 & nchar(date_df$date3[1])<=2){
                              column_names1 <- c("station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "year", "month","day","hhmm", "length_recording",
                                                 "Sampling_Frequency", "Quality_Code", "additional_parameters", "Number_Wave_Heights","Number_Wave_Periods","Number_Spectral_Estimates", "DateTime")
                              colnames(date_df)<-column_names1
                              # convert to numeric
                              
                              # fix date_time
                              date_df$DateTime <- paste0(date_df$year,formatC(date_df$month, width = 2, format = "d", flag = "0"),formatC(date_df$day, width = 2, format = "d", flag = "0")," ",formatC(date_df$hhmm, width = 4, format = "d", flag = "0"))
                              library(lubridate)
                              date_df$DateTime <- ymd_hm(date_df$DateTime)
                              date_df$year <- NULL; date_df$month <- NULL; date_df$day <- NULL; date_df$hhmm <- NULL
                              date_df <- date_df[,1:14]
                              # check date format worked
                              if(is.na(date_df$DateTime)){stop("Execution stopped. Check date format")}
                              # join to new_df
                              new_df <- rbind.fill(new_df,date_df)

                         }else if(nchar(date_df$date1[1])==4 & nchar(date_df$date2[1])==3){
                              column_names1 <- c("station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "year", "month_day","hhmm", "length_recording",
                                                 "Sampling_Frequency", "Quality_Code", "additional_parameters", "Number_Wave_Heights","Number_Wave_Periods","Number_Spectral_Estimates", "DateTime")
                              colnames(date_df)<-column_names1
                              # fix date_time
                              date_df$DateTime <- paste0(date_df$year,formatC(date_df$month_day, width = 4, format = "d", flag = "0")," ",formatC(date_df$hhmm, width = 4, format = "d", flag = "0"))
                              library(lubridate)
                              if(as.numeric(date_df$hhmm) <2400){
                                   date_df$DateTime <- ymd_hm(date_df$DateTime)
                                   date_df$year <- NULL; date_df$month_day <- NULL; date_df$hhmm <- NULL; date_df$`NA` <- NULL
                                   date_df <- date_df[,1:14]
                                   # check date format worked
                                   if(is.na(date_df$DateTime)){stop("Execution stopped. Check date format")}
                                   # join to new_df
                                   new_df <- rbind.fill(new_df,date_df)
                              }
                         }
                         rm(date_df)
                    }
                    new_df <- new_df[-1,]
                    base <- new_df

                    # empty df
                    freq_num <- unique(as.numeric(base$Number_Spectral_Estimates))
                    freq_num <- freq_num[!is.na(freq_num)]
                    
                    if(length(freq_num)>0 & freq_num != -51){
                         fixed_df <- data.frame(matrix(ncol = (max(freq_num, na.rm = TRUE)*3)))
                         colnames(fixed_df) <- paste0("X",seq(1, ncol(fixed_df),1))
                         fixed_df <- merge(base,fixed_df)
                         fixed_new <- data.frame()
                    }
                    bulk_ck_df <- data.frame()

                    # parse by row to handle different columns
                    print("working on spec freqs format...")
                    for(p in 1:nrow(base)){
                         # p <- 1
                         # print(p)
                         temp_df <- dat[p,]
                         # remove empty fields
                         temp_df <- temp_df[,colSums(is.na(temp_df))<nrow(temp_df)]
                         temp_df <- temp_df[!sapply(temp_df, function(x) all(x == ""))]
                         # rename cols
                         colnames(temp_df) <- temp_df[1,]
                         
                         if("1-569" %in% names(temp_df)){
                              print("1-569 error")
                         }else{
                              # find bulk checks
                              index_num_bulk <- grep(as.numeric(base$Number_Spectral_Estimates[p]), names(temp_df))
                              index_num_bulk <- min(index_num_bulk[index_num_bulk>13])+1
                              # find last quality check code
                              library(stringr)
                              chrs <- unlist(str_extract_all(names(temp_df),"\\D+"))
                              chrs <- gsub("[[:punct:]]", "", chrs) 
                              chrs <- unique(chrs[chrs != ""]) 
                              chrs <- tail(chrs[!grepl('E|e', chrs)],n=1)
                              index_num_spec <- max(grep(chrs, names(temp_df)))+1
                              rm(chrs)
                              
                              # extract bulk checks
                              # print("extracting bulk checks...")
                              bulk_df <- temp_df[,index_num_bulk:(index_num_spec-1)]
                              bulk_df$DateTime <- base$DateTime[p]
                              bulk_names <- gsub("[[:digit:]]", "", names(bulk_df))
                              bulk_names <- gsub("\\.", "", bulk_names)
                              bulk_names <- gsub("E","",bulk_names)
                              bulk_names <- gsub("-","",bulk_names)
                              bulk_names <- gsub("\\+","",bulk_names)
                              colnames(bulk_df) <- bulk_names
                              # Remove column
                              lose.cols <- names(bulk_df) %in% c("")
                              bulk_df <- bulk_df[!lose.cols] 
                              # add to master bulk check file
                              bulk_ck_df <- rbind.fill(bulk_ck_df,bulk_df)
                              rm(bulk_names)
                              
                              # extract spec checks
                              # print("extracting spec data...")
                              if(exists('fixed_df')){
                                   if(index_num_spec < ncol(temp_df)){
                                        temp_df <- temp_df[,index_num_spec:ncol(temp_df)]
                                        colnames(temp_df) <- paste0("X",seq(1, ncol(temp_df),1))
                                        # temp_df$DateTime <- base$DateTime[p]
                                        
                                        # merge DF's
                                        fixed_df_2 <- cbind(fixed_df[p,1:14], temp_df)
                                        fixed_new <- rbind.fill(fixed_new, fixed_df_2)
                                        
                                   }else{print(paste0("no spec data for: ", base$DateTime[p]))}
                              }else{print("no spec data")}
                              rm(index_num_spec, bulk_df, index_num_bulk, fixed_df_2)
                         }
                         rm(temp_df)
                    }
                    
                    if(exists('fixed_new')){
                         spec_num <- max(unique(as.numeric(fixed_new$Number_Spectral_Estimates)),na.rm = TRUE)
                         num_ls <- seq(1,spec_num,1)
                         freq_ls <- vector()
                         for(m in num_ls){freq_ls <- c(freq_ls, paste0("freq_",m), paste0("bandwidth_",m), paste0("density_",m))}
                         column_names1 <- c("DateTime","station_id","station_type", "station_name",  "Latitude", "Longitude", "depth", "length_recording", "Sampling_Frequency",
                                            "Quality_Code", "additional_parameters", "Number_Wave_Heights","Number_Wave_Periods","Number_Spectral_Estimates", freq_ls)
                         colnames(fixed_new) <- column_names1
                    }else{
                         fixed_new <- base
                    }
                    rm(base,dat,df, new_df, fixed_df)
                    
                    ## create buoy specific concat folder
                    if (!file.exists(paste0(spec_format_dir,buoy,"/"))) {dir.create((paste0(spec_format_dir,buoy,"/")))}
                    
                    # export
                    write.csv(fixed_new, file = paste0(spec_format_dir,gsub("rows.txt","rows_format.csv",file_name)), row.names = FALSE)
                    saveRDS(fixed_new, file = paste0(spec_format_dir,gsub("rows.txt","rows_format.rds",file_name)))
                    write.table(fixed_new, file = paste0(spec_format_dir,gsub("rows.txt","rows_format.txt",file_name)), append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)                    

                    write.csv(bulk_ck_df, file = paste0(spec_format_dir,gsub("rows.txt","bulk_checks.csv",file_name)), row.names = FALSE)
                    saveRDS(bulk_ck_df, file = paste0(spec_format_dir,gsub("rows.txt","bulk_checks.rds",file_name)))
                    write.table(bulk_ck_df, file = paste0(spec_format_dir,gsub("rows.txt","bulk_checks.txt",file_name)), append = FALSE, sep = ",", row.names = FALSE, col.names = TRUE)                    

                    rm(fixed_new, bulk_ck_df)
               } # end of individual file

               print(paste0("Finished MEDS stdmet... ",buoy))
               rm(dat,hulls,meta,i, j,p,m,index_num, num_ls,spec_num,sta_name, freq_num, freq_ls)

               ## Stop writing to the file
               # sink()

          }else{print("no new MEDS data for this buoy")}

          #----------------------------------------------------------------------------------------
          print(paste0("Finished MEDS stdmet... ",buoy))
     }

     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     # clean glob environ
     # rm(list = ls())
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
     
}

