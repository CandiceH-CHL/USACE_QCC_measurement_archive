concat_ndbc <- function(files = "list of files", start_year = "earliest dataset year", t = t, buoy = buoy){
  
  print(paste0("Working on...",t))
  
  colNames_stdmet <- c("YYYY","MM","DD","hh","mm","WDIR","WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP",
                       "DEWP","VIS","TIDE")
  
  NA_strings <- c(9.96920996838687E+36,"9.96920996838687E+36","9.96921e+36",9.96921e+36,999.00,999, 
                  "-32767",-32767,"-2147483647",-2147483647,NA, NaN,"NA","NaN","9969209968386869046778552952102584320.000",
                  "9.96920996838686E+36","99.0","9999.0","999","999.0","99.00","999.00")
  
  date_formatted <- function(df){
        # find and removed rows with names/text
        selectedRows <- df[grep("YY", df[,1]), ]
        if(nrow(selectedRows) > 0){
              df <- df[!(df[,1] %in% selectedRows[,1]),]
        }
        rm(selectedRows)
        # convert characters to numeric
        df[,] <- apply(df[,],2,function(x) as.numeric(as.character(x)))
        # adding leading 0 to month, day and hour
        df$MM <-formatC(df$MM, width = 2, format = "d", flag = "0")
        df$DD <-formatC(df$DD, width = 2, format = "d", flag = "0")
        df$hh <-formatC(df$hh, width = 2, format = "d", flag = "0")
        df$mm <-formatC(df$mm, width = 2, format = "d", flag = "0")
        if(names(df[1])== "YY"){
            if(nchar(df[1,1]) == 2){
              df$year <- as.integer(19)
              colY = c(grep("year", colnames(df)), grep("YY", colnames(df)))
              df <- cbind(YYYY = do.call(paste0, df[colY]), df)
              df <- dplyr::select(df,-"YY",-"year")
            }else{
              df <- dplyr::rename(df,"YYYY" = "YY")
            }
        }
        # Creating concatenated individual and combined date and time columns
        colst = c(grep("YYYY", colnames(df)), grep("MM", colnames(df)), 
                  grep("DD", colnames(df)), grep("hh", colnames(df)), 
                  grep("mm", colnames(df)))
        df <- cbind(DateTime = do.call(paste0, df[colst]), df)
        ## setting the date and time format
        library(lubridate)
        df$DateTime <- ymd_hm(df$DateTime)
        # ordering the dataset by date and selecting unique values only
        df <- df[order(df$DateTime),]
        df <- unique(df)
        # rename the rows to reflect unique data
        row.names(df) <- 1:nrow(df)
        # remove redundant columns
        library(dplyr)
        df <- dplyr::select(df,-"YYYY",-"MM",-"DD",-"hh",-"mm")
        # removed rows that are all NA
        df <- df[rowSums(is.na(df)) != ncol(df), ]
  }
  
  read_header <- function(file){
      # read file
      header <- readLines(file, n = 1)
      if(grepl("#", header)== TRUE){header <- gsub("#", "",header)}
      header <- gsub("\\s+", ",", gsub("^\\s+|\\s+$", "",header))
      if(grepl("mm", header)== TRUE){
          years <- unlist(strsplit(header, "mm"))[1]
          freq <- unlist(strsplit(header, "mm"))[2]
      }else{
          years <- unlist(strsplit(header, "hh"))[1]
          freq <- unlist(strsplit(header, "hh"))[2]
      }
      years <- unlist(strsplit(years, ","))
      freq <- unlist(strsplit(freq, ","))
      freq <- stringi::stri_remove_empty(freq, na_empty = FALSE)
      freq <- as.numeric(freq)
      freq <- sprintf("%1.4f", freq)
      freq <- as.character(freq)
      if(grepl("mm", header)== TRUE){
          header <- c(years, "mm", freq)
      }else{header <- c(years, "hh", freq)}
  }

  if(t != "h"){
        colNames_spec_new <- c("DateTime", "0.0200", "0.0325", "0.0375", "0.0425", "0.0475", "0.0525",
                                      "0.0575", "0.0625", "0.0675", "0.0725", "0.0775", "0.0825", "0.0875", "0.0925", "0.1000", "0.1100",
                                      "0.1200", "0.1300", "0.1400", "0.1500", "0.1600", "0.1700", "0.1800", "0.1900", "0.2000", "0.2100", 
                                      "0.2200", "0.2300", "0.2400", "0.2500", "0.2600", "0.2700", "0.2800", "0.2900", "0.3000", "0.3100", 
                                      "0.3200", "0.3300", "0.3400", "0.3500", "0.3650", "0.3850", "0.4050", "0.4250", "0.4450", "0.4650", 
                                      "0.4850")
        colNames_spec_old <- c("DateTime", "0.0300","0.0400","0.0500","0.0600","0.0700","0.0800","0.0900","0.1000","0.1100","0.1200",
                                      "0.1300","0.1400","0.1500","0.1600","0.1700","0.1800","0.1900","0.2000","0.2100","0.2200",   
                                      "0.2300","0.2400","0.2500","0.2600","0.2700","0.2800","0.2900","0.3000","0.3100","0.3200",   
                                      "0.3300","0.3400","0.3500","0.3600","0.3700","0.3800","0.3900","0.4000")

        dataset_spec_new <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames_spec_new)))
        colnames(dataset_spec_new) <- colNames_spec_new

        dataset_spec_old <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames_spec_old)))
        colnames(dataset_spec_old) <- colNames_spec_old
        
        count <- 0
  }

  ##----------------------------------------------------------------------------------------
  # Concatenate all data files to handle different data formats
  ##----------------------------------------------------------------------------------------
  
  ## 2007 to present: 
  ## stdmet format:     ##YY  MM DD hh mm WDIR WSPD GST  WVHT   DPD   APD MWD   PRES  ATMP  WTMP  DEWP  VIS  TIDE
  ##         units:     ##yr  mo dy hr mn degT m/s  m/s     m   sec   sec deg    hPa  degC  degC  degC  nmi    ft
  ## spectral format:   #YY  MM DD hh mm   .0200  .0325  .0375  .0425  .0475  .0525  .0575  .0625  .0675  .0725  .0775  
  ##                                       .0825  .0875  .0925  .1000  .1100  .1200  .1300  .1400  .1500  .1600  .1700  
  ##                                       .1800  .1900  .2000  .2100  .2200  .2300  .2400  .2500  .2600  .2700  .2800  
  ##                                       .2900  .3000  .3100  .3200  .3300  .3400  .3500  .3650  .3850  .4050  .4250  
  ##                                       .4450  .4650  .4850
   
  ## selecting files to concatenate, then concatenating 
  # loop through files and concat
  if(t == "h"){

        library(lubridate)
        if(buoy == "46029"){
              dates <- c(2006:year(Sys.Date()))
        }else{
              dates <- c(2007:year(Sys.Date()))
        }
        ## find the files for this data setup
        file_dates <- list()
        for (i in 1:length(dates)){
            file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
        }
        # remove list function from subset data list
        file_dates <- unlist(file_dates)
        
        # print(file_dates)
        if(length(file_dates) > 0){
              for(file in file_dates){
                    # if the merged dataset doesn't exist, create it
                    if (!exists("dataset")){
                      dataset <- read.table(file, header=FALSE, na.strings = NA_strings,fill = T, skip = 2)
                    }
                    # if the merged dataset does exist, append to it
                    if (exists("dataset")){
                      temp_dataset <-read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 2)
                      dataset<-rbind(dataset, temp_dataset)
                      rm(temp_dataset)
                    }
              }
              colnames(dataset) <- colNames_stdmet
              dataset <- date_formatted(dataset)
              dataset_master <- dataset  
              rm(dataset)
              # return(dataset_master)
        }
  }else{
        library(lubridate)
        dates <- c(2007:year(Sys.Date()))
        ## find the files for this data setup
        file_dates <- list()
        for (i in 1:length(dates)){
            file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
        }
        # remove list function from subset data list
        file_dates <- unlist(file_dates)
        # print(file_dates)
        if(length(file_dates) > 0){
            for(file in file_dates){
                # check for empty files
                if(file.info(file)$size !=0){
                    # read header data
                    header <- read_header(file)
                    # read file                      
                    dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 0)
                    names(dataset) <- header
                    # format date
                    dataset <- date_formatted(dataset)
                    # find all available frames
                    dataset_list <- ls(pattern = "dataset_spec")

                    for(matchable in dataset_list){
                      dat <- get(matchable)
                      if(exists("dataset")){
                          if(identical(names(dat),names(dataset))){
                            dat<-rbind(dat, dataset)
                            print(paste0("added to: ",matchable))
                            assign(matchable,dat)
                            rm(dataset)
                          }
                      }
                      rm(dat)
                    }
                    if(exists("dataset")){
                      count <- count + 1  
                      dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = dim(dataset)[2]))
                      dataset_spec<-rbind(dataset_spec, dataset)
                      new_name <- paste0("dataset_spec_",count)
                      assign(new_name, dataset_spec)
                      print(paste0("data added to NEW DF:: ",new_name))
                      rm(dataset, dataset_spec)
                    }
                }else{print("no data in these files")}
            }
        }
  }
    
  ##----------------------------------------------------------------------------------------
      
  ## 2005 & 2006 - skip first line, has no # flag
      
  ## stdmet data format:    YYYY MM DD hh mm  WD  WSPD GST  WVHT  DPD   APD  MWD  BAR    ATMP  WTMP  DEWP  VIS  TIDE
  ##                        units: no units in files
  ## spectral data format:  YYYY MM DD hh mm  .030   .040   .050   .060   .070   .080   .090   .100   .110   .120   
  ##                                          .130   .140   .150   .160   .170   .180   .190   .200   .210   .220   
  ##                                          .230   .240   .250   .260   .270   .280   .290   .300   .310   .320   
  ##                                          .330   .340   .350   .360   .370   .380   .390   .400
      
  # loop through files and concat
  if(t == "h"){
        ## selecting files to concatenate, then concatenating 
        library(lubridate)
        if(buoy == "46029"){
          dates <- c(2004,2005)
        }else{
          dates <- c(2005,2006)
        }
        ## find the files for this data setup
        file_dates <- list()
        for (i in 1:length(dates)){
            file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
        }
        # remove list function from subset data list
        file_dates <- unlist(file_dates)
        # print(file_dates)
        
        if(length(file_dates) > 0){
              for(file in file_dates){
                        # if the merged dataset doesn't exist, create it
                        if (!exists("dataset")){
                             dataset <- read.table(file, header=FALSE, na.strings = NA_strings,fill = T, skip = 1)
                        }
                        # if the merged dataset does exist, append to it
                        if (exists("dataset")){
                              temp_dataset <-read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 1)
                              dataset<-rbind(dataset, temp_dataset)
                              rm(temp_dataset)
                        }
              }
              colnames(dataset) <- colNames_stdmet
              dataset <- date_formatted(dataset)
              ## combining datasets
              if(exists("dataset_master")){
                dataset_master<-rbind(dataset_master, dataset)
              }else{dataset_master<-dataset}
              rm(dataset)
              # return(dataset_master)
              
        }
  }else{
      dates <- c(2005,2006)
      ## find the files for this data setup
      file_dates <- list()
      for (i in 1:length(dates)){
          file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
      }
      # remove list function from subset data list
      file_dates <- unlist(file_dates)
      # print(file_dates)
      if(length(file_dates) > 0){
          for(file in file_dates){
            # check for empty files
            if(file.info(file)$size !=0){
                header <- read_header(file)
                # read file                      
                dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 0)
                names(dataset) <- header
                # format date
                dataset <- date_formatted(dataset)
                # find all available frames
                dataset_list <- ls(pattern = "dataset_spec")

                for(matchable in dataset_list){
                    dat <- get(matchable)
                    if(exists("dataset")){
                          if(identical(names(dat),names(dataset))){
                              dat<-rbind(dat, dataset)
                              print(paste0("added to: ",matchable))
                              assign(matchable,dat)
                              rm(dataset)
                          }
                    }
                    rm(dat)
                }
                if(exists("dataset")){
                    count <- count + 1  
                    dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = dim(dataset)[2]))
                    dataset_spec<-rbind(dataset_spec, dataset)
                    new_name <- paste0("dataset_spec_",count)
                    assign(new_name, dataset_spec)
                    print(paste0("data added to NEW DF:: ",new_name))
                    rm(dataset, dataset_spec)
                }
            }
          }
      }
  }


  ##----------------------------------------------------------------------------------------
  
  ## 2000 & 2004 - no minute column - don't skip lines, missing tide data in some sets
  
  ## stdmet data format:    YYYY MM DD hh WD   WSPD GST  WVHT  DPD   APD  MWD  BAR    ATMP  WTMP  DEWP  VIS  TIDE
  ##                        units: no units in files
  ## spectral data format:  YYYY MM DD hh   .030   .040   .050   .060   .070   .080   .090   .100   .110   .120   
  ##                                        .130   .140   .150   .160   .170   .180   .190   .200   .210   .220   
  ##                                        .230   .240   .250   .260   .270   .280   .290   .300   .310   .320   
  ##                                        .330   .340   .350   .360   .370   .380   .390   .400
  
  Names <- c("YYYY", "MM", "DD", "hh", "WDIR", "WSPD", "GST",  "WVHT", "DPD", "APD", "MWD",  "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE")
  
  # loop through files and concat
  if(t == "h"){
      ## selecting files to concatenate, then concatenating 
      library(lubridate)
      if(buoy == "46029"){
        dates <- c(2000:2003)
      }else{
        dates <- c(2000:2004)
      }
      ## find the files for this data setup
      file_dates <- list()
      for (i in 1:length(dates)){
          file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
      }
      # remove list function from subset data list
      file_dates <- unlist(file_dates)
      # print(file_dates)
      if(length(file_dates) > 0){
            for(file in file_dates){
                    # if the merged dataset doesn't exist, create it
                    if (!exists("dataset")){
                      dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T)
                    }
                    # if the merged dataset does exist, append to it
                    if (exists("dataset")){
                      temp_dataset <-read.table(file, header=FALSE, na.strings = NA_strings, fill = T)
                      dataset<-rbind(dataset, temp_dataset)
                      rm(temp_dataset)
                    }
            }
            # rename columns
            colnames(dataset) <- Names
            ## adding in minutes column
            dataset$mm <- as.integer(0)
            dataset <- date_formatted(dataset)
            ## combining datasets
            if(exists("dataset_master")){
              dataset_master<-rbind(dataset_master, dataset)
            }else{dataset_master<-dataset}
            rm(dataset)
            # return(dataset_master)
            
      }
  }else{
      dates <- c(2000:2004)
      ## find the files for this data setup
      file_dates <- list()
      for (i in 1:length(dates)){
          file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)]
      }
      # remove list function from subset data list
      file_dates <- unlist(file_dates)
      if(length(file_dates) > 0){
          for(file in file_dates){
              # check for empty files
              if(file.info(file)$size !=0){
                  header <- read_header(file)
                  # read file                      
                  dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 0)
                  names(dataset) <- header
                  # add missing minute column
                  dataset$mm <- as.integer(0)
                  # format date
                  dataset <- date_formatted(dataset)
                  # find all available frames
                  dataset_list <- ls(pattern = "dataset_spec")

                  for(matchable in dataset_list){
                    dat <- get(matchable)
                    if(exists("dataset")){
                          if(identical(names(dat),names(dataset))){
                            dat<-rbind(dat, dataset)
                            print(paste0("added to: ",matchable))
                            assign(matchable,dat)
                            rm(dataset)
                          }
                    }
                    rm(dat)
                  }
                  if(exists("dataset")){
                      count <- count + 1  
                      dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = dim(dataset)[2]))
                      dataset_spec<-rbind(dataset_spec, dataset)
                      new_name <- paste0("dataset_spec_",count)
                      assign(new_name, dataset_spec)
                      print(paste0("data added to NEW DF:: ",new_name))
                      rm(dataset, dataset_spec)
                  }
              }
          }
      }
  }
 
  ##----------------------------------------------------------------------------------------
  
  ## 1999 - no TIDE or minute column
  
  ## stdmet data format:    YYYY MM DD hh WD WSPD GST  WVHT  DPD   APD  MWD  BAR    ATMP  WTMP  DEWP  VIS
  ##                        units: no units in files
  ## spectral data format:  YYYY MM DD hh   .030   .040   .050   .060   .070   .080   .090   .100   .110   .120   
  ##                                        .130   .140   .150   .160   .170   .180   .190   .200   .210   .220   
  ##                                        .230   .240   .250   .260   .270   .280   .290   .300   .310   .320      
  ##                                        .330   .340   .350   .360   .370   .380   .390   .400
  
  Names <- c("YYYY", "MM", "DD", "hh", "WDIR", "WSPD", "GST",  "WVHT", "DPD", "APD", "MWD",  "PRES", "ATMP", "WTMP", "DEWP", "VIS")
  
  ## selecting files to concatenate, then concatenating 
  library(lubridate)
  dates <- c(1999)
  ## find the files for this data setup
  file_dates <- list()
  for (i in 1:length(dates)){
      file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
  }
  # remove list function from subset data list
  file_dates <- unlist(file_dates)
  # print(file_dates)
  
  # loop through files and concat
  if(t == "h"){
        if(length(file_dates) > 0){
              for(file in file_dates){
                  # if the merged dataset doesn't exist, create it
                  if (!exists("dataset")){
                    dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 1)
                  }
                  # if the merged dataset does exist, append to it
                  if (exists("dataset")){
                    temp_dataset <-read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 1)
                    dataset<-rbind(dataset, temp_dataset)
                    rm(temp_dataset)
                  }
              }
              ## rename columns
              colnames(dataset) <- Names
              ## adding in minutes column
              if(buoy == 41009){ # using minute data from NetCDF files
                dat1 <- filter(dataset, dataset$MM < 9)
                min <- data.frame(c(NA,rep(c(20,50),nrow(dat1)/2)), stringsAsFactors = FALSE)
                dat1$mm <- min[1:nrow(min)-1,]
                dat2 <- filter(dataset, dataset$MM >= 9)
                dat2$mm <- as.integer(0)
                dataset <- rbind(dat1,dat2)
                rm(dat1,dat2,min)
              }else{
                dataset$mm <- as.integer(0)
              }
              dataset$TIDE <- as.logical(NA)
              dataset <- date_formatted(dataset)
              ## combining datasets
              if(exists("dataset_master")){
                dataset_master<-rbind(dataset_master, dataset)
              }else{dataset_master<-dataset}
              rm(dataset)

        }
  }else{
      if(length(file_dates) > 0){
          for(file in file_dates){
              # check for empty files
              if(file.info(file)$size !=0){
                  header <- read_header(file)
                  # read file                      
                  dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 0)
                  names(dataset) <- header
                  # add missing minute column
                  dataset$mm <- as.integer(0)
                  # format date
                  dataset <- date_formatted(dataset)
                  # find all available frames
                  dataset_list <- ls(pattern = "dataset_spec")

                  for(matchable in dataset_list){
                      dat <- get(matchable)
                      if(exists("dataset")){
                            if(identical(names(dat),names(dataset))){
                              dat<-rbind(dat, dataset)
                              print(paste0("added to: ",matchable))
                              assign(matchable,dat)
                              rm(dataset)
                            }
                      }
                      rm(dat)
                  }
                  if(exists("dataset")){
                      count <- count + 1  
                      dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = dim(dataset)[2]))
                      dataset_spec<-rbind(dataset_spec, dataset)
                      new_name <- paste0("dataset_spec_",count)
                      assign(new_name, dataset_spec)
                      print(paste0("data added to NEW DF:: ",new_name))
                      rm(dataset, dataset_spec)
                  }
              }
          }
      }
  }

  ##----------------------------------------------------------------------------------------
  
  ##1979 & 1998 - no tide and minute columns
  
  ## stdmet data format:    YY MM DD hh WD   WSPD GST  WVHT  DPD   APD  MWD  BAR    ATMP  WTMP  DEWP  VIS
  ##                        units: no units in files
  ## spectral data format:  YY MM DD hh  .0200  .0325  .0375  .0425  .0475  .0525  .0575  .0625  .0675  .0725  
  ##                        .0775  .0825  .0875  .0925  .1000  .1100  .1200  .1300  .1400  .1500  .1600  .1700  
  ##                        .1800  .1900  .2000  .2100  .2200  .2300  .2400  .2500  .2600  .2700  .2800  .2900  
  ##                        .3000  .3100  .3200  .3300  .3400  .3500  .3650  .3850  .4050  .4250  .4450  .4650  .4850
   
  Names <- c("YY", "MM", "DD", "hh", "WDIR", "WSPD", "GST",  "WVHT", "DPD", "APD", "MWD",  "PRES", "ATMP", "WTMP", "DEWP", "VIS")
  
  ## selecting files to concatenate, then concatenating 
  library(lubridate)
  dates <- c(start_year:1998)
  ## find the files for this data setup
  file_dates <- list()
  for (i in 1:length(dates)){
      file_dates[[i]] <- files[grepl(paste0(dates[i],"_"),files)] 
  }
  # remove list function from subset data list
  file_dates <- unlist(file_dates)

  # loop through files and concat
  if(t == "h"){
      if(length(file_dates) > 0){
            for(file in file_dates){
                  # if the merged dataset doesn't exist, create it
                  if (!exists("dataset")){
                    dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 1)
                  }
                  # if the merged dataset does exist, append to it
                  if (exists("dataset")){
                    temp_dataset <-read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 1)
                    dataset<-rbind(dataset, temp_dataset)
                    rm(temp_dataset)
                  }
            }
            ## rename columns
            colnames(dataset) <- Names
            ## adding in missing column
            dataset$TIDE <- as.logical(NA)
            dataset$Year <- as.integer(19)
            ## Creating new column with '19', and combining YY column to be 4 digits.
            colst = c(grep("Year", colnames(dataset)), grep("YY", colnames(dataset)))
            dataset <- cbind(YYYY = do.call(paste0, dataset[colst]),
                             dataset)
            dataset$YYYY <- as.numeric(as.character(dataset$YYYY))
            ## delete working columns of YYY and YY values
            dataset$Year <- NULL; dataset$YY <- NULL
            ## adding in minutes column
            if(buoy == 41009){ # using minute data from NetCDF files
                  dataset$mm <- as.integer(0)
                  dataset <- date_formatted(dataset)
                  dat1 <- filter(dataset,dataset$DateTime < as.Date("1992-08-01 00:00:00"))
                  dat1$mm <- as.numeric(rep(c(00,30),nrow(dat1)/2))
                  minute(dat1$DateTime) <- as.numeric(dat1$mm)
                  dat1 <- dplyr::select(dat1,-"mm")
                  dat2 <- filter(dataset,dataset$DateTime >= as.Date("1992-08-01 00:00:00"))
                  dat2$mm <- as.numeric(rep(c(20,50),nrow(dat2)/2))
                  minute(dat2$DateTime) <- as.numeric(dat2$mm)
                  dat2 <- dplyr::select(dat2,-"mm")
                  dataset <- rbind(dat1,dat2)
                  rm(dat1,dat2)
            }else{
                  dataset$mm <- as.integer(0)
                  dataset <- date_formatted(dataset)
            }
      
            ## combining datasets
            if(exists("dataset_master")){
              dataset_master<-rbind(dataset_master, dataset)
            }else{dataset_master<-dataset}
            rm(dataset)
            # return(dataset_master)
            
      }
  }else{
      if(length(file_dates) > 0){
          for(file in file_dates){
              # check for empty files
              if(file.info(file)$size !=0){
                  header <- read_header(file)
                  # read file                      
                  dataset <- read.table(file, header=FALSE, na.strings = NA_strings, fill = T, skip = 0)
                  names(dataset) <- header
                  # add missing minute column
                  dataset$mm <- as.integer(0)
                  # format date
                  dataset <- date_formatted(dataset)
                  # find all available frames
                  dataset_list <- ls(pattern = "dataset_spec")

                  for(matchable in dataset_list){
                      dat <- get(matchable)
                      if(exists("dataset")){
                            if(identical(names(dat),names(dataset))){
                              dat<-rbind(dat, dataset)
                              print(paste0("added to: ",matchable))
                              assign(matchable,dat)
                              rm(dataset)
                            }
                        }
                      rm(dat)
                    }
                  if(exists("dataset")){
                      count <- count + 1  
                      dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = dim(dataset)[2]))
                      dataset_spec<-rbind(dataset_spec, dataset)
                      new_name <- paste0("dataset_spec_",count)
                      assign(new_name, dataset_spec)
                      print(paste0("data added to NEW DF:: ",new_name))
                      rm(dataset, dataset_spec)
                  }
              }
          }
      }
  }

  ##----------------------------------------------------------------------------------------
  # fix structure
  ##----------------------------------------------------------------------------------------
  if(t == "h"){
        dataset <- dataset_master
        rm(dataset_master)
        # ordering the dataset by date and selecting unique values only
        dataset <- dataset[order(dataset$DateTime),]
        dataset <- unique(dataset)
        # rename the rows to reflect unique data
        row.names(dataset) <- 1:nrow(dataset)
        assign("dataset",dataset)
  }else{
        # adding leading 0 to month, day and hour
        data_list <- ls(pattern = "dataset_spec")
        for(d in data_list){
            dataset <- get(d)
            if(dim(dataset)[1] != 0){
                  # ordering the dataset by date and selecting unique values only
                  dataset <- dataset[order(dataset$DateTime),]
                  dataset <- unique(dataset)
                  # rename the rows to reflect unique data
                  row.names(dataset) <- 1:nrow(dataset)
                  # export from function
                  assign(d, dataset, envir=parent.frame())
            }
            rm(dataset)
        }
  }
}