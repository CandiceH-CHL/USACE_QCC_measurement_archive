concat_ndbc_2023 <- function(files = "list of files", start_year = "earliest dataset year", t = t, buoy = buoy){
  
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
        colNames_spec <- c("DateTime", "0.0200", "0.0325", "0.0375", "0.0425", "0.0475", "0.0525",
                                      "0.0575", "0.0625", "0.0675", "0.0725", "0.0775", "0.0825", "0.0875", "0.0925", "0.1000", "0.1100",
                                      "0.1200", "0.1300", "0.1400", "0.1500", "0.1600", "0.1700", "0.1800", "0.1900", "0.2000", "0.2100", 
                                      "0.2200", "0.2300", "0.2400", "0.2500", "0.2600", "0.2700", "0.2800", "0.2900", "0.3000", "0.3100", 
                                      "0.3200", "0.3300", "0.3400", "0.3500", "0.3650", "0.3850", "0.4050", "0.4250", "0.4450", "0.4650", 
                                      "0.4850")
        dataset_spec <- data.frame(matrix(NA, nrow = 0, ncol = length(colNames_spec)))
        colnames(dataset_spec) <- colNames_spec
  }

  ##----------------------------------------------------------------------------------------
  # Concatenate all data files to handle different data formats
  ##----------------------------------------------------------------------------------------
  if(start_year >= 2006){
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
          library(dplyr)
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
                    # file <- file_dates[1]
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
        # find search var
        if(t=="w"){var <- "_swden"}
        if(t=="d"){var <- "_swdir"} # alpha1
        if(t=="i"){var <- "_swdir2"} # alpha2
        if(t=="j"){var <- "_swr1"}
        if(t=="k"){var <- "_swr2"}
        files_use <- files[grepl(var,files)]
        
        for(df in files_use){
            # df <- files_use[1]
            # check for empty files
            # read header data
            header <- read_header(df)
            # read df                      
            dataset <- read.table(df, header=FALSE, na.strings = NA_strings, fill = T, skip = 0)
            names(dataset) <- header
            # format date
            dataset <- date_formatted(dataset)
            # append df
            dataset_spec <- rbind(dataset_spec,dataset)
            rm(dataset)
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
        # ordering the dataset by date and selecting unique values only
        dataset_spec <- dataset_spec[order(dataset_spec$DateTime),]
        dataset_spec <- unique(dataset_spec)
        # rename the rows to reflect unique data
        row.names(dataset_spec) <- 1:nrow(dataset_spec)
        # export from function
        assign('dataset_spec', dataset_spec, envir=parent.frame())
  }
}