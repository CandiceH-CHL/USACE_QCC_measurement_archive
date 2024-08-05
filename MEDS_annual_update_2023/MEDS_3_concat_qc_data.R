MEDS_concat_qc_data_3 <- function(buoys = "list of buoys", data_dir = "data_dir"){
        
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
     drive <- "D:/"
     data_dir <- paste0(drive, "Candice/projects/WaveTrends/MEDS_2024/data/")
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
        
     ## MEDS concat stdmet data files
        
     #----------------------------------------------------------------------------------------
     #----------------------------------------------------------------------------------------
        
     # looping through the listed buoy ID's
     for (buoy in buoys){
          # buoy <- buoys[9]

          print(paste0("Starting on MEDS... ",buoy))

          #----------------------------------------------------------------------------------------

          # selecting files to concatenate, then concatenating
          file_list <- list.files(path = paste0(output_dir,buoy), pattern = "stdmet.rds", full.names = TRUE)
          met_list <- list.files(path = paste0(output_dir,buoy), pattern = "hullId.rds", full.names = TRUE)

          if(length(file_list)>0){

               # # start writing to an output file
               # sink(paste0(data_dir,"2_MEDS_concat_",buoy,"_",Sys.Date(),".txt"))
               print(paste0("Starting on MEDS... ",buoy))
               print(file_list)

               ## create buoy specific concat folder
               if (!file.exists(paste0(output_dir,buoy,"/"))) {dir.create((paste0(output_dir,buoy,"/")))}

               # read in files
               dat <- readRDS(file_list)
               if(length(met_list)>0){met <- readRDS(met_list)}

               #----------------------------------------------------------------------------------------
               # qc data
               # https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/formats-eng.html#Co-Quad
               # 0 Blank - No quality control (QC) has been performed
               # 1 Good - QC has been performed: record appears correct
               # 3 Doubtful - QC has been performed: record appears doubtful
               # 4 Erroneous - QC has been performed: record appears erroneous
               # 5 Changes - The record has been changed as a result of QC
               # 6 Acceptable - QC has been performed: record seems inconsistent with other records
               # 7 Off Position - There is a problem with the buoy position or mooring. Data may still be useful.
               # 8 Reserved
               # 9 Reserved - indicates missing elements
               #----------------------------------------------------------------------------------------
               dat <- dat[dat$Q_FLAG != 3, ]
               dat <- dat[dat$Q_FLAG != 4, ]
               dat <- dat[dat$Q_FLAG != 7, ]

               #----------------------------------------------------------------------------------------
               # join dat and met
               if(exists("met")){
                    dat$Date <- as.Date(dat$DateTime)
                    dat$hull_ID <- dat$hull_id
                    dat$hull_id <- NULL
                    dat <- left_join(dat,met, by = "Date")
                    dat$hull_ID <- ifelse(is.na(dat$hull_ID), dat$hull_id, dat$hull_ID)
                    dat$Date <- NULL; dat$hull_id <- NULL
                    rm(met)
               }else{print("no met data to join")}
               
               # export
               write.csv(dat, file = paste0(output_dir, buoy, "/c",buoy,'_stdmet_QC_met.csv'), row.names = FALSE)
               saveRDS(dat, file = paste0(output_dir, buoy, "/c",buoy,'_stdmet_QC_met.rds'))

               print(paste0("Finished MEDS stdmet... ",buoy))
               rm(dat)

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

