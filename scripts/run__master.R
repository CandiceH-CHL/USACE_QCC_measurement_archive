#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## master script

# This script initiates the function scripts within this product suite within the correct order. 
# This script can run all the buoy stations in serial, or can be copied to run the function scripts 
# on the HPC in parallel. 

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
## install libraries
# install.packages("lubridate")
# install.packages("R.utils")
# install.packages("qpcR")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("ncdf4")
# install.packages("tidyverse")
# install.packages("readxl") # dependent on tidyverse
# install.packages("stringr") 
# install.packages("data.table") 
# install.packages("tidyr") 
# install.packages("gridExtra") 
# install.packages("oce") 
# install.packages("naniar") 
# install.packages("broom") 
# install.packages("openair")  # polarplots
# install.packages("plotly") 
# install.packages("magrittr") 
# install.packages("grid") 
# install.packages("devtools") 
# install.packages("lsr") 
# install.packages("RColorBrewer") 
# install.packages("viridis") 
# install.packages("colorRamps") 
# install.packages("ggplot2") 
# install.packages("ggmap") 
# install.packages("maps") 
# install.packages("mapdata") 

## load libraries (local runs)
library(lubridate)
library(R.utils)
library(qpcR)
library(plyr)
library(dplyr)
library(tibble)
# library(ncdf4)
library(tidyverse)
library(readxl) # dependent on tidyverse
library(stringr)
library(data.table)
library(tidyr)
library(gridExtra)
# library(sf)
# library(oce)
library(naniar)
library(broom)
library(openair) # polarplots
library(plotly)
library(magrittr)
# library(grid)
# library(devtools)
library(lsr)
library(RColorBrewer)
library(viridis)
library(colorRamps)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(modeest)

## load libraries (HPC runs)

# library(lubridate, lib="/p/home/candice/Rlibs/")
# library(R.utils, lib="/p/home/candice/Rlibs/")
# library(qpcR, lib="/p/home/candice/Rlibs/")
# library(plyr, lib="/p/home/candice/Rlibs/")
# library(dplyr, lib="/p/home/candice/Rlibs/")
# library(ncdf4, lib="/p/home/candice/Rlibs/")
# library(tidyverse, lib="/p/home/candice/Rlibs/")
# library(readxl, lib="/p/home/candice/Rlibs/") # dependent on tidyverse
# library(stringr, lib="/p/home/candice/Rlibs/")
# library(data.table, lib="/p/home/candice/Rlibs/")
# library(tidyr, lib="/p/home/candice/Rlibs/")
# library(gridExtra, lib="/p/home/candice/Rlibs/")
# library(sf, lib="/p/home/candice/Rlibs/")
# library(oce, lib="/p/home/candice/Rlibs/")
# library(naniar, lib="/p/home/candice/Rlibs/")
# library(broom, lib="/p/home/candice/Rlibs/")
# library(openair, lib="/p/home/candice/Rlibs/") # polarplots
# library(plotly, lib="/p/home/candice/Rlibs/")
# library(magrittr, lib="/p/home/candice/Rlibs/")
# library(grid, lib="/p/home/candice/Rlibs/")
# library(devtools, lib="/p/home/candice/Rlibs/")
# library(lsr, lib="/p/home/candice/Rlibs/")
# library(RColorBrewer, lib="/p/home/candice/Rlibs/")
# library(viridis, lib="/p/home/candice/Rlibs/")
# library(colorRamps, lib="/p/home/candice/Rlibs/")
# library(ggplot2, lib="/p/home/candice/Rlibs/")
# library(ggmap, lib="/p/home/candice/Rlibs/")
# library(maps, lib="/p/home/candice/Rlibs/")
# library(mapdata, lib="/p/home/candice/Rlibs/")
# library(modeest, lib="/p/home/candice/Rlibs/")


## select the following data locations and start date
start_date <- 1970
drive <- "F:/Candice/"
# drive <- "/p/work/candice/"
data_dir <- paste0(drive, "projects/WaveTrends/data/")
# data_dir <- paste0(drive, "projects/WaveTrends/annual_runs/data/")
setwd(data_dir)

## set colors for each buoy
plot_colors <- viridis(n=6)
# plot_colors <- matlab.like2(4)
color_ndbc_raw <- plot_colors[1]
color_ndbc_orig <- plot_colors[5]    #"#440154FF" # purple
color_ndbc_recalc <- plot_colors[3] # "#3B528BFF"  #"5 = #FDE725FF" # yellow
color_chl_calc <- plot_colors[4]  # "#21908CFF"   #"#21908CFF" # green
color_WIS <- "red"     # "#5DC863FF"

## set colors for wind/wave polar plots
# brewer.pal(n = 8, name = "RdBu")
colour1 <- viridis(n=4)
cols1 <- c(colour1[4], colour1[3], colour1[2], colour1[1])
colour2 <- viridis(n=5)
cols2 <- c(colour2[5], colour2[4], colour2[3], colour2[2], colour2[1])
# cols1 <- c("black", "green", "blue", "red")
# plot type, symbol, size
type = "l"
pch = "."
lwd = 0.5
cex = 0.5

## set plot parameters
## set parameters common to all plots
xlab = "Date"   # label for x axis
width = 2000+2000    # width of exported plot
height = 1500+1500   # height of exported plot
res = 300  # plot resolution
width1 = 1000
height1 = 700
par1 = c(5,5,4,4)
Delta <- '\U0394'
degree <- '\U00B0'
## my.grid function formats
my.format <- "%m-%d-%Y" # "%m-%Y" (long datasets) or "%m-%d-%Y" (short datasets)
my.period <- "weeks" # "months" (long datasets) or "weeks (short datasets)
## my grid function for plots
my.grid <-function(dataset, my.period = "year", my.format = "%Y"){
        grid(nx=NA, ny=NULL)
        abline(v=axis.POSIXct(1, at=seq(min(dataset[1,1]), max(dataset[nrow(dataset),1]), 
                                        by= my.period), format=my.format),
               col = "lightgray", lty = "dotted", lwd = par("lwd"))
}
## function to capitalize string
simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
}

## The choice of significance level at which you reject H0 is arbitrary. 
sig <- 0.01

## to run all of the buoy stations in serial, read in the NDBC buoy list as follows:
list_ndbc <- read.csv(paste0(data_dir,"NDBC_buoys.csv"),header = TRUE)
list_ndbc <- dplyr::filter(list_ndbc, list_ndbc$owner == "NDBC")
list_ndbc_buoy <- as.character(list_ndbc$station)
buoy_ls <- list_ndbc_buoy
rm(list_ndbc_buoy, list_ndbc)
print(buoy_ls)

# ## to run all of the buoy stations in serial, use the following code that assigns the names of this master 
# ## files to the scripts to isolate a specific buoy number:
# buoy_ls <- gsub(".R","",unlist(strsplit(rstudioapi::getSourceEditorContext()$path,"__"))[2])
# print(buoy_ls)
# print(Sys.time())

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## download ndbc and ncei datasets from ndbc and ncei websites
print("starting data download...")
start_time <- Sys.time()
print(Sys.time())
## Be aware that NDBC changes 'Dxx' extensions in NCEI NetCDF data! The code is currently set to test for 'D' extensions between 1-20.
for (buoys in buoy_ls){
        print(buoys)
        source(paste0(data_dir,"download_data_1.R"))
        download_data_1(buoys, start_year = start_date, data_dir)
        rm(download_data_1, read_NetCDF_post_2011)
}
print(Sys.time())
end_time <- Sys.time()
print("finished data download")

#----------------------------------------------------------------------------------------

## look for empty folders in each data directory and delete

## ndbc unzipped folder
# Get vector of all folder names
dirlist <- list.dirs(paste0(data_dir,"raw_data/ndbc/unzipped/"))
length(dirlist)
# Extract vector of empty folder names
empty_dir <- dirlist[sapply(dirlist, function(x) length(list.files(x))==0)]
length(empty_dir)
print(empty_dir)
# Remove empty folders
unlink(empty_dir, recursive=TRUE, force=FALSE)
# Get vector of all file names
ff <- dir(paste0(data_dir,"raw_data/ndbc/unzipped/"), recursive=TRUE, full.names=TRUE)
length(ff)
# Extract vector of empty files' names
eff <- ff[file.info(ff)[["size"]]==0]
length(eff)
print(eff)
# Remove empty files
unlink(eff, recursive=TRUE, force=FALSE)
rm(dirlist,empty_dir,ff, eff)

## ndbc zipped folder
# Get vector of all folder names
dirlist <- list.dirs(paste0(data_dir,"raw_data/ndbc/zipped/"))
length(dirlist)
# Extract vector of empty folder names
empty_dir <- dirlist[sapply(dirlist, function(x) length(list.files(x))==0)]
length(empty_dir)
print(empty_dir)
# Remove empty folders
unlink(empty_dir, recursive=TRUE, force=FALSE)
# Get vector of all file names
ff <- dir(paste0(data_dir,"raw_data/ndbc/zipped/"), recursive=TRUE, full.names=TRUE)
length(ff)
# Extract vector of empty files' names
eff <- ff[file.info(ff)[["size"]]==0]
length(eff)
print(eff)
# Remove empty files
unlink(eff, recursive=TRUE, force=FALSE)
rm(dirlist,empty_dir,ff, eff)
 
##----------------------------------------------------------------------------------------
 
## ncei netCDF folder
# Get vector of all folder names
dirlist <- list.dirs(paste0(data_dir,"raw_data/ncei/netCDF/"))
length(dirlist)
# Extract vector of empty folder names
empty_dir <- dirlist[sapply(dirlist, function(x) length(list.files(x))==0)]
length(empty_dir)
print(empty_dir)
# Remove empty folders
unlink(empty_dir, recursive=TRUE, force=FALSE)
# Get vector of all file names
ff <- dir(paste0(data_dir,"raw_data/ncei/netCDF/"), recursive=TRUE, full.names=TRUE)
length(ff)
# Extract vector of empty files' names
eff <- ff[file.info(ff)[["size"]]==0]
length(eff)
print(eff)
# Remove empty files
unlink(eff, recursive=TRUE, force=FALSE)
rm(dirlist,empty_dir,ff, eff)

## ncei ASCII folder
# Get vector of all folder names
dirlist <- list.dirs(paste0(data_dir,"raw_data/ncei/ascii/"))
length(dirlist)
# Extract vector of empty folder names
empty_dir <- dirlist[sapply(dirlist, function(x) length(list.files(x))==0)]
length(empty_dir)
print(empty_dir)
# Remove empty folders
unlink(empty_dir, recursive=TRUE, force=FALSE)
# Get vector of all file names
ff <- dir(paste0(data_dir,"raw_data/ncei/ascii/"), recursive=TRUE, full.names=TRUE)
length(ff)
# Extract vector of empty files' names
eff <- ff[file.info(ff)[["size"]]==0]
length(eff)
print(eff)
# Remove empty files
unlink(eff, recursive=TRUE, force=FALSE)
rm(dirlist,empty_dir,ff, eff)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## If this is an annual run, merge the previous and updated metadata sheets for use in concat step.
print("starting merge metadata...")
start_time <- Sys.time()
print(Sys.time())
source(paste0(data_dir,"merge_metadata_1a.R"))
merge_metadata_1a(data_dir)
rm(merge_metadata_1a)
print(Sys.time())
end_time <- Sys.time()
print("finished merge metadata")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## concatenate ndbc and ncei datasets from downloaded data
print("starting data concat...")
start_time <- Sys.time()
print(Sys.time())
for (buoys in buoy_ls){
     source(paste0(data_dir,"concat_data_2.R"))
     concat_data_2(buoys, start_year = start_date, data_dir)
     rm(concat_data_2,concat_ndbc, concat_ncei)
     rm_ls = ls(pattern = paste0("s_", buoys))
     rm(list = rm_ls)
     rm(rm_ls)
}
print(Sys.time())
end_time <- Sys.time()
print("finished data concat")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## ** update NDBC google spreadsheets downloads **
## Download latest NDBC metadata sheets (download folders)

## verify ncei netcdf metadata with NDBC Google spreadsheets
print("starting ncei metadata verification...")
start_time <- Sys.time()
print(Sys.time())
for (buoys in buoy_ls){
     source(paste0(data_dir,"verify_netcdf_3.R"))
     verify_netcdf_3(buoys, data_dir)
     rm(verify_netcdf_3)
}
print(Sys.time())
end_time <- Sys.time()
print("finished ncei metadata verification")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## geoclean the ndbc and ncei data
print("starting data geoClean...")
start_time <- Sys.time()
print(Sys.time())
for (buoys in buoy_ls){
     source(paste0(data_dir,"geoClean_data_4.R"))
     geoClean_data_4(buoys, data_dir)
     rm(geoClean_data_4, plot_stdmet, plots_spec)
}
print(Sys.time())
end_time <- Sys.time()
print("finished data geoClean")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

## create best available ndbc dataset with ncei metadata
print("starting best data...")
start_time <- Sys.time()
print(Sys.time())
for (buoys in buoy_ls){
     source(paste0(data_dir,"create_best_data_5.R"))
     create_best_data_5(buoys, data_dir)
     rm(create_best_data_5)
}
print(Sys.time())
end_time <- Sys.time()
print("finished best data")
 
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

# build CHL Thredds NDBC netcdf file
start_time <- Sys.time()
print(Sys.time())
for (buoys in buoy_ls){
     source(paste0(data_dir,"build_thredds_netcdf_6.R"))
     build_thredds_netcdf_6(buoys, data_dir)
     rm(build_thredds_netcdf_6)
}
print(Sys.time())
end_time <- Sys.time()

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

