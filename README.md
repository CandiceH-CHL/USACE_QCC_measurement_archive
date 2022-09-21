# USACE_QCC_measurement_archive
USACE Quality Controlled, Consistent Measurement Archive

SYNOPSIS:
The U.S. NOAA National Data Buoy Center (NDBC) releases duplicate meteorological and wave measurement data in multiple archive locations. Each data source has their own idiosyncrasies that cause differences within data with matching dates and times, even when collected by the same sensor at the same location. These variances need to be accounted for when using these NBDC data within U.S. Army Corps of Engineers (USACE) Engineers and Research Development Center (ERDC) products. These data issues are clearly outlined within the following USACE document: ERDC/CHL CHETN-I-100.

These NDBC archive errors necessitated the development of in-house USACE quality control (QA/QC) checks and metadata corrections to develop a best available measurement archive (herewith called the USACE QCC measurement archive) that includes comprehensive collection metadata. Data within this archive are sourced from the NDBC website historical station pages (https://www.ndbc.noaa.gov/) and the office NOAA archives stored on the National Center for Environmental Information (NCEI) servers (https://www.ncei.noaa.gov/access/marine-environmental-buoy-database/).

Additionally, Fisheries and Oceans Canada (DFO) Marine Environmental Data Section (MEDS) Archive wave data are incorporated into this USACE QCC measurement data.  Wave data from buoys and models, and unofficial Meteorological Service of Canada (MSC) observations are downloaded, concatenated, qc'd and exported as .nc files. MEDS Link: https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/data-donnees/index-eng.asp.

These self-described USACE QCC measurement data are stored within the USACE ERDC CHL Data Server (Thredds) (https://chlthredds.erdc.dren.mil/), alongside the WIS long-term hindcast, and on the USACE ERDC CHL WIS Portal (https://wisportal.erdc.dren.mil/). Both are accessible to both the USACE and the public. 

Near-real time data updates to this USACE QCC measurement archive are completed annually.

This Standard Operating Procedure (SOP) outlines the processing steps necessary to produce these annual updates. To prevent knowledge loss, this SOP also includes instructions on how to re-process the full historical dataset. For full comprehension, review this SOP in conjunction with an associated USACE QCC NDBC Archive R scripts pdf.
