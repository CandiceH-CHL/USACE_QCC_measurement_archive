plot_stdmet <- function(dat = "dataset", i = "i", buoy = "buoy"){

        ## set plot types
        lty_main <- 5
        pch_main <- 2

        # inputs for main title and ylabel
        xlablist <- c("Significant Wave Height",  "Average Period",  "Dominant Period",  "Mean Wave Direction",
                      "Wind Speed", "Wind Direction", "Wind Gust Speed", "Sea Level Pressure", 
                      "Air Temperature", "Water Temperature","Dew Point Temperature")
        
        ylablist <- c("WVHGT (m)", "AVGPD (s)", "DOMPD (s)", paste("MWDIR (", degree,")"),
                      "WSPD (m/s)", paste0("WDIR (", degree, ")"), "GST (m/s)", "BARO (hPa)",
                      paste0("ATMP (", degree, "C)"),paste0("WTMP (", degree, "C)"), paste0("DEWP (", degree, "C)"))
        #----------------------------------------------------------------------------------------
        ## NDBC / NCEI inputs set
        #----------------------------------------------------------------------------------------
        if(i=="significant_wave_height"){main = xlablist[1] ; ylab = ylablist[1]}
        if(i=="average_wave_period" ){main = xlablist[2] ; ylab = ylablist[2]}
        if(i=="dominant_wave_period" ){main = xlablist[3] ; ylab = ylablist[3]}
        if(i=="mean_wave_direction" ){main = xlablist[4] ; ylab = ylablist[4]}
        if(i=="wind_speed"){main = xlablist[5] ; ylab = ylablist[5]}
        if(i=="wind_direction"){main = xlablist[6] ; ylab = ylablist[6]}
        if(i=="wind_gust" ){main = xlablist[7] ; ylab = ylablist[7]}
        if(i=="air_pressure_at_sea_level"){main = xlablist[8] ; ylab = ylablist[8]}
        if(i=="air_temperature"){main = xlablist[9] ; ylab = ylablist[9]}
        if(i=="sea_surface_temperature"){main = xlablist[10]; ylab = ylablist[10]}
        if(i=="dew_point_temperature"){main = xlablist[11]; ylab = ylablist[11]}
        #----------------------------------------------------------------------------------------

        # plot 
        plot(dat[[1]], dat[[2]], main = paste0("NDBC Station ",buoy," ", main), xlab = "Date", ylab = ylab,
             col = "darkblue", type = type, pch = pch, cex = cex)#,  xaxt="n") #lwd = lwd, 
        if(i == "significant_wave_height") {abline(v= 0, h = 0.25, col = "black", lty = 2)}
        # my.grid(z, "years", "%Y")
        grid()
}