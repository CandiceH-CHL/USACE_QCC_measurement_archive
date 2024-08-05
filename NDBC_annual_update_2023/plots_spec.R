plots_spec <- function(dat = "dat1",year1 = "year1", df = "df", mainTitle = "mainTitle"){
        # spec plot function - 02/12/2020 ch
        par(mar=par1)
        # dat <- dat1

        dat_index <- which(names(dat)==year1)
        type_dat <- unlist(strsplit(df,"_"))[4]
        freq1 <- unlist(strsplit(df,"_"))[6]
                
        dir_type <- c("alpha1", "alpha2", "r1", "r2", "gamma2", "gamma3", "a1", "a2", "b1", "b2","phih", "rhq") 
        c11_type <- c("c11", "c11m","C12", "Q12", "C13", "Q13","C22", "C23","C33", "Q23") 
        
        if(unlist(strsplit(type_dat,"_"))[1] %in% dir_type){
                if(sum(dir_type ==  "a1"| dir_type ==  "a2"| dir_type ==  "b1"| dir_type ==  "b2" | dir_type ==  "phih" | dir_type ==  "rhq")>0){
                        ylab = type_dat
                        ylim <- c(floor(min(dat[,dat_index], na.rm = TRUE)),ceiling(max(dat[,dat_index], na.rm = TRUE)))
                }else{
                        ylab = paste0("Directional Spectrum (",degree,")")
                        ylim <- c(floor(min(dat[,dat_index], na.rm = TRUE))-20,ceiling(max(dat[,dat_index], na.rm = TRUE))+20)
                }

                if(freq1 == "new"){xlim = c(0,0.5)
                }else if(freq1 == "old"){xlim = c(0,0.4)
                }else{xlim = c(0,0.5) 
                }

                 
        }else if(unlist(strsplit(type_dat,"_"))[1] %in% c11_type){
                if(unlist(strsplit(type_dat,"_"))[1] == "c11"){ylab = expression('C'[11]*' (m'^2*'/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "c11m"){ylab = expression('C'[11]*''^m*' (m/s'^2*')'^2*'/Hz '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "C12"){ylab = expression('C'[12]*' (m/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "Q12"){ylab = expression('Q'[12]*' (m/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "C13"){ylab = expression('C'[13]*' (m/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "Q13"){ylab = expression('Q'[13]*' (m/Hz) '['mean'])
                
                }else if(unlist(strsplit(type_dat,"_"))[1] == "C22"){ylab = expression('C'[22]*' (1/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "C23"){ylab = expression('C'[23]*' (1/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "C33"){ylab = expression('C'[33]*' (1/Hz) '['mean'])
                }else if(unlist(strsplit(type_dat,"_"))[1] == "Q23"){ylab = expression('Q'[23]*' (1/Hz) '['mean'])
                }
                
                

                ylim <- c(floor(min(dat[,dat_index], na.rm = TRUE))-0.2,ceiling(max(dat[,dat_index], na.rm = TRUE))+0.2)

                if(freq1 == "new"){xlim = c(0,0.5)
                }else if(freq1 == "old"){xlim = c(0,0.4)
                }else{xlim = c(0,0.5) 
                }
        }
        
        if(ylim[1] != Inf){
                # dat data 
                if(unlist(strsplit(type_dat,"_"))[1] %in% dir_type){
                  plot(dat$Frequency, dat[,dat_index], 
                       ylab = ylab, xlab = "Frequency (Hz)", 
                       # type = "o", 
                       lty = 5, pch = 0, col = "skyblue2", cex = 1, cex.lab = 1.2, 
                       xaxt="n", xlim = xlim, ylim = ylim, panel.first=grid(), main = mainTitle)
                } 
                if(unlist(strsplit(type_dat,"_"))[1] %in% c11_type){
                  plot(dat$Frequency, dat[,dat_index], 
                       ylab = ylab, xlab = "Frequency (Hz)", 
                       type = "o", lty = 5, pch = 0, col = "skyblue2", cex = 1, cex.lab = 1.2,
                       xaxt="n", yaxt="n", xlim = xlim,ylim = ylim, panel.first=grid(), main = mainTitle) 
                }
        
                 # axes
                # axis(side = 4)
                # mtext(side = 4, line = 3, 'Standard Deviation')
                axis(1, seq(xlim[1],xlim[2], by = 0.05))
                # if(freq == "new"){axis(1, seq(0,0.5, by = 0.05))}
                # if(freq == "old"){axis(1, seq(0,0.4, by = 0.05))}
                # if(gsub("cols","",freq) == gsub("cols","",freq)){xlim = c(0,round(dat_all$Frequency[nrow(dat_all)],1))}
                # 
                if(unlist(strsplit(type_dat,"_"))[1] %in% c11_type){
                        tryCatch(
                                {# ylim <- c(floor(min(dat[,dat_index])-1),ceiling(max(dat[,dat_index])+1))
                                aty <- seq(ylim[1],ylim[2],by = 0.5)
                                labels <- sapply(aty,function(i)
                                        as.expression(bquote(10^ .(i)))
                                )
                                axis(2,at=aty,labels=labels, las=1)
                                },
                                error = function(cond){
                                        aty <- c(ylim[1],mean(ylim[2]-ylim[1]),ylim[2])
                                        labels <- sapply(aty,function(i)
                                                as.expression(bquote(10^ .(i)))
                                        )
                                        axis(2,at=aty,labels=labels, las=1)
                                }
                        )
                } 
        }
        # # legend
        # par(xpd=TRUE)
        # # legend(2.8,-1,c("group A", "group B"), pch = c(1,2), lty = c(1,2))
        # if(length(dat_index) > 0){
        #         legend("topright", inset=c(0,-0.08), 
        #                box.lty = 0, legend=c(paste0(toupper(dat_source)," GeoClean"), paste0(toupper(dat_source)," All")), 
        #                col=c("blue","skyblue2"), lty = c(5,5), 
        #                pch = c(1,0), cex=0.75)
        # }else{
        #         legend("topright", inset=c(0.08,-0.08), 
        #                box.lty = 0, legend=c(paste0(toupper(dat_source)," No GeoClean Data"), paste0(toupper(dat_source)," All")), 
        #                col=c(NA,"skyblue2"), lty = c(NA,5), 
        #                pch = c(NA,0), cex=0.75)
        # }
}



