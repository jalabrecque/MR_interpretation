#################################################################################
# Packages required to run this script:                                         #
# install.packages(c("here","pracma","magrittr", "dplyr"))                      #
#                                                                               #
# If you open the "src.R" file by clicking on the file (i.e. not through        #
# RStudio) and run 'library(here)' the working directory will automatically be  #
# set to the right folder.                                                      #
#                                                                               #
# Simulation by Jeremy A. Labrecque                                             #
# Postdoctoral research fellow                                                  #
# Department of Epidemiology, Erasmus MC                                        #
# j.labrecque@erasmusmc.nl                                                      #
#################################################################################

library(here)
library(pracma)
library(magrittr)
library(dplyr)
source("MR_longitudinal_simulation.R")
source("MR_longitudinal_simulation_plot.R")


# Plots ------------------------------------------------------------------------

# Set plot parameters
par(mfrow = c(3, 4)) # 2-by-2 grid of plots
par(oma = c(4, 6, 1.6, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(2, 2, 1, 1)) # make the plots be closer together

# Plot exposure window and genetic effects
grid <- expand.grid(GbyE_shape=c("unif","FTO","incr","decr"),
                    exp_window_shape=c("unif","recent","critical"))

ds <- mapply(grid[ ,1], grid[ ,2],FUN=function(x,y) MR_longitudinal_sim_plot(GbyE_shape = x, exp_window_shape = y)) 


# Add 
mtext("Age (years)", outer=TRUE, side =1, line =1.5 )
mtext("BMI", outer = TRUE, side = 2, line = 1.5)
mtext(c("Constant","FTO","Increasing","Decreasing"), outer = TRUE, side = 3, at = seq(0.135, 0.89, length.out = 4))
mtext(c("Uniform","Recent","Critical"), outer = TRUE, side = 2, at = seq(0.84, 0.135, length.out = 3))


# Tables

# Generate results 
grid <- expand.grid(GbyE_shape=c("unif","incr","decr","FTO"),exp_window_shape=c("unif","recent","critical","incr"))

ds <- do.call(rbind, mapply(grid[ ,1], grid[ ,2],
                            FUN=function(x,y) MR_longitudinal_sim(GbyE_shape = x, exp_window_shape = y), SIMPLIFY = FALSE)) %>% as.data.frame                
ds$GbyE <- as.character(grid$GbyE_shape)
ds$exp_win <- as.character(grid$exp_window_shape)

# Clean up table
ds %<>% select(GbyE, exp_win, everything())
ds$GbyE <- rep(c("Constant", "Increasing", "Decreasing", "FTO"),4)
ds$exp_win <- rep(c("Uniform", "Recent", "Critical", "Increasing"), each=4)
ds <- ds[order(match(ds$GbyE,c("Constant", "Increasing", "Decreasing", "FTO"))),]
row.names(ds) <- NULL
names(ds) <- c("Genetic scenario", "Exposure window", 
               "True effect", "MR estimate", "Absolute bias", "Relative bias (%)",
               "True effect", "MR estimate", "Absolute bias", "Relative bias (%)")

ds[, c(3,4,5,7,8,9)] <- format(round(ds[, c(3,4,5,7,8,9)],1), nsmall = 1)
ds[, c(6,10)] <- format(round(ds[, c(6,10)],0), nsmall = 0)


# Results
ds
