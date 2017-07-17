

## BTD - Function to create flow duration curve with error bars

## define these parameters: chr.dir.main, chr.dir.flow.data, chr.dir.figures, chr.eb

flow.d.c <- function(chr.dir.main, chr.dir.flow.data, chr.dir.figures, chr.eb) {

                                              ## chr.dir.main - path to main workspace
                                              ## chr.dir.flow.data - path to flow data 
                                              ## chr.dir.figures - path to write the figures  
                                              ## chr.eb - path to error bar data 
  
  library(ggplot2)
  library(scales)                             ## BTD - Generic plot scaling nethods
  library(tidyr)                              ## BTD -  Makes it easy to "tidy" your data. Tidy data is data that's easy to work with.
  library(Hmisc)                              ## BTD - Contains functions useful for data analysis, high-level graphics, etc.
  library(plyr)                               ## BTD - Tools for splitting, applying, and combining data
  library(dplyr)                              ## BTD - A fast, consistent tool for working with data frame like objects, both in and out of memory



## BTD - Read the error bar data into R
eb <- read.csv(file = chr.eb )

## BTD - Create unique combination of a set of vectors pertaining to each station number, each station number is a character vector
chr.stn.num <- as.character(unique(eb$station))


## BTD - Create a for loop for each station

for(j in 1:length(chr.stn.num)) {


  ## plot fdc for Upper Yaquina River Bacteria TMDL report document

  options(stringsAsFactors = FALSE)


  ## flow data file
  chr.file.flow.data <- paste0("flow_stn",chr.stn.num[j], ".txt")

  ## name of output figure file
  chr.file.figure <- paste0("fdc-stn-", chr.stn.num[j], ".png")

  ## creat empty data frome for flow data
  df.data.flow <- data.frame(stn = character(0), date = character(0),
                             flow_cfs = character(0))

  ## path and file name for flow data file
  tmp.flow.file <- paste0(chr.dir.flow.data, "/", chr.file.flow.data)

  ## read the flow data file into R
  tmp.data <-
    read.table(file=tmp.flow.file,sep="\t",
               header=TRUE, stringsAsFactors=FALSE, colClasses="character")

  ## populate the flow data frame with the flow data
  df.data.flow <- data.frame(stn = tmp.data$stn,
                             date =  as.POSIXct(strptime(tmp.data$date,
                                                         format = "%m-%d-%Y")),
                             flow_cfs = as.numeric(tmp.data$flow_cfs))


  ## replace NAs in flow data with nearest previous non-NA value
  tmp.flow.no.na <- (df.data.flow %>% fill(flow_cfs))
  df.data.flow <- cbind(df.data.flow,
                        value = tmp.flow.no.na$flow_cfs)

  ## create a column for the flow exceedance in the flow data frame
  df.data.flow <- cbind(df.data.flow, flow.exceed = -1)


  ## create a function to calculate the flow exceedance
  flow.exceed <- function(v.flow) {
    tmp.rank <- rank(v.flow, ties.method = "average")
    tmp.exceed <- tmp.rank / length(v.flow)
    tmp.exceed <- 100 * (1 - tmp.exceed)
    return(tmp.exceed)
  }

  ## calculate the flow exceedance for the flow data
  df.data.flow$flow.exceed <- flow.exceed(df.data.flow$value)


  ## make plots of fdc

  ## dynamically calculate the breaks for the numbers on the y-axis
  tmp.breaks <- 10^seq(from = floor(log10(min(df.data.flow$value,
                                              na.rm = TRUE))),
                       to = ceiling(log10(max(df.data.flow$value,
                                              na.rm = TRUE))),
                       length.out = 5)

  fancy_scientific <- function(l) {
    ## function taken from stackoverflow.com post
    ## http://stackoverflow.com/questions/11610377/how-do-i-change-the-formatting-of-numbers-on-an-axis-with-ggplot/24241954
    # turn in to character string in scientific notation
    x <- format(l, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    #y <- gsub("^(.*)e", "'\\1'e", x)
    # turn the 'e+' into plotmath format
    z <- gsub("^.*e", "10^", x)
    # return this as an expression
    parse(text=z)
  }

  ## add the fdc to the plot and set how the axes will appear
  p1 <- ggplot(data = df.data.flow) +
    geom_line(aes(x = flow.exceed, y = value),
              color = "blue", size = 1.5) +
    scale_y_log10(limits = range(tmp.breaks), breaks = tmp.breaks,
                  minor_breaks = c(sapply(tmp.breaks, function(x) seq(0, x, x/10))),
                  labels = fancy_scientific) +
    scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    labs(
      x = "Flow Exceedance (%)",
      y = "Average Daily Flow (cfs)"
    )

  ## set the appeareance of the grid lines and the text in the figure
  p2 <- p1 +
    theme(
      axis.title = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 8, color = "black"),
      panel.grid.major = element_line(colour = "grey60"),
      panel.grid.minor = element_line(colour = "grey60"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )

  ## now add information dynamically for the flowzone boundaires
  ## flowzone descriptions
  chr.flz.desc <- c("High Flows", "Transitional Flows", "Typical Flows",
                    "Dry Flows", "Low Flows")

  ## flowzone for exceedance boundaries
  num.flow.exceed.bnd <- c(10,40,60,90)

  ## add lines for flowzone boundaries
  p3 <- p2 +
    geom_vline(xintercept = num.flow.exceed.bnd, size = 1.5,
               linetype = "dashed")


  ## adding text for flowzone boundaires
  ## get the mid-pojnts of the x-values of the flowzones
  junk <- c(0, num.flow.exceed.bnd, 100)
  junk.mid <- c()
  for(ii in 2:length(junk)) {
    junk.mid <- c(junk.mid, junk[ii-1] + (junk[ii] - junk[ii-1]) / 2)
  }
  num.flow.exceed.bnd.mids <- junk.mid
  rm(junk, junk.mid)

  ## create a data frame that has the x and y values along with the labels for the flowzone boundaries
  df.fz.lables <- data.frame(x = num.flow.exceed.bnd.mids,
                             y = 10^ (rep(1, length(num.flow.exceed.bnd.mids))* min(p3$scales$scales[[1]]$limits) +
                                        0.015 * (p3$scales$scales[[1]]$limits[2] - p3$scales$scales[[1]]$limits[1])),
                             chr.label = chr.flz.desc)
  ## add flowzone text to the fdc plot
  p4 <- p3 + geom_text(data = df.fz.lables,
                       aes(x = x, y = y, label = chr.label),
                       size = 6 * 0.352777778, fontface = "bold")

  ## set the width of the plot in inches
  p.width = 6

  ## write the plot to a graphics file
  png(filename = paste0(chr.dir.figures, "/", chr.file.figure), width = p.width,
      height = round(p.width / 1.61803398875, 1), units = "in",
      res = 1200)
  plot(p4)
  dev.off()

  ## BTD - Add Error Bars

  ## BTD - Take all data from error bar dataset and filter or return rows according to each station number
  df.err.bar <- eb %>% filter(station == chr.stn.num[j])

  ## BTD - Turn flow.exceed column fron a decimal into a percentage
  df.err.bar$flow.exceed <- df.err.bar$flow.exceed * 100


  ## BTD - Add ggplot for error bars to the fdc plot, make error bars red
  p5 <- p4 +
    geom_point(data = df.err.bar, aes(x = flow.exceed, y = value), color = "red") +
    geom_errorbar(data = df.err.bar,
                  aes(x=flow.exceed, ymin = err.lower.limit ,
                      ymax = err.upper.limit), color = "red")



  ## BTD - Name of the fdc plot with the added error bars
  png(filename = paste0(chr.dir.figures, "/",
                         paste0('fdc-stn-', chr.stn.num[j], '-w-err-bar.png')),
       width = p.width, height = round(p.width / 1.61803398875, 1), units = "in",
       res = 1200)
  plot(p5)
  dev.off()


}
}

flow.d.c(chr.dir.main = "M:/Models/Bacteria/LDC/Bernadette-workspace",
         chr.dir.flow.data = "M:/Models/Bacteria/LDC/Bernadette-workspace/data" ,
         chr.dir.figures = "M:/Models/Bacteria/LDC/Bernadette-workspace/figures",
         chr.eb = "//deqhq1/TMDL/TMDL_WR/MidCoast/Models/Bacteria/LDC/Bernadette-workspace/data/fdc-err-bars.csv")
