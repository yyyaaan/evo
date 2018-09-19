library(tidyverse); library(hrbrthemes); library(gridExtra)
data <- readRDS("dataOK.rds")
# myplot <- function(vector) plot_ly(data) %>% add_markers(x = 1:length(vector), y = vector)
# ok plot are masked in function
# RColorBrewer::display.brewer.all()


# new plots ---------------------------------------------------------------

grid.arrange(
  ggplot(data %>% filter(the.standby.hours < 1600), aes(x = the.release.date, y = the.standby.hours)) + 
    geom_jitter(color = "grey") + geom_smooth() + 
    labs(x = "release date", y = "standby time (hours)",
         title = "Evolution of mobile phone hardware (group 2)",
         subtitle = "by standby time") +
    theme_ipsum_tw()
  ,
  ggplot(data %>% filter(the.weight.gram < 1000), aes(x = the.release.date, y = the.weight.gram)) + 
    geom_jitter(color = "grey") + geom_smooth() + 
    labs(x = "release date", y = "phone weight (gram)", title = " ",
         subtitle = "by weight") +
    theme_ipsum_tw()
  ,
  ncol = 2
)






 
# process images ----------------------------------------------------------

plotScreens <- function(){
  grid.arrange(
    ggplot(data , aes(x = the.release.date, y = the.total.pixels.group)) + 
      geom_jitter(color = "grey") + geom_smooth() + 
      labs(x = "release date", y = "screen resolutions category", 
           title = "Evolution of mobile phone hardware", subtitle = "measured by screen resolutions") +
      theme_ipsum_tw()
    ,
    ggplot(data, aes(x = the.release.date, y = the.screen2body.percent)) + 
      geom_jitter(color = "grey") + geom_smooth() + 
      labs(x = "release date", y = "screen-to-body ratio", title = " ",
           subtitle = "by screen-to-body ratios") +
      theme_ipsum_tw()
    ,
    ggplot(data %>% filter(the.camera.megapixels < 40), aes(x = the.release.date, y = the.camera.megapixels)) + 
      geom_jitter(color = "grey") + geom_smooth() + 
      labs(x = "release date", y = "Camera (megapixels)", title = " ",
           subtitle = "by camera") +
      theme_ipsum_tw()
    ,
    ggplot(data %>% filter(the.ram.gb < 9), aes(x = the.release.date, y = the.ram.gb)) + 
      geom_jitter(color = "grey") + geom_smooth() + 
      labs(x = "release date", y = "System Memory - RAM (GB)", title = " ",
           subtitle = "by system memory") +
      theme_ipsum_tw()
    ,
    ncol = 2
  )
}

plotChips <- function(){
  library(treemap)
  data %>% 
    select("id","the.systemchip.brand") %>%
    na.omit() %>% 
    count(the.systemchip.brand, sort = T) %>%
    mutate(share = n / sum(n)) %>%
    treemap(index = "the.systemchip.brand",
            vSize = "share",
            vColor = "share",
            title = "Market Share of System Chipset by Brand",
            type = "value",
            palette = "GnBu",
            fontfamily.title = "Titillium Web",
            fontsize.title = 18)
}


# processed data ----------------------------------------------------------

weightNcamera <- function(){
  data$the.weight.gram <- data$Weight %>% str_extract("[0-9]{2,4} g") %>% str_extract("[0-9]{2,4}") %>% as.numeric()
  data$the.camera.megapixels <- data$Camera %>% str_extract("[0-9]*\\.?[0-9]*") %>% as.numeric()
  data$the.ram.gb <- data$System.memory %>% str_extract("[0-9]*\\.?[0-9]* GB RAM") %>% str_extract("[0-9]*\\.?[0-9]*") %>% as.numeric()
  data$the.standby.hours <- data$Stand.by.time %>% str_extract("[0-9]*\\.?[0-9]* hours") %>% str_extract("[0-9]*\\.?[0-9]*") %>% as.numeric()
  saveRDS(data, "dataOK.rds")
}

cleanSystemChip <- function(){
  data$the.systemchip.brand <- data$System.chip %>% word()
  data$the.systemchip.brand[which(data$the.systemchip.brand == "")] <- NA
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Broadcomm")] <- "Broadcom"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Qulacomm")] <- "Qualcomm"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Xiaolong")] <- "Qualcomm"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "RDA8810")] <- "RDA"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "RDA8810PL")] <- "RDA"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "MT6276a")] <- "MediaTek"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Hi-Silicon")] <- "HiSilicon"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Nvidia")] <- "NVIDIA"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "STE")] <- "ST-Ericsson"
  # factorize
  data$the.systemchip.brand <- as.factor(data$the.systemchip.brand)
  # see it
  data$the.systemchip.brand %>% unique() %>% sort()
}

sceenSpecs <- function(){
  
  # pixels and groups
  value <- str_extract_all(data$Resolution, "[0-9]{1,4}", simplify = T)
  data$the.total.pixels <- as.numeric(value[,1]) * as.numeric( value[,2])
  grp <- c("> 4 million pixels", "1.5 - 4 million pixels" , "0.8 - 1.5 million pixels" , "0.3 - 0.8 million pixels" , "< 0.3 million pixels" )
  grp <- factor(grp, ordered = T, levels = grp[5:1])
  data$the.total.pixels.group <- NA
  data$the.total.pixels.group[between(data$the.total.pixels, 4e6, Inf)] <- grp[1]
  data$the.total.pixels.group[between(data$the.total.pixels, 1.5e6, 4e6)] <- grp[2] 
  data$the.total.pixels.group[between(data$the.total.pixels, 8e5, 1.5e6)] <- grp[3]
  data$the.total.pixels.group[between(data$the.total.pixels, 3e5, 8e5)] <- grp[4]
  data$the.total.pixels.group[between(data$the.total.pixels, 0, 3e5)] <- grp[5]
  remove(value, grp)
  
  # screen to body ratio
  data$the.screen2body.percent <- data$Screen.to.body.ratio %>% parse_number()
}

releaseDateNbrand <- function(){
  data$Release.date[2825] <- "Jun  6, 2011" #correct erros
  data$the.release.date <- data$Release.date %>%parse_date("%b %d, %Y")
  data$the.brand <- data$phoneName %>% word(1,1)
}

saveDatasets <- function(){
  saveRDS(data, file = "dataOK.rds")     # full set
  library(DBI); library(odbc)
  con <- dbConnect(odbc::odbc(), "Study Database")
  dbWriteTable(con, "mobile", data, overwrite = T)
}

