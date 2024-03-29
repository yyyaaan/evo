library(tidyverse); library(hrbrthemes); library(gridExtra); 
library(scales); library(visNetwork)
options(stringsAsFactors = F)
data <- readRDS("dataOK.rds")
# myplot <- function(vector) plot_ly(data) %>% add_markers(x = 1:length(vector), y = vector)
# ok plot are masked in function
# RColorBrewer::display.brewer.all()



# verbatim output ---------------------------------------------------------

printCorr <- function(){
  df.new <- data[,c(162,165,168:171)]
  names(df.new)
  names(df.new) <- c("var1", "var2", "var3", "var4", "var5", "var6")
  cor(df.new, use = "pairwise.complete.obs", method = "spearman") %>% 
    xtable::xtable(digits = 4)
}

plotNrintNormal <- function(){
  par(mfrow = c(4,2))
  qqnorm(rnorm(8020), main = "Theoretical normal distribution")
  abline(0,1, col = "red")
  qqnorm(scale(data$the.total.pixels), main = "Normalized Camera Resolution")
  abline(0,1, col = "red")
  qqnorm(scale(data$the.camera.megapixels), main = "Normalized Camera Resolution")
  abline(0,1, col = "red")
  qqnorm(scale(data$the.ram.gb), main = "Normalized RAM")
  abline(0,1, col = "red")
  qqnorm(scale(data$the.standby.hours), main = "Normalized Stand-by Hours")
  abline(0,1, col = "red")
  qqnorm(scale(data$the.weight.gram), main = "Normalized Weight")
  abline(0,1, col = "red")
  qqnorm(scale(data$the.screen2body.percent), main = "Normalized Screen-to-Body Ratio")
  abline(0,1, col = "red")
  
  ks.test(scale(data$the.camera.megapixels), pnorm)
  ks.test(scale(data$the.ram.gb), pnorm)
  ks.test(scale(data$the.standby.hours), pnorm)
  ks.test(scale(data$the.weight.gram), pnorm)
  ks.test(scale(data$the.screen2body.percent), pnorm)
  ks.test(scale(data$the.total.pixels), pnorm)
}

printSummary <- function(){
  summary(data[,c(162,165,168:171)])
}

printInfo <- function(){
  df <- data[,162:171]
  df.new <- data.frame(description = "integer",
                       variable.name = colnames(df),
                       available.cases = 0,
                       percent = 0,
                       explanation = "hello")
  
  for (i in 1:ncol(df)) {
    n <- sum(complete.cases(df[,i]))
    df.new$available.cases[i] <- n 
    df.new$percent[i] <- n / nrow(df)
  }
  
  xtable::xtable(df.new)
  
}



# Network Graphs ----------------------------------------------------------

plotFeedbackLoop <- function(){
  label <- c("Feedback Loop", "Diffusion of Technology", "Adaptation of Agents", 
             "Evolution of Networks/Clusters", "Co-evolution of Industry")
  nodes <- data.frame(id = 1:length(label),
                      label = label,
                      shape = c("star",rep("box",4)))
  edges <- data.frame(from = c(1,1,1,1,2,3,4),
                      to   = c(2,3,4,5,3,4,5),
                      shape = "text")
  visNetwork(nodes, edges)
}

plotNetwork <- function(){
  nodes <- data$the.systemchip.brand %>% as.character() %>%
    na.omit() %>%
    unique() %>%
    c(unique(data$the.brand))
  nodes <- data.frame(id = 1:length(nodes), 
                      label = nodes,
                      shape = "dot")
  N <- 660
  edges <- data.frame(from = ceiling(runif(N) * 145),
                      to = ceiling(runif(N) * 145))
  visNetwork(nodes, edges)
}


# Evolution Pattern with out labels ---------------------------------------


plotEvoPattern <- function(){
  ggplot(data , aes(x = the.release.date, y = the.total.pixels)) + 
    geom_point() +
    labs(x = "release date", y = "screen resolutions - total pixels", 
         title = "Evolution evidence for mobile phones", subtitle = "measured by screen resolutions / non-transformed data") +
    scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
    theme_ipsum_tw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


# 2 evolution trend -------------------------------------------------------


plotTrendA <- function(){
  grid.arrange(
    ggplot(data , aes(x = the.release.date, y = the.total.pixels.group)) + 
      geom_jitter(shape = 42) + geom_smooth() + 
      labs(x = "release date", y = "screen resolutions category", 
           title = "Evolution of mobile phone hardware (group 1)", subtitle = "measured by screen resolutions") +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      theme_ipsum_tw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    ,
    ggplot(data, aes(x = the.release.date, y = the.screen2body.percent)) + 
      geom_jitter(shape = 42) + geom_smooth() + 
      labs(x = "release date", y = "screen-to-body ratio", title = " ",
           subtitle = "by screen-to-body ratios") +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      theme_ipsum_tw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    ,
    ggplot(data %>% filter(the.camera.megapixels < 40), aes(x = the.release.date, y = the.camera.megapixels)) + 
      geom_jitter(shape = 42) + geom_smooth() + 
      labs(x = "release date", y = "Camera (megapixels)", title = " ",
           subtitle = "by camera") +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      theme_ipsum_tw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    ,
    ggplot(data %>% filter(the.ram.gb < 9), aes(x = the.release.date, y = the.ram.gb)) + 
      geom_jitter(shape = 42) + geom_smooth() + 
      labs(x = "release date", y = "System Memory - RAM (GB)", title = " ",
           subtitle = "by system memory") +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      theme_ipsum_tw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    ,
    ncol = 2
  )
}

plotTrendB <- function(){
  grid.arrange(
    ggplot(data %>% filter(the.standby.hours < 1600), aes(x = the.release.date, y = the.standby.hours)) + 
      geom_jitter(shape = 42) + geom_smooth() + 
      labs(x = "release date", y = "standby time (hours)",
           title = "Evolution of mobile phone hardware (group 2)",
           subtitle = "by standby time") +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      theme_ipsum_tw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    ,
    ggplot(data %>% filter(the.weight.gram < 1000), aes(x = the.release.date, y = the.weight.gram)) + 
      geom_jitter(shape = 42) + geom_smooth() + 
      labs(x = "release date", y = "phone weight (gram)", title = " ",
           subtitle = "by weight") +
      scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
      theme_ipsum_tw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
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
            title = "",
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
  data$the.brand <- data$phoneName %>% word(1,1)
  data$Release.date[2825] <- "Jun  6, 2011" #correct erros
  data$the.release.date <- data$Release.date %>%parse_date("%b %d, %Y")
  x <- data$Announce %>% parse_date("%b %d, %Y")
  data$the.release.date[!is.na(x)] <- x[!is.na(x)]
  data$the.release.year <- data$the.release.date %>% format("%Y") %>% as.numeric()
}

saveDatasets <- function(){
  saveRDS(data, file = "dataOK.rds")     # full set
  library(DBI); library(odbc)
  con <- dbConnect(odbc::odbc(), "Study Database")
  dbWriteTable(con, "mobile", data, overwrite = T)
}

