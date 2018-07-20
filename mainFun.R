library(tidyverse)
library(rvest)
options(stringsAsFactors = FALSE)

myError <- function(string){
  cat("\n\n",string,file="log.txt",append=TRUE)
}

findSpecs <- function(theURL){
  library(rvest)
  options(stringsAsFactors = FALSE)
  
  idstr <- substr(theURL, nchar(theURL)-4, nchar(theURL))
  theURL <- paste0(theURL, "/fullspecs")
  page <- read_html(theURL)
  
  ### marketing info
  phoneName <- page %>% html_node(xpath = "//h1//span") %>% html_text()
  meta.value <- page %>% html_node(".metainfo") %>% html_text()
  meta.value <- gsub("   ","", meta.value)
  dfKey <- "meta"
  dfVal <- meta.value
  
    #L1 entrie
  keys <- page %>% html_nodes(xpath = '//div[@class="s_specs_box s_box_4"]/ul/li') 

    #work on each entry
  for (i in 1:length(keys)) {

      # the level-1 key always exist
    curKey <- keys[[i]] %>% html_node("strong") %>% html_text()
      # is there level-2 under?
    xml.L2 <- keys[[i]] %>% html_nodes(xpath = './ul/li[@class=" s_lv_2  clear clearfix"]')
    
    if(length(xml.L2)){ # level 2 exisits
      
          # first, see is the Level 1 value exist
      curVal <- keys[[i]] %>% html_nodes(xpath = './ul/li[@class=" s_lv_2  "]') %>% html_text()
      if(length(curVal)) {
          dfKey <- c(dfKey, curKey)
          dfVal <- c(dfVal, curVal)
      }
      
        # then, get the Level 2 key-value in vector
      curKey2v <- xml.L2 %>% html_nodes("strong") %>% html_text()
      curVal2v <- xml.L2 %>% html_nodes("ul") %>% html_text()
      
      if (length(curKey2v) > length(curVal2v)){
        valNA <- rep("NA", length(curKey2v) - length(curVal2v))
        curVal2v <- c(curVal2v, valNA)
        myError(paste('add dim for Val', curKey, theURL, sep = " ||| "))
      }
      if (length(curKey2v) < length(curVal2v)){
        valNA <- rep("unknown", length(curVal2v) - length(curKey2v))
        curKey2v <- c(curKey2v, valNA)
        myError(paste('add dim for Key', curKey, theURL, sep = " ||| "))
      }
      
      curKey2v <- paste(curKey, curKey2v, sep = ' || ')
      dfKey <- c(dfKey, curKey2v)
      dfVal <- c(dfVal, curVal2v)
      
    } else { # no level 2, then the value must exist

      curVal <- keys[[i]] %>% html_node("ul") %>% html_text()
      dfKey <- c(dfKey, curKey)
      dfVal <- c(dfVal, curVal)
    }
  }
  
  data.frame(id = idstr,
             phoneName = phoneName,
             key = dfKey,
             value = dfVal)
}

findPhonesInBrand <- function(theURL){
  
  page <- read_html(theURL)
  
  # get phones on the given page
  this.phones <- findPhones(page)
  
  # find how many pages
  lastPage <- page %>% 
    html_node(".s_pager") %>% 
    html_nodes(".s_last") %>% 
    html_node("a") %>% 
    html_attr("onclick")
  
  if(length(lastPage)){
    lastPage <- sub("changePage\\(", "", lastPage)
    lastPage <- sub(", this.href\\); return false", "", lastPage)
  } else {
    return(list(df = this.phones,
                lastPage = 1))
  }
  
  # if more than 1, continue
  for (j in 2:lastPage) {
    page <- read_html(paste0(theURL, "/page/",j))
    df.new <- findPhones(page)
    this.phones <- rbind(this.phones, df.new)
  }
  
  return(list(df = this.phones,
              lastPage = lastPage))}

findPhones <- function(page){
  xml.phones <-  page %>% html_nodes(".s_block_4")
  if(length(xml.phones) == 0) {
    return(data.frame(phoneName = character(),
                      phoneURL = character(),
                      phonePic = character(),
                      brandName = character()))
  }
  data.frame(phoneName = xml.phones %>% html_node("img") %>% html_attr("alt"),
             phoneURL = xml.phones %>% html_node(".s_thumb") %>% html_attr("href"),
             phonePic = xml.phones %>% html_node("img") %>% html_attr("src"),
             brandName = "")  
}

getMetaInfo <- function(){
  meta <- df.specs %>% filter(key == 'meta')
  
  df.meta <- meta[1,]
  df.meta[1,] <- c("", "", "", "")
  for (cur in 1:nrow(meta)) {
    temp <- meta$value[cur]
    
    df.temp <- temp %>% 
      str_split('\r\n', simplify = T) %>% 
      str_split(":", simplify = T) %>%
      as.data.frame()
    
    df.temp <- data.frame(id = meta$id[cur],
                          phoneName = meta$phoneName[cur],
                          key = df.temp$V1,
                          value = df.temp$V2)
    
    df.meta <- rbind(df.meta, df.temp)
  }
  
  empty <- df.meta$key %>% str_detect("[a-z]")
  df.meta <- df.meta[empty,]
  remove(cur, empty, temp, df.temp, meta)
 
    #meta data sorted, and save separately
  saveRDS(df.meta, "df.meta.rds")
  
  df <- rbind(df.meta, df.specs) %>% filter(key != 'meta')
  saveRDS(df, "data.rds")
  
}
