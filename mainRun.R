source("mainFun.R")
options(stringsAsFactors = FALSE)
leading <- "https://www.phonearena.com"

# Brands ------------------------------------------------------------------


xml.brands <- read_html("https://www.phonearena.com/phones/manufacturers") %>%
  html_nodes(".s_block_4") %>%
  html_nodes(".ahover")

df.brands <- data.frame(brandName = xml.brands %>% html_text(),
                        brandURL = xml.brands %>% html_attr("href"),
                        brandPages = 1,
                        brandPhones = 1)

remove("xml.brands")

# Phones ------------------------------------------------------------------

  # init with the first brand
print("Task start: getting list phone list")
df.phones <- findPhonesInBrand(paste0(leading, df.brands$brandURL[1]))$df
df.phones$brandName <- df.brands$brandName[1]

  # run all brands
for (i in 2:nrow(df.brands)) {
  temp <- findPhonesInBrand(paste0(leading, df.brands$brandURL[i]))
  
    # configure brands
  df.brands$brandPages[i] <- temp$lastPage
  df.brands$brandPhones[i] <- nrow(temp$df)
    # correct brand name
  if(nrow(temp$df)) temp$df$brandName <- df.brands$brandName[i]
    # save phones
  df.phones <- rbind(df.phones, temp$df)
    # system info
  remove("temp")
  print(paste("Success:", i, "/", nrow(df.brands), df.brands$brandName[i]))
}

save(df.brands, df.phones, file = paste0("z_phones_",Sys.Date(),".rdata"))


# specs -------------------------------------------------------------------

print("Task start: get specifications of phones")
  # init with first phone
df.specs <- findSpecs(paste0(leading,df.phones$phoneURL[1]))
  # now loop for all
maxN <- nrow(df.phones)
for (i in 1:maxN) {
  df.this <- findSpecs(paste0(leading,df.phones$phoneURL[i]))
  print(paste("Success", i, "/", maxN, df.phones$phoneName[i]))
  df.specs <- rbind(df.specs, df.this)
}

print("All ok, saving")
save(df.specs, file = paste0("z_specsL_",Sys.Date(),".rdata"))
save.image(file = paste0("z_workspace_",Sys.Date(),".rdata")) #duplicate backup


# getMeta -----------------------------------------------------------------

getMetaInfo()  #starting with df.specs

