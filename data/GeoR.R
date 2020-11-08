library(sjlabelled)
library(foreign)
library(haven)
library(tidyverse)
library(Hmisc)
library(MASS)
library(ordinal)
library(sf)
library(ggmap)
library(httr)
library(stringdist)


gadmcodes <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV",
               "DJI", "EGY", "ERI", "ESH", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", 
               "LBR", "LSO", "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", "NGA", 
               "REU", "RWA", "SDN", "SEN", "SLE", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TUN", 
               "TZA", "UGA", "ZAF", "ZMB", "ZWE", "DZA") 
Geodata <- matrix(NA, ncol = 11,nrow = 1)
Geodata <- as.data.frame(Geodata)
for (i in 1:54) {
  rdsname <- paste("D:/research/state building/geodata/gadm36_", gadmcodes[i], "_1_sf.rds",sep = "")
  rds <- readRDS(rdsname)
  colnames(Geodata) <- colnames(rds)
  Geodata <- rbind(Geodata,rds)
}
Geodata <- Geodata[-1,]
Geonames <- data.frame(country.g = Geodata$NAME_0,provinces = Geodata$NAME_1, matched = 1, converted_names = Geodata$NAME_1)
Geonames$converted_names <- gsub("è", "e", Geonames$converted_names)
Geonames$converted_names <- gsub("é", "e", Geonames$converted_names)
Geonames$converted_names <- gsub("í", "i", Geonames$converted_names)
Geonames$converted_names <- gsub("ï", "i", Geonames$converted_names)
Geonames$converted_names <- gsub("ô", "o", Geonames$converted_names)
Geonames$converted_names <- gsub("ê", "e", Geonames$converted_names)
Geonames$converted_names <- gsub("É", "E", Geonames$converted_names)
Geonames$converted_names <- gsub("ã", "a", Geonames$converted_names)
Geonames$converted_names <- gsub("`i", "i", Geonames$converted_names)
Geonames$mergednames <- paste(Geonames$country,"_", Geonames$converted_names, sep = "")
NoGeometry <- Geodata[,1:4]



afrobrmt2 <- read_sav("D:/research/state building/data/afro/merged_r2_data.sav")
afrobrmt3 <- read_sav("D:/research/state building/data/afro/merged_r3_data.sav")
afrobrmt4 <- read_sav("D:/research/state building/data/afro/merged_r4_data.sav")
afrobrmt5 <- read_sav("D:/research/state building/data/afro/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav")
afrobrmt6 <- read_sav("D:/research/state building/data/afro/merged_r6_data_2016_36countries2.sav")
afrobrmt7 <- read_sav("D:/research/state building/data/afro/r7_merged_data_34ctry.release.sav")
w3locations <- data.frame(wave = 3, country = afrobrmt3$country%>%as.numeric(), province = afrobrmt3$region%>%as.numeric())
w4locations <- data.frame(wave = 4, country = afrobrmt4$COUNTRY%>%as.numeric(), province = afrobrmt4$REGION%>%as.numeric())
w5locations <- data.frame(wave = 5, country = afrobrmt5$COUNTRY%>%as.numeric(), province = afrobrmt5$REGION%>%as.numeric())
w6locations <- data.frame(wave = 6, country = afrobrmt6$COUNTRY%>%as.numeric(), province = afrobrmt6$REGION%>%as.numeric())
w7locations <- data.frame(wave = 7, country = afrobrmt7$COUNTRY%>%as.numeric(), province = afrobrmt7$REGION%>%as.numeric())
w3plabels <- data.frame(code =  attr(afrobrmt3$region,"labels"), province = attr(afrobrmt3$region,"labels")%>%names())
w4plabels <- data.frame(code =  attr(afrobrmt4$REGION,"labels"), province = attr(afrobrmt4$REGION,"labels")%>%names())
w5plabels <- data.frame(code =  attr(afrobrmt5$REGION,"labels"), province = attr(afrobrmt5$REGION,"labels")%>%names())
w6plabels <- data.frame(code =  attr(afrobrmt6$REGION,"labels"), province = attr(afrobrmt6$REGION,"labels")%>%names())
w7plabels <- data.frame(code =  attr(afrobrmt7$REGION,"labels"), province = attr(afrobrmt7$REGION,"labels")%>%names())
w3clabels <- data.frame(code =  attr(afrobrmt3$country,"labels"), country = attr(afrobrmt3$country,"labels")%>%names())
w4clabels <- data.frame(code =  attr(afrobrmt4$COUNTRY,"labels"), country = attr(afrobrmt4$COUNTRY,"labels")%>%names())
w5clabels <- data.frame(code =  attr(afrobrmt5$COUNTRY,"labels"), country = attr(afrobrmt5$COUNTRY,"labels")%>%names())
w6clabels <- data.frame(code =  attr(afrobrmt6$COUNTRY,"labels"), country = attr(afrobrmt6$COUNTRY,"labels")%>%names())
w7clabels <- data.frame(code =  attr(afrobrmt7$COUNTRY,"labels"), country = attr(afrobrmt7$COUNTRY,"labels")%>%names())
w3locations <- merge(w3locations,w3plabels,by.x = "province", by.y = "code") %>% merge(w3clabels,by.x = "country", by.y = "code")
w4locations <- merge(w4locations,w4plabels,by.x = "province", by.y = "code") %>% merge(w4clabels,by.x = "country", by.y = "code")
w5locations <- merge(w5locations,w5plabels,by.x = "province", by.y = "code") %>% merge(w5clabels,by.x = "country", by.y = "code")
w6locations <- merge(w6locations,w6plabels,by.x = "province", by.y = "code") %>% merge(w6clabels,by.x = "country", by.y = "code")
w7locations <- merge(w7locations,w7plabels,by.x = "province", by.y = "code") %>% merge(w7clabels,by.x = "country", by.y = "code")

locations <- rbind(w3locations, w4locations,w5locations,w6locations,w7locations)
locations <- locations[!duplicated(locations),]

countrylist1 <- unique(NoGeometry$NAME_0)%>%as.data.frame()
countrylist1$matched <-1
countrylist2 <- unique(locations$country.y)%>%as.data.frame()
countrylist2$matched <- 2
countrylist <- merge(countrylist1, countrylist2, by.x = ".", by.y = ".",all.x = T, all.y = T)
nametrans <- data.frame(AFVname=c("Cabo Verde", "Swaziland"), Tname = c("Cape Verde","eSwatini"))
locations$country.y <- gsub("Cabo Verde","Cape Verde", locations$country.y)
locations$country.y <- gsub("eSwatini","Swaziland", locations$country.y)
locations$country.y <- gsub("Cote d’Ivoire","Côte d'Ivoire", locations$country.y)
locations$country.y <- gsub("Cote d'Ivoire","Côte d'Ivoire", locations$country.y)
locations$mergednames <- paste(locations$country.y,"_", locations$province.y,sep = "")
locations$matchednames <- 0



nameconversion <- data.frame(country = NA, afro=NA, gadm=NA, distance = NA)
m <- 1
for (i in 1:length(unique(locations$country.y))) {
  country.match <- unique(locations$country.y)[i]
  rows.afro <- which(locations$country.y == country.match)
  rows.gadm <- which(Geonames$country.g == country.match)
  onecountry <- locations[rows.afro,]
  namedist <- stringdistmatrix(locations[rows.afro,]$province.y, Geonames[rows.gadm,]$provinces)
  bestnames <- apply(namedist, 1, which.min)
  mindist <- apply(namedist, 1, min)
  for (j in 1:length(rows.afro)) {
  if (mindist[j] > 2) {
    print(paste(country.match,"distance:", mindist[j],locations$province.y[rows.afro[j]], 
                "to", Geonames$provinces[rows.gadm[bestnames[j]]], sep = " "))
  }
    onerow <- c(country.match,locations$province.y[rows.afro[j]],Geonames$provinces[rows.gadm[bestnames[j]]],mindist[j])
    nameconversion <- rbind(nameconversion, onerow)
    

  }
    
}
write.csv(nameconversion,"D:/research/state building/data/afro/namecoversion.csv")
write.csv(Geonames,"D:/research/state building/data/afro/gadmnames.csv")




