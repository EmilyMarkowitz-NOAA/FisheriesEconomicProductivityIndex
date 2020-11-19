
#####LOAD CRAN LIBRARIES#######

options(java.parameters = "-Xmx1000m")
options(scipen=10000)

ln<-log #tricky, tricky, Base R! Didn't fool me this time!!!

#########***########

####COLOR PALLET#####
#mostly for figures
NOAALightBlue<-"#C9E1E6"
NOAADarkBlue<-"#0098A6"
NOAADarkGrey<-"#56575A" #text
NOAABlueScale<-colorRampPalette(colors = c(NOAALightBlue, NOAADarkBlue))

#########***########
######DEALING WITH TOP 10 KEY SPECIES########

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

tolower2 <- function(str0, capitalizefirst=F) {
  str2<-c()
  
  if (str0[1] %in% "") { 
    str<-""
  } else {
    for (i in 1:length(str0)) {
      str1<-gsub(pattern = "\\(", replacement = "\\( ", x = tolower(str0[i]))
      str1<-gsub(pattern = "\\)", replacement = " \\)", x = str1)
      str1<-strsplit(x = str1, split = " ")[[1]]
      
      keywords <- c(
        #State
        "Alabama", "Alaska", "California", "Connecticut", 
        "Delaware", "East Florida", "West Florida", "Florida", "Georgia", 
        "Louisiana", "Maine", "Maryland", "Massachusetts", 
        "Mississippi", "New Hampshire", "New Jersey", "New York", 
        "North Carolina", "Oregon", "Rhode Island", "South Carolina", 
        "Texas",  "Virginia", "Washington", 
        #Region
        "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "Western Pacific",
        "New England",
        "Mid-Atlantic","Gulf of Mexico",
        "South Atlantic", 
        #For specific Species
        "Spanish", "Gulf", "Bringham's", "Von Siebold's", "Pfluger's", "African", "Eurpoean",
        # Other
        "Atlantic", "American", 
        "Atka", "Chinook", "Great Lakes") 
      
      # keywords<-c(keywords, paste0("(", keywords), paste0(keywords, ")"))
      
      
      for (ii in 1:length(keywords)) {
        keywords1<-strsplit(x = keywords[ii], split = " ")[[1]]
        if (length(keywords1) %in% 1 & 
            sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T))>0) {
          str1[grep(x = str1, pattern = keywords[ii], ignore.case = T)]<-keywords[ii]
        } else if (length(keywords1) %in% 2 & 
                   sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T)>0) & 
                   sum(grepl(x = str0, pattern = keywords1[2], ignore.case = T)>0)) {
          str1[grep(x = str1, pattern = keywords1[1], ignore.case = T)]<-keywords1[1] 
          str1[grep(x = str1, pattern = keywords1[2], ignore.case = T)]<-keywords1[2] 
        } else if (length(keywords1) %in% 3 & 
                   grepl(x = str0, pattern = keywords1[1], ignore.case = T) & 
                   grepl(x = str0, pattern = keywords1[2], ignore.case = T) &
                   grepl(x = str0, pattern = keywords1[3], ignore.case = T)) {
          str1[sum(grep(x = str1, pattern = keywords1[1], ignore.case = T)>0)]<-keywords1[1] 
          str1[sum(grep(x = str1, pattern = keywords1[2], ignore.case = T)>0)]<-keywords1[2] 
          str1[sum(grep(x = str1, pattern = keywords1[3], ignore.case = T)>0)]<-keywords1[3] 
        }     
      }
      
      str1<-paste(str1, collapse = " ")
      str1<-gsub(pattern = "\\( ", replacement = "\\(", x = str1)
      str1<-gsub(pattern = " \\)", replacement = "\\)", x = str1)
      if (capitalizefirst==T) {
        str1<-paste(toupper(substr(str1, 1, 1)), substr(str1, 2, nchar(str1)), sep="")
        
      }
      
      str1<-gsub(pattern = "&", replacement = "and", x = str1)
      
      str2<-c(str2, str1)
    }
    str2<-trimws(str2)
  }
  return(str2)
}


itis_reclassify<-function(tsn, categories, missing.name){
  
  # Find which codes are in which categories
  tsn0<-as.numeric(tsn)[!(is.na(tsn))]
  tsn.indata<-classification(sci_id = tsn0, db = 'itis')
  tsn.indata<-tsn.indata[!(names(tsn.indata) %in% 0)]
  valid0<- sciname<-category0<-bottomrank<-sppname<- TSN<-c() 
  
  TSN<-c()
  bottomrank<-c()
  category0<-c()
  sciname<-c()
  valid0<-c()
  
  
  for (i in 1:length(categories)) {

    a<-list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]] %in% . )
    
    # for (ii in 1:length(categories[i][[1]])) {
      # a<-c(a, list.search(lapply(X = tsn.indata, '[', 3), categories[i][[1]][[ii]] %in% . ))
    # }
    
    if (length(a)!=0) {
      
      sppcode<-names(a)
      sppcode<-gsub(pattern = "[a-zA-Z]+", replacement = "", x = sppcode)
      sppcode<-gsub(pattern = "\\.", replacement = "", x = sppcode)
      
      for (ii in 1:length(sppcode)) {
        TSN<-c(TSN, sppcode[ii])
        
        bottomrank<-c(bottomrank, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$rank[
          nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
        
        category0<-c(category0, names(categories[i]))  
        
        sciname<-c(sciname, tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]]$name[
          nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])])
        
        valid0<-c(valid0, 
                  ifelse(nrow(tsn.indata[names(tsn.indata) %in% sppcode[ii]][[1]])>1, 
                         "valid", "invalid"))
      }
    }
  }
  
  df.out<-data.frame(TSN = TSN, 
                     category = category0, 
                     valid = valid0, 
                     rank = bottomrank, 
                     sciname = sciname )
  
  return(list("df.out" = df.out, 
              "tsn.indata" = tsn.indata))
}


spp_reclassify<-function(landings.df, spcat.list, place){
  
  ####
  ####FINFISH AND SHELFISH AND OTHER TOTALS
  ###
  
  temp<-landings.df
  total.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                      by=list(year=temp$year),
                      FUN=sum,na.rm=TRUE)
  
  #Finfish
  temp<-landings.df[landings.df$OFS %in% "F",]
  if (nrow(temp)!=0) {
    finfish.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                          by=list(year=temp$year),
                          FUN=sum, na.rm=TRUE)
    
    if (sum(!(min(landings.df$year):max(landings.df$year) %in% finfish.df$year)) >0){
      
      finfish.df<-rbind.data.frame(finfish.df, 
                                   cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
                                                    "POUNDS" = 0, 
                                                    "DOLLARS" = 0))
      landings.df<-landings.df[order(landings.df$year),]
      
    }
  } else {
    finfish.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
                           POUNDS = 0,
                           DOLLARS = 0)
  }
  
  #Shellfish
  temp<-landings.df[landings.df$OFS %in% "S",]
  if (nrow(temp)!=0) {
    shellfish.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                            by=list(year=temp$year),
                            FUN=sum,na.rm=TRUE)
    
    if (sum(!(min(landings.df$year):max(landings.df$year) %in% shellfish.df$year)) >0){
      
      shellfish.df<-rbind.data.frame(shellfish.df, 
                                     cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
                                                      "POUNDS" = 0, 
                                                      "DOLLARS" = 0))
      shellfish.df<-shellfish.df[order(shellfish.df$year),]
    }
  } else {
    shellfish.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
                             POUNDS = 0,
                             DOLLARS = 0)
  }
  
  
  #Other
  temp<-landings.df[landings.df$OFS %in% "O",]
  if (nrow(temp)!=0) {
    other.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] ,
                        by=list(year=temp$year),
                        FUN=sum,na.rm=TRUE)
    
    if (sum(!(min(landings.df$year):max(landings.df$year) %in% other.df$year)) > 0){
      
      other.df<-rbind.data.frame(other.df, 
                                 cbind.data.frame("year" = (min(landings.df$year):max(landings.df$year))[
                                   !(min(landings.df$year):max(landings.df$year) %in% other.df$year)], 
                                   "POUNDS" = 0, 
                                   "DOLLARS" = 0))
      other.df<-other.df[order(other.df$year),]
    }
  } else {
    other.df<-data.frame(year = min(landings.df$year):max(landings.df$year),
                         POUNDS = 0,
                         DOLLARS = 0)
  }
  
  
  land.df<-land.tot<-cbind.data.frame("Total" = total.df$POUNDS,
                                      "Finfish" = finfish.df$POUNDS,
                                      "Shellfish" = shellfish.df$POUNDS,
                                      "Other" = other.df$POUNDS,
                                      "Key Species" = NA)
  rev.df<-rev.tot<-cbind.data.frame("Total" = total.df$DOLLARS,
                                    "Finfish" = finfish.df$DOLLARS,
                                    'Shellfish' = shellfish.df$DOLLARS,
                                    "Other" = other.df$DOLLARS,
                                    "Key Species" = NA)
  price.df<-price.tot<-cbind.data.frame("Key Species" = rep_len(x = NA, length.out = nrow(rev.df)))
  
  
  ####
  ####SPECIES
  ###
  uniquespp<-data.frame()
  
  
  if (!(is.na(spcat.list))) {
    temp<-itis_reclassify(tsn = as.numeric(paste(unique(landings.df$TSN))), 
                          categories = spcat.list$Areas[place][[1]], 
                          missing.name="Uncategorized")
    
    tsn.id<-temp[1][[1]]
    # tsn.id<-tsn.id[!(tsn.id$category %in% c("Other", "Uncategorized")), 
    #                c("TSN", "category")]
    tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
    
    
    land.df<-data.frame(t(land.df))
    rev.df<-data.frame(t(rev.df))
    price.df<-data.frame(t(price.df))
    
    
    #Add species to tables
    land.df$Footnotes<-rev.df$Footnotes<-price.df$Footnotes<-""
    
    for (i in 1:length(names(spcat.list$Areas[place][[1]]))) {
      
      spp0<-sort(names(spcat.list$Areas[place][[1]]))[i]
      
      temp<-landings.df[landings.df$TSN %in% tsn.id$TSN[tsn.id$category %in% spp0],]
      # temp<-temp[!(is.na(temp$POUNDS) & is.na(temp$DOLLARS)), ]
      
      #FOOTNOTES
      Footnotes<-""
      
      #Species that should be included in this sum and footnote
      sppno<-as.numeric(unlist(spcat.list$Areas[place][[1]][spp0]))
      spp<-tsn.indata.foot<-classification(sci_id = sppno[sppno>0], db = 'itis')
      sppnames<-taxspp(spp = tsn.indata.foot, spp0, landings.df)
      species<-sppnames$species
      notspecies<-sppnames$notspecies
      
      spp00<-strsplit(x = spp0, split = ",")
      spp00<-strsplit(x = spp00[[length(spp00)]], split = "and")
      spp00<-gsub(x = unlist(spp00), pattern = "and ", replacement = "", ignore.case = T)
      spp00<-gsub(x = unlist(spp00), pattern = "& ", replacement = "", ignore.case = T)
      spp00<-trimws(spp00)
      
      
      if (!(is.null(species)) && !(tolower(species) %in% tolower(spp00))) {        #If species names the few specific species in the group, then dont footnote
        Footnotes<-""
        #If a specific species (and not a gorup) #No Footnote required
      } else if (length(spp) == 1 & !(spp[[1]]$rank[nrow(spp[[1]])] %in% "species") ) { #If there is only 1 code and its not a specific species
        Footnotes<-paste0("This species group includes species within the ",
                          paste0(ifelse(spp[[1]]$rank[nrow(spp[[1]])] %in% "genus",
                                        paste0("*", (spp[[1]]$name[nrow(spp[[1]])]), "*"),
                                        tolower(spp[[1]]$name[nrow(spp[[1]])])) ,
                                 " ", spp[[1]]$rank[nrow(spp[[1]])]), ".")
      } else if (length(spp) > 1) {
        
        
        if (length(species)>0 & length(notspecies)>0) {
          Footnotes<-paste0("This species group includes species within the ",
                            funct_list(notspecies), " and ", funct_list(species) , ".")
        } else if (length(species)==0 & length(notspecies)>0) {
          Footnotes<-paste0("This species group includes species within the ",
                            funct_list(notspecies), ".")
        } else if (length(species)>0 & length(notspecies)==0) {
          Footnotes<-paste0("This species group includes ",
                            funct_list(species) , ".")
        }
      }
      #Species that should be excluded from this sum and footnote
      if (sum(sppno<0)>0) {
        spp<-tsn.indata.foot<-classification(sci_id = (sppno[sppno<0])*-1, db = 'itis')
        sppnames<-taxspp(spp = tsn.indata.foot, spp0, landings.df)
        species<-sppnames$species
        notspecies<-sppnames$notspecies
        
        if (length(tsn.indata.foot)>1) {
          if (length(species)>0 & length(notspecies)>0) {
            Footnotes<-paste0(Footnotes, " This species group excludes species within the ",
                              funct_list(notspecies), " and, specifically, ", funct_list(species) , ".")
          } else if (length(species)==0 & length(notspecies)>0) {
            Footnotes<-paste0(Footnotes, " This species group excludes species within the ",
                              funct_list(notspecies), ".")
          } else if (length(species)>0 & length(notspecies)==0) {
            Footnotes<-paste0(Footnotes, " This species group excludes ",
                              funct_list(species) , ".")
          }
        }
      }
      
      #COMPILE TABLES
      
      if (nrow(temp) %in% 0) {
        land.df<-rbind.data.frame(land.df, 
                                  c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
        rev.df<-rbind.data.frame(rev.df, 
                                 c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
        price.df<-rbind.data.frame(price.df, 
                                   c(rep_len(x = NA, length.out = (ncol(land.df)-1)), Footnotes))
        
      } else {
        
        #Unique Species
        uniquespp0<-unique(temp[,c("TSN", "CommonName")])
        uniquespp0$Category<-spp0
        uniquespp0$CommonName1<-funct_list(spp00)
        uniquespp<-rbind.data.frame(uniquespp, uniquespp0)  
        
        
        
        temp.df<-aggregate(temp[,names(temp) %in% c("POUNDS", "DOLLARS")] , 
                           by=list(year=temp$year), 
                           FUN=sum,na.rm=TRUE)
        
        #make sure columns match
        if (sum((!(min(landings.df$year):max(landings.df$year) %in% temp.df$year)) >0)) {
          yr0<-(min(landings.df$year):max(landings.df$year))[!(min(landings.df$year):max(landings.df$year) %in% temp.df$year)]
          temp.df<-rbind.data.frame(temp.df, 
                                    cbind.data.frame('year' = yr0, 
                                                     'POUNDS' = rep_len(x = NA, length.out = length(yr0)), 
                                                     'DOLLARS' = rep_len(x = NA, length.out = length(yr0))))
        }
        temp.df<-temp.df[order(temp.df$year, decreasing = F),]
        
        land.df<-rbind.data.frame(land.df, 
                                  c(temp.df$POUNDS, Footnotes))
        rev.df<-rbind.data.frame(rev.df, 
                                 c(temp.df$DOLLARS, Footnotes))
        price.df<-rbind.data.frame(price.df, 
                                   c(temp.df$POUNDS/temp.df$DOLLARS, Footnotes))
        
      }
      
      rownames(land.df)[length(rownames(land.df))]<-
        rownames(rev.df)[length(rownames(rev.df))]<-
        rownames(price.df)[length(rownames(price.df))]<-tolower2(spp0, capitalizefirst = T)  
    }
    
    
    
    uniquespp$Area<-place
    uniquespp$SciName<-""
    a<-classification(sci_id = uniquespp$TSN, db = 'itis')
    for (i in 1:nrow(uniquespp)){
      if (a[i][[1]]$rank[nrow(a[i][[1]])] %in% "species") {
        uniquespp$SciName[i]<-a[i][[1]]$name[nrow(a[i][[1]])]
      } else {
        uniquespp$SciName[i]<-paste0(a[i][[1]]$name[nrow(a[i][[1]])], " ", a[i][[1]]$rank[nrow(a[i][[1]])])
      }
    } 
    
    
    land.df<-cbind.data.frame(rownames(land.df), land.df)
    rev.df<-cbind.data.frame(rownames(rev.df), rev.df)
    price.df<-cbind.data.frame(rownames(price.df), price.df)
    
  } else {
    
    land.df<-t(land.df)
    land.df<-cbind.data.frame("keyspecies" = rownames(land.df)[-nrow(land.df)], 
                              land.df[-nrow(land.df),], 
                              Footnotes = NA)
    rev.df<-t(rev.df)
    rev.df<-cbind.data.frame("keyspecies" = rownames(rev.df)[-nrow(rev.df)], 
                             rev.df[-nrow(rev.df),], 
                             Footnotes = NA)
    price.df<-t(price.df)
    price.df<-cbind.data.frame("keyspecies" = NA, 
                               price.df, 
                               Footnotes = NA)
  }
  
  colnames(land.df)<-colnames(rev.df)<-colnames(price.df)<-c("keyspecies", 
                                                             as.character(min(landings.df$year):
                                                                            max(landings.df$year)), 
                                                             "Footnotes")
  
  #####footnote with species
  return(list("revenue" = rev.df, 
              "landings" = land.df, 
              "price" = price.df, 
              "uniquespp" = uniquespp))
}



######***GENERAL########
SpCodeName.General<-list(
  "Finfish" = 914179, # Infraphylum	Gnathostomata
  "Shellfish" = c(82696, # Phylum	Arthropoda  – Artrópode, arthropodes, arthropods
                  69458), # Phylum	Mollusca  – mollusques, molusco, molluscs, mollusks     
  
  ###
  #Crustaceans
  ###
  'Arthropoda' = 82696, # Phylum	Arthropoda  – Artrópode, arthropodes, arthropods
  'American lobster' = 97314, #Species: Homarus americanus H. Milne Edwards, 1837 – valid
  'Shrimp' = c(96106, # Infraorder	Caridea Dana, 1852
               83688, # Order	Anostraca G. O. Sars, 1867 – brine shrimp, fairy shrimp
               621194, # Superfamily	Callianassoidea Dana, 1852
               97294, #  Infraorder	Stenopodidea Claus, 1872
               95600), # Suborder	Dendrobranchiata Bate, 1888
  'Mantis Shrimp' = 99140, # Order	Stomatopoda Latreille, 1817 – mantis shrimp, mantis shrimps
  'Penaeidae Shirmp' = 95602, # Family	Penaeidae Rafinesque, 1815 – penaeid shrimps, crevettes pénaéïdes
  'Crab'= c(#95599, # Order	Decapoda Latreille, 1802 – crabs, crayfishes, lobsters, prawns, shrimp, crabes, crevettes, écrevisses, homards # Has previously also included all decapods
    98276, #Infraorder	Brachyura Latreille, 1802 – short-tailed crabs, true crabs
    97698), #  Infraorder	Anomura MacLeay, 1838
  'Sea urchins' = 157821, #Class	Echinoidea Leske, 1778 – heart urchins, sand dollars, sea urchins, châtaignes de mer, clypéastres, oursins, bolacha da praia, equinóide, ouriço do mar, heart urchins, sand dollars, sea urchins, urchins
  'Pac Spiny lobster' = 97650, # Species	Panulirus interruptus (J. W. Randall, 1840) – California spiny lobster
  'Crawfish' = 97306, # Superfamily	Astacoidea Latreille, 1802 – crayfishes
  'Lobsters' = c(97660, # Family	Scyllaridae Latreille, 1825 – slipper lobsters
                 97646), # Family	Palinuridae Latreille, 1802 – spiny lobsters
  'Stone crab' = 98810, # Genus	Menippe De Haan, 1833
  'Blue crab' = 98696, # Species	Callinectes sapidus M. J. Rathbun, 1896 – blue crab, bluepoint
  'Caribbean Spiny lobster' = 97648, # Species	Panulirus argus (Latreille, 1804) – Caribbean spiny lobster
  
  ###
  #Other
  ###
  'Bloodworms' = 66107, # Species	Glycera dibranchiata Ehlers, 1868
  
  
  ###
  #Fish
  ###
  'Halibut' = c(172931, # Genus	Hippoglossus Cuvier, 1816 – halibuts
                172929), #  Genus	Reinhardtius Gill, 1861
  'Pacific halibut' = 172932, #Species: Hippoglossus stenolepis Schmidt, 1904 – valid
  'Flatfish' = c(172707), #Suborder	Pleuronectoidei
  'Summer flounder' = 172735, #Species	Paralichthys dentatus (Linnaeus, 1766) – summer flounder, fluke, cardeau d'été, Summer Flounder
  'Flounders' = c(553179, #  Family	Paralichthyidae  – flétans de sable, lenguados areneros, sand flounders, lefteye flounders 
                  172714, # Family	Bothidae  – lefteyed flounders, lefteye flounders, lenguados chuecos, turbots flounders
                  172859), # Family	Pleuronectidae  – halibuts, dabs, righteyed flounders, platijas, plies, righteye flounders    
  
  'Menhaden' = 161731, #Genus	Brevoortia Gill, 1861 – menhadens
  'Herring' = 551153, # Subfamily	Clupeinae  – herrings, sardines, sprats
  'Atlantic Cod' = 164712, # Species	Gadus morhua Linnaeus, 1758 – morue de l'Atlantique, bacalao del Atlántico, cod, rock cod, morue franche, Atlantic cod 
  'Atlantic haddock' = 164744, # Species	Melanogrammus aeglefinus (Linnaeus, 1758) – haddock, aiglefin
  'Atlantic mackerel' = 172414, #Species	Scomber scombrus Linnaeus, 1758 – caballa del Atlántico, maquereau commun, maquereau bleu, Atlantic mackerel
  'Atlantic pollock' = 164727, #Species	Pollachius virens (Linnaeus, 1758) – pollock, coalfish, carbonero, lieu noir, saithe, goberge 
  'Atlantic croaker' = 169283, # Species	Micropogonias undulatus (Linnaeus, 1766) – Atlantic croaker, roncadina, gurrubata
  
  
  'Pacific salmon' = c(
    # 161974 # Genus	Oncorhynchus Suckley, 1861 – Pacific salmon
    161975, # Species	Oncorhynchus gorbuscha (Walbaum, 1792) – pink salmon, humpback, humpbacked salmon, saumon rose
    161976, # Species	Oncorhynchus keta (Walbaum in Artedi, 1792) – chum salmon
    161977, # Species	Oncorhynchus kisutch (Walbaum, 1792) – coho salmon, salmón plateado, saumon coho, silver salmon
    161979, # Species	Oncorhynchus nerka (Walbaum in Artedi, 1792) – blueback salmon, kokanee, red salmon, sockeye salmon, saumon rouge
    161980 # Species	Oncorhynchus tshawytscha (Walbaum in Artedi, 1792) – Chinook salmon, salmón boquinegra, king salmon, saumon chinook
    # 161989 # Species	Oncorhynchus mykiss (Walbaum, 1792) – rainbow trout, trucha arcoiris, steelhead, truite arc-en-ciel, redband trout
  ),  
  'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
  # 'Pacific herring' = 551209, #Species	Clupea pallasii Valenciennes in Cuvier and Valenciennes, 1847 – arenque del Pacífico, Pacific herring
  'Pacific hake (whiting)' = 164792, #Species	Merluccius productus (Ayres, 1855) – North Pacific hake, whiting, Pacific hake, merluza norteña, Pacific whiting
  'Pacific sardine' = 161729,
  
  'Sablefish' = 167123, # Species	Anoplopoma fimbria (Pallas, 1814) – sablefish, bacalao negro
  'Tunas' = c(638252, # Tribe	Thunnini Starks, 1910
              -172454, # Genus	Auxis Cuvier, 1829 – frigate mackerels, frigate tunas
              -172459), # Genus	Allothunnus Serventy, 1948
  'Alaska Pollock' = c(#164722, #invalid - subsequent name/combination 
    934083), # Species	Gadus chalcogrammus Pallas, 1814 – Walleye    
  'Rockfish' = 166705, # Genus	Sebastes Cuvier, 1829 – rockfishes, rockcod, rosefishes
  'Atka mackerel'= 167119, #Genus	Pleurogrammus Gill, 1861 – atka mackerels
  'Swordfish' = 172480, #Family	Xiphiidae  – swordfishes, espadas, espadons
  'Albacore tuna' = 172419, # Species	Thunnus alalunga (Bonnaterre, 1788) – atún blanco, albacore, longfinned albacore, albacora, germon
  'Goosefish' = c(#164500, #Species	Lophius gastrophysus Miranda Ribeiro, 1915 – blackfin goosefish, rape pescador 
    164498), # Genus	Lophius Linnaeus, 1758
  'Porgies' = 169206, # Genus	Pagrus Cuvier, 1816
  'Scups' = 169181, # Genus	Stenotomus Gill, 1865
  'Weakfish' = 169241, #Species	Cynoscion regalis (Bloch and Schneider, 1801) – weakfish, gray trout, sea trout
  'Hake' = c(164729, #Genus	Urophycis Gill, 1863 – codlings 
             164790), # Genus	Merluccius Rafinesque, 1810 – hakes
  'Red hake' = c(164730 #  Species	Urophycis chuss (Walbaum, 1792) – red hake, squirrel hake, merluche-écureuil
  ),#164729), # Genus	Urophycis Gill, 1863 – codlings # Toledo, includes other hake
  'Silver hake' = c(164791 #Species	Merluccius bilinearis (Mitchill, 1814) – silver hake, merlu argenté
  ), # 164790), # Genus	Merluccius Rafinesque, 1810 – hakes # Toledo, includes other hake
  'Snappers' = 168845, #Family	Lutjanidae  – sea perches, snappers, perches de mer, vivaneaux, fusiliers, pargos y huachinangos    
  'Spiny dogfish' = 160617, #Species	Squalus acanthias Linnaeus, 1758 – cazón espinoso común, piked dogfish, spiny dogfish, galludo espinoso, aiguillat commun, dogfish, grayfish, spurdog
  'White perch' = 167678, #Species	Morone americana (Gmelin, 1789) – white perch, baret
  'Striped bass' = 167680, # Species	Morone saxatilis (Walbaum, 1792) – rockfish, striped bass, lobina estriada, bar rayé
  'Spot' = 169267, # VSpecies	Leiostomus xanthurus Lacepède, 1802 – spot, croca
  'King mackerel' = 172435, # Species	Scomberomorus cavalla (Cuvier, 1829) – king mackerel, sierra, carito, carite lucio, thazard serra
  'Black sea bass' = 167687, # Species	Centropristis striata (Linnaeus, 1758) – black sea bass
  'Spanish mackerel' = 172436, # Species	Scomberomorus maculatus (Mitchill, 1815) – serrucho, sierra común, carite Atlántico, thazard Atlantique, Atlantic Spanish mackerel, Spanish mackerel    
  'Tilefish' = 168537, #Family	Malacanthidae  – tilefishes, blanquillos, tiles
  'Sharks' = c(563987 # Superorder	Euselachii #TOLEDO - some of these are sawfish, rays, and etc. 
               # 551500, # Order	Carcharhiniformes  – ground sharks
               # 159788, # Order	Heterodontiformes  – bullhead sharks
               # 159810, # Order	Hexanchiformes Compagno, 1973 – cow sharks, frilled sharks
               # 159851, # Order	Lamniformes  – mackerel sharks
               # 551499, # Order	Orectolobiformes  – gatas nodrizas, requins-tapis, tiburones tapiceros, carpet sharks
               # 551498, # Order	Pristiophoriformes  – saw sharks
               # 160602, #Order	Squaliformes Compagno, 1973 – dogfish sharks
               # 563990 # Order	Squatiniformes 
  ), 
  'Mullets' = 170333, # Family	Mugilidae  – mullets, grey mullets, lisas, muges
  'Vermilion snapper' = 168909, # Rhomboplites aurorubens (Cuvier in Cuvier and Valenciennes, 1829) – vermilion snapper, cotorro, besugo
  'Bluefin tuna' = 172421, # Species	Thunnus thynnus (Linnaeus, 1758) – bluefin tuna, atún aleta azul, atún, horse mackerel, northern bluefin tuna, Atlantic bluefin tuna, thon rouge
  'American eel' = 161127, # Species	Anguilla rostrata (Lesueur, 1817) – American eel, anguila, anguila americana, anguille d'Amérique
  'Red grouper' = 167702, # Species	Epinephelus morio (Valenciennes in Cuvier and Valenciennes, 1828) – red grouper, cherna americana
  'Red snapper' = 168853, # Species	Lutjanus campechanus (Poey, 1860) – red snapper, pargo colorado, huachinango del Golfo, northern red snapper   
  'Black drum' = 169288, # Species	Pogonias cromis (Linnaeus, 1766) – black drum, corvina negra, tambor negro
  'Groupers' = 643094, # Tribe	Epinephelini  
  'Gag grouper' = 167759, # 167759 – Mycteroperca microlepis (Goode and Bean, 1879) – valid – abadejo, charcoal belly, gag
  'Billfishes' = 172486, # Family	Istiophoridae  – billfishes, sailfishes, marlins, spearfishes, picudos, voiliers
  
  ###
  # Mollescus
  ###
  'Mollusca' = 69458, # Phylum	Mollusca  – mollusques, molusco, molluscs, mollusks    
  'Sea scallop' = 79718, #Species	Placopecten magellanicus (Gmelin, 1791) – sea scallop
  'Squid' = 555706, # Superorder	Decabrachia Boettger, 1952
  'Oysters' = c(79777, # Family	Spondylidae Gray, 1826
                79857, # Family	Gryphaeidae Vyalov, 1936
                79866), #  Family	Ostreidae Rafinesque, 1815
  'Clams' = 80384, #Order	Veneroida H. Adams and A. Adams, 1856
  'Mussels' = c(79451, #Family	Mytilidae Rafinesque, 1815
                79913), #  Family	Unionidae Rafinesque, 1820 # TOLEDO - why include freshwater spp?
  'Blue mussel' = 79454, #Species	Mytilus edulis Linnaeus, 1758 – edible blue mussel, blue mussel
  'Conchs' = 72554, # Family	Strombidae Rafinesque, 1815 (Conches)
  'Whelks' = 74069, # Family	Melongenidae Gill, 1867 (Whelks)
  'Snails' = 72878, # Family	Naticidae Guilding, 1834 (Snails)
  'Loligo squid' = 82370, # Genus	Loligo Lamarck, 1798
  'Atlantic surf clam' = 80944, #Species	Spisula solidissima (Dillwyn, 1817) – Atlantic surfclam
  'Arctic surf (Stimpson) clam' = 80983, #Species	Mactromeris polynyma (Stimpson, 1860) – Arctic surfclam
  'Softshell clam' = 81692, #  Species	Mya arenaria Linnaeus, 1758 – softshell clam, softshell
  "Scallops" = 79611, # Family	Pectinidae Rafinesque, 1815
  'Eastern oyster' = 79872, # Species	Crassostrea virginica (Gmelin, 1791) – eastern oyster
  
  
  'Quahog clams' =  81495, # Genus	Mercenaria Schumacher, 1817     
  'Ocean quahog clam' = 81343, # Species	Arctica islandica (Linnaeus, 1767) – ocean quahog
  'Snails' = 69459, # Class	Gastropoda Cuvier, 1797 – gastropods, slugs, snails, escargots, gastéropodes, limaces, caracol, caramujo, lesma
  
  ###
  #HAWAII
  ###
  'Lobsters (*ula*)' = c(206946), # Superfamily	Palinuroidea Latreille, 1802
  'Dolphinfish (*mahimahi*)' = 168790, # Genus	Coryphaena Linnaeus, 1758
  "Marlin (*a'u*)" = c(#172486, # Family	Istiophoridae  – billfishes, sailfishes, marlins, spearfishes, picudos, voiliers TOLEDO
    172490), #Genus	Makaira Lacepède, 1802 – marlins, blue marlin
  'Moonfish (*opah*)' = 166326, # Species	Lampris guttatus (Brünnich, 1788) – opah, opah, pez mariposa
  'Pomfrets (*Monchong*)' = 170287, #Family	Bramidae  – pomfrets, castagnoles, tristones
  'Scad (*opelu*)' = c(168723, # Genus	Decapterus Bleeker, 1851 – mackerel scads, round scads
                       168585, # Genus	Trachurus Rafinesque, 1810 – saurels
                       168676), # Genus	Selar Bleeker, 1851 – bigeyed scads, goggle-eyes, gogglers 
  'Hawaii Snappers' = c(168181, #Heteropriacanthus cruentatus (Lacepède, 1801) – catalufa espinosa, glasseye snapper, catalufa roquera
                        168845), #Family	Lutjanidae  – sea perches, snappers, perches de mer, vivaneaux, fusiliers, pargos y huachinangos
  'Wahoo (*ono*)' = 172451 #Species	Acanthocybium solandri (Cuvier in Cuvier and Valenciennes, 1832) – peto, wahoo, thazard-bâtard
  
)

######***STATE and UNITED STATES########
### Species names and codes for the state tables

#########******United States#############
SpCodeName<-list(
  "United States" = list('American lobster' = SpCodeName.General$`American lobster`, 
                         'Blue crab' = SpCodeName.General$`Blue crab`,
                         'Menhaden' = SpCodeName.General$Menhaden,
                         'Pacific halibut' = SpCodeName.General$`Pacific halibut`,
                         'Pacific salmon' = SpCodeName.General$`Pacific salmon`,  
                         'Sablefish' = SpCodeName.General$Sablefish,
                         'Sea scallop' = SpCodeName.General$`Sea scallop`,
                         'Shrimp' = SpCodeName.General$Shrimp,
                         'Tunas' = SpCodeName.General$Tunas,
                         'Alaska pollock' = SpCodeName.General$`Alaska Pollock`),
  
  #####******North Pacific Region#####
  "North Pacific"= list('Atka mackerel'=  SpCodeName.General$`Atka mackerel`,
                        'Crab'= SpCodeName.General$Crab,
                        'Flatfish' = SpCodeName.General$Flatfish,
                        'Pacific cod' = SpCodeName.General$`Pacific cod`,
                        'Pacific halibut' = SpCodeName.General$`Pacific halibut`,
                        'Pacific herring' = SpCodeName.General$`Herring`,
                        'Rockfish' = SpCodeName.General$Rockfish,
                        'Sablefish' = SpCodeName.General$Sablefish,
                        'Salmon' = SpCodeName.General$`Pacific salmon`,
                        'Alaska Pollock' = SpCodeName.General$`Alaska Pollock`
  ),
  
  #####******Pacific Region#####
  "Pacific" = list('Albacore tuna' = SpCodeName.General$`Albacore tuna`, 
                   'Crab' = SpCodeName.General$Crab, 
                   'Flatfish' = SpCodeName.General$Flatfish, 
                   'Pacific hake (whiting)' = SpCodeName.General$`Pacific hake (whiting)`, 
                   'Other shellfish' = c(SpCodeName.General$Arthropoda, 
                                         SpCodeName.General$Mollusca, 
                                         -(SpCodeName.General$Crab),
                                         -(SpCodeName.General$Shrimp), 
                                         -(SpCodeName.General$Squid)),
                   'Rockfish' = SpCodeName.General$Rockfish, 
                   'Sablefish' = SpCodeName.General$Sablefish, 
                   'Salmon' = SpCodeName.General$`Pacific salmon`, 
                   'Shrimp' = SpCodeName.General$Shrimp,
                   'Squid' = SpCodeName.General$Squid),
  
  

  ########******Western Pacific Region (Hawaii)#######
  "Western Pacific (Hawai`i)" = list('Lobsters (*ula*)' = SpCodeName.General$`Lobsters (*ula*)`,
                                     'Dolphinfish (*mahimahi*)' = SpCodeName.General$`Dolphinfish (*mahimahi*)`,
                                     "Marlin (*a'u*)" = SpCodeName.General$`Marlin (*a'u*)`,
                                     'Moonfish (*opah*)' = SpCodeName.General$`Moonfish (*opah*)`,
                                     'Pomfrets (*Monchong*)' = SpCodeName.General$`Pomfrets (*Monchong*)`,
                                     'Scad (*opelu*)' = SpCodeName.General$`Scad (*opelu*)`,
                                     'Snappers' = SpCodeName.General$`Hawaii Snappers`,
                                     'Swordfish (*mekajiki*)' = SpCodeName.General$Swordfish,
                                     'Tunas (*aku*)' = SpCodeName.General$Tunas,
                                     'Wahoo (*ono*)' = SpCodeName.General$`Wahoo (*ono*)`
  ),
  ########******New England Region########
  "New England" = list('American lobster' = SpCodeName.General$`American lobster`, 
                       'Atlantic herring' = SpCodeName.General$`Herring`, 
                       'Atlantic mackerel' = SpCodeName.General$`Atlantic mackerel`, 
                       'Bluefin tuna' = SpCodeName.General$`Bluefin tuna`, 
                       'Cod and haddock' = c(SpCodeName.General$`Atlantic Cod`, 
                                             SpCodeName.General$`Atlantic haddock`), 
                       'Flounders' = SpCodeName.General$Flounders, 
                       'Goosefish' = SpCodeName.General$Goosefish,
                       'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                       'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                       'Squid' = SpCodeName.General$Squid), 
  
  ########******Mid-Atlantic Region########
  "Mid-Atlantic" = list('American lobster' = SpCodeName.General$`American lobster`, 
                        'Atlantic surf clam' = SpCodeName.General$`Atlantic surf clam`, 
                        'Blue crab' = SpCodeName.General$`Blue crab`, 
                        'Eastern oyster' = SpCodeName.General$`Eastern oyster`,
                        'Menhaden' = SpCodeName.General$Menhaden,
                        'Quahog clam' = SpCodeName.General$`Quahog clam`, 
                        'Sea scallop' = SpCodeName.General$`Sea scallop`, 
                        'Squid' = SpCodeName.General$Squid, 
                        'Striped bass' = SpCodeName.General$`Striped bass`, 
                        'Summer flounder' = SpCodeName.General$`Summer flounder`), 
  
  
  ########******South Atlantic Region########
  "South Atlantic" = list('Blue crab' = SpCodeName.General$`Blue crab`, 
                          'Clams' = SpCodeName.General$Clams, 
                          'Flounders' = SpCodeName.General$Flounders, 
                          'Groupers' = SpCodeName.General$Groupers, 
                          'King mackerels' = SpCodeName.General$`King mackerel`,
                          'Oysters' = SpCodeName.General$Oysters, 
                          'Shrimp' = SpCodeName.General$Shrimp, 
                          'Snappers' = SpCodeName.General$Snappers, 
                          'Swordfish' = SpCodeName.General$Swordfish, 
                          'Tunas' = SpCodeName.General$Tunas), 
  
  ########******Gulf of Mexico Region########
  "Gulf of Mexico" = list('Blue crab' = SpCodeName.General$`Blue crab`, 
                          'Crawfish' = SpCodeName.General$Crawfish, 
                          'Groupers' = SpCodeName.General$Groupers, 
                          'Menhaden' = SpCodeName.General$Menhaden,
                          'Mullets' = SpCodeName.General$Mullets,
                          'Oysters' = SpCodeName.General$Oysters, 
                          'Red snapper' = SpCodeName.General$`Red snapper`,
                          'Shrimp' = SpCodeName.General$Shrimp, 
                          'Spiny lobster' = SpCodeName.General$`Caribbean Spiny lobster`, 
                          'Tunas' = SpCodeName.General$Tunas)
)




#####***spcat.list#####
spcat.list<-SpCodeName
###******Get the unique codes associated with the species listed in the tables####

##########USER FUNCTIONS##############

echoTF<-function(typical, code = TRUE) {
  return(ifelse(code == TRUE, typical, FALSE))
}

includeTF<-function(typical, showresults = TRUE) {
  return(ifelse(showresults == TRUE, typical, FALSE))
}


CreateLoadedDataMetadata<-function(dir.out, data, title) {
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    # body_add_par("System Info:", style = "heading 2") %>%
    # body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    # body_add_par("R Version", style = "heading 2") %>%
    # body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
    body_add_par("Input Data used in this Run",
                 style = "heading 2")
  
  for (i in 1:length(loaded.data)){
    
    temp<-loaded.data[[i]]
    
    my_doc <- my_doc %>%
      body_add_par(names(loaded.data)[i], style = "heading 3") %>%
      body_add_table(head(temp)) %>%
      body_add_par(summary(temp), style = "Normal")
  }
  
  # a<-sessionInfo()
  # my_doc <- my_doc %>% 
  #   body_add_par("R Packages Loaded", style = "heading 2")
  # for (i in 1:length(a$basePkgs)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(a$basePkgs[i], style = "Normal") 
  # }
  # for (i in 1:length(a$otherPkgs)){
  #   temp<-a$otherPkgs[[i]]
  #   my_doc <- my_doc %>% 
  #     body_add_par(temp$Package,
  #                  style = "heading 3") %>%
  #     body_add_par(temp$Version, style = "Normal") %>%
  #     body_add_par(temp$Title, style = "Normal") %>%
  #     body_add_par(temp$Description, style = "Normal") %>%
  #     body_add_par(temp$SystemRequirements, style = "Normal") %>%
  #     body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
  #     body_add_par(temp$URL, style = "Normal")
  # }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}


CreateMetadata<-function(dir.out, title){
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 # body_add_par(paste0("Population Narrative of ", commorg, " (", fp_text(sciname, italic = T, color = "black", font.size=10), ")"," in ", region),
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    body_add_par("System Info:", style = "heading 2") %>%
    body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    body_add_par("R Version", style = "heading 2") %>%
    body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
  #   body_add_par("Populations Run in this Iteration", 
  #                style = "heading 2") 
  # for (i in 1:length(org_pop)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(org_pop[i], style = "Normal") 
  # }
  
  a<-sessionInfo()
  my_doc <- my_doc %>% 
    body_add_par("R Packages Loaded", style = "heading 2")
  for (i in 1:length(a$basePkgs)){
    my_doc <- my_doc %>% 
      body_add_par(a$basePkgs[i], style = "Normal") 
  }
  for (i in 1:length(a$otherPkgs)){
    temp<-a$otherPkgs[[i]]
    my_doc <- my_doc %>% 
      body_add_par(temp$Package,
                   style = "heading 3") %>%
      body_add_par(temp$Version, style = "Normal") %>%
      body_add_par(temp$Title, style = "Normal") %>%
      body_add_par(temp$Description, style = "Normal") %>%
      body_add_par(temp$SystemRequirements, style = "Normal") %>%
      body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
      body_add_par(temp$URL, style = "Normal")
  }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}


OutputAnalysis<-function(landings.data, category0, baseyr, 
                         state.codes, titleadd,
                         counter, dir.rawdata, dir.reports, pctmiss, dir.figures, 
                         dir.outputtables, analysisby = "P", 
                         MinimumNumberOfSpecies = 5, 
                         reg.order = c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", 
                                       "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico"), 
                         reg.order0 = c("US", "NP", "Pac", "WP", "NE", "MA", "SA", "GOM", "NorE"), 
                         skipplots = F) {
  
  dir.analyses1<-paste0(dir.analyses, "/",titleadd, "_", analysisby, "_", 
                        gsub(pattern = "\\.", replacement = "", x = category0), "_pctmiss", 
                        gsub(pattern = "\\.", replacement = "", x = pctmiss*100), "/")
  dir.create(dir.analyses1) 
  dir.reports<-paste0(dir.analyses1, "/reports/")
  dir.create(paste0(dir.analyses1, "/reports/")) 
  dir.figures<-paste0(dir.analyses1, "/figures/")
  dir.create(paste0(dir.analyses1, "/figures/")) 
  dir.outputtables<-paste0(dir.analyses1, "/outputtables/")
  dir.create(paste0(dir.analyses1, "/outputtables/")) 
  
  
  #Save Stuff
  editeddata.list<-list()
  rawtable.list<-list()
  tottable.list<-list()
  finaltable.list<-list()
  spptable<-data.frame()
  figures.list<-list()
  
  for (r in 1:length(reg.order)) {
    
    if (r != 1) { #only because I am tired of getting the warning messages
      remove(place, title0, temp00, temp0, temp, title000)
    }
    
    if (reg.order[r] == "Northeast") {
      state.codes$Region[state.codes$Region %in% c("Mid-Atlantic", "New England")]<-"Northeast"
      state.codes$abbvreg[state.codes$Region %in% c("Mid-Atlantic", "New England")]<-"NorE"
    }
    
    ### A. Import and Edit data
    #subset data
    place<-reg.order[r]
    print(place)
    counter<-funct_counter(counter)
    
    
    title000<-paste0("_","byr",baseyr, "_", analysisby, 
                     "_",gsub(pattern = "\\.", replacement = "", x = category0), 
                     "_pctmiss", gsub(pattern = "\\.", replacement = "", x = pctmiss))
    title0<-paste0(counter, "_", gsub(pattern = "\\(", replacement = "", x = 
                                        gsub(pattern = ")", replacement = "", x = 
                                               gsub(pattern = "`", replacement = "", x = 
                                                      gsub(reg.order0[r], pattern = " ", replacement = "")))), 
                   title000, "_", titleadd)
    
    
    idx<-c(1:nrow(landings.data))
    if (reg.order[r] != "National") {
      idx<-which(landings.data$State %in% state.codes$State[state.codes$Region %in% place])
    }
    
    temp00<-EditCommData(dat = landings.data[idx,], category0)
    temp.orig<-temp00[[1]] ### Data
    spp.editeddata<-temp00[[2]] ### By the way, which species are included in each category?
    tsn.editeddata<-temp00[[3]]  ### By the way, which species are included in each category by code number?
    
    NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
                                                  strsplit(x = names(temp.orig)[1], 
                                                           split = "_")[[1]][2], 
                                                split = "[a-zA-Z]")[[1]][1]))[1]
    
    ### B. Enter base year
    
    ### C. Run the function
    if (analysisby == "P") {
      temp00<-PriceMethodOutput(temp = temp.orig, baseyr, pctmiss, 
                                title0 = title0, place = place, 
                                MinimumNumberOfSpecies = MinimumNumberOfSpecies)
    } else if (analysisby == "Q") {
      temp00<-QuantityMethodOutput(temp = temp.orig, baseyr, pctmiss, 
                                   title0 = title0, place = place, 
                                   MinimumNumberOfSpecies = MinimumNumberOfSpecies)
    }
    
    temp<-temp00[[1]] #Data Output
    warnings.list0<-temp00[[2]] # Warnings
    figures.list0<-temp00[[3]] #Figures
    figures.list<-c(figures.list, figures.list0)
    spptable0<-temp00[[4]] #Species overview info
    spp.output<-temp00[[5]] #List of Species
    
    ### D. Obtain the implicit quantity estimates
    
    #EditedData
    editeddata.list[[r]]<-temp.orig
    names(editeddata.list)[r]<-place
    write.csv(x = editeddata.list[[r]], file = paste0(dir.outputtables, title0,"_EditedData.csv"))
    
    #Raw
    write.csv(x = temp, file = paste0(dir.outputtables, title0,"_AllData.csv"))
    rawtable.list[[r]]<-temp
    names(rawtable.list)[r]<-place
    
    #Review
    temp0<-temp[, grepl(pattern = paste0("_", NumberOfSpecies), x = names(temp))]
    names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
    # temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
    
    tottable.list[[r]]<-temp0
    names(tottable.list)[r]<-place
    write.csv(x = tottable.list[[r]], file = paste0(dir.outputtables, title0,"_Review.csv"))
    
    #Final
    temp0<-temp[, grepl(pattern = paste0("0_", NumberOfSpecies, "Total"), x = names(temp))]
    names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
    # temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
    
    finaltable.list[[r]]<-temp0
    names(finaltable.list)[r]<-place
    write.csv(x = finaltable.list[[r]], file = paste0(dir.outputtables, title0,"_Final.csv"))
    
    #Species Table
    # spptable0<-data.frame(Analysis  = title0,
    #                 Place = place,
    #                 Catagory = rep_len(x = NA, length.out = length(spp.editeddata)), 
    #                 TotCount = rep_len(x = NA, length.out = length(spp.editeddata)), 
    #                 RmCount = rep_len(x = NA, length.out = length(spp.editeddata)), 
    #                 UsedCount = rep_len(x = NA, length.out = length(spp.editeddata)))
    # 
    # for (i in 1:length(spp.editeddata)) {
    #   
    #   #Find the name of the ith species group (in terms of how the data is organized)
    #   cat<-names(spp.editeddata)[i]
    #   XColumns<-grep(pattern = paste0(NumberOfSpecies, cat),
    #                  x = names(temp))
    #       #Test
    #   XColumns<-c(XColumns, 1) #in case there is only one column for the next step
    #   cat0<-as.character(lapply(X = strsplit(x = names(temp[,XColumns])[1], 
    #                                             split = paste0("_", NumberOfSpecies)), 
    #                                function(x) x[2]))
    #   #Find the number of this ith species group (in terms of how the data is organized)
    #   ii<-as.character(lapply(X = strsplit(x = names(temp[,XColumns])[1], 
    #                                        split = paste0("_", NumberOfSpecies)), 
    #                           function(x) x[1]))
    #   ii<-gsub(pattern = "[a-zA-Z]", replacement = "", x = ii)
    # 
    #   #check your work
    #   # VColumns<-grep(pattern = paste0("V", ii,"_"),
    #   #                x = substr(x = names(temp),
    #   #                           start = 1,
    #   #                           stop = (2+nchar(ii))))
    #   
    #   RColumns<-grep(pattern = paste0("R", ii,"_"),
    #                  x = substr(x = names(temp),
    #                             start = 1,
    #                             stop = (2+nchar(ii))))
    #   RColumns<-RColumns[-grep(pattern = paste0(NumberOfSpecies, cat),
    #                            x = names(temp)[RColumns])]
    #   
    #   spptable0$Catagory[i]<- cat
    #   spptable0$TotCount[i]<-length(spp.editeddata[names(spp.editeddata) %in% cat][[1]])
    #   spptable0$UsedCount[i]<-ifelse(is.na(length(RColumns)), 0, length(RColumns))
    #   spptable0$RmCount[i]<-spptable0$TotCount[i] - spptable0$UsedCount[i]
    # }
    
    spptable<-rbind.data.frame(spptable, spptable0)
    write.csv(x = temp0, file = paste0(dir.outputtables, title0,"_Species.csv"))
    
    #Report
    rmarkdown::render(ProdI.Report, 
                      output_dir = paste0(dir.reports), 
                      output_file = paste0(title0,".docx"))
    
  }
  ########SPREADSHEETS########
  
  print("Create spreadsheets")
  
  spptable<-data.frame(spptable)
  spptable$pct.used<-round(x = (spptable$UsedCount/spptable$TotCount)*100,
                           digits = 2)
  
  save(editeddata.list, rawtable.list, finaltable.list, tottable.list, spptable, spp.output,
       file = paste0(dir.outputtables, "AllOutputs.rdata"))
  
  write.csv(x = spptable, file = paste0(dir.outputtables, "000_All", title000,"_Species.csv"))
  
  
  for (r in 1:length(reg.order)){
    
    #Raw
    # write.xlsx2(x = rawtable.list[[r]], 
    #             file = paste0(dir.outputtables, "000_All", title000, "_Raw.xlsx"), 
    #             sheetName = reg.order[r], 
    #             col.names = T, row.names = T, append = T)
    
    #Edited Data
    # write.xlsx2(x = editeddata.list[[r]],
    #             file = paste0(dir.outputtables, "000_All", title000, "_EditedData.xlsx"),
    #             sheetName = reg.order[r],
    #             col.names = T, row.names = T, append = T)
    
    #Print
    write.xlsx2(x = finaltable.list[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_FinalOutput.xlsx"), 
                sheetName = reg.order[r], 
                col.names = T, row.names = T, append = T)
    
    #Review
    write.xlsx2(x = tottable.list[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_Review.xlsx"), 
                sheetName = reg.order[r], 
                col.names = T, row.names = T, append = T)
    
  }
  
  ######PLOTS##########
  
  print("Create plots")
  
  save(figures.list, #gridfigures.list,
       file = paste0(dir.figures, "AllFigures.rdata"))
  
  #Side by Side graphs
  figs<-unique(paste0(lapply(X = strsplit(x = names(figures.list),
                                          split = gsub(pattern = "\\.", replacement = "", x = category0)),
                             function(x) x[2])))
  gridfigures.list<-list()
  
  # if (length(r)>1){
  for (i in 1:length(figs)){
    
    a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
    
    dir.create(paste0(dir.figures, "/", a, "/"))
    
    fig<-figs[i]
    list0<-figures.list[grep(pattern = fig, x = names(figures.list))]
    
    # g<-ggarrange(list0[[1]],
    #                 list0[[2]],
    #                 list0[[3]],
    #                 list0[[4]],
    #                 list0[[5]],
    #                 list0[[6]],
    #                 list0[[7]],
    #                 nrow=3, ncol = 3)
    
    g<-ggarrange(plotlist = list0,
                 nrow=3, ncol = 3)
    
    ggsave(filename = paste0(dir.figures, "/", a, "/", "000_All_byr",baseyr,
                             "_",gsub(pattern = "\\.", replacement = "", x = category0), fig, ".png"),
           plot = g,
           width = 11, height = 8.5)
    
    gridfigures.list<-c(gridfigures.list, list(g))
    names(gridfigures.list)[length(gridfigures.list)]<-paste0("000_All_byr",baseyr,
                                                              "_",gsub(pattern = "\\.", replacement = "", x = category0), fig)
  }
    save(gridfigures.list,
         file = paste0(dir.figures, "AllFiguresGrid.rdata"))
  # } else {
  # 
  # 
  # 
  # 
  #   #make single plots
  #   for (i in 1:length(figures.list)) {
  #     print(paste0(names(figures.list)[i]))
  #     a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
  #     dir.create(paste0(dir.figures, "/", a, "/"))
  # 
  #     # dir.create(paste0(dir.figures, "/", a, "/"))
  # 
  #     ggsave(filename = paste0(dir.figures, "/", a, "/", names(figures.list)[i], ".png"),
  #            plot = figures.list[[i]],
  #            width = 11, height = 8.5)
  # }
  # }
}


#########***########
##########LOAD DATA##############

##########*** Reg Order##############
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico")
  
##########*** State Codes##############
state.codes <- statereg <- read.csv(paste0(dir.data, '/statereg.csv'), stringsAsFactors = FALSE)
write.csv(x = state.codes, file = paste0(dir.rawdata, "/statereg.csv"))




