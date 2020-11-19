##########INTRODUCTION####################
#' ---
#' title: Productivity Output Analysis
#' purpose: Construct the FEUS Commerical Fisheries state and national tables and output them to csv files
#' author: Emily Markowitz (Emily.Markowitz AT NOAA.gov)
#' date: 2019-2020
#' ---

rm(list=ls())


##########LIBRARIES####################
PKG <- c("readr", "data.table","plyr","dplyr", "IndexNumR", 
         
         # devtools::install_github("emilyhmarkowitz/FishEconProdOutput", force = TRUE)
         
         "FishEconProdOutput",
         #Seperating species by taxonomic group
         # install.packages("remotes")
         # remotes::install_github("ropensci/taxize")
         "taxize", 
         
         # Data Managment
         "tidyr",
         "reshape2",
         "tidyverse",
         "filesstrings", 
         "data.table", # := to tag species codes
         "plyr",  #ddply function
         "sas7bdat", 
         "rlist",
         
         #RMarkdown
         "rmarkdown",
         "knitr",
         "gridExtra", 
         "ggpubr",
         "magrittr", 
         "flextable", 
         
         #Excel File Management
         "officer",
         "rJava", 
         "xlsx",
         "readxl",
         
         #Visuals
         "ggplot2",
         
         #Package Management
         # library(roxygen2)
         "devtools",
         
         #Presentations
         #remotes::install_github('yihui/xaringan')
         "xaringan",
         "tidyverse", 
         "stargazer"
)

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

options(htmltools.dir.version = FALSE)


#######DIRECTORIES############
dir.in<-getwd()
#Local Directories
dir.output<-paste0(dir.in, "/output/")
dir.data<-paste0(dir.in, "/data/")
dir.out<-paste0(dir.output, "ProductivityIndex_", Sys.Date(), "/")
dir.create(dir.out)
dir.parent<-dirname(dir.in)
dir.presdat<-paste0(dir.in, "/rpresentation/")
dir.pres<-paste0(dir.out, "/Presentation")
dir.create(dir.pres)
dir.scripts<-paste0(dir.in, "/rscripts/")
dir.create(paste0(dir.out, "/rscripts")) 
dir.reports<-paste0(dir.out, "/reports/")
dir.create(paste0(dir.out, "/analyses")) 
dir.analyses<-paste0(dir.out, "/analyses/")
dir.create(paste0(dir.out, "/reports/")) 
dir.create(paste0(dir.out, "/metadata/")) #Save word files
dir.rawdata<-paste0(dir.out, "/rawdata/")
dir.create(paste0(dir.out, "/rawdata/")) 
# dir.figures<-paste0(dir.out, "/figures/")
# dir.create(paste0(dir.out, "/figures")) 
dir.create(paste0(dir.out, "/documentation/")) 
dir.docu<-paste0(dir.out, "/documentation/")
dir.outputtables<-paste0(dir.out, "outputtables/")
dir.create(dir.outputtables)

date0<-""
date00<-paste0(Sys.Date())

# ProdI.Run<-paste0(dir.scripts, "ProductivityIndex_Run",date0,".R")
ProdI.Data<-paste0(dir.scripts, "ProductivityIndex_Data",date0,".R")
ProdI.Funct<-paste0(dir.scripts, "ProductivityIndex_Functions",date0,".R")
ProdI.Report<-paste0(dir.scripts, "ProductivityIndex_Report",date0,".rmd")
ProdI.Pres<-paste0(dir.pres, "ProductivityIndex_Presentation.rmd")

######*** Save RScripts########
#From Specific Files
listfiles<-list.files(path = dir.scripts, pattern = date0) 
listfiles<-listfiles[-(listfiles %in% "old")]

for (i in 1:length(listfiles)){
  file.copy(from = paste0(dir.scripts, listfiles[i]), 
            to = paste0(dir.out, "/rscripts/", listfiles[i]), 
            overwrite = T)
}


#Move most updated  word styles file from reference "common" file to getwd()/rscript
file.copy(from = paste0(dir.scripts, "/word-styles-reference.docx"), 
          to = paste0(dir.out, "/rscripts", "/word-styles-reference.docx"), 
          overwrite = T)


###FUNCTIONS#####
#Load data for this section
#Functions specific to this section
source(ProdI.Funct)


###IMPORT DATA#####
# source(ProdI.Data)
landings.data<-read.csv(file = paste0(dir.data, "landings_edited.csv"))
write.csv(x = landings.data, file = paste0(dir.rawdata, "landings_edited.csv"))
landings.data<-subset(landings.data, Collection=="Commercial")
maxyr<-max(landings.data$Year)
state.codes <- statereg<-read.csv(file = paste0(dir.data, "statereg.csv"))





######PRELIMINARY ANALYSIS##########
# barplot(table(landings.data$category.tax))
# a<-data.frame(t(table(landings.data[c("category.tax", "Year")])))
# xnames<-as.numeric(paste0(a$Year))
# xnames[!(xnames %in% seq(from = min((as.numeric(xnames))), 
#                          to = max(as.numeric(xnames)), 
#                          by = 10))]<-""
# ggplot(data = a, aes(x = factor(Year), y = Freq, color = category.tax)) +
#   geom_line(aes(group = category.tax)) +
#   geom_point() +
#   theme(
#     panel.grid.major.y = element_line( color=NOAALightBlue, size = .1 ),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     axis.line = element_line( color=NOAALightBlue, size = .1 ),
#     axis.ticks = element_blank(), # remove ticks
#     panel.background = element_blank()
#   )  +
#   scale_x_discrete(labels= xnames) +
#   guides(fill=FALSE)

counter<-0

########ANALYSIS############
OutputAnalysis<-function(landings.data, category0, baseyr, 
                         state.codes, titleadd,
                         counter, dir.rawdata, dir.reports, dir.analyses, pctmiss, dir.figures, 
                         dir.outputtables, analysisby = "P", 
                         MinimumNumberOfSpecies = 5, 
                         reg.order = c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", 
                                       "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico"), 
                         reg.order0 = c("US", "NP", "Pac", "WP", "NE", "MA", "NorE", "SA", "GOM"), 
                         skipplots = F) {
  
  dir.analyses1<-paste0(dir.analyses, "/",titleadd, "_", analysisby, "_", 
                        gsub(pattern = "\\.", replacement = "", x = category0), "_pctmiss", 
                        gsub(pattern = "\\.", replacement = "", x = pctmiss*100), "/")
  # print(dir.analyses1)
  
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
  alldatatable.list<-list()
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
    
    temp.orig<-temp00<-landings.data[idx,
                          c(category0, #"AFS.Name", 
                            "Year", "Pounds", "Dollars", "Tsn"#, 
                            #"State", "State1", "fips", "Region", "abbvst", "abbvreg", "xstate", "xreg", "Region.no"
                            )]
    
    # temp00<-EditCommData(dat = landings.data[idx,], category0)
    # temp.orig<-temp00[[1]] ### Data
    # spp.editeddata<-temp00[[2]] ### By the way, which species are included in each category?
    # tsn.editeddata<-temp00[[3]]  ### By the way, which species are included in each category by code number?
    
    # NumberOfSpecies<-numbers0(x = c(0, strsplit(x = 
    #                                               strsplit(x = names(temp.orig)[1], 
    #                                                        split = "_")[[1]][2], 
    #                                             split = "[a-zA-Z]")[[1]][1]))[1]
    
    ### B. Enter base year
    
    ### C. Run the function
    if (analysisby == "P") {
      temp00<-PriceMethodOutput(temp = temp.orig, baseyr, pctmiss, 
                                title0 = title0, place = place, 
                                MinimumNumberOfSpecies = MinimumNumberOfSpecies, category0 = category0)
    } else if (analysisby == "Q") {
      temp00<-QuantityMethodOutput(temp = temp.orig, baseyr, pctmiss, 
                                   title0 = title0, place = place, 
                                   MinimumNumberOfSpecies = MinimumNumberOfSpecies)
    }
    
    temp<-temp00$index.data #Data Output
    warnings.list0<-temp00$warnings.list # Warnings
    figures.list0<-temp00$figures.list #Figures
    figures.list<-c(figures.list, figures.list0)
    # spptable0<-temp00[[4]] #Species overview info
    # spp.output<-temp00[[5]] #List of Species
    
    ### D. Obtain the implicit quantity estimates
    
    #EditedData
    editeddata.list[[r]]<-temp.orig
    names(editeddata.list)[r]<-place
    write.csv(x = editeddata.list[[r]], file = paste0(dir.outputtables, title0,"_EditedData.csv"))
    
    #Raw
    write.csv(x = temp, file = paste0(dir.outputtables, title0,"_AllData.csv"))
    rawtable.list[[r]]<-temp
    names(rawtable.list)[r]<-place
    
    # #Review
    # temp0<-temp[, grepl(pattern = paste0("_", NumberOfSpecies), x = names(temp))]
    # names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
    # temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
        
    tottable.list[[r]]<-temp[temp$cat %in% "Total",]
    names(tottable.list)[r]<-place
    write.csv(x = tottable.list[[r]], file = paste0(dir.outputtables, title0,"_Review.csv"))
    
    # #Final
    # temp0<-temp[, grepl(pattern = paste0("0_", NumberOfSpecies, "Total"), x = names(temp))]
    # names(temp0)<-gsub(pattern = "0", replacement = "", x = names(temp0))
    # # temp0<-temp0[,-grep(pattern = "REMOVED_", x = names(temp0))]
    
    finaltable.list[[r]]<-temp[temp$cat %in% "Total",]
    names(finaltable.list)[r]<-place
    write.csv(x = finaltable.list[[r]], file = paste0(dir.outputtables, title0,"_Final.csv"))
    
    alldatatable.list[[r]]<-temp#[temp$cat %in% "Total",]
    names(alldatatable.list)[r]<-place
    write.csv(x = alldatatable.list[[r]], file = paste0(dir.outputtables, title0,"_AllData.csv"))
    
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
    # 
    # spptable<-rbind.data.frame(spptable, spptable0)
    # write.csv(x = temp0, file = paste0(dir.outputtables, title0,"_Species.csv"))
    # 
    # #Report
    # rmarkdown::render(ProdI.Report, 
    #                   output_dir = paste0(dir.reports), 
    #                   output_file = paste0(title0,".docx"))
    
  }
  ########SPREADSHEETS########
  
  print("Create spreadsheets")
  
  # spptable<-data.frame(spptable)
  # spptable$pct.used<-round(x = (spptable$UsedCount/spptable$TotCount)*100,
  #                          digits = 2)
  # 
  save(editeddata.list, rawtable.list, finaltable.list, tottable.list, #spptable, spp.output,
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
    
    #All Data
    write.xlsx2(x = alldatatable.list[[r]], 
                file = paste0(dir.outputtables, "000_All", title000, "_", titleadd, "_AllData.xlsx"), 
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
    #make single plots
    for (i in 1:length(figures.list)) {
      print(paste0(names(figures.list)[i]))
      a<-strsplit(x = names(figures.list)[i], split = "_")[[1]][length(strsplit(x = names(figures.list)[i], split = "_")[[1]])]
      dir.create(paste0(dir.figures, "/", a, "/"))

      # dir.create(paste0(dir.figures, "/", a, "/"))

      ggsave(filename = paste0(dir.figures, "/", a, "/", names(figures.list)[i], ".png"),
             plot = figures.list[[i]],
             width = 11, height = 8.5)
  }
  # }
}

# TPI0<-function(dat, Year, pvar, qvar, vvar, prodID) {
#   
#   names(dat)[names(dat) %in% Year]<-"Year"
#   names(dat)[names(dat) %in% pvar]<-"p"
#   names(dat)[names(dat) %in% qvar]<-"q"
#   names(dat)[names(dat) %in% vvar]<-"v"
#   names(dat)[names(dat) %in% prodID]<-"prod"
#   dat<-dat[,c("Year", "p", "q", "v", "prod")]
#   
#   N=as.numeric(length(min(dat$Year) : max(dat$Year))) #Number of years in series
#   SPI<-as.data.frame(matrix(0,nrow=N,2)) #Set up Data frame to hold results
#   colnames(SPI)[1]<-"Year"               #Nmae columns in Data Frame PI
#   colnames(SPI)[2]<-"CPI"
#   
#   SPI$Year<-(min(dat$Year) : max(dat$Year))                       #Put Year into first column of dataframe
#   SPI[1,2]=1                           #Put value of 1 in first row, second column, for Chain Index
#   
#   t0=1
#   
#   for (i in (min(dat$Year) : max(dat$Year)) ) {
#     
#     year1<-subset(dat, (Year==i & p>0 & q>0 & v>0))    #only keep observations with positive p,q,v
#     year2<-subset(dat, (Year==(i+1) & p>0 & q>0 & v>0))#only keep observations with positive p,q,v
#     
#     year1_2<-merge(year1, year2, by="prod", all.x=TRUE, all.y=TRUE, no.dups=TRUE) #merge two data frames
#     
#     year1_2<-na.omit(year1_2)  #Any rows with "NA" values are deleted.
#     
#     yr1tval=sum(year1_2$v.x)   #Calculate total value for year 1
#     yr2tval=sum(year1_2$v.y)   #Calculate total value for year 2
#     
#     year1_2$yr1shr=((year1_2$q.x*year1_2$p.x)/yr1tval) #calculate share values for year 1 products
#     year1_2$yr2shr=((year1_2$q.y*year1_2$p.y)/yr2tval) #calculate share values for year 2 products
#     
#     year1_2$avgr=(year1_2$yr1shr+year1_2$yr2shr)/2     #calculate average share values for years 1 and 2
#     
#     year1_2$tp=(year1_2$p.y/year1_2$p.x)^(year1_2$avgr) #calculate tornqvist value for each product
#     
#     SPTI=prod(year1_2$tp)   #calculates total tornqvist value
#     
#     
#     SPI[(t0+1),2] <- SPI[t0,2]*SPTI #creates chained Tornqvist Value
#     
#     t0=t0+1
#   }
#   
#   
#   return(SPI)
# }
# 
# TPI<-function(dat, Year, pvar, qvar, vvar, prodID) {
#   
#   names(dat)[names(dat) %in% Year]<-"Year"
#   names(dat)[names(dat) %in% pvar]<-"p"
#   names(dat)[names(dat) %in% qvar]<-"q"
#   names(dat)[names(dat) %in% vvar]<-"v"
#   names(dat)[names(dat) %in% prodID]<-"prod"
#   dat<-dat[,c("Year", "p", "q", "v", "prod")]
#   
#   N=as.numeric(length(min(dat$Year) : max(dat$Year))) #Number of years in series
#   SPI<-as.data.frame(matrix(0,nrow=N,2)) #Set up Data frame to hold results
#   colnames(SPI)[1]<-"Year"               #Nmae columns in Data Frame PI
#   colnames(SPI)[2]<-"CPI"
#   
#   SPI$Year<-(min(dat$Year) : max(dat$Year))                       #Put Year into first column of dataframe
#   SPI[1,2]=1                           #Put value of 1 in first row, second column, for Chain Index
#   
#   t0=1
#   
#   for (i in (min(dat$Year) : max(dat$Year)) ) {
#     
#     year1<-subset(data1, (Year==i & p>0 & q>0 & v>0))    #only keep observations with positive p,q,v
#     year2<-subset(data1, (Year==(i+1) & p>0 & q>0 & v>0))#only keep observations with positive p,q,v
#     
#     year1_2<-merge(year1, year2, by="prod", all.x=TRUE, all.y=TRUE, no.dups=TRUE) #merge two data frames
#     
#     year1_2<-na.omit(year1_2)  #Any rows with "NA" values are deleted.
#     
#     yr1tval=sum(year1_2$v.x)   #Calculate total value for year 1
#     yr2tval=sum(year1_2$v.y)   #Calculate total value for year 2
#     
#     year1_2$yr1shr=(year1_2$v.x/yr1tval) #calculate share values for year 1 products
#     year1_2$yr2shr=(year1_2$v.y/yr2tval) #calculate share values for year 2 products
#     
#     year1_2$avgr=(year1_2$yr1shr+year1_2$yr2shr)/2     #calculate average share values for years 1 and 2
#     
#     year1_2$tp=(year1_2$p.y/year1_2$p.x)^(year1_2$avgr) #calculate tornqvist value for each product
#     
#     SPTI=prod(year1_2$tp)   #calculates total tornqvist value
#     
#     
#     SPI[(t0+1),2] <- SPI[t0,2]*SPTI #creates chained Tornqvist Value
#     
#     t0=t0+1
#   }
#   
#   baseval=SPI[SPI$Year == base.year,2]
#   SPI$BPI=SPI$SPI/baseval
#   
#   return(SPI)
# }
# 
# PriceMethodOutput_Category<-#ImplicitQuantityOutput.speciescat.p<-
#   function(temp, ii, category, category0, 
#            baseyr, maxyr, minyr,
#            pctmiss, warnings.list = ls(),
#            MinimumNumberOfSpecies = 1) {
#     
#     maxyr<-max(temp$Year)
#     minyr<-min(temp$Year)
#     
#     temp<-data.frame(temp)
#     temp.cat<-temp[temp[,category0] %in% category, ]
#     # temp.cat<-as.data.table(subset(data2, category.orig==category))
#     # temp.cat<-temp.cat[,.(sum(Pounds), sum(Dollars)),  #aggregate by Year and Tsn
#     #                      keyby= .(Year,Tsn)]
#     
#     temp.cat<-aggregate.data.frame(x = temp.cat[,c("Pounds", "Dollars")], 
#                                    by = list("Year" = temp.cat$Year, "Tsn" = temp.cat$Tsn), 
#                                    FUN = sum, na.rm=T)
#     
#     temp.cat$cat<-category
#     
#     names(temp.cat)[names(temp.cat) %in% "Pounds"]<-"q"
#     names(temp.cat)[names(temp.cat) %in% "Dollars"]<-"v"
#     
#     temp.cat$p<-temp.cat$v/temp.cat$q
#     
#     Year<-c(minyr:maxyr)                   #set array from 1950:2019
#     time<-(Year-(minyr-1))                        #set array from 1 to 70
#     tyear<-as.data.frame(cbind(Year,time)) #create data frame with Year and time 
#     
#     temp.cat<-join(temp.cat, tyear, by="Year", type="inner") #join shellfish and tyear
#     names(temp.cat)[names(temp.cat) %in% "Tsn"]<-"prod"  
#     
#     temp.cat<-subset(temp.cat, p>0 & v>0 & q>0)  #only keep observations with positive p,v,q
#     
#     if (sum(minyr:maxyr %in% unique(temp.cat$Year)) != length(minyr:maxyr)) {
#       
#       #    if (sum(minyr:maxyr %in% unique(temp.cat$Year))/(length(minyr:maxyr))>pctmiss) {
#       #      
#       #      warnings.list<-c(warnings.list, list(paste0("FYI: ", NameBasecategory, " is no longer being calculated because there were no more available columns o P after data was removed for not meeting the pctmiss")))
#       #    }
#       # } else {
#       
#       # if (sum(minyr:maxyr %in% unique(temp.cat$Year))/(length(minyr:maxyr))>pctmiss) {
#       #   warnings.list<-c(warnings.list, list(paste0("FYI: ", NameBasecategory, " is no longer being calculated because there were no more available columns o P after data was removed for not meeting the pctmiss")))
#       # } else {
#       
#       temp0<-data.frame(matrix(data = NA, 
#                                nrow = length(setdiff(x = minyr:maxyr, y = unique(temp.cat$Year))), 
#                                ncol = ncol(temp.cat)))
#       names(temp0)<-names(temp.cat)
#       temp0$Year<-setdiff(x = minyr:maxyr, y = unique(temp.cat$Year))
#       temp0$cat<-category
#       temp0$time<-setdiff(x = ((minyr:maxyr)-(minyr-1)), y = unique(temp.cat$time))
#       temp0$prod<-0
#       
#       temp.cat<-rbind.data.frame(temp.cat, temp0)
#       temp.cat<-temp.cat[order(temp.cat$Year),]
#       
#       #imputed
#       temp.cat<-ReplaceFirst(colnames = c("p", "q", "v"), temp = temp.cat)
#       temp.cat<-ReplaceMid(colnames = c("p", "q", "v"), temp = temp.cat)
#       
#       warnings.list<-c(warnings.list, list(paste0('Warning: ',category,': Error in priceIndex(temp.cat, pvar = "p", qvar = "q", pervar = "time",  : The time period variable is not continuous. ')))
#       # }
#     }
#     
#     temp.ind<-data.frame(tyear)
#     temp.ind$PI_Base_funct<-priceIndex(temp.cat,
#                                        pvar='p',
#                                        qvar='q',
#                                        pervar='time',
#                                        prodID = 'prod',
#                                        sample='matched',
#                                        output='fixedBase',
#                                        indexMethod='Tornqvist')  #This is a fixed base index
#     
#     
#     
#     if ((sum(minyr:maxyr %in% unique(temp.cat$Year)) != length(minyr:maxyr))) {
#       warnings.list<-c(warnings.list, 
#                        list(paste0(category,' with fixed base: ', warnings())))
#     }
#     
#     temp.ind$PI_Chained_funct<-priceIndex(temp.cat,
#                                           pvar='p',
#                                           qvar='q',
#                                           pervar='time',
#                                           prodID = 'prod',
#                                           sample='matched',
#                                           indexMethod='Tornqvist',
#                                           output='chained')      #This is a chain Index
#     
#     
#     
#     if ((sum(minyr:maxyr %in% unique(temp.cat$Year)) != length(minyr:maxyr))) {
#       warnings.list<-c(warnings.list, 
#                        list(paste0(category,' with chained: ', warnings())))
#     }
#     
#     a<-TPI(dat = temp.cat, 
#            Year = "Year", 
#            pvar = "p", 
#            qvar = "q", 
#            vvar = "v", 
#            prodID = "prod")
#     
#     
#     names(a)[names(a) %in% "CPI"]<-"PI_Chained_John"
#     
#     temp.ind<-merge.data.frame(x = a, y = temp.ind, by = "Year")
#     
#     
#     
#     temp.cat0<-aggregate.data.frame(x = temp.cat[,c("v", "q")], 
#                                     by = list("Year" = temp.cat$Year), 
#                                     FUN = sum, na.rm=T)
#     temp.cat0$p<-temp.cat0$v/temp.cat0$q
#     
#     temp.ind<-merge.data.frame(x = temp.cat0, y = temp.ind, by = "Year")
#     
#     # rownames(temp.ind)<-temp.ind$Year
#     # temp.ind$Year<-NULL
#     # temp.ind$time<-NULL
#     temp.ind<-cbind.data.frame(cat = category, 
#                                cat0 = ii, 
#                                temp.ind)
#     
#     temp.cat$cat0<-ii
#     
#     return(list("Index" = temp.ind, 
#                 "Species Level" = temp.cat, 
#                 "warnings.list" = warnings.list))
#   }
# 
# PriceMethodOutput<-#ImplicitQuantityOutput.p<-
#   function(temp, baseyr, pctmiss = 1.00,
#            title0 = "", place = "", MinimumNumberOfSpecies = 2, category0){
#     
#     temp.orig<-data.frame(temp)
#     
#     warnings.list<-list()
#     figures.list<-list()
#     spp.level<-data.frame()
#     index.data<-data.frame()
#     
#     ########Housekeeping
#     # Here I am just going to collect some housekeeping items
#     
#     # NumberOfSpecies<-numbers0(x = c(0, strsplit(x =
#     #                                               strsplit(x = names(temp)[2],
#     #                                                        split = "_")[[1]][2],
#     #                                             split = "[a-zA-Z]")[[1]][1]))[1]
#     
#     category_name<-sort(unique(temp[,category0]))
#     category.rm<-c()
#     
#     maxyr<-max(temp$Year)
#     minyr<-min(temp$Year)
#     
#     for (ii in 1:length(category_name)) {
#       
#         category<-category_name[ii]
#         
#         #if there are still columns to assess that haven't been "removed"
#         # if (length(VColumns) != 0) {
#         ###Append species and category level calculations
#         temp00<-PriceMethodOutput_Category(temp, ii=ii, category = category, category0 = category0, 
#                                               baseyr, maxyr, minyr,
#                                               pctmiss, warnings.list,
#                                               MinimumNumberOfSpecies)
#         
#         index.data<-rbind.data.frame(index.data, 
#                                      temp00$Index)
#         
#         spp.level<-rbind.data.frame(spp.level, 
#                                     temp00$`Species Level`)
#         
#         warnings.list1<-temp00$warnings.list
#         warnings.list1<-unique(warnings.list1)
#         
#         
#         # #If data for a catagory is no longer available after precentmissingthreshold etc, remove it from the category lineup
#         # 
#         # if (sum(names(temp00[1][[1]]) %in% paste0("QI", NameBasecategory)) == 0) {
#         #   category.rm<-c(category.rm, ii)
#         # } else {
#         #   temp1<-temp00[[1]]
#         #   #remove duplicates
#         #   temp1<-temp1[, !(grepl(pattern = "\\.[0-9]+", x = names(temp1)))]
#         #   temp1 <- temp1[, !duplicated(colnames(temp1))]
#         #   temp0<-cbind.data.frame(temp0, temp1)
#         #   
#         #   ###Remove duplicate columns
#         #   temp0<-temp0[, !(grepl(pattern = "\\.[0-9]+", x = names(temp0)))]
#         # }
#         # 
#         # 
#         # # warnings.list1<-QuantityMethodOutput_Category(temp, ii=category[ii],
#         # #                                                                 baseyr, maxyr, minyr,
#         # #                                                                 pctmiss, warnings.list)[[2]]
#         
#         warnings.list<-c(warnings.list, warnings.list1)
#       
#     }
#     
#     
#     #Whole fishery
#     
#     temp.ind0<-data.frame(matrix(data = NA, 
#                                 nrow = length(minyr:maxyr), 
#                                 ncol = ncol(index.data)))
#     names(temp.ind0)<-names(index.data)
#     temp.ind0$Year<-minyr:maxyr
#     temp.ind0$time<-((minyr:maxyr)-(minyr-1))
#     temp.ind0$cat<-"Total"
#     temp.ind0$cat0<-0
#     
#     
#     temp.ind00<-aggregate.data.frame(x = index.data[,c("v", "q")],
#                                     by = list("Year" = index.data$Year),
#                                     FUN = sum, na.rm=T)
#     
#     temp.ind00$p<-temp.ind00$v/temp.ind00$q
#     
#     temp.ind0$v<-temp.ind00$v
#     temp.ind0$q<-temp.ind00$q
#     temp.ind0$p0<-temp.ind00$p
#     
#     
#     ########TOLEDO - am I supposed to find implicit Q by V*PI?
#     
#     # #Implicit V
#     # index.data$v_Chained_John<-index.data$q/index.data$PI_Chained_John
#     # index.data$v_Base_funct<-index.data$q/index.data$PI_Base_funct
#     # index.data$v_Chained_funct<-index.data$q/index.data$PI_Chained_funct
#     
#     #Implicit Q
#     index.data$Q_Chained_John<-index.data$v*index.data$PI_Chained_John
#     index.data$Q_Base_funct<-index.data$v*index.data$PI_Base_funct
#     index.data$Q_Chained_funct<-index.data$v*index.data$PI_Chained_funct
#     
#     
#     temp.ind0$PI_Base_funct<-priceIndex(index.data,
#                                        pvar='PI_Base_funct',
#                                        qvar='q', # this might just need to be "Q_Base_funct"
#                                        pervar='time',
#                                        prodID = 'cat',
#                                        sample='matched',
#                                        output='fixedBase',
#                                        indexMethod='Tornqvist')  #This is a fixed base index
#     
#     
#     temp.ind0$PI_Chained_funct<-priceIndex(index.data,
#                                           pvar='PI_Chained_funct',
#                                           qvar='q', # this might just need to be "Q_Chained_John"
#                                           pervar='time',
#                                           prodID = 'cat',
#                                           sample='matched',
#                                           indexMethod='Tornqvist',
#                                           output='chained')      #This is a chain Index
#     
#     
#     names(index.data)[names(index.data) %in% "p"]<-"p0"
#     a<-TPI(dat = index.data, 
#            Year = "Year", 
#            pvar = "PI_Chained_John", 
#            qvar = "q",  # this might just need to be "Q_Chained_John"
#            vvar = "v", 
#            prodID = "cat")
#     
#     temp.ind0$PI_Chained_John<-a$CPI[-nrow(a)]
#     
#     # temp.ind0$p<-temp.ind0$p0
#     temp.ind0$p<-NULL
#     
#     ########TOLEDO - am I supposed to find implicit Q by V*PI?
#     
#     # #Implicit V
#     # index.data$v_Chained_John<-index.data$q/index.data$PI_Chained_John
#     # index.data$v_Base_funct<-index.data$q/index.data$PI_Base_funct
#     # index.data$v_Chained_funct<-index.data$q/index.data$PI_Chained_funct
#     
#     #Implicit Q
#     temp.ind0$Q_Chained_John<-temp.ind0$v*temp.ind0$PI_Chained_John
#     temp.ind0$Q_Base_funct<-temp.ind0$v*temp.ind0$PI_Base_funct
#     temp.ind0$Q_Chained_funct<-temp.ind0$v*temp.ind0$PI_Chained_funct
#     
#     temp.ind0<-temp.ind0[, match(table = names(temp.ind0), x = names(index.data))]
#     
#     index.data<-rbind.data.frame(index.data, temp.ind0)
# 
#     
# 
#     
#     ##########Make plots#########
#     
#     place<-title0
#     
#     NOAALightBlue<-"#C9E1E6"
#     NOAADarkBlue<-"#0098A6"
#     NOAADarkGrey<-"#56575A" #text
#     NOAABlueScale<-colorRampPalette(colors = c(NOAALightBlue, NOAADarkBlue))
#     
#     #############Compare Price Indexes for Total and Each Category
#     
#     for (i in 1:length(unique(index.data$cat))) {
#       
#       title00<-paste0("_PI_", unique(index.data$cat)[i])
#       
#       a0<-index.data[index.data$cat %in% unique(index.data$cat)[i],
#                         c("Year", "PI_Chained_John", "PI_Base_funct", "PI_Chained_funct")]
#       
#       a <- gather(a0, cat, val, c("PI_Chained_John", "PI_Base_funct", "PI_Chained_funct"), factor_key=TRUE)
#       
#       g<-plotnlines(dat = a, title00, place)
#       
#       figures.list[[length(figures.list)+1]]<-g
#       names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#       
#     }
#     
#     #############Compare each type of Price Indexes across Each Category
#     for (i in c("PI_Chained_John", "PI_Base_funct", "PI_Chained_funct") ) {
#       
#       title00<-paste0("_", i)
#       
#       a0<-index.data[,
#                      c("Year", "cat", i)]
#       names(a0)[3]<-"val"
#       
#       a<-a0
#       
#       g<-plotnlines(dat = a, title00, place)
#       
#       figures.list[[length(figures.list)+1]]<-g
#       names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#       
#     }
#     
#     #############Compare each type of Price Indexes across Each Category
#     for (i in c("q", "Q_Chained_John", "Q_Base_funct", "Q_Chained_funct") ) {
#       
#       title00<-paste0("_", i)
#       
#       a0<-index.data[,
#                      c("Year", "cat", i)]
#       names(a0)[3]<-"val"
#       
#       a<-a0
#       
#       g<-plotnlines(dat = a, title00, place)
#       
#       figures.list[[length(figures.list)+1]]<-g
#       names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#       
#     }
#     
#     #############Plot Category and Total Q
#     title00<-"_Q_CatTot"
#     
#     a<-index.data[,c("Year", "cat", "q")]
#     names(a)[3]<-"val"
#     
#     g<-plotnlines(dat = a, title00, place)
#     
#     figures.list[[length(figures.list)+1]]<-g
#     names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#     
#     #############Plot Category and Total Q
#     title00<-"_Q_Chained_John_CatTot"
#     
#     a<-index.data[,c("Year", "cat", "Q_Chained_John")]
#     names(a)[3]<-"val"
#     
#     g<-plotnlines(dat = a, title00, place)
#     
#     figures.list[[length(figures.list)+1]]<-g
#     names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#     
#     #############Plot Category and Total Q
#     title00<-"_Q_Base_funct_CatTot"
#     
#     a<-index.data[,c("Year", "cat", "Q_Base_funct")]
#     names(a)[3]<-"val"
#     
#     g<-plotnlines(dat = a, title00, place)
#     
#     figures.list[[length(figures.list)+1]]<-g
#     names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#     
#     #############Plot Category and Total Q
#     title00<-"_Q_Chained_funct_CatTot"
#     
#     a<-index.data[,c("Year", "cat", "Q_Chained_funct")]
#     names(a)[3]<-"val"
#     
#     g<-plotnlines(dat = a, title00, place)
#     
#     figures.list[[length(figures.list)+1]]<-g
#     names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#     
#     #############Plot Category and Total V
#     title00<-"_V_CatTot"
#     
#     a<-index.data[,c("Year", "cat", "v")]
#     names(a)[3]<-"val"
#     
#     g<-plotnlines(dat = a, title00, place)
#     
#     figures.list[[length(figures.list)+1]]<-g
#     names(figures.list)[length(figures.list)]<-paste0(title0, title00)
#     
#     
#     
#     #############Save Wrok
#     return(list("index.data" = index.data, 
#                 "warnings.list" = warnings.list, 
#                 "figures.list" = figures.list))
#     
#   }










# ########*** Price Driven Analysis - category.orig############
# analysisby = "P"
# category0 = "category.orig"
# pctmiss = 0.60
# MinimumNumberOfSpecies = 10
# baseyr<-2007
# 
# # # Data for the whole Time Series
# # OutputAnalysis(landings.data, category0, baseyr,
# #                state.codes, titleadd = paste("1950To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
# #                MinimumNumberOfSpecies)
# # 
# # # Data just from the last 20 years
# # OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
# #                category0, baseyr,
# #                state.codes, titleadd = paste0("1997To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
# #                MinimumNumberOfSpecies)
# # 
# # #Data just from the last 10 years
# # OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,], 
# #                category0, baseyr, 
# #                state.codes, titleadd = paste0("2007To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
# #                MinimumNumberOfSpecies = 6) 
# 
# 
# # analysisby = "P"
# # category0 = "category.orig"
# # pctmiss = 0.95
# # MinimumNumberOfSpecies = 2
# # 
# # # Data for the whole Time Series
# # OutputAnalysis(landings.data, category0, baseyr,
# #                state.codes, titleadd = paste0("1950To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby)
# # 
# # #Data just from the last 20 years
# # OutputAnalysis(landings.data = landings.data[landings.data$Year>1997,], 
# #                category0, baseyr, 
# #                state.codes, titleadd = paste0("1997To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby) 
# 
# 
# # #Data just since 2008
# # OutputAnalysis(landings.data[landings.data$Year>=2007,], category0, baseyr, 
# #                state.codes, titleadd = "2007ToPresent",
# #                counter, dir.rawdata, pctmiss = pctmiss, 
# #                MinimumNumberOfSpecies)
# 
# # 
# # #Data for the whole timeseries with no pctmiss
# # OutputAnalysis(landings.data, category0, baseyr, 
# #                state.codes, titleadd = "WholeTimeseries1",
# #                counter, dir.rawdata, pctmiss = 1.00,
# #                #MinimumNumberOfSpecies = 6) 
# 
# 
# 
# 
# category0 = "category.origFSO"
# MinimumNumberOfSpecies = 1
# 
# a<-landings.data
# a$category.origFSO<-a$category.orig
# 
# ###Without Other
# b<-a
# a<-a[!(a$category.origFSO %in% "Other"),]
# 
# 
# # Data for the whole Time Series
# OutputAnalysis(landings.data = a, 
#                category0, baseyr,
#                state.codes, titleadd = paste0("1950To", maxyr, "_FS"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = a[a$Year>=1997,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("1997To", maxyr, "_FS"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = a[a$Year>=2007,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("2007To", maxyr, "_FS"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# 
# ### Data just with key 10 species
# #
# #For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 
# 
# #Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 
# 
# #method to get QI you can do both as for the 10 species case. 
# 
# a<-b
# cc<-data.frame()
# tsn.id0<-data.frame()
# for (i in 1:length(unique(a$Region))) {
#   place<-unique(a$Region)[i]
#   tempdat<-a[a$Region %in% place,]
#   tempdat$TSN<-tempdat$Tsn
#   
#   temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
#                         categories = spcat.list[place][[1]], 
#                         missing.name="Uncategorized")
#   
#   tsn.id<-data.frame(temp$df.out)
#   tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
#   tsn.id<-cbind.data.frame(tsn.id, 
#                            "area" = place)
#   
#   tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
#   cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
# }
# a<-cc
# category0 = "category.origFSO"
# MinimumNumberOfSpecies = 1
# 
# OutputAnalysis(landings.data = a, 
#                category0, baseyr,
#                state.codes, titleadd = paste0("1950To", maxyr, "_FSKey"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = a[a$Year>=1997,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("1997To", maxyr, "_FSKey"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = a[a$Year>=2007,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("2007To", maxyr, "_FSKey"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)


########*** Price Driven Analysis - category.tax############
analysisby = "P"
category0 = "category.tax"
pctmiss = 0.60
MinimumNumberOfSpecies = 10
baseyr<-2007

# Data for the whole Time Series
OutputAnalysis(landings.data = landings.data, 
               titleadd = paste0("1950To", maxyr, "_AllTax"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_AllTax"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

#Data just from the last 10 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,],
               titleadd = paste0("2007To", maxyr, "_AllTax"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = 6)

# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00,
#                MinimumNumberOfSpecies)




category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1

a<-landings.data
a$category.taxFSO<-a$category.taxsimp
a$category.taxFSO[a$category.taxFSO %in% c("Other Fish", "Chondrichthyes", "Actinopterygii")]<-"Finfish"
a$category.taxFSO[a$category.taxFSO %in% c("Mollusca","Arthropoda","Echinodermata")]<-"Shellfish"
a$category.taxFSO[a$category.taxFSO %in% c("Other", "Tetrapoda")]<-"Other"


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               titleadd = paste0("1950To", maxyr, "_FSO"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_FSO"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               state.codes, titleadd = paste0("2007To", maxyr, "_FSO"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = 6)


###Without Other
b<-a
a<-a[!(a$category.taxFSO %in% "Other"),]


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               titleadd = paste0("1950To", maxyr, "_FS"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_FS"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               titleadd = paste0("2007To", maxyr, "_FS"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)


### Data just with key 10 species
#
#For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 

#Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 

#method to get QI you can do both as for the 10 species case. 

a<-b
cc<-data.frame()
tsn.id0<-data.frame()
for (i in 1:length(unique(a$Region))) {
  place<-unique(a$Region)[i]
  tempdat<-a[a$Region %in% place,]
  tempdat$TSN<-tempdat$Tsn
  
  temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
                        categories = spcat.list[place][[1]], 
                        missing.name="Uncategorized")
  
  tsn.id<-data.frame(temp$df.out)
  tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
  tsn.id<-cbind.data.frame(tsn.id, 
                           "area" = place)
  
  tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
  cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
}
a<-cc
category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1


OutputAnalysis(landings.data =a, 
               titleadd = paste0("1950To", maxyr, "_FSKey"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_FSKey"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               titleadd = paste0("2007To", maxyr, "_FSKey"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# ########*** Quantity Driven Analysis - category.orig############
# analysisby = "Q"
# category0 = "category.orig"
# pctmiss = 0.60
# MinimumNumberOfSpecies = 10
# baseyr<-2007
# 
# # # Data for the whole Time Series
# # OutputAnalysis(landings.data, category0, baseyr,
# #                state.codes, titleadd = paste0("1950To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
# #                MinimumNumberOfSpecies)
# # 
# # # Data just from the last 20 years
# # OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
# #                category0, baseyr,
# #                state.codes, titleadd = paste0("1997To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
# #                MinimumNumberOfSpecies)
# # 
# # #Data just from the last 10 years
# # OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,], 
# #                category0, baseyr, 
# #                state.codes, titleadd = paste0("2007To", maxyr),
# #                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
# #                MinimumNumberOfSpecies = 6)  
# # 
# # #Data for the whole timeseries with no pctmiss
# # OutputAnalysis(landings.data, category0, baseyr, 
# #                state.codes, titleadd = "WholeTimeseries1",
# #                counter, dir.rawdata, pctmiss = 1.00) 
# category0 = "category.origFSO"
# MinimumNumberOfSpecies = 1
# 
# 
# a<-landings.data
# a$category.origFSO<-a$category.orig
# 
# ###Without Other
# b<-a
# a<-a[!(a$category.origFSO %in% "Other"),]
# 
# 
# # Data for the whole Time Series
# OutputAnalysis(landings.data = a, 
#                category0, baseyr,
#                state.codes, titleadd = paste0("1950To", maxyr, "_FS"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = a[a$Year>=1997,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("1997To", maxyr, "_FS"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# OutputAnalysis(landings.data = a[a$Year>=2007,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("2007To", maxyr, "_FS"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# 
# ### Data just with key 10 species
# #
# #For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 
# 
# #Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 
# 
# #method to get QI you can do both as for the 10 species case. 
# 
# a<-b
# cc<-data.frame()
# tsn.id0<-data.frame()
# for (i in 1:length(unique(a$Region))) {
#   place<-unique(a$Region)[i]
#   tempdat<-a[a$Region %in% place,]
#   tempdat$TSN<-tempdat$Tsn
#   
#   temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
#                         categories = spcat.list[place][[1]], 
#                         missing.name="Uncategorized")
#   
#   tsn.id<-data.frame(temp$df.out)
#   tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
#   tsn.id<-cbind.data.frame(tsn.id, 
#                            "area" = place)
#   
#   tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
#   cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
# }
# a<-cc
# category0 = "category.origFSO"
# MinimumNumberOfSpecies = 1
# 
# OutputAnalysis(landings.data = a, 
#                category0, baseyr,
#                state.codes, titleadd = paste0("1950To", maxyr, "_FSKey"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
#                MinimumNumberOfSpecies)
# 
# # Data just from the last 20 years
# OutputAnalysis(landings.data = a[a$Year>=1997,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("1997To", maxyr, "_FSKey"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)
# 
# OutputAnalysis(landings.data = a[a$Year>=2007,],
#                category0, baseyr,
#                state.codes, titleadd = paste0("2007To", maxyr, "_FSKey"),
#                counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
#                MinimumNumberOfSpecies)

########*** Quantity Driven Analysis - category.tax############
analysisby = "Q"
category0 = "category.tax"
pctmiss = 0.60
MinimumNumberOfSpecies = 10
baseyr<-2007

# Data for the whole Time Series
OutputAnalysis(landings.data =landings.data, 
               titleadd = paste0("1950To", maxyr, "_AllTax"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_AllTax"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# #Data just from the last 10 years
OutputAnalysis(landings.data = landings.data[landings.data$Year>=2007,],
               titleadd = paste0("2007To", maxyr, "_AllTax"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = 6)

# #Data for the whole timeseries with no pctmiss
# OutputAnalysis(landings.data, category0, baseyr,
#                state.codes, titleadd = "WholeTimeseries1",
#                counter, dir.rawdata, pctmiss = 1.00)


category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1


a<-landings.data
a$category.taxFSO<-as.character(a$category.taxsimp)
a$category.taxFSO[a$category.taxFSO %in% c("Other Fish", "Chondrichthyes", "Actinopterygii")]<-"Finfish"
a$category.taxFSO[a$category.taxFSO %in% c("Mollusca","Arthropoda","Echinodermata")]<-"Shellfish"
a$category.taxFSO[a$category.taxFSO %in% c("Other", "Tetrapoda")]<-"Other"


# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               titleadd = paste0("1950To", maxyr, "_FSO"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_FSO"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               titleadd = paste0("2007To", maxyr, "_FSO"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)


###Without Other
b<-a
a<-a[!(a$category.taxFSO %in% "Other"),]


# Data for the whole Time Series
OutputAnalysis(landings.data =a, 
               titleadd = paste0("1950To", maxyr, "_FS"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               titleadd = paste0("1997To", maxyr, "_FS"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               titleadd = paste0("2007To", maxyr, "_FS"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)


### Data just with key 10 species
#
#For 10 key species, just to use those 10 species to calculate both quantity method QI. For price method please calculate PI using those 10 species first, then use both value sum of 10 species/PI, and total landing value/PI to get two column of QI 

#Regarding other category, if you are using Quantity method you don't need to include other in the aggregate QI estimate. If you are using price 

#method to get QI you can do both as for the 10 species case. 

a<-b
cc<-data.frame()
tsn.id0<-data.frame()
for (i in 1:length(unique(a$Region))) {
  place<-unique(a$Region)[i]
  tempdat<-a[a$Region %in% place,]
  tempdat$TSN<-tempdat$Tsn
  
  temp<-itis_reclassify(tsn = as.numeric(paste(unique(tempdat$TSN))), 
                        categories = spcat.list[place][[1]], 
                        missing.name="Uncategorized")
  
  tsn.id<-data.frame(temp$df.out)
  tsn.id<-tsn.id[!(tsn.id$category %in% c("Uncategorized", "Other")),]
  tsn.id<-cbind.data.frame(tsn.id, 
                           "area" = place)
  
  tsn.id0<-rbind.data.frame(tsn.id0, tsn.id)
  cc<-rbind.data.frame(cc, tempdat[tempdat$Tsn %in% tsn.id$TSN, ])
}
a<-cc
category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1


OutputAnalysis(landings.data =a, 
               titleadd = paste0("1950To", maxyr, "_FSKey"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

# Data just from the last 20 years
OutputAnalysis(landings.data = data.frame(a[a$Year>=1997,]),
               titleadd = paste0("1997To", maxyr, "_FSKey"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)

OutputAnalysis(landings.data = data.frame(a[a$Year>=2007,]),
               titleadd = paste0("2007To", maxyr, "_FSKey"),
               category0 = category0, 
               baseyr = baseyr,
               state.codes = state.codes, 
               counter = counter, 
               dir.rawdata = dir.rawdata, 
               dir.analyses = dir.analyses, 
               pctmiss = pctmiss, 
               analysisby = analysisby,
               MinimumNumberOfSpecies = MinimumNumberOfSpecies)


##############*** Northeast##############

#######KNOWNS
maxyr0 <- 2019
minyr <- 2007
NMFSorITIS<-"ITISTax"

#Functions common to all sections, to help instill standardization
# Common.Funct<-paste0(dirname(getwd()), "/FEUS/2018/FEUS",maxyr,"Common/rscripts/Common_Functions.r")
# source(Common.Funct)
Comm.SppData<-paste0(dir.scripts, "ProductivityIndex_Species.R")
dir.data0<-dir.data
dir.data<-paste0(dirname(getwd()), "/FEUS/2018/FEUS2018Commercial/data/")
dir.data.common<-paste0(dirname(getwd()), "/FEUS/2018/FEUS2018Common/data/")
Comm.Data<-paste0(dirname(getwd()), "/FEUS/2018/FEUS2018Commercial/rscripts/Commercial_Data.R")
dir.nore<-paste0(dir.out,"/analyses/",minyr,"To",maxyr0,"_Fisheries_Northeast/")
create_dir(dir.nore)
#Download new data
# source(Comm.DataDL)

#Data specific to this section
source(Comm.Data)
source(Comm.SppData)
dir.data<-dir.data0


spcat<-c()
spcat0<-read.csv(paste0(dir.data, "/specodes_W_names.csv"))

#Find index of where these species can be found in spcat_list$General
for (i in 1:nrow(spcat0)) {
  spcat<-c(spcat, 
           ifelse(sum(grepl(pattern = spcat0$Name[i], x = names(spcat.list$General) , ignore.case = T)) == 0, 
                  NA, grep(pattern = spcat0$Name[i], x = names(spcat.list$General), ignore.case = T) ))
}

spcat1<-spcat.list$General[spcat]
names(spcat1)<-spcat0$Name

spcat00<-data.frame("TSN" = unlist(spcat.list$General[spcat]), 
                    spcat0)

write.xlsx(spcat00, file = paste0(dir.nore, "NortheastData.xlsx"), sheetName = "AskedFor", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

spcat.list<-list()
spcat.list$Areas<-list()
spcat.list$Areas$Northeast<-spcat1



landings.data.ne<-data.frame("State1" = landings.data$State, 
                           "State" = landings.data$State, 
                           "TSN" = landings.data$Tsn, 
                           "year" = landings.data$Year, 
                           "POUNDS" = landings.data$Pounds, 
                           "DOLLARS"=landings.data$Dollars, 
                           "CommonName" = landings.data$AFS.Name, 
                           "Region" = landings.data$Region, 
                           "category" = landings.data$category.orig, 
                           "OFS" = substr(x = landings.data$category.orig, 1,1), 
                           "abbvst" = landings.data$abbvst, 
                           "abbvreg" = landings.data$abbvreg, 
                           "xstate" = landings.data$xstate, 
                           "xreg" = landings.data$xreg, 
                           "State.no" = landings.data$State.no, 
                           "Region.no" = landings.data$Region.no, 
                           "fips" = landings.data$fips, 
                           "AFS_NAME1" = landings.data$AFS_NAME1, 
                           "AFS.Name" = landings.data$AFS.Name,                           
                           "TS_SCIENTIFIC_NAME" = landings.data$TS_SCIENTIFIC_NAME)


landings.data.ne<-landings.data.ne[landings.data.ne$Region %in% c("New England", "Mid-Atlantic"),]

spp.cat<-itis_reclassify(tsn = unique(landings.data.ne$TSN), 
                        categories = spcat.list$Areas$Northeast, 
                         missing.name="Uncategorized")

spp.cat$df.out$categoryNEFishery<-spp.cat$df.out$category
spp.cat$df.out$categoryNEFishery.1<-as.numeric(factor(spp.cat$df.out$categoryNEFishery))
landings.data.ne<-merge(x = landings.data.ne, y = spp.cat$df.out, by.y = "TSN", by.x = "TSN")

# statereg<-read.csv(file = paste0(dir.data, "statereg.csv"))
# landings.data.ne<-merge(x = landings.data.ne, y = statereg, by = "State.no")


# source(ProdI.Funct) # overwrite functions from Comm.SppData

a<-spp_reclassify(landings.df = landings.data.ne, 
                  spcat.list = spcat.list, 
                  place = "Northeast")


spcat0$Name<-tolower(spcat0$Name)

for (jjj in 1:length(a)) {
  aa<-a[jjj][[1]]
  if (sum(names(aa) %in% "keyspecies")>0) {
    aa$keyspecies<-tolower(aa$keyspecies)
    aa<-merge(x = aa, y = spcat0, by.x = "keyspecies", by.y = "Name")
  }
  a[jjj][[1]]<-aa
  assign(names(a)[jjj], a[[jjj]])
  write.xlsx(a[[jjj]], file = paste0(dir.nore, "NortheastData.xlsx"), sheetName = names(a)[jjj], 
             col.names = TRUE, row.names = TRUE, append = TRUE)
}

# revenue$keyspecies<-tolower(revenue$keyspecies)
# spcat0$Name<-tolower(spcat0$Name)
# FisherySums<-merge(x = revenue, y = spcat0, by.x = "keyspecies", by.y = "Name")
FisherySums<-revenue
FisherySums<-aggregate.data.frame(x = sapply(X = FisherySums[,as.character(minyr:maxyr0)], FUN = as.numeric), 
                                      by = list("PLAN" = FisherySums$PLAN),
                                      FUN = sum, na.rm = T)

FisherySums$Total<-rowSums(FisherySums[,as.character(minyr:maxyr0)])

FisherySums <- rbind.data.frame(FisherySums, 
                                    cbind.data.frame("PLAN" = "Fisheries Total", 
                                                     t(colSums(FisherySums[,c(as.character(minyr:maxyr0), "Total")]))), 
                                    cbind.data.frame("PLAN" = a$revenue$keyspecies[1:4], 
                                                     a$revenue[1:4,as.character(minyr:maxyr0)], 
                                                     "Total" = rowSums(sapply(X = a$revenue[1:4,as.character(minyr:maxyr0)], 
                                                                              FUN = as.numeric))))

write.xlsx(FisherySums, file = paste0(dir.nore, "NortheastData.xlsx"), 
           sheetName = "simplesum_agg_fishery_revenue", 
           col.names = TRUE, row.names = TRUE, append = TRUE)


colnames0<-c("Analysis", "Variable", as.character(1950:maxyr), "Total")
FisherySums0<-data.frame(t(rep_len(x = NA, length.out = length(colnames0))))
names(FisherySums0)<-colnames0
FisherySums<-FisherySums0[-1,]

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = T)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "2008", x = fold))]

for (i in 1:length(fold)){
  fold0<-list.files(path = paste0(fold[i], "/outputtables"), full.names = T, pattern = "000_All_")
  if (!(length(fold0) %in% 0)) {
  fold0<-fold0[grep(pattern = "_Review", x = fold0)]
  fold00<-list.files(path = paste0(fold[i], "/outputtables"), pattern = "000_All_")
  fold00<-fold00[grepl(pattern = "_Review", x = fold00)]
  
  # for (ii in 1:length(length( excel_sheets( fold0 ) ))){
    temp <- read.xlsx2(file = fold0, sheetName = "Northeast")

    # temp000<-data.frame(t(rep_len(x = NA, length.out = length(1950:maxyr))))
    # names(temp000)<-as.character(1950:maxyr)
    # temp000<-temp000[-1,]
    
    temp00<-data.frame(t(as.numeric(temp$V_Total)))
    names(temp00)<-temp[,1]
    
    yrdiff<-setdiff(y = as.numeric(temp[,1]), 
                    x = as.numeric(1950:maxyr))
    
    if (length(yrdiff)>0) {
      temp0000<-data.frame(t(rep_len(x = NA, length.out = length(yrdiff))))
      names(temp0000)<-yrdiff
      temp00<-cbind.data.frame(temp0000, temp00)
      temp00<-temp00[,match(table = 1950:maxyr, x = names(temp00))]
    }
    
    temp0<- data.frame("Analysis" = fold00, 
                       "Variable" = "V_Total",
                        temp00, 
                        "Total" = rowSums(temp00))  
    names(temp0)<-c("Analysis", "Variable", names(temp00), "Total")
    
        
    if (sum(names(temp) %in% "VV_Total")==1) {
      
      temp00<-data.frame(t(as.numeric(temp$VV_Total)))
      names(temp00)<-temp[,1]
      
      if (length(yrdiff)>0) {
        temp0000<-data.frame(t(rep_len(x = NA, length.out = length(yrdiff))))
        names(temp0000)<-yrdiff
        temp00<-cbind.data.frame(temp0000, temp00)
        temp00<-temp00[,match(table = 1950:maxyr, x = names(temp00))]
      }
      
      tempQ<- data.frame("Analysis" = fold00, 
                         "Variable" = "VV_Total",
                         temp00, 
                         "Total" = rowSums(temp00))  
      names(tempQ)<-c("Analysis", "Variable", names(temp00), "Total")
      
      temp00<-temp00[,match(table = 1950:maxyr, x = names(temp00))]
      
      temp0<-rbind.data.frame(temp0, tempQ)
    }
    
    names(temp0)<-names(FisherySums)
    FisherySums<-rbind.data.frame(FisherySums, temp0)
  }
}

rownames(FisherySums)<-NULL


FisherySums.rev<-FisherySums

write.xlsx(FisherySums.rev, file = paste0(dir.nore, "NortheastData.xlsx"), 
           sheetName = "FS_FSO_FSKey_analysis_revenue", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

########Landings
FisherySums<-landings
FisherySums<-aggregate.data.frame(x = sapply(X = FisherySums[,as.character(minyr:maxyr0)], FUN = as.numeric), 
                                  by = list("PLAN" = FisherySums$PLAN),
                                  FUN = sum, na.rm = T)

FisherySums$Total<-rowSums(FisherySums[,as.character(minyr:maxyr0)])

FisherySums <- rbind.data.frame(FisherySums, 
                                cbind.data.frame("PLAN" = "Fisheries Total", 
                                                 t(colSums(FisherySums[,c(as.character(minyr:maxyr0), "Total")]))), 
                                cbind.data.frame("PLAN" = a$revenue$keyspecies[1:4], 
                                                 a$revenue[1:4,as.character(minyr:maxyr0)], 
                                                 "Total" = rowSums(sapply(X = a$revenue[1:4,as.character(minyr:maxyr0)], 
                                                                          FUN = as.numeric))))

write.xlsx(FisherySums, file = paste0(dir.nore, "NortheastData.xlsx"), 
           sheetName = "simplesum_agg_fishery_landings", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

# landings$keyspecies<-tolower(landings$keyspecies)
# spcat0$Name<-tolower(spcat0$Name)
# FisherySums<-merge(x = landings, y = spcat0, by.x = "keyspecies", by.y = "Name")

colnames0<-c("Analysis", "Variable", as.character(1950:maxyr), "Total")
FisherySums0<-data.frame(t(rep_len(x = NA, length.out = length(colnames0))))
names(FisherySums0)<-colnames0
FisherySums<-FisherySums0[-1,]


fold<-list.files(paste0(dir.out, "/analyses/"), full.names = T)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "2008", x = fold))]

for (i in 1:length(fold)){
  fold0<-list.files(path = paste0(fold[i], "/outputtables"), full.names = T, pattern = "000_All_")
  if (!(length(fold0) %in% 0)) {
    fold0<-fold0[grep(pattern = "_Review", x = fold0)]
    fold00<-list.files(path = paste0(fold[i], "/outputtables"), pattern = "000_All_")
    fold00<-fold00[grepl(pattern = "_Review", x = fold00)]
    
    # for (ii in 1:length(length( excel_sheets( fold0 ) ))){
    temp <- read.xlsx2(file = fold0, sheetName = "Northeast")
    
    # temp000<-data.frame(t(rep_len(x = NA, length.out = length(1950:maxyr))))
    # names(temp000)<-as.character(1950:maxyr)
    # temp000<-temp000[-1,]
    
    temp00<-data.frame(t(as.numeric(temp$QI_Total)))
    names(temp00)<-temp[,1]
    
    yrdiff<-setdiff(y = as.numeric(temp[,1]), 
                    x = as.numeric(1950:maxyr))
    
    if (length(yrdiff)>0) {
      temp0000<-data.frame(t(rep_len(x = NA, length.out = length(yrdiff))))
      names(temp0000)<-yrdiff
      temp00<-cbind.data.frame(temp0000, temp00)
      temp00<-temp00[,match(table = 1950:maxyr, x = names(temp00))]
    }
    
    temp0<- data.frame("Analysis" = fold00, 
                       "Variable" = "QI_Total",
                       temp00, 
                       "Total" = rowSums(temp00))  
    names(temp0)<-c("Analysis", "Variable", names(temp00), "Total")
    
    
    if (sum(names(temp) %in% "Q_Total")==1) {
      
      temp00<-data.frame(t(as.numeric(temp$VV_Total)))
      names(temp00)<-temp[,1]
      
      if (length(yrdiff)>0) {
        temp0000<-data.frame(t(rep_len(x = NA, length.out = length(yrdiff))))
        names(temp0000)<-yrdiff
        temp00<-cbind.data.frame(temp0000, temp00)
        temp00<-temp00[,match(table = 1950:maxyr, x = names(temp00))]
      }
      
      tempQ<- data.frame("Analysis" = fold00, 
                         "Variable" = "Q_Total",
                         temp00, 
                         "Total" = rowSums(temp00))  
      names(tempQ)<-c("Analysis", "Variable", names(temp00), "Total")
      
      temp00<-temp00[,match(table = 1950:maxyr, x = names(temp00))]
      
      temp0<-rbind.data.frame(temp0, tempQ)
    }
    
    names(temp0)<-names(FisherySums)
    FisherySums<-rbind.data.frame(FisherySums, temp0)
  }
}

rownames(FisherySums)<-NULL
FisherySums.land<-FisherySums

write.xlsx(FisherySums.land, file = paste0(dir.nore, "NortheastData.xlsx"), 
           sheetName = "FS_FSO_FSKey_analysis_landings", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

reg.order<-"Northeast"
analysisby = "P"
pctmiss = 0.60
MinimumNumberOfSpecies = 1
baseyr<-2007
a<-landings.data.ne
category0<-"categoryNEFishery"

# names(landings.data)[names(landings.data) %in% "S_STATE_ID"]<-"State.no"
# a$AFS_NAME1<-a$CommonName
names(a)[names(a) %in% "TSN"]<-"Tsn"
# a$TSN[is.na(a$H_NEW_ITIS)]<-a$H_NEW_ITIS[is.na(a$H_NEW_ITIS)]
# a$H_NEW_ITIS<-NULL
# names(a)[names(a) %in% "TS_AFS_NAME"]<-"AFS.Name"
names(a)[names(a) %in% "year"]<-"Year"
names(a)[names(a) %in% "POUNDS"]<-"Pounds"
names(a)[names(a) %in% "DOLLARS"]<-"Dollars"
# names(landings.data)[names(landings.data) %in% "COLLECTION"]<-"Collection"

# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_NEFish"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies, 
               reg.order = "Northeast", reg.order0 = 1)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_NEFish"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies, 
               reg.order = "Northeast", reg.order0 = 1)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_NEFish"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies, 
               reg.order = "Northeast", reg.order0 = 1)

analysisby = "Q"

# Data for the whole Time Series
OutputAnalysis(landings.data = a, 
               category0, baseyr,
               state.codes, titleadd = paste0("1950To", maxyr, "_NEFish"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby, 
               MinimumNumberOfSpecies, 
               reg.order = "Northeast", reg.order0 = 1)

# Data just from the last 20 years
OutputAnalysis(landings.data = a[a$Year>=1997,],
               category0, baseyr,
               state.codes, titleadd = paste0("1997To", maxyr, "_NEFish"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies, 
               reg.order = "Northeast", reg.order0 = 1)

OutputAnalysis(landings.data = a[a$Year>=2007,],
               category0, baseyr,
               state.codes, titleadd = paste0("2007To", maxyr, "_NEFish"),
               counter, dir.rawdata, pctmiss = pctmiss, analysisby = analysisby,
               MinimumNumberOfSpecies, 
               reg.order = "Northeast", reg.order0 = 1)

#######*** Summary Files##########
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico") 

dir.sum<-paste0(dir.out,"/analyses/SummaryFiles/")
create_dir(dir.sum)

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = FALSE)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "SummaryFiles", x = fold))]
fold<-fold[-(grep(pattern = "nobother", x = fold))]
fold<-fold[-(grep(pattern = "2009To2018_FS_P_categorytaxFSO_pctmiss60", x = fold))]

fold<-strsplit(x = fold, split = "_")
yearrange<-unique(unlist(lapply(fold, `[[`, 1)))
category0<-unique(unlist(lapply(fold, `[[`, 2)))
methods0<-unique(unlist(lapply(fold, `[[`, 3)))

fold<-list.files(paste0(dir.out, "/analyses/"), full.names = T)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "SummaryFiles", x = fold))]
fold<-fold[-(grep(pattern = "nobother", x = fold))]
fold<-fold[-(grep(pattern = "2009To2018_FS_P_categorytaxFSO_pctmiss60", x = fold))]

summarydata<-list()

for (yrange0 in 1:length(yearrange)) {
  
  # for (cat0 in 1:length(category0)) {
  
  fold0<-fold[#grepl(pattern = reg.order[reg0], x = fold) & 
    # grepl(pattern = category0[cat0], x = fold) & 
    grepl(pattern = yearrange[yrange0], x = fold)]
  
  
  for (reg0 in 1:length(reg.order)) {
    
    for (i in 1:length(fold0)){
      file00<-list.files(path = paste0(fold0[i], "/outputtables"), full.names = T, pattern = "000_All_")
      if (!(length(file00) %in% 0)) {
        file0<-file00[grep(pattern = "_Review", x = file00)]      
        
        # for (ii in 1:length(length( excel_sheets( fold0 ) ))){
        temp <- read.xlsx2(file = file0, sheetIndex = reg.order[reg0])
        rownames(temp)<-temp[,1]
        
        if (i==1) {
          temp0<-rbind.data.frame(data.frame("Year" = as.numeric(temp$X.), 
          "QE" = temp$QE_Total,
          "QEI" = temp$QEI_Total))
        }            
        
        if (sum(names(temp) %in% c("QE_Total", "QEI_Total", "QI_Total", "Q_Total", "PI_Total")) != 0) {
          temp00<-data.frame(temp[,names(temp) %in% c("QI_Total", "Q_Total", "PI_Total")])
          names(temp00)<-names(temp)[names(temp) %in% c("QI_Total", "Q_Total", "PI_Total")]
          names(temp00)<-gsub(pattern = "_Total", replacement = "", x = names(temp00))
          names(temp00)<-paste0(names(temp00), " (", 
                                ifelse(grepl(pattern = "_P_", x = file0), "Price", "Quantity"), 
                                " ", 
                                unique(strsplit(x = file0, split = "_")[[1]][grep(pattern = "category", 
                                                                                  x = strsplit(x = file0, 
                                                                                               split = "_")[[1]])]), 
                                " ", 
                                as.character(category0[sapply(paste0("_", category0, "_"), grepl, file0) %in% TRUE]), 
                                ")")
          temp0<-cbind.data.frame(temp0, temp00)
        }
      }  
    }
    
    temp0names<-names(temp0)
    temp0<-data.frame(sapply(temp0, as.numeric))
    names(temp0)<-temp0names
    
    write.xlsx(temp0, file = paste0(dir.sum, "Summary.xlsx"), sheetName = paste0(reg.order[reg0], "_", yearrange[yrange0]), 
               col.names = TRUE, row.names = TRUE, append = TRUE)  
    
    tempM<-temp0
    
    tempM[,grep(pattern = "Q ", x = names(tempM))]<-tempM[,grep(pattern = "Q ", x = names(tempM))]/1e6
    names(tempM)[grep(pattern = "Q ", x = names(tempM))]<-paste0(names(tempM)[grep(pattern = "Q ", x = names(tempM))], " (Millions)")
    
    tempM[,names(tempM) %in% "QE"]<-tempM[,names(tempM) %in% "QE"]/1e6
    names(tempM)[names(tempM) %in% "QE"]<-paste0(names(tempM)[names(tempM) %in% "QE"], " (Millions)")
    
    write.xlsx(tempM, file = paste0(dir.sum, "SummaryMill.xlsx"), sheetName = paste0(reg.order[reg0], "_", yearrange[yrange0]), 
               col.names = TRUE, row.names = TRUE, append = TRUE)  
    
    summarydata[length(summarydata)+1]<-list(temp0)
    names(summarydata)[length(summarydata)]<-paste0(reg.order[reg0], "_", yearrange[yrange0])
    
  }
  
}

save(summarydata, file = paste0(dir.sum, "Summary.rdata"))


########PRESENTATION######################
library(tidyverse)
library(gapminder)
library(scales)
library(gridExtra)
library(patchwork)

Date0<-"2020-09-02"
datadl<-"2020-08-14"

# Opening Slides

rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_Presentation_Opening.Rmd"),
                  output_dir = dir.pres,
                  output_file = paste0("ProductivityIndex_EmilyMarkowitz_Opening.pptx"))


# Loop through Slide Placement of Key Slides (Q, QE, QI, PI)
fold<-list.files(paste0(dir.out, "/analyses/"), full.names = FALSE)
fold<-fold[-(grep(pattern = "Northeast", x = fold))]
fold<-fold[-(grep(pattern = "SummaryFiles", x = fold))]
fold<-fold[-(grep(pattern = "nobother", x = fold))]

fold0<-strsplit(x = fold, split = "_")
aaa<-unique(paste(lapply(fold0, `[[`, 2), 
                lapply(fold0, `[[`, 3), 
                lapply(fold0, `[[`, 4), 
                lapply(fold0, `[[`, 5), 
                sep = "_"))

for (i in 1:length(aaa)){
  folderpattern <- aaa[i]
  rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_Presentation_KeyRegionPlots.Rmd"),
                    output_dir = paste0(dir.out, "/Presentation/"),
                    output_file = paste0("ProductivityIndex_EmilyMarkowitz_KeyRegionPlots_", folderpattern, ".pptx"))
}


# Loop through Slide Placement of Cross Analysis Comparisons by Regions
aaa<-unlist(unique(lapply(fold0, `[[`, 1)))
load(file = paste0(dir.out, "/analyses/SummaryFiles/Summary.rdata")) #summarydata
reg.order<-c("National", "North Pacific", "Pacific", "Western Pacific", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico") 
for (i in 1:length(aaa)){
  yr<-aaa[i]
  rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_Presentation_CrossAnalysis.Rmd"),
                    output_dir = paste0(dir.out, "/Presentation/"),
                    output_file = paste0("ProductivityIndex_Presentation_CrossAnalysis_", yr, ".pptx"))
}




######*** FEUS############

#*** Price Driven Analysis - category.tax

analysisby = "P"
category0 = "category.tax"
pctmiss = 0.60
MinimumNumberOfSpecies = 10


category0 = "category.taxFSO"
MinimumNumberOfSpecies = 1

NortheastTFs<-F
  a<-landings.data
  a$category.taxFSO<-a$category.taxsimp
  a$category.taxFSO[a$category.taxFSO %in% c("Other Fish", "Chondrichthyes", "Actinopterygii")]<-"Finfish"
  a$category.taxFSO[a$category.taxFSO %in% c("Mollusca","Arthropoda","Echinodermata")]<-"Shellfish"
  a$category.taxFSO[a$category.taxFSO %in% c("Other", "Tetrapoda")]<-"Other"
  
  ###Without Other
  b<-a
  a<-a[!(a$category.taxFSO %in% "Other"),]

  minyr.data<-as.numeric(paste0(floor((maxyr-24)/10), 
                                ifelse(substr(maxyr, start = nchar((maxyr-24)), 
                                              stop = nchar((maxyr-24)))>=5, 6, 1))) #of data going into the analysis
  maxyr<-2018 # of report and analysis
  minyr<-maxyr-19 # That will be shown in the analysis
  baseyr<-as.numeric(paste0(floor(maxyr/10), 
                            ifelse(substr(maxyr, start = nchar(maxyr), 
                                          stop = nchar(maxyr))>=5, 5, 0))) #Will change every 5 years, e.g., maxyr 2019 = byr 2015; maxyr 2020 = byr 2020; maxyr 2021 = byr 2020
  idx<-"Chained_Base_John" 
  
  titleadd = paste0(minyr, "To", maxyr, "_FSFEUS")
  
  OutputAnalysis(landings.data = a[a$Year>=minyr.data & a$Year<=maxyr,],
                 titleadd = titleadd,
                 category0 = category0, 
                 baseyr = baseyr,
                 state.codes = state.codes, 
                 counter = counter, 
                 dir.rawdata = dir.rawdata, 
                 dir.analyses = dir.analyses, 
                 pctmiss = pctmiss, 
                 analysisby = analysisby,
                 MinimumNumberOfSpecies = MinimumNumberOfSpecies, skipplots = T)  
  
  
  designflowin<-FALSE
  
  dir.temp<-paste0(dir.analyses, 
                   titleadd, "_", analysisby, "_", 
                   gsub(pattern = "\\.", replacement = "", x = category0), "_pctmiss", (pctmiss*100),
                   "/print/")
  dir.create(dir.temp)
  
  rmarkdown::render(paste0(dir.scripts, "/ProductivityIndex_FEUS.Rmd"),
                    output_dir = paste0(dir.temp),
                    output_file = paste0("ProductivityIndex_FEUS_", minyr, "_", maxyr, ".docx"))
# }



###############METADATA##################
CreateMetadata(dir.out = paste0(dir.out, "/metadata"), 
               title = paste0("Fisheries Economic Productivity Index Metadata ", Sys.Date()))

# file.copy.rename(from = paste0(dir.out, "/metadata/Metadata_", Sys.Date(), ".docx"),
#                  to = paste0(dir.output, "/Metadata.docx"))

