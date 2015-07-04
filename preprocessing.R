require('plyr')
require("RCurl")
require("rjson")

dir.create("data")

#<-"AUS+AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA18+EU28+NMEC+CHN+COL+IND+IDN+LVA+RUS+ZAF+FRME"

countryCodes<-"BEL"

getAmountsForKey<-function(keyValues) {
  laply(keyValues[[1]]$observations,function(x) x[[1]][1])*1000
}

for (countryCode in countryCodes) {
  
  tryCatch({
    message(paste0("Downloading data from ODCE for country code ",countryCode))
    data<-fromJSON(getURL(paste0("http://stats.oecd.org/SDMX-JSON//data/SNA_TABLE11/",countryCode,"+DEW.TLYCG.T+010+0101+0102+0103+0104+0105+0106+0107+0108+020+0201+0202+0203+0204+0205+030+0301+0302+0303+0304+0305+0306+040+0401+0402+0403+0404+0405+0406+0407+0408+0409+050+0501+0502+0503+0504+0505+0506+060+0601+0602+0603+0604+0605+0606+070+0701+0702+0703+0704+0705+0706+080+0801+0802+0803+0804+0805+0806+090+0901+0902+0903+0904+0905+0906+0907+0908+100+1001+1002+1003+1004+1005+1006+1007+1008+1009.GS13.C/all?startTime=2008&endTime=2014")))
    
    amounts<-data$dataSets[1,2][1,]
    
    codeCOFOG<-data$structure$dimensions$series$values[[3]]
    country<-data$structure$dimensions$series$values[[1]]
    years<-data$structure$dimensions$observation$value[[1]]
    
    #The dimensions are referenced in the format x:x:x:x:x, the third entries gives the index
    # of the COFOG code. E.g., x:x:3:x:x refers to the COFOG code whose index is 23 in the 
    #codeCOFOG array. The following extracts the COFOG code id from teh x:x:x:x:x key
    idCodeCOFOG<-as.numeric(laply(sapply(colnames(amounts),strsplit,":"),function(x) x[3]))+1
    
    govExpenditure<-NULL
    for (i in 1:length(keys)) govExpenditure<-rbind(govExpenditure,c(codeCOFOG[idCodeCOFOG[i],],getAmountsForKey(amounts[i])))
    
    colnames(govExpenditure)<-c("COFOG code","Name",years$name)
    
    write.csv(file=paste0("data/spending",country$name,".csv"),govExpenditure,row.names=F,quote=F)
    message(paste0("Succesfully create data/spending",country$name,".csv"))
    
  }, warning = function(cond) {
    message("Warning: Data not properly processed")
    message(cond)
  }, error = function(cond) {
    message("Error: Data not properly processed")
    message(cond)
  })
  
}


