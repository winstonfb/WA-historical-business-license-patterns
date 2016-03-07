#-----------------------------
# Modified WA_business_license script, without modelling.
# ** Rework original before adding to github.
#-----------------------------


##----Import Libraries-----
require('data.table')
require('logging')
require('ggplot2')
require('reshape2')
require('forecast')


  ##----Default Parameters----
  data_dir <- "C:/Users/Locus/Documents/WAScrape/data"
  market_data <- "dow_jones_data.csv"
  population_data <- "wa_combined_intercensal_pop_data.txt"

  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')  
  
  ##----Load and shape data----
  
  # Load and combine scraped business license data from multiple files.
  # UBIs are IDs; they are numeric but may have leading 0s, so must be read as strings.
  
  load.data <- function(dir = data_dir){
  
  setwd(dir)
  
  df <- data.frame(UBI=character(),NAICS=character(),OPEN.DATE=character(),CLOSE.DATE=character(),stringsAsFactors=FALSE)
  for(dataFile in list.files()){
    if (substr(dataFile,1,5)=="batch"){
    temp <- read.csv(dataFile,sep='\t',header=TRUE,stringsAsFactors=FALSE,colClasses=c(rep("character",4)),na.strings=c('null'))
    df<-rbind(df,temp)}
  }
  rm(temp)
  
  # Drop observations with NAs
  df<-df[complete.cases(df),]
  
  # Format date columns. 'OPEN' becomes today's date.
  df$OPEN.DATE <- as.Date(df$OPEN.DATE,format="%m/%d/%Y")
  df[df$CLOSE.DATE=='OPEN',]$CLOSE.DATE <- format(Sys.Date(), "%m/%d/%Y")
  df$CLOSE.DATE <- as.Date(df$CLOSE.DATE,format="%m/%d/%Y")

  df$OPEN.YEAR <- as.numeric(format(df$OPEN.DATE, format="%Y"))
  df$CLOSE.YEAR <- as.numeric(format(df$CLOSE.DATE, format="%Y"))

  df$OPEN.MONTH <- as.numeric(format(df$OPEN.DATE, format="%m"))
  df$CLOSE.MONTH <- as.numeric(format(df$CLOSE.DATE, format="%m"))

  df$OPEN.DAY <- as.numeric(format(df$OPEN.DATE, format="%d"))
  df$CLOSE.DAY <- as.numeric(format(df$CLOSE.DATE, format="%d"))
  
  # Get the leading 2 digits of the taxonomic NAICS code, representing a sector.
  df$SECTOR <- sapply(df$NAICS,function(x) substr(x,1,2))  
  df <- data.table(df)
 
  
  return(df)
  }



  ## Returns yearValues and monthlyValues, counts of business licensing activity over time
  ## and licenseYears, licenseMonths
  get.business.activity <- function(df) {   
  # Build tables of counts of businesses open/created/closed over time.
  # Aggregating at both the annual and monthly levels.
  licenseYears <- seq(min(df$OPEN.YEAR),max(df$OPEN.YEAR),by=1)
  yearValues <- data.frame(YEAR=licenseYears, CREATED=rep(0, length(licenseYears)), TOTAL.OPEN=rep(0, length(licenseYears)), CLOSED=rep(0, length(licenseYears)))  
  yearValues$TOTAL.OPEN <- sapply(licenseYears, function(x) nrow(df[OPEN.YEAR <= x & x <= CLOSE.YEAR,]))
  yearValues$CREATED <- sapply(licenseYears,function(x) nrow(df[OPEN.YEAR == x,]))
  yearValues$CLOSED <- sapply(licenseYears, function(x) nrow(df[CLOSE.YEAR == x,]))
  yearValues <- data.table(yearValues)

  licenseMonths <- seq(as.Date(paste0(min(licenseYears),"-1-1","")), by = "month", length.out=length(licenseYears)*12)
  monthlyValues <- data.frame(YEAR=licenseMonths, CREATED=rep(0,length(licenseMonths)),TOTAL.OPEN=rep(0,length(licenseMonths)),CLOSED=rep(0,length(licenseMonths)))
  monthlyValues$TOTAL.OPEN <- sapply(licenseMonths, function(x) nrow(df[OPEN.DATE <= as.Date(x) & as.Date(x) <= CLOSE.DATE]))
  monthlyValues$CREATED <- sapply(licenseMonths, function(x) nrow(df[OPEN.YEAR == as.numeric(format(x, format="%Y")) & OPEN.MONTH == as.numeric(format(x, format="%m")),]))
  monthlyValues$CLOSED <- sapply(licenseMonths, function(x) nrow(df[CLOSE.YEAR == as.numeric(format(x, format="%Y")) & CLOSE.MONTH == as.numeric(format(x, format="%m")),]))
  monthlyValues$MONTH <- as.numeric(format(monthlyValues$YEAR, format="%m"))
  monthlyValues <- data.table(monthlyValues)
  
  licenseActivity <- list("yearValues" = yearValues, "monthlyValues" = monthlyValues,
                          "licenseYears" = licenseYears, "licenseMonths" = licenseMonths)
  return(licenseActivity)
  }

  
  get.sector.data <- function(licenseYears, df){
  # Live businesses per year per sector
  sectors <- unique(df$SECTOR)
  yearBySector <- data.table(SECTOR=sectors)
  setkey(yearBySector,SECTOR)
  
  for(y in licenseYears){
  sectorInstances <- df[OPEN.YEAR <= y & y <= CLOSE.YEAR,.(length(UBI)),by=SECTOR]
  setnames(sectorInstances, "V1", as.character(y))
  yearBySector <- merge(yearBySector, sectorInstances, all.x=TRUE)
  }
  
  rm(sectorInstances)

  # Reshape for plotting
  yearBySector <- melt(yearBySector,id.vars=c("SECTOR"))
  yearTotalLive <- yearBySector[,.(sum(na.omit((value)))),by=variable]
  setnames(yearTotalLive,c("YEAR","TOTAL"))
  yearBySector$yeartotal <- sapply(yearBySector$variable, function(x) yearTotalLive[YEAR==x]$TOTAL)
  yearBySector[,share:=value/yeartotal]
  
  # Remove '99' sectors, which are unclassified establishments
  yearBySector <- yearBySector[SECTOR!='99']
  
  # Limit to years with more than 2000 observations, 
  # and omit cases where sectors have no instances (NA).
  yearBySector <- na.omit(yearBySector[yeartotal > 2000])
  
  # Re-type the years from factor to numeric, for later comparisons
  yearBySector$variable <- as.numeric(levels(yearBySector$variable))[yearBySector$variable]
  return(yearBySector)
  }

  # Garbage colleciton
  gc()
  
  
  ## DATA EXPLORATION
  
  # Businesses live each year
  plot.live.per.year <- function(yearValues){
  p <- ggplot(data=yearValues, aes(x=YEAR,y=TOTAL.OPEN)) + 
    geom_bar(stat="identity", width=.8, fill="#DD8888",colour="black") + 
    ggtitle("Live businesses in WA by year") +
    theme(plot.title = element_text()) +
    ylab("Registered businesses") +
    xlab("Year")
  return(p)
  }
  
  # Plot distribution of sector shares over time
  plot.share.over.time <- function(yearBySector, start = 1969, end = 2015){
  p <- ggplot(data=yearBySector[variable >= start & variable <= end,], aes(x=SECTOR,y=share,fill=SECTOR)) + 
    geom_boxplot() +
    ggtitle("Sector shares over time in WA") +
    theme(plot.title = element_text(), legend.position="none") +
    ylab("Distribution of share of economy") +
    xlab("NAICS sector")
  return(p)
  }
  
 # Plot individual sector's shares
  plot.sector.share <- function(yearBySector, sector = 11){
  p <- ggplot(data=yearBySector[SECTOR==sector,], aes(x=variable, y=share, group=1)) +
   geom_point(fill="orange", shape=21) +
   geom_line(linetype="dashed") +
    ggtitle(paste("Share of total WA establishments over time for sector",sector)) +
    ylab("Share of economy") + 
    xlab("Year")   
  return(p)
  }
    
  # Registration rates per month
  plot.open.rates.per.month <- function(df){
  monthlyOpens <- aggregate(UBI ~ OPEN.MONTH, data = df, FUN=length)
  names(monthlyOpens) <- c('MONTH','CREATED')
  p <- ggplot(data=monthlyOpens, aes(x=MONTH,y=CREATED)) + 
    geom_bar(stat="identity", width=.8, fill="#DD8888",colour="black") + 
    ggtitle("Opened businesses in WA by month") +
    theme(plot.title = element_text()) +
    scale_x_continuous(breaks=1:12) +
    ylab("Businesses opened") +
    xlab("Month")
  return(p)
  }
  
  # Close rates per month
  plot.close.rates.per.month <- function(df){
  monthlyCloses <- aggregate(UBI ~ CLOSE.MONTH, data = df, FUN=length)
  names(monthlyCloses) <- c('MONTH','CLOSED')
  p <- ggplot(data=monthlyCloses, aes(x=MONTH,y=CLOSED)) + 
    geom_bar(stat="identity", width=.8, fill="#DD8888",colour="black") + 
    ggtitle("Business closures in WA by month") +
    theme(plot.title = element_text()) +
    scale_x_continuous(breaks=1:12) +
    ylab("Businesses closed") +
    xlab("Month")  
  return(p)
  }
  
  # Businesses created/closed each year
  plot.annual.turnover <- function(yearValues){ 
  yv2<-subset(yearValues, select=-c(TOTAL.OPEN))
  yv2 <- melt(yv2,id.vars=c("YEAR"))
  p <- ggplot(data=yv2[YEAR!=2015,],aes(x=YEAR,y=value,fill=variable))+
    geom_bar(stat="identity",position=position_dodge())+
    scale_y_continuous() +
    ggtitle("Businesses created/closed in WA") +
    theme(plot.title = element_text()) +
    ylab("Actions") +
    xlab("Year") +
    scale_fill_hue(name="Action")
  return(p)
  }
  
  # Distribution of business lifetimes by sector
  plot.lifetime.by.sector <- function(df){
  p <- ggplot(data=df[CLOSE.YEAR!=2015,], aes(x=SECTOR,y=CLOSE.YEAR - OPEN.YEAR,fill=SECTOR)) + 
  geom_boxplot() +
  ggtitle("Lifetimes of WA businesses by sector\n(Not including firms currently open)") +
  theme(plot.title = element_text(),legend.position="none") +
  ylab("Years between open and close") +
  xlab("NAICS sector")
  return(p)
  }

  # Distribution of business lifetimes by year opened 
  plot.lifetime.by.start.year <- function(df){
  lifetimesByYear <- df[,mean(CLOSE.YEAR-OPEN.YEAR),by=OPEN.YEAR]
  p <- ggplot(data=lifetimesByYear[OPEN.YEAR>1950,], aes(x=OPEN.YEAR,y=V1)) + 
    geom_line(colour="red") +
    ggtitle("Average age of WA corporations by opening year\n(Including firms currently open)") +
    theme(plot.title = element_text(),legend.position="none") +
    ylab("Mean years between open and close") +
    xlab("Year opened")
  return(p)
  }