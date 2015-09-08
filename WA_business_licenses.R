#-----------------------------
#
#  WA historical business formation patterns
# 
#
#  Purpose: Analyze and model the formation and closing of businesses in Washington State.
#           Class project for Methods for Data Analysis course (UW Certificate in Data Science).
#
#  Created by: Winston Featherly-Bean (wfb3@uw.edu)
#
#  Created: August 2015
#
#-----------------------------


##----Import Libraries-----
require('data.table')
require('logging')
require('ggplot2')
require('reshape2')
require('forecast')

##----Runs in interactive----

if(interactive()){
  ##----Default Parameters----
  data_dir <- "C:/Users/Locus/Documents/WAScrape/data"
  market_data <- "dow_jones_data.csv"
  population_data <- "wa_combined_intercensal_pop_data.txt"

  setwd(data_dir)

  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="~/testing.log", level='DEBUG')  
  
  ##----Load and shape data----
  
  # Load and combine scraped business license data from multiple files.
  # UBIs are IDs; they are numeric but may have leading 0s, so must be read as strings.
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

  # List sectors for future reference.
  sectors <- unique(df$SECTOR)
      
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
            
  # Live businesses per year per sector
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
  
  # Garbage colleciton
  gc()
  
  ## DATA EXPLORATION
  
  # Businesses live each year
  plot(yearValues$YEAR,yearValues$TOTAL.OPEN,type='l')
  ggplot(data=yearValues, aes(x=YEAR,y=TOTAL.OPEN)) + 
    geom_bar(stat="identity", widfh=.8, fill="#DD8888",colour="black") + 
    ggtitle("Live businesses in WA by year") +
    theme(plot.title = element_text()) +
    ylab("Registered businesses") +
    xlab("Year")
  
  # Plot distribution of sector shares over time
  ggplot(data=yearBySector, aes(x=SECTOR,y=share,fill=SECTOR)) + 
    geom_boxplot() +
    ggtitle("Sector shares over time in WA") +
    theme(plot.title = element_text(), legend.position="none") +
    ylab("Distribution of % share of economy") +
    xlab("NAICS sector")
  
 # Plot individual sector's shares
  ggplot(data=yearBySector[SECTOR==62,], aes(x=variable, y=share, group=1)) +
   geom_point(fill="white", shape=21) +
   geom_line(linetype="dashed")
    
  # Registration rates per month
  monthlyOpens <- aggregate(UBI ~ OPEN.MONTH, data = df, FUN=length)
  names(monthlyOpens) <- c('MONTH','CREATED')
  ggplot(data=monthlyOpens, aes(x=MONTH,y=CREATED)) + 
    geom_bar(stat="identity", widfh=.8, fill="#DD8888",colour="black") + 
    ggtitle("Opened businesses in WA by month") +
    theme(plot.title = element_text()) +
    scale_x_continuous(breaks=1:12) +
    ylab("Businesses opened") +
    xlab("Month")
  
  # Close rates per month
  monthlyCloses <- aggregate(UBI ~ CLOSE.MONTH, data = df, FUN=length)
  names(monthlyCloses) <- c('MONTH','CLOSED')
  ggplot(data=monthlyCloses, aes(x=MONTH,y=CLOSED)) + 
    geom_bar(stat="identity", widfh=.8, fill="#DD8888",colour="black") + 
    ggtitle("Business closures in WA by month") +
    theme(plot.title = element_text()) +
    scale_x_continuous(breaks=1:12) +
    ylab("Businesses closed") +
    xlab("Month")  
  
  # Businesses created/closed each year
  yv2<-subset(yearValues, select=-c(TOTAL.OPEN))
  yv2 <- melt(yv2,id.vars=c("YEAR"))
  ggplot(data=yv2[YEAR!=2015,],aes(x=YEAR,y=value,fill=variable))+
    geom_bar(stat="identity",position=position_dodge())+
    scale_y_continuous() +
    ggtitle("Businesses created/closed in WA") +
    theme(plot.title = element_text()) +
    ylab("Actions") +
    xlab("Year") +
    scale_fill_hue(name="Action")
  
  # Distribution of business lifetimes by sector
  ggplot(data=df[CLOSE.YEAR!=2015,], aes(x=SECTOR,y=CLOSE.YEAR - OPEN.YEAR,fill=SECTOR)) + 
  geom_boxplot() +
  ggtitle("Lifetimes of WA businesses by sector\n(Not including firms currently open)") +
  theme(plot.title = element_text(),legend.position="none") +
  ylab("Years between open and close") +
  xlab("NAICS sector")

  # Distribution of business lifetimes by year opened  
  lifetimesByYear <-df[,mean(CLOSE.YEAR-OPEN.YEAR),by=OPEN.YEAR]
  ggplot(data=lifetimesByYear[OPEN.YEAR>1950,], aes(x=OPEN.YEAR,y=V1)) + 
    geom_line(colour="red") +
    ggtitle("Average age of WA corporations by opening year\n(Including firms currently open)") +
    theme(plot.title = element_text(),legend.position="none") +
    ylab("Mean years between open and close") +
    xlab("Year opened")
 
 ## LOAD EXOGENOUS VARIABLE DATA
 
 # Dow Jones Industrial Average + U.S. Census Burea population estimates
 
 dj <- read.csv(market_data, stringsAsFactors = FALSE)
 dj <- dj[,1:2]
 dj$Date <- as.Date(dj$Date, format = "%Y-%m-%d")
 dj <- data.table(dj)
 setnames(dj,c("DATE","DJIA"))
 dj <- dj[,YEAR.MONTH:=paste0(format(DATE,format="%Y-%m"),"-1")]
 dj <- dj[,YEAR:=format(DATE,format="%Y")]
 
 # Get monthly averages. 
 djm <- dj[,mean(DJIA),by=YEAR.MONTH]
 setnames(djm,c("DATE","DJIA"))
 djm$DATE <- as.Date(djm$DATE,format = "%Y-%m-%d")
 djm[,delta:= c(NA,diff(DJIA))]
 djm[,pct.change:= c(delta/DJIA)]
 
 # Get annual averages.
 dja <- dj[,mean(DJIA),by=YEAR]
 setnames(dja,c("YEAR","DJIA"))
 dja$YEAR <- as.numeric(dja$YEAR)
 dja$DATE <- as.Date(dja$YEAR,format="%Y")
 dja[,delta:= c(NA,diff(DJIA))]
 dja[,pct.change:= c(delta/DJIA)]
 
 # Load and shape population data
 pop <- read.csv(population_data, sep="\t",stringsAsFactors = FALSE)
 pop <- pop[,1:2]
 pop$DATE <- as.Date(pop$YEAR, format= "%m/%d/%Y")
 pop$YEAR <- as.numeric(format(pop$DATE,format="%Y"))
 pop$POPULATION <- gsub(",","",pop$POPULATION)
 pop$POPULATION <- as.numeric(pop$POPULATION)
 pop <- data.table(pop)
 pop[,delta:= c(NA,diff(POPULATION))]
 pop[,pct.change:= c(delta/POPULATION)]
 
 # Imputation for month-by-month population data.
 # Start/stop dates defined by the latest/earliest dates in the two datasets.
 # Ends a year short of the final population tally, as a year's population figure is imputed back across the prior calendar year.
 popm <- data.table(DATE=djm[as.numeric(format(DATE,format="%Y")) <= pop[nrow(pop),as.numeric(format(DATE,format="%Y"))-1],DATE])
 popm$delta <- unlist(sapply(popm$DATE,function(x) pop[as.numeric(format(DATE,format="%Y"))==as.numeric(format(x,format="%Y"))+1]$delta/12))
 
 # Only look at observations dated after our latest dataset start date.
 # For now, also looking only before the year with the maximum number of live business registered.
 # This peaks around 2001 (see chart, above) and the decline after that indicates an incomplete data set.
 
 startYear <- max(c(min(dj$DATE), min(pop$DATE)))
 startYear <- match(as.numeric(format(startYear, format = "%Y")), licenseYears)
 endYear <- yearValues[TOTAL.OPEN==max(yearValues$TOTAL.OPEN)]$YEAR
 endYear <- match(as.numeric(format(endYear, format = "%Y")), licenseYears)
 
 studyYears <- licenseYears[startYear:endYear]
 studyValues <- yearValues[YEAR >= studyYears[1],]
 setkey(studyValues,YEAR)
 
 studyValuesMonthly <- monthlyValues[as.numeric(format(YEAR, format="%Y")) >= studyYears[1],]
 setnames(studyValuesMonthly,c('DATE','CREATED','TOTAL.OPEN','CLOSED','MONTH'))
 setkey(studyValuesMonthly, DATE)
 
 # Merge annual datasets
 studyValues <- merge(studyValues,dja)
 studyValues <- merge(studyValues,pop)
 studyValues <- subset(studyValues,select=-c(DATE.x,POPULATION,DATE.y))
 setnames(studyValues,c('DATE','CREATED','TOTAL.OPEN','CLOSED','DJIA','CHANGE.DJIA','PCT.CHANGE.DJIA','CHANGE.POP','PCT.CHANGE.POP'))  
 
 # Merge monthly datasets
 studyValuesMonthly <- merge(studyValuesMonthly,djm)
 studyValuesMonthly <- merge(studyValuesMonthly,popm)
 setnames(studyValuesMonthly,c('DATE','CREATED','TOTAL.OPEN','CLOSED','MONTH','DJIA','CHANGE.DJIA','PCT.CHANGE.DJIA','CHANGE.POP'))  
 
 
  ## HYPOTHESIS TESTING 
 
  # Difference in average lifetime between firms in sector 55 ("Management of Companies and Enterprises")
  # and all other sectors.
  # Null: no difference between mean lifetime of firms in NAICS sector 55 and all other firms.
  # Alt: true difference in means is not 0.

  # Prep test data: select only closed companies, assign non-55 sectors the code '00' 
  sectorAge <- subset(df,select=c(OPEN.YEAR,CLOSE.YEAR,SECTOR))
  sectorAge <- sectorAge[CLOSE.YEAR != '2015',]
  sectorAge[SECTOR != "55",]$SECTOR <- "00"
  sectorAge[,AGE:=CLOSE.YEAR - OPEN.YEAR] 
 
  # Perform two sample t-test, look at probability of population means being equal.
  t.test(formula = sectorAge$AGE ~ sectorAge$SECTOR)

  ## MODELLING
 
  # Trying to predict new business rates, using linear and ARIMA models.
  studyData <- studyValues
  
  # Add autoregressive factor:
  new_last_period <- sapply(1:nrow(studyData), function(x){
    if(x == 1){
      return(0)
    }else{
      return(studyData$CREATED[x-1])
    }
  })
  studyData$new_last_period <- new_last_period

  
  f <- lm(CREATED ~ .-DATE-DJIA-CHANGE.DJIA-CHANGE.POP,data=studyData)
  summary(f)

  studyDataMonthly <- studyValuesMonthly 
 
  # Add autoregressive factor:
  new_last_period <- sapply(1:nrow(studyDataMonthly), function(x){
    if(x == 1){
      return(0)
    }else{
      return(studyDataMonthly$CREATED[x-1])
    }
  })
  studyDataMonthly$new_last_period <- new_last_period
  
  studyDataMonthly$YEAR_COUNT<-as.numeric(format(studyDataMonthly$DATE,format="%Y"))-1990
  studyDataMonthly$DAY_COUNT<-as.numeric(studyDataMonthly$DATE-min(studyDataMonthly$DATE))
  studyDataMonthly$WEEK_COUNT<-floor(studyDataMonthly$DAY_COUNT/7.0)
  studyDataMonthly$MONTH_COUNT<-floor(studyDataMonthly$DAY_COUNT/30.5)
  
  g <- lm(CREATED ~ .-DATE-DJIA-PCT.CHANGE.DJIA,data=studyDataMonthly)
  summary(g)
 
 # Use ARIMA model to predict monthly openings
 # Speculate there is an autoregressive factor, an integration factor and short term average factor.
 # Fit time series:

  monthlyArima <- arima(studyDataMonthly$CREATED, order = c(2,1,2))
  arimaFitted <- studyDataMonthly$CREATED - monthlyArima$residuals
  summary(monthlyArima)
  arimaPredictions <- predict(monthlyArima, n.ahead=12, se.fit=TRUE)
 
  # Predict out to 12 months.
  xPred <- nrow(studyDataMonthly) : (nrow(studyDataMonthly) + 12)
  
  plot(studyDataMonthly$CREATED, type="l", lwd = 2, col="black", xlim = c(0,200), xlab="Month from data set start", ylab="Created", main="ARIMA model of monthly business creations in WA")
  lines(arimaFitted, lwd = 2, col="red")
  lines(xPred, c(arimaFitted[length(arimaFitted)],arimaPredictions$pred),lwd=2, col="red", lty=8)
  
  # Add in standard error lines
  lines(seq(from=xPred[1], to=xPred[1]+12, by=1)[-1],arimaPredictions$pred + arimaPredictions$se, lwd=2, col='green')
  lines(seq(from=xPred[1], to=xPred[1]+12, by=1)[-1],arimaPredictions$pred - arimaPredictions$se, lwd=2, col='green')

  # Garbage collection
  gc()
}
