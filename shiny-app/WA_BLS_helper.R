#-----------------------------
# Modified WA_business_license script, without modelling.
# 
#-----------------------------


##----Import Libraries-----
require('data.table')
require('ggplot2')

  
  
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