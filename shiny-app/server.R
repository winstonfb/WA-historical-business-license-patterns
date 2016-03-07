# server.R for WA BLS project Shiny app
setwd('C:/Users/Locus/Documents/WAScrape/shiny-app')
source("WA_BLS_helper.R")
df <- readRDS("data/wa_df.rds")
licenseActivity <- readRDS("data/wa_license_activity.rds")
licenseYears <- licenseActivity$licenseYears
yearValues <- licenseActivity$yearValues
monthlyValues <- licenseActivity$monthlyValues
yearBySector <- readRDS("data/wa_sector_data.rds")

shinyServer(function(input, output) {
    
  output$shares <- renderPlot({
      plot.share.over.time(yearBySector, start = input$range[1], end = input$range[2])
    })
  
  output$sector <- renderPlot({
    plot.sector.share(yearBySector, sector = input$sector)
  })
  
  }
)