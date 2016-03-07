# ui.R

shinyUI(fluidPage(
  titlePanel("The economy of Washington State from 1969 - 2015"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Show NAICS industrial classification sectors' share of total establishments in WA, according to five decades of business licenses."),        
      sliderInput("range", 
        label = "All sectors, years of interest:",
        min = 1969, max = 2015, value = c(1969, 2015),
        sep = ""),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      selectInput(
        "sector", label = "One sector, share over time:", 
        choices = list("11 - Agriculture, Forestry, Fishing and Hunting" = 11, "21 - Mining" = 21,
                       "22 - Utilities" = 22, "23 - Construction" = 23,
                      "31 - Food & Apparel Manufacturing" = 31, "32 - Materials Manufacturing" =  32,
                      "33 - Heavy Materials and Equipment Manufacturing" = 33, "42 - Wholesale Trade" = 42,
                      "44 - Food, Personal and Home Retail" = 44, "45 - Other Retail" = 45, "48 - Transportation" = 48,
                      "49 - Delivery and Warehousing" = 49, "51 - Information" = 51,
                      "52 - Finance and Insurance" = 52, "53 - Real Estate Rental and Leasing" = 53,
                      "54 - Professional, Scientific, and Technical Services" = 54, "55 - Management of Companies and Enterprises" = 55,
                      "56 - Administrative and Support and Waste Management and Remediation Services" = 56, "61 - Educational Services" = 61,
                      "62 - Health Care and Social Assistance" = 62, "71 - Arts, Entertainment, and Recreation" = 71,
                      "72 - Accommodation and Food Services" = 72, "81 - Other Services (except Public Administration)" = 81,
                      "92 - Public Administration" = 92), selected = 11),
      tags$small(paste0(
        "Data scraped from Washington Department of Revenue's business license database.",
        "Contact: winstonfb at gmail.com"))    
      ),
  
    mainPanel(plotOutput("shares"),
              plotOutput("sector"))
  )
))