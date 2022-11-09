library(shiny)
library(shinythemes)
library(FEMSdevBase)
library(ggplot2)


# Define UI for application that visualizes cashflows for a exmaple bond
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  #top images 
  img(src = "actus-logo.png", height = 77, width = 220,
      style="float:right; padding-right:25px"),
  img(src="Logo_Weiss.png",height = 80, width = 100),
  
  # Title and bar with tabs
  navbarPage("DaDFiR3 Demo",   #navbar App title
             
             tabPanel("Portfolio Analysis",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("ptfFile","Choose your Portfolio",
                                                 choices = c("BondPortfolio",
                                                             "MortgagePortfolio",
                                                             "UploadedPortfolio"),
                                                 selected = "BondPortfolio"
                                     ),
                                     fileInput(inputId = "uploadedFile",
                                               label = "Your csv file of ACTUS PAM Contracts",
                                               accept = ".csv",
                                               buttonLabel = "Browse...",
                                               placeholder = "No file selected",
                                     ),
                                     selectInput(
                                       inputId = "analysisType",
                                       label = "Choose the analysis type",
                                       choices =  c("monthly income",
                                                    "cumulative income",
                                                    "monthly liquidity change",
                                                    "accumulated liquidity")
                                     ),
                                     selectInput(
                                       inputId = "rfScenario",
                                       label = "Choose the Risk Factor Scenario",
                                       choices = c("increasing Rates",
                                                   "decreasing Rates",
                                                   "steady Rates",
                                                   "recovering Rates")
                                     )
                        ),   #sidebarpanel Close
                        
                        mainPanel(width = 8 ,
                                  textOutput("warning"),
                                  tags$head(tags$style("#warning{color: red;
                                 font-size: 30px;
                                 font-style: bold;
                                 }")),
                                  shinycssloaders::withSpinner(
                          plotOutput("CFPlot")
                        ),   #spinner close
                        column(dataTableOutput("portfolioDF"),width = 12)
                        )   #main panel close
                      )   #sidebarLayout close
                      
             ),   #tab panel close
            
            
  )   #navbarPAGE close
)   #fluid Page close

# Define server logic required to create the cash flows of a simple bond
server <- function(input, output) {
  
  #reactive creation of the example bond
  # set the ACTUS serverURL
  serverURL = "https://demo.actusfrf.org:8080/"
  #   serverURL <- "http://ractus.ch:8080/"
  
  #read the data files from inside of the package
  falling_fp <- system.file("extdata","UST5Y_fallingRates.csv", 
                            package = "FEMSdevBase")
  rising_fp <-  system.file("extdata","UST5Y_risingRates.csv", 
                            package = "FEMSdevBase")
  steady_fp <-  system.file("extdata","UST5Y_steadyRates.csv", 
                            package = "FEMSdevBase")
  recovering_fp <-  system.file("extdata","UST5Y_recoveringRates.csv", 
                                package = "FEMSdevBase")
  #create ACTUS ReferenceIndex
  rfx_falling <- sampleReferenceIndex(rxdfp = falling_fp,
                                      rfID = "UST5Y_fallingRates", 
                                      moc = "YC_EA_AAA",base = 100 )
  rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                     "YC_EA_AAA",100 )
  rfx_steady <- sampleReferenceIndex(steady_fp,"UST5Y_steadyRates",
                                     "YC_EA_AAA",100 )
  rfx_recovering <- sampleReferenceIndex(recovering_fp,"UST5Y_steadyRates",
                                         "YC_EA_AAA",100 )
  #reactive creation of the example bond
 
  
  #desired plot outbut based on selection of input with label: "dataSetName"
  # observe close
  
  observe({
    if(input$ptfFile == "BondPortfolio"){
      cdfn <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevBase")
      output$warning <- renderText("")
    }
    if(input$ptfFile == "MortgagePortfolio"){
      cdfn <- system.file("extdata","AnnuityPortfolio.csv",package = "FEMSdevBase")
      output$warning <- renderText("")
    }
    if(input$ptfFile == "UploadedPortfolio"){
      if(is.null(input$uploadedFile) == TRUE){
        output$warning <- renderText("No File uploaded yet")
      }
      else if(is.null(input$uploadedFile) != TRUE){
        file <- reactive(input$uploadedFile)
        cdfn <- file()$datapath
        output$warning <- renderText("")
      }
      req(is.null(input$uploadedFile) != T,cancelOutput = FALSE)
    }
    
    rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevBase")
    
    
    #create the portfolio with the respective files
    ptf   <-  samplePortfolio(cdfn,rfdfn)
    
    portfolioDF <- read.csv(cdfn)
    #portfolioDF <- portfolioDF[ , colSums(portfolioDF == "NULL") < nrow(portfolioDF)]
    portfolioDF <- portfolioDF[,c("contractType","statusDate","contractRole","contractID",
                                  "nominalInterestRate","currency","initialExchangeDate",
                                  "premiumDiscountAtIED","maturityDate","notionalPrincipal",
                                  "rateSpread","description")]
    output$portfolioDF <- renderDataTable(portfolioDF,options = list(autoWidth = TRUE, scrollX = TRUE))
    
    #create eventSeries for the selected contract
    if(input$rfScenario == "decreasing Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL, list(rfx_falling),
                                             rfx_falling$riskFactorID))
    }
    
    if(input$rfScenario == "increasing Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL, list(rfx_rising),
                                             rfx_rising$riskFactorID))
    }
    if(input$rfScenario == "steady Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL, list(rfx_steady),
                                             rfx_rising$riskFactorID))
    }
    if(input$rfScenario == "recovering Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL, 
                                             list(rfx_recovering),
                                             rfx_rising$riskFactorID))
    }
    
    output$CFPlot <- renderPlot({
      plotlist()[[input$analysisType]]
    })
    
  })   #observe close
}      #server close

# Run the application 
shinyApp(ui = ui, server = server)

