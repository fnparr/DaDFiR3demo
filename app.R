library(shiny)
library(shinythemes)
library(FEMSdevBase)
library(ggplot2)

#reactive creation of the example bond

shinycssloaders::withSpinner(
  plotOutput("plot")
)

# Define UI for application that visualizes cashflows for a exmaple bond
ui <- fluidPage(
        theme = shinytheme("cerulean"),
                
        #top images 
        img(src = "actus-logo.png", height = 77, width = 220,
                  style="float:right; padding-right:25px"),
        img(src="Logo_Weiss.png",height = 80, width = 100),
        
        # inputs for contract terms of Bond
        navbarPage("DaDFiR3 Demo Application",   #navbar App title
                   tabPanel("RF Scenario Views", #first Tab title
                      sidebarLayout(
                         #dataset choice input
                         selectInput(inputId = "dataSetName",
                                     label = "Choose a Dataset", 
                                     choices = c("Steady Rates", 
                                                 "Increasing Rates",
                                                 "Decreasing Rates", 
                                                 "Recovering Rates")),
                         #output plot of the respective rate scenario
                         mainPanel( plotOutput("ratesPlot")
                                    ) #main panel close
                         )  #sidebar close
                   ),  #tabpanel close
                   #second tabpanel Cashflow Exmaple
                   tabPanel("Cashflow Example for a Bond",
                       sidebarLayout(
                          sidebarPanel(width = 3,
                              #inputs for Bond Contract Terms
                              dateInput(inputId = "issueDate", 
                                       label = "Choose Issuedate",
                                       value = "2020-12-31"
                                       ),
                              sliderInput(inputId = "maturity",
                                          label = "Choose the maturity",
                                          min = 1,max = 10,
                                          value = 5,
                                          step = 1),
                              numericInput(inputId = "nominal",
                                          label = "Choose the nominal",
                                          value = 10000,min = 0, 
                                          max = 10000000, step = 1000
                                          ),
                              numericInput(inputId = "coupon",
                                          label = "Choose the Couponrate",
                                          value = 0.02,min = 0,max = 0.05,
                                          step = 0.005
                                          ),
                              selectInput(inputId = "paymentFreq",
                                          label = "Choose the payment frequency",
                                          choices = c("3 months", "6 months", "1 year")),
                              selectInput(inputId = "rfScenarioBond", 
                                         label = "Choose the Risk Factor Scenario",
                                         choices = c("increasing Rates",
                                                     "decreasing Rates",
                                                     "steady Rates",
                                                     "recovering Rates")),
                              selectInput(inputId = "rateResetFreq",
                                          label = "Choose the rate reset frequency",
                                          choices = c("Fixed rate","1 month", 
                                                      "3 months", "6 months",
                                                      "1 year")),
                              sliderInput(inputId = "rateSpread",
                                          label = "Choose the rate spread",
                                          min = 0,max = 0.05, 
                                          value = 0.02,step = 0.01)
                              ),   #sidebar panel close
                                      
                              # Show a plot of the generated cashflows
                              mainPanel(width = 8,
                                       shinycssloaders::withSpinner(
                                           plotOutput("cashFlowPlot", 
                                                      width = "800px",
                                                      height="500px")
                                                      ),     #spinnerclose,
                                                      dataTableOutput("CFDF")
                                       )    #main panel close
                              ) #sidebar close
                   ),   #tabpanel close
                   #inputs for the modelbank
                   tabPanel("Analysis of ModelBank",
                      sidebarLayout(
                          sidebarPanel(width = 3,
                              selectInput("ptfFile","Choose your Portfolio",
                                          choices = c("BondPortfolio",
                                                      "MixedPortfolio"),
                                          selected = "BondPortfolio"),
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
                                   
                              mainPanel( shinycssloaders::withSpinner(
                                              plotOutput("CFPlot")
                                              )   #spinner close
                                       )   #main panel close
                              )   #sidebarLayout close
                                     
                       ),   #tab panel close
                   tabPanel("Analysis of your data",
                      sidebarLayout(
                          sidebarPanel(width = 6, p(" "),
  " Here you can upload a data file from your workstation specifying a portfolio of bond contracts ",
  "and request ACTUS contract simulation and analysis of it. ",
  " The uploaded file must be in .csv format and patterned on file: ",
  tags$a("BondPortfolio.csv", 
         href="https://github.com/fnparr/FEMSdevBase/tree/main/inst/extdata/BondPortfolio.csv"),
  " - with any variable rate setting based on Market Object Code YC_EA_AAA.",
  "For a more detailed explanation of each contract term, consult the ",
  tags$a("ACTUS Data Dictionary", href = "https://www.actusfrf.org/dictionary"),
  ".", p(" "),
                                           fileInput(inputId = "uploadedFile",
                                                     label = "Your csv file of ACTUS PAM Contracts",
                                                     accept = ".csv",
                                                     buttonLabel = "Browse...",
                                                     placeholder = "No file selected"
                                                     ),
                                           selectInput(
                                             inputId = "analysisTypeCus",
                                             label = "Choose the analysis type",
                                             choices =  c("monthly income",
                                                          "cumulative income",
                                                          "monthly liquidity change",
                                                          "accumulated liquidity")
                                                      ),
                                           selectInput(
                                             inputId = "rfScenarioCus",
                                             label = "Choose the Risk Factor Scenario",
                                             choices = c("increasing Rates",
                                                         "decreasing Rates",
                                                         "steady Rates",
                                                         "recovering Rates")
                                                      )
                                           ),#sidebarpanel close
                              mainPanel(
                                plotOutput(outputId = "CFPlotCus")
                              )  #mainPanel close
                            )  #sidebarlayout close
                            )  #tab panel close
                   )   #navbarPAGE close
              )   #fluid Page close

# Define server logic required to create the cash flows of a simple bond
server <- function(input, output) {
  
  #reactive creation of the example bond
  pam1 <- reactive({bondvr(as.character(input$issueDate), 
                         maturity = paste(as.character(input$maturity), " years"), 
                         nominal = input$nominal, coupon = input$coupon,
                         paymentFreq = input$paymentFreq, role = "RPA",
                         rateResetFreq = input$rateResetFreq,
                         rateResetSpread = input$rateSpread)
                  })
  
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
  # set the ACTUS serverURL
   serverURL = "https://demo.actusfrf.org:8080/"
#   serverURL <- "http://ractus.ch:8080/"
  
  #reactive creation of the events for the bond
  observe({
     if(input$rfScenarioBond == "increasing Rates"){
          evs1 <- reactive({generateEventSeries(contract = pam1(), 
                                                list(rfx_rising),
                                                serverURL  
                                                )
                          })
          }
    if(input$rfScenarioBond == "decreasing Rates"){
          evs1 <- reactive({generateEventSeries(contract = pam1(), 
                                                list(rfx_falling),
                                                serverURL  
                                                )
                          })
           }
    if(input$rfScenarioBond == "steady Rates"){
          evs1 <- reactive({generateEventSeries(contract = pam1(), 
                                                list(rfx_steady),
                                                serverURL  
                                                )
                           })
          }
    if(input$rfScenarioBond == "recovering Rates"){
          evs1 <- reactive({generateEventSeries(contract = pam1(), 
                                                list(rfx_recovering),
                                                serverURL  
                                                )
                          })
          }
    #creation of the desired plot
    output$cashFlowPlot <- renderPlot({
      cashflowPlot(evs1())
    })
  
    #optional data table of the event list
    output$CFDF <- renderDataTable({
      evs1()$events_df
      })
    })   #observe close
  
  # reading of the different Rate scenario datafiles
  steadyRates <-      read.csv(system.file("extdata","UST5Y_steadyRates.csv", 
                                           package = "FEMSdevBase"))
  increasingRates <-  read.csv(system.file("extdata","UST5Y_risingRates.csv", 
                                           package = "FEMSdevBase"))
  decreasingRates <-  read.csv(system.file("extdata","UST5Y_fallingRates.csv", 
                                           package = "FEMSdevBase"))
  recoveringRates <-  read.csv(system.file("extdata","UST5Y_recoveringRates.csv",
                                           package = "FEMSdevBase"))
  
  # aggregate to monthly for quick nice looking plots 
  monthlySteadyRates     <- monthlyAverageRate(steadyRates)
  monthlyIncreasingRates <- monthlyAverageRate(increasingRates)
  monthlyDecreasingRates <- monthlyAverageRate(decreasingRates)
  monthlyRecoveringRates <- monthlyAverageRate(recoveringRates)
  
  #desired plot outbut based on selection of input with label: "dataSetName"
  observe({ 
    
    if(input$dataSetName == "Steady Rates"){
         plot <-  ggplot(monthlySteadyRates, aes(x=Date,y=Rate)) +
              geom_line(colour = "black") +
              labs(title = "Steady rates scenario - US Treasury 5 Year Rates")
      }
    
    if(input$dataSetName == "Increasing Rates"){
        plot<- ggplot(monthlyIncreasingRates, aes(x=Date,y=Rate)) +
              geom_line(colour = "black") +
              labs(title = "Increasing rates scenario - US Treasury 5 Year Rates")
    }
    
    if(input$dataSetName == "Decreasing Rates"){
        plot <- ggplot(monthlyDecreasingRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Decreasing rates scenario - US Treasury 5 Year Rates")
    }
    
    if(input$dataSetName == "Recovering Rates"){
        plot <- ggplot(monthlyRecoveringRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Recovering rates scenario - US Treasury 5 Year Rates")
    }
    
    output$ratesPlot <- renderPlot({ plot })

  })      # observe close
  
  observe({
    if(input$ptfFile == "BondPortfolio"){
      cdfn <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevBase")
      }
    if(input$ptfFile == "MixedPortfolio"){
      cdfn <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevBase")
      }
    rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevBase")
    
    #create the portfolio with the respective files
    ptf   <-  samplePortfolio(cdfn,rfdfn)
    

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
  
  observe({
    if(is.null(input$uploadedFile) != TRUE){
      file <- reactive(input$uploadedFile)
      cdfn <- file()$datapath
      rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevBase")
      
      #create the portfolio with the respective files
      ptf1   <-  samplePortfolio(cdfn,rfdfn)
      
      
      #create eventSeries for the selected contract
      if(input$rfScenarioCus == "decreasing Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL, list(rfx_falling),
                                                  rfx_falling$riskFactorID))
      }
      
      if(input$rfScenarioCus == "increasing Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL, list(rfx_rising),
                                                  rfx_rising$riskFactorID))
      }
      if(input$rfScenarioCus == "steady Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL, list(rfx_steady),
                                                  rfx_rising$riskFactorID))
      }
      if(input$rfScenarioCus == "recovering Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL, 
                                                  list(rfx_recovering),
                                                  rfx_rising$riskFactorID))
      }
      
      output$CFPlotCus <- renderPlot({
        plotlistCus()[[input$analysisTypeCus]]
      })
    }
    
  })   #observe close
  
}      #server close

# Run the application 
shinyApp(ui = ui, server = server)
