#!/usr/bin/env Rscript

#' User Interface for the diagnostic plotting tab of the infercnv shiny app.
#'
#' @title diagnosticPlotsUI()
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing the UI elements for the diagnostic portion of the infercnv shiny app
#' 

diagnosticPlotsUI <- function(id) {
    ns <- NS(id)
    
    # create a tag list 
    tagList(
        tags$div( 
            class = "analysisTabTitles",
                  
            #*****************************************      Diagnostic Plot UI      *****************************************
            
            fluidRow(
                tags$h2("Bayesian Diagnostic Plots", 
                        align = "center",
                        style = "font-weight:bold")
            ),
            
            fluidRow(
                column(1),
                column(10,
                       tags$p("Several diagnostic plots and tables are created for the Bayesian network model to ensure convergence along with providing credibility intervals. More information about these plots is avalible", 
                            HTML('<a href="https://github.com/broadinstitute/infercnv/wiki/Diagnostic-Plots">here</a>'),
                            "."),
                       tags$p("If the app is running in a browser, the plots will appear in this tab. If not running in a browser, the PDF files 
                              will open outside of the app.")
                )
            ),
            br(),
            br(),
            hr(),
        ),
        
        
        
        #______________________________ button to run the analysis  ______________________________
        fluidRow(
            actionButton(inputId = ns("runDiagnosticsButton"), 
                         label = "Show Diagnostic Plots",
                         width = '100%',
                         style="color: #fff; background-color: #68D77B")
        ),
        
        # Message to user if finished diagnostic plots or if HMM mode is not ran 
        fluidRow(
            alertMessage <-"",
            tags$div(id = "diagnosticAlertMessage", alertMessage)
        ),
        
        
        
        
        #______________________________ Output informative text ______________________________
        # Output text information when HMM is chosen
        
        
        fluidRow(
            column(width = 10,
                   offset = 1,
                   # ******* trace and density plots text ********
                   tags$h4( "Density And Trace Plots",
                            align = "center",
                            style = "font-weight:bold"),
                   tags$p("Density and trace plots are used to help determine if the Markov chains are stable and the symmetry of the data. Trace plots should appear flat, this ensures that simulations are coming from a stable Markov chain. Because of the stochastic nature of the simulation, continuous variables should look like flat 'fuzzy caterpillars'. Density show the posterior distribution of the simulated values and should appear relatively symmetrical around the mean."),
                   # ******* autocorrelation plot text ********
                   tags$h4( "Autocorrelation plots",
                            align = "center",
                            style = "font-weight:bold"),
                   tags$p("Autocorrelation plots are a way to evaluate the randomness of the data, comparing simulated values at a specific iteration to previous iterations (lags). Autocorrelation plots show the correlation coefficients, a value between 1 and -1. values around 1 and -1 show correlation while values around 0 show no correlation."),
                   # ******* Gelman plot text ********
                   tags$h4( "Gelman Plots",
                            align = "center",
                            style = "font-weight:bold"),
                   tags$p("Gelman Plots are another source for determining convergence. Gelman Plots show the Gelman-Rubin statistics as the sampling iterations progress along the six chains. The Gelman-Rubin statistics measures the variance between and within chains, therefore a value of 1 means there is no variation between chains. A plot is created for each of the six chains, which corresponds to the CNV states probabilities (theta). If the chain is converging, the black line should converge onto the horizontal line stationary at 1."),
                   # ******* Summary Table  text ********
                   tags$h4( "Summary Tables",
                            align = "center",
                            style = "font-weight:bold"),
                   tags$p("Summary tables are created for each CNV region. These summery tables contain the Mean, Standard Deviation, 95% CI, Median, Geweke Diagnostic with Fraction In 1st Window = 0.1 and 2nd Window = 0.5, for the posterior distributions for each of the 6 states (thetas). The Geweke convergence diagnostic is a Z-score that reveals whether convergence is occurring. This is a Z-score with Normal(0,1), values 1.96 or more extreme are considered significant and therefore not converging. The 95% credibility interval is given by the 2.5%, 50%, 97.5% quantiles."),
                   
                   # # ******* trace and density plots text ********
                   # tags$h4( 
                   #     textOutput(outputId = ns("diagnosticTitle1")), 
                   #     align = "center",
                   #     style = "font-weight:bold"
                   # ),
                   # tags$p( textOutput(ns("diagnosticText1"))),
                   # 
                   # # ******* autocorrelation plot text ********
                   # 
                   # tags$h4( 
                   #     textOutput(outputId = ns("diagnosticTitle2")), 
                   #     align = "center",
                   #     style = "font-weight:bold"
                   # ),
                   # tags$p( textOutput(ns("diagnosticText2")) ),
                   # 
                   # # ******* Gelman plot text ********
                   # tags$h4( 
                   #     textOutput(outputId = ns("diagnosticGelmanTitle")), 
                   #     align = "center",
                   #     style = "font-weight:bold"
                   # ),
                   # tags$p( textOutput(ns("diagnosticGelmanText"))),
                   # 
                   # # ******* Summary Table  text ********
                   # tags$h4( 
                   #     textOutput(outputId = ns("diagnosticTablesTitle")), 
                   #     align = "center",
                   #     style = "font-weight:bold"
                   # ),
                   # tags$p( textOutput(ns("diagnosticTablesText"))),
            )
        ),
        
        
        #______________________________ Rendering the diagnostic plots  ______________________________
        fluidRow(
            # uiOutput(ns("diagPlots")),
            htmlOutput(ns('CNVDiagnosticPlots')),
            htmlOutput(ns('CNVautocorrelationPlots')),
            htmlOutput(ns('CNVGelmanPlots')),
            htmlOutput(ns('CNVSummaryTablels'))
        )
    )
}



# Function to help return the HTML to display the PDF files 
displayDiagnosticPDF <- function(pdf_file, directory){
    # Check for the file in the specified directory 
    check <- grepl(pattern    = pdf_file,
                   x          = directory) 
    
    if ( any(check) ){
        # Path to the file 
        pdf_path <- directory[check]
        # copy the file to the www directory 
        file.copy(from = pdf_path,
                  to   = "www")
        # render the pdf 
        return(paste('<iframe style="height:400px; width:100%" src="',pdf_file,'"></iframe>', sep = ""))
    }
}






#' Server portion of the HMM output portion of the infercnv Shiny app
#' 
#' @title diagnosticPlots()
#' 
#' @param input, output, session standard \code{shiny} 
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#'
#' @return Returns the infercnv diagnostic output figures. 
#' 
diagnosticPlots <- function(input, output, session,
                            infercnv_inputs){
    

    #------------------- Create diagnostic plots ---------------------------------
    
    output_path <- infercnv_inputs$dir()
    
    vals <- lapply( infercnv_inputs, function(i){ i() } )
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # output the text
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Text output if the plots are created 
    # output$diagnosticTitle1 <- renderText({ "Density And Trace Plots" })
    # output$diagnosticText1 <- renderText({ "Density and trace plots are used to help determine if the Markov chains are stable and the symmetry of the data. Trace plots should appear flat, this ensures that simulations are coming from a stable Markov chain. Because of the stochastic nature of the simulation, continuous variables should look like flat 'fuzzy caterpillars'. Density show the posterior distribution of the simulated values and should appear relatively symmetrical around the mean." })
    # output$diagnosticTitle2 <- renderText({ "Autocorrelation plots" })
    # output$diagnosticText2 <- renderText({ "Autocorrelation plots are a way to evaluate the randomness of the data, comparing simulated values at a specific iteration to previous iterations (lags). Autocorrelation plots show the correlation coefficients, a value between 1 and -1. values around 1 and -1 show correlation while values around 0 show no correlation."})
    # output$diagnosticGelmanTitle <- renderText({ "Gelman Plots" })
    # output$diagnosticGelmanText <- renderText({ "Gelman Plots are another source for determining convergence. Gelman Plots show the Gelman-Rubin statistics as the sampling iterations progress along the six chains. The Gelman-Rubin statistics measures the variance between and within chains, therefore a value of 1 means there is no variation between chains. A plot is created for each of the six chains, which corresponds to the CNV states probabilities (theta). If the chain is converging, the black line should converge onto the horizontal line stationary at 1." })
    # output$diagnosticTablesTitle <- renderText({"Summary Tables"})
    # output$diagnosticTablesText <- renderText({"Summary tables are created for each CNV region. These summery tables contain the Mean, Standard Deviation, 95% CI, Median, Geweke Diagnostic with Fraction In 1st Window = 0.1 and 2nd Window = 0.5, for the posterior distributions for each of the 6 states (thetas). The Geweke convergence diagnostic is a Z-score that reveals whether convergence is occurring. This is a Z-score with Normal(0,1), values 1.96 or more extreme are considered significant and therefore not converging. The 95% credibility interval is given by the 2.5%, 50%, 97.5% quantiles."})
    
    
    
    observeEvent(
        input$runDiagnosticsButton,
        {
            
                # Get the png files in the Bayesian output directory 
                bayesian_output_path <- list.files(path       = output_path, 
                                                   pattern    = "BayesNetOutput*", 
                                                   all.files  = FALSE, 
                                                   full.names = TRUE)
                
                bayesian_pdfs <- list.files(path       = bayesian_output_path, 
                                            pattern    = "*.pdf", 
                                            all.files  = FALSE, 
                                            full.names = TRUE)
                
                #--------------- CNV Trace and density -------------------
                
                
                if (vals$diagnostics == TRUE){
                    
                    output$CNVDiagnosticPlots <- renderText({
                        displayDiagnosticPDF(pdf_file = "CNVDiagnosticPlots.pdf",
                                             directory = bayesian_pdfs)
                    })
                    
                    output$CNVautocorrelationPlots <- renderText({
                        displayDiagnosticPDF(pdf_file = "CNVautocorrelationPlots.pdf",
                                             directory = bayesian_pdfs)
                    })
                    
                    output$CNVGelmanPlots <- renderText({
                        displayDiagnosticPDF(pdf_file = "CNVGelmanPlots.pdf",
                                             directory = bayesian_pdfs)
                    })
                    
                    output$CNVSummaryTablels <- renderText({
                        displayDiagnosticPDF(pdf_file = "CNVSummaryTablels.pdf",
                                             directory = bayesian_pdfs)
                    })
                

                
                insertUI(
                    # ID 
                    selector = "#diagnosticAlertMessage",
                    # Make a class so can remove the message 
                    ui = tags$div(
                        class = "alert alert-success alert-dismissible",
                        HTML("<span class='fa fa-check fa-fw' aria-hidden='true'> \
                             </span> Showing Diagnostic Plots <button type='button' \
                             class='close' data-dismiss='alert'>&times;</button>"
                        )
                    )
                )
                
                
            } else {
                
                insertUI(
                    # ID 
                    selector = "#diagnosticAlertMessage",
                    # Make a class so can remove the message 
                    ui = tags$div(
                        class = "alert alert-danger alert-dismissible",
                        HTML("<span class='fa fa-times fa-fw' aria-hidden='true'> </span> \
                 Must run infercnv in HMM mode with the diagnostics option in order to create diagnostic plots. \
                 <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
                )
                
            }
                
        }) #observe event 
}

















