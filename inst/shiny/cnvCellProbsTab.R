#!/usr/bin/env Rscript

#' User Interface for the CNV and Cell probability analysis tab of the infercnv shiny app.
#' 
#' @title cnvCellProbsUI()
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements for the NV and Cell probability output
#' 

cnvCellProbsUI <- function(id) {
    ns <- NS(id)
    
    # create a tag list 
    tagList(
        tags$div( 
            class = "analysisTabTitles",
            
            fluidRow(
                tags$h2("State Probabilitiy Plots", 
                        align = "center",
                        style = "font-weight:bold")
            ),
            
            fluidRow(
                column(width = 10, offset = 1,
                       tags$p( "By default InferCNV generates several posterior probability plots if HMM and Bayesian mixture model is used. 
                               For each predicted CNV region, the posterior probability of the entire CNA region belonging to each of the 6 
                               states is plotted in cnvProbs.pdf, along with posterior probability of each cell line belonging to each state 
                               in cellProbs.pdf. More information can be found ",
                               HTML('<a href="https://github.com/broadinstitute/infercnv/wiki/Bayesian-Network-Latent-Mixture-Model">here</a>'),
                               "."
                       ),
                       tags$p("If the app is running in a browser, the plots will appear in this tab. If not running in a browser, the PDF files 
                              will open outside of the app.")
                )
            ),
            br(),br(),hr()
        ),
        
        
        
        # ~~~~~~~~~~~~~~~~~ button to run the analysis  ~~~~~~~~~~~~~~~~~
        fluidRow(
            actionButton(inputId = ns("showProbabilityButton"), 
                         label = "Show Probability Plots",
                         width = '100%',
                         style="color: #fff; background-color: #68D77B")
        ),
        
        # Message to user if sucessfully printed plots
        fluidRow(
            alertMessage <-"",
            tags$div(id = "probAlertMessage", alertMessage)
        ),
        
        
        
        
        # ~~~~~~~~~~~~~~~~~ Output informative text  ~~~~~~~~~~~~~~~~~
        # Output text information 
        fluidRow(
            column(width = 10, offset = 1,
                   
                   # ******* trace and density plots text ********
                   tags$h4( 
                       textOutput(outputId = ns("probabilityTitle1")), 
                       align = "center",
                       style = "font-weight:bold"
                   ),
                   tags$p( textOutput(ns("probabilityText1"))),
                   
                   # ******* autocorrelation plot text ********
                   
                   tags$h4( 
                       textOutput(outputId = ns("probabilityTitle2")), 
                       align = "center",
                       style = "font-weight:bold"
                   ),
                   tags$p( textOutput(ns("probabilityText2")) )
            )
        ),
        
        
        # ~~~~~~~~~~~~~~~~~ Rendering the probability plots  ~~~~~~~~~~~~~~~~~
        fluidRow(
            htmlOutput(ns('cnvProbs')),
            htmlOutput(ns('cellProbs'))
        )
    )
}



#' Function to help return the HTML to display the PDF files 
#' 
#' @title displayProbabilityPDF()
#' 
#' @param pdf_file The name of the PDF file 
#' @param directory The directory where to look for the PDF
#'
#' @return Returns text needed needed to plot the image 
#' 
displayProbabilityPDF <- function(pdf_file, directory){
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
        return(paste('<iframe style="height:800px; width:100%" src="',pdf_file,'"></iframe>', sep = ""))
    }
}


#' Server portion of the CNV and cell probability output portion of the infercnv Shiny app
#' 
#' @title cnvCellProbs()
#' 
#' @param input, output, session standard \code{shiny} 
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#'
#' @return Returns the infercnv CNV and Cell probability output figure. 
#' 
cnvCellProbs <- function(input, output, session,
                             infercnv_inputs){
    
    
    #------------------- Get Probabilty plots ---------------------------------
    
    output_path <- infercnv_inputs$dir()
    
    vals <- lapply( infercnv_inputs, function(i){ i() } )
    
    observeEvent(
        input$showProbabilityButton,
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
            
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            #--------------- Cell and CNV probability plots -------------------
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            
            if (vals$HMM == TRUE){
                
                # CNV Probability
                cnvProbsPDF <- list.files(path       = bayesian_output_path, 
                                          pattern    = "*cnvProbs.*", 
                                          all.files  = FALSE, 
                                          full.names = FALSE)
                
                output$cnvProbs <- renderText({
                    displayProbabilityPDF(pdf_file = cnvProbsPDF,
                                          directory = bayesian_pdfs)
                })
                
                # Cell Probabilities 
                cellProbsPDF <- list.files(path       = bayesian_output_path, 
                                           pattern    = "*cellProbs.*", 
                                           all.files  = FALSE, 
                                           full.names = FALSE)
                output$cellProbs <- renderText({
                    displayProbabilityPDF(pdf_file = cellProbsPDF,
                                          directory = bayesian_pdfs)
                })
                
                


                # Message to user for success 
                insertUI(
                    # ID 
                    selector = "#probAlertMessage",
                    # Make a class so can remove the message 
                    ui = tags$div(
                        class = "alert alert-success alert-dismissible",
                        HTML("<span class='fa fa-check fa-fw' aria-hidden='true'> \
                             </span> Showing Probability Plots <button type='button' \
                             class='close' data-dismiss='alert'>&times;</button>"
                        )
                    )
                )
                
                
            } else {
                # Message to user if not sucessful 
                insertUI(
                    # ID 
                    selector = "#diagnosticAlertMessage",
                    # Make a class so can remove the message 
                    ui = tags$div(
                        class = "alert alert-danger alert-dismissible",
                        HTML("<span class='fa fa-times fa-fw' aria-hidden='true'> </span> \
                 Must run infercnv in HMM mode in order to create plots. \
                 <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
                )
            }
        }) #observe event 
}

















