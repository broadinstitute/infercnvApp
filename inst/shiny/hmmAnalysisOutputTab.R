#!/usr/bin/env Rscript

#' User Interface for the HMM analysis tab of the infercnv shiny app.
#' 
#' @title hmmAnalysisUI()
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements for the HMM output
#' 

hmmAnalysisUI <- function(id) {
    # set the name space
    ns <- NS(id)
    
    tags$head(includeCSS("www/infercnv.style.css"))
    
    # create the tag list 
    tagList(
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ############################      HMM Analysis output tab      ##########################
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tags$div( class = "analysisTabTitles",
                  # HMM Title 
                  fluidRow(
                      tags$h2("HMM Infercnv Figure", 
                              align = "center",
                              style = "font-weight:bold")
                  ),
                  # HMM body 
                  fluidRow(
                      column(width = 10, offset = 1,
                             tags$p("Infercnv currently supports two models for HMM-based CNV prediction, what we refer to as the i3 and i6 models.")
                      )
                  ),
                  br(),br(),hr()
        ),
        
        fluidRow(
            # HMM state title and text 
            column(width = 3, offset = 1,
                   tags$h4( textOutput(outputId = ns("hmm_state_title"))),
                   tags$p( textOutput(outputId = ns("hmm_state_text")))
            ),
            # HMM image output 
            column(width = 8,
                   imageOutput(outputId = ns('hmm_mode_samples_png'))
            )
        )
    ) # TagList
}



#' Server portion of the HMM output portion of the infercnv Shiny app
#' 
#' @title analysisPage()
#' 
#' @param input, output, session standard \code{shiny} 
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#'
#' @return Returns the infercnv HMM output figure. 

hmmAnalysis <- function(input, output, session,
                        infercnv_inputs
                        ){
    
    # Set the  constants
    # output directory
    output_path <- infercnv_inputs$dir()
    
    # Get the png files in the output directory 
    output_file_list <- list.files(path       = output_path, 
                                   pattern    = "*.png", 
                                   all.files  = FALSE, 
                                   full.names = TRUE)
    

    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # *****************    Remaining Plots    *******************
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # hmm_mode-samples.png
    check <- grepl(pattern = "*hmm_mode-samples.png", 
                   x = output_file_list)
    if ( any(check) ){
        output$hmm_state_title <- renderText({ "HMM CNV State Predictions" })
        output$hmm_state_text <- renderText({ "The following figure is a heatmap revealing CNV states as predicted by the Hidden Markov Model (HMM)." })
        
        # Get the HMM PNG file
        hmm_mode_samples <- output_file_list[check]
        
        output$hmm_mode_samples_png <- renderImage({
            # create the list for image rendering
            list(src = hmm_mode_samples,
                 contentType = 'image/png',
                 width = "100%",
                 alt = "This is alternate text")
        },
        deleteFile = FALSE)
    }
}
