#!/usr/bin/env Rscript

#' User inteface portion for the Bayesian Analysis portion of the infercnv app
#' 
#' @title bayesianAnalysisUI()
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements for the bayesian analysis 
#' 

bayesianAnalysisUI <- function(id) {
    # set the name space
    ns <- NS(id)
    
    # CSS styling to be included 
    tags$head(includeCSS("www/infercnv.style.css"))
    
    # create teh tag list 
    tagList(
        
        tags$div( class = "analysisTabTitles",
                  
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # #############################         Bayesian Output TAB      ###########################
                  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  
                  fluidRow(
                      tags$h2("Bayesian Analysis Plots", 
                              align = "center",
                              style = "font-weight:bold")
                  ),
                  
                  fluidRow(
                      column(width = 10,
                             offset = 1,
                             tags$p("\t If infercnv was ran with the Hidden Markov Model option, 
                         then a subsequent bayesian analysis is preformed and the following 
                         plots are generated . 
                         All of these plots can be found in the output folder.")
                      )
                  ),
                  br(),
                  br(),
                  hr(),
        ),
        
        #______________________________ Output informative text ______________________________
        
        # Output text information when HMM is chosen
        fluidRow(
            column(width = 10,
                   offset = 1,
                   tags$h4( textOutput(outputId = ns("ProbOfNormalTitle")), 
                            align = "center",
                            style = "font-weight:bold"),
                   tags$p( textOutput(ns("ProbOfNormal")))
            )
        ),
        
        br(),
        
        fluidRow(
            column(width = 10,
                   offset = 1,
                   tags$h4( textOutput(outputId = ns("ProbOfNormalTextBeforeFilter")),
                            style    = "font-weight:bold")
            )
        ),
        fluidRow(
            tags$div(
                imageOutput(outputId = ns('PreFilteringPng'),
                            width    = "90%",
                            height   = "90%"),
                align = "center")
        ),
        
        br(),br(),hr(),
        
        fluidRow(
            column(width = 11,
                   offset = 1,
                   tags$h4( textOutput(outputId = ns("ProbOfNormalTextAfterFilter")),
                            style    = "font-weight:bold")
            )
        ),
        fluidRow(
            tags$div(
                imageOutput(outputId = ns('PostFilteringPng'),
                            width    = "90%",
                            height   = "90%"),
                align = "center")
        )
    ) # taglist
}



#' Server portion of the Bayesian Analysis portion of the infercnv Shiny app
#' 
#' 
#' @title bayesianAnalysis()
#' 
#' @param input, output, session standard \code{shiny} 
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#'
#' @return Returns the prefiltered and postfiltered bayesian analysis figures 

bayesianAnalysis <- function(input, output, session,
                             infercnv_inputs
                             ){

    # Set the constants
    ## output directory
    output_path <- infercnv_inputs$dir()
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # *************************   user inputs    *************************
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Seperate the user inputs into a friendly format 
    vals <- lapply( infercnv_inputs, function(i){ i() } )
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # *********************      Bayesian      ****************************
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Get the png files in the Bayesian output directory 
    bayesian_output_path <- list.files(path       = output_path, 
                                       pattern    = "BayesNetOutput*", 
                                       all.files  = FALSE, 
                                       full.names = TRUE)
    bayesian_pngs <- list.files(path       = bayesian_output_path, 
                                pattern    = "*.png", 
                                all.files  = FALSE, 
                                full.names = TRUE)
    
    
    
    # ______________     PreFiltering     ____________________________ 
    
    check <- grepl(pattern = "*PreFiltering.png", 
                   x = bayesian_pngs)
    if ( vals$HMM == TRUE && any(check) ){
        output$ProbOfNormalTitle <- renderText({ "Probability of a CNA being a normal state" })
        output$ProbOfNormal <- renderText({ "The following Heat maps show the probability of each CNA not being normal.
                                                Color intensities correspond to the posterior probability between 0 and 1
                                                of the CNA regions not being normal (1 - P(CNA=normal)). The darker the
                                                red, the greater the probability that CNA is not normal. 
                                                
                                                CNA regions identified by the HMM are filtered out if the CNA region's posterior probability of being normal exceeds a user specified threshold.
                                                
                                                The first figure shows the CNA regions probabilities before filtering is applied.
                                                The second figure shows CNA regions probabilities after filtering is applied" })
        output$ProbOfNormalTextBeforeFilter <- renderText({ "Before Filtering:" })
        output$ProbOfNormalTextAfterFilter <- renderText({ "After Filtering:" })
        
        
        # Path to the file 
        PreFiltering <- bayesian_pngs[check]
        
        output$PreFilteringPng <- renderImage({
            # create the list for image rendering
            list(src = PreFiltering,
                 contentType = 'image/png',
                 width = "90%",
                 hight = "90%",
                 alt = "This is alternate text")
        },
        deleteFile = FALSE)
    }    
    
    
    #______________     PostFiltering     ____________________________ 
    
    check <- grepl(pattern = "*PostFiltering.png", 
                   x = bayesian_pngs)
    if ( any(check) ){
        PostFiltering <- bayesian_pngs[check]
        
        output$PostFilteringPng <- renderImage({
            # create the list for image rendering
            list(src = PostFiltering,
                 contentType = 'image/png',
                 width = "90%",
                 hight = "90%",
                 alt = "This is alternate text")
        },
        deleteFile = FALSE)
    }
    
}
