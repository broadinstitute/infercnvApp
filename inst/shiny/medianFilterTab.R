#!/usr/bin/env Rscript


#' User Interface for the median analysis tab of the infercnv shiny app.
#' 
#' @title medianFilterUI()
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements for the median filter
#' 

medianFilterUI <- function(id) {
    # set the name space
    ns <- NS(id)
    
    tags$head(includeCSS("www/infercnv.style.css"))
    # create teh tag list 
    tagList(
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ##############################     Median Filter output tab     ###########################
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tags$div( class = "analysisTabTitles",
                  fluidRow(
                      
                      # __________________    Header tite and text description     __________________
                      tags$h2("Median Filter Figure Output", 
                              align = "center",
                              style = "font-weight:bold"),
                  ),
                  
                  fluidRow(
                      column(width = 10, offset = 1,
                             tags$p("The following is an option for an add-on ",
                                    HTML('<a href="https://github.com/broadinstitute/inferCNV/wiki/De-noising-Filters">median filtering</a>'),
                                    " that can be applied to smooth the visual output 
                                    of inferCNV." ),
                             
                             tags$br(),
                             
                             tags$p("The filtering takes into account chromosomes and the clusters or subclusters 
                                    that have been defined as boundaries. It also keeps the hierarchical clustering 
                                    previously defined intact in order for it to be representative of how it was 
                                    obtained.")
                      )
                  ),
                  
                  br(),
                  hr()
        ),
        
        #__________________     Post Processing Options     __________________
        fluidRow(
            column(width = 10, offset = 1, 
                   fluidRow(
                       actionButton(inputId = ns("Median_Filter"), 
                                    label   = "Apply Median Filter",
                                    width   = '100%',
                                    style   = "color: #fff; background-color: #68D77B")
                   )
            )
        ),
        
        br(),
        
        # Output the image 
        fluidRow(
            column(
                width = 10, offset = 1, 
                imageOutput(outputId = ns('median_filtered_png')) 
            )
        )
    ) # taglist
}



#' Server portion of the Median Filter output portion of the infercnv Shiny app
#' 
#' 
#' @title medianFilter()
#' 
#' @param input, output, session standard \code{shiny}
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#' @param infercnv_obj infercnv object created using createObject().
#'
#' @return Returns the median filter output figure. 

medianFilter <- function(input, output, session,
                         infercnv_inputs,
                         infercnv_obj ){
    
    
    # Set the  constants
    # output directory
    output_path <- infercnv_inputs$dir()
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Loading Message Creation 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Function for loading message 
    shiny_busy1 <- function() {
        # use &nbsp; for some alignment, if needed
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
            '<p> Applying Median Filter...',
            '<span data-display-if="$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;">',
            '<i class="fa fa-spinner fa-pulse fa-2x fa-fw" style="color:orange"></i>',
            '</span>',
            '</p>'
        ))
    }
    # Function for loading message 
    shiny_busy2 <- function() {
        # use &nbsp; for some alignment, if needed
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
            '<p> Plotting Median Filter...',
            '<span data-display-if="$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;">',
            '<i class="fa fa-spinner fa-pulse fa-2x fa-fw" style="color:orange"></i>',
            '</span>',
            '</p>'
        ))
    }
    
    
    
    observeEvent(
        input$Median_Filter,
        {
            if (input$Median_Filter == TRUE){
                
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #      Run Median Filtering
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Print the loading message 
                showNotification(id = "loadingMessage1",
                                 ui = shiny_busy1(),
                                 duration = NULL)
                
                # Apply the median filter 
                infercnv_obj_medianfiltered = infercnv::apply_median_filtering(infercnv_obj)
                removeNotification(id="loadingMessage1")
                
                
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #            Plotting 
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Print the loading message 
                showNotification(id = "loadingMessage2",
                                 ui = shiny_busy2(),
                                 duration = NULL)
                
                infercnv::plot_cnv(infercnv_obj     =  infercnv_obj_medianfiltered, 
                                   out_dir          =  output_path,
                                   output_filename  =  'infercnv.median_filtered', 
                                   x.range          =  "auto",
                                   x.center         =  1,
                                   title            =  "Infercnv Median Filtered", 
                                   color_safe_pal = FALSE)
                
                # Remove the loading message 
                removeNotification(id = "loadingMessage2")
                
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                #       Render the image 
                # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # check that the image is created 
                
                # Get the png files in the output directory 
                output_file_list <- list.files(path       = output_path, 
                                               pattern    = "*.png", 
                                               all.files  = FALSE, 
                                               full.names = TRUE)
                
                check <- grepl(pattern = "infercnv.median_filtered.png", 
                               x       = output_file_list)
                if ( any(check) ){
                    median_filtered <- output_file_list[check]
                    output$median_filtered_png <- renderImage({
                        # create the list for image rendering
                        list(src = median_filtered,
                             contentType = 'image/png',
                             width       = "90%",
                             alt         = "This is alternate text")
                    },
                    deleteFile = FALSE)
                }
            }
        })
}
