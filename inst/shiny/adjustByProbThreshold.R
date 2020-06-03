#!/usr/bin/env Rscript


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Functions 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

run_plotNormal <- function(obj){
    # run_plotNormal:
    #   plot the infercnv output (expression values)
    #
    if (infercnv:::getArgs(obj)$plotingProbs == TRUE){
        # get probability of the cnv's belonging to each state
        cnv_means <- sapply(obj@cnv_probabilities,function(i) colMeans(i))
        # Adjust the probabilities so greater probability corresponds to less likely to be normal
        if ( infercnv:::getArgs(obj)$HMM_type == 'i6'){
            normal_prob <- 1 - cnv_means[3,]
        } else {
            normal_prob <- 1 - cnv_means[2,]
        }
        obj@expr.data[,] <- 0
        lapply(seq_along(normal_prob), function(i) {
            ## change the states to normal states
            obj@expr.data[obj@cell_gene[[i]]$Genes , obj@cell_gene[[i]]$Cells ] <<- normal_prob[i]
        })
        # sub_obj <- simplify(obj)
        # plotting(sub_obj)
        plotting(obj)
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HMM PLOT 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the HMM state outputs 
run_HMM_plot <- function(obj, hmm_obj){
    # reduce matrix size to one cell for each group
    # hmm_obj <- simplify(hmm_obj)
    # plot the hmm groups
    HMM_plot(obj     = obj, 
             hmm_obj = hmm_obj)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reduce the size of the matrix to one cell per group 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
simplify <- function(obj){
    # Combine the Cell groups into one cell per group
    # 1. Subset the data for each reference and get the gene means 
    # 2. Subset the observed data and get the gene means 
    # 3. Change the indexing of the cells 
    # 4. plot 
    
    # 1. 
    # get MEAN of the references
    ref <- sapply(obj@reference_grouped_cell_indices,function(i){ rowMeans(obj@expr.data[,i]) })
    
    # 2.
    # get obsered groupings and subset the data acordingly 
    a <- as.matrix(sapply(obj@observation_grouped_cell_indices, function(i){ rowMeans(obj@expr.data[,i]) }))
    ## combine data 
    sub_obj <- obj
    obj@expr.data <- cbind(a,ref)
    
    # 3. 
    # Observed
    lapply(seq_along(obj@observation_grouped_cell_indices), function(i) {
        obj@observation_grouped_cell_indices[[i]] <<- which(colnames(obj@expr.data) %in% names(obj@observation_grouped_cell_indices[i]))
    })
    # Reference
    lapply(seq_along(obj@reference_grouped_cell_indices), function(i) {
        obj@reference_grouped_cell_indices[[i]] <<- which(colnames(obj@expr.data) %in% names(obj@reference_grouped_cell_indices[i]))
    })
    # tumor subcluster
    sapply(colnames(obj@expr.data), function(i){
        obj@tumor_subclusters$subclusters[[i]] <<- which(colnames(obj@expr.data) %in% i)
    })
    obj@tumor_subclusters$hc <- NULL
    
    return(obj)
    
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting probabilities of normal 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate the plot that shows the probability of the individule CNV's of being normal
plotting <- function(obj) {  
    # 4. 
    # PLOT 
    plotTitle <- sprintf(" 1 - Probabilities of Normal \n Threshold: %s", infercnv:::getArgs(obj)$BayesMaxPNormal)
    infercnv::plot_cnv(infercnv_obj          = obj,
                       out_dir               = infercnv:::getArgs(obj)$out_dir,
                       k_obs_groups          = infercnv:::getArgs(obj)$k_obs_groups,
                       # cluster_by_groups     = getArgs(obj)$cluster_by_groups,
                       # cluster_by_groups = F, 
                       # cluster_references = F,
                       # k_obs_groups = 4,
                       title                 = plotTitle,
                       output_filename       = "test",
                       write_expr_matrix     = FALSE,
                       x.center              = 0,
                       x.range               = c(0,1)
    )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plotting HMM States Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate the plot that shows the identified CNV's colored by their state 
HMM_plot <- function(obj, hmm_obj){
    # determine the states that are used 
    ifelse(infercnv:::getArgs(obj)$HMM_type == 'i6',
           yes = {hmm_center = 3 ; hmm_state_range = c(0,6)},
           no = {hmm_center = 2; hmm_state_range = c(1,3)})
    # Generate the plot
    plotTitle <- sprintf(" 1 - Probabilities of Normal \n Threshold: %s", infercnv:::getArgs(obj)$BayesMaxPNormal)
    infercnv::plot_cnv(infercnv_obj          = hmm_obj,
                       out_dir               = infercnv:::getArgs(obj)$out_dir,
                       k_obs_groups          = infercnv:::getArgs(obj)$k_obs_groups,
                       # cluster_by_groups     = getArgs(obj)$cluster_by_groups,
                       # cluster_by_groups = F, 
                       # cluster_references = F,
                       # k_obs_groups = 4,
                       title                 = plotTitle,
                       output_filename       = "test",
                       write_expr_matrix     = FALSE,
                       x.center              = hmm_center,
                       x.range               = hmm_state_range
    )
    
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create summary table Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a summer table to show the number of CNVs before and after changing the threshold/cutoff value 
SummaryTable <- function(obj, 
                         filteredObj){
    # total CNV's unfiltered
    unfilteredCNV <- length(obj@cell_gene)
    # total CNV's filtered
    filteredCNV <- length(filteredObj@cell_gene)
    df <- data.frame(check.names = FALSE,
                     # Step = c ("Before Filtering",
                     #           "After Filtering"),
                     Threshold = c (1.0,  # <-  this is zero for now because it is set by run, the object is saved before the threshold is saved in the args 
                                    infercnv:::getArgs(filteredObj)$BayesMaxPNormal),
                     "Total CNVs"  = c(unfilteredCNV,
                                       filteredCNV)
                     
    )
    return(df)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getBayesMaxPNormal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getBayesMaxPNormal <- function(obj){
    title <- sprintf("Changes in CNV count with threshold set to %s", infercnv:::getArgs(obj)$BayesMaxPNormal)
    print(title)
    return(title)
}




#' User Interface for the Probability Threshold tab of the infercnv shiny app.
#' 
#' @title adjustByProbThresholdUI()
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements for the probability threshold output
#' 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                               UI (User Interface)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## 1. add slider window numbereed 0.1-0.9 representing the probability threshold
## 2. add button for plotting after updating the threshold value
## 3. table for summarizations
## 4. set the image output 
adjustByProbThresholdUI <- function(id) {
    # set the name space
    ns <- NS(id)
    
    tags$head(includeCSS("www/infercnv.style.css"))
    
    # create the tag list 
    tagList(
        
        tags$div( class = "analysisTabTitles",
                  # Title 
                  fluidRow(
                      tags$h2("Filtering CNA's based on probabilities", 
                              align = "center",
                              style = "font-weight:bold")
                  ),
                  
                  fluidRow(
                      column(1),
                      column(10,
                             tags$p("CNA regions identified by the HMM are filtered out if the CNA region's posterior probability of being 
                              normal exceeds a specified threshold. 
                              This combats possibility of miss identified CNAs by removing CNAs that are most likely to be normal and not a true CNA events. 
                              By default this threshold is set to 0.5, given this any CNA region that has a posterior probability of being of a normal state greater than 0.5 is relabeled 
                                as 'normal' and no longer considered an identified CNA region. 
                              A threshold of 0.5 was chosen for default as it tends to be more lenient threshold. 
                              This threshold can be adjusted by setting the Bayes Max Probability of Normal State argument to a value between 0 and 1 in InferCNV's analysis options. 
                              The Bayesian network latent mixture model can be completely avoided by setting R BayesMaxPNormal to 0.0")
                      )
                  ),
                  tags$hr()
        ),
        
        # Slider and table 
        fluidRow(
            column(width = 6,
                   ## Slider
                   wellPanel( 
                       # 1. 
                       sliderInput(inputId = ns("num"), 
                                   width = "100%",
                                   label   = "Select A Threshold: ", 
                                   value = 0.5, 
                                   min = 0.0, max = 1.0, 
                                   step=0.05),
                       # 2.
                       actionButton(inputId = ns("hmmStateButton"), 
                                    label = "HMM State View",
                                    width = '100%',
                                    style="color: #fff; background-color: #68D77B"),
                       actionButton(inputId = ns("probNormalButton"), 
                                    label = "Prob of Being Normal",
                                    width = '100%',
                                    style="color: #fff; background-color: #68D77B")
                   )
            ),
            # 3. TABLE
            column(width = 5, offset = 1,
                   tableOutput(outputId = ns("tableSummary"))
            )
        ),
        # numericInput(inputId = "num", label = "Select a threshold.", value = 0.5, min = 0.1, max = 1, step=.1),
        
        # 4. PLOT
        fluidRow(
            imageOutput(outputId = ns("threshold_ploting"))
        )
    )
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                       SERVER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#' Server portion of the  probability threshold output portion of the infercnv Shiny app
#' 
#' @title adjustByProbThreshold()
#' 
#' @param input, output, session standard \code{shiny} 
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#'
#' @return Returns the infercnv HMM output figure. 
#' 
adjustByProbThreshold <- function(input, output,session,
                                  infercnv_inputs) {
    
    # ~~~~~~~~~~~~~~~~~
    #   user inputs    
    # ~~~~~~~~~~~~~~~~~
    # Seperate the user inputs into a friendly format 
    vals <- lapply( infercnv_inputs, function(i){ i() } )
    
    # if infercnv was ran using HMM 
    if ( vals$HMM == TRUE ){
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Find and read object file
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # output path 
        output_path <- infercnv_inputs$dir()
        # path to the bayesian output directory
        bayesian_output_path <- list.files(path       = output_path, 
                                           pattern    = "BayesNetOutput*", 
                                           all.files  = FALSE, 
                                           full.names = TRUE)
        # Bayesian output Object
        bayesian_object_path <- list.files(path       = output_path, 
                                           pattern    = ".hmm_mode-samples.mcmc_obj", 
                                           all.files  = FALSE, 
                                           full.names = TRUE)
        # read in the object 
        obj <- readRDS(bayesian_object_path)
        
        # HMM output Object
        HMM_object_path <- list.files(path       = output_path, 
                                      pattern    = "*hmm_mode-samples.infercnv_obj", 
                                      all.files  = FALSE, 
                                      full.names = TRUE)
        # read in the HMM object 
        hmm_obj <- readRDS(HMM_object_path)
        hmm_states <- hmm_obj@expr.data
        # obj@expr.data <- hmm_obj@expr.data
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Remove CNVs
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        sub_obj <- reactive({
            obj@args$BayesMaxPNormal <- input$num
            infercnv:::removeCNV(obj = obj, 
                                 HMM_states = hmm_states)
        })
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # TABLE
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Generate the statistics table 
        output$tableSummary <- renderTable(
            {
                SummaryTable(obj = obj, 
                             filteredObj = sub_obj()[[1]])
            }, 
            caption = paste("\t Difference in CNV count after applying new threshold value."),
            align = "l",
            caption.placement = getOption("xtable.caption.placement", "top")
        )
        
        
        
        
        # Function for loading message 
        shiny_busy <- function() {
            # use &nbsp; for some alignment, if needed
            HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
                '<p> Plotting ...',
                '<span data-display-if="$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;">',
                '<i class="fa fa-spinner fa-pulse fa-2x fa-fw" style="color:orange"></i>',
                '</span>',
                '</p>'
            ))
        }
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # IMAGE GERNERATION
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Generate the CNV plots based on what button is clicked 
        #   hmmStateButton   :  Plot the probablity of being normal 
        #   probNormalButton :  Plot the probablity of being normal 
        #
        # Once the hmmStateButton button is pressed, proform the following actions
        observeEvent(
            input$hmmStateButton, # action button to wait for 
            {
                # Start the loading message 
                showNotification(id = "loadingMessage",
                                 ui = shiny_busy(),
                                 duration = NULL)
                
                # set the new states after removing
                hmm_obj@expr.data <- sub_obj()[[2]]
                # now plot the new heatmap
                run_HMM_plot( obj = sub_obj()[[1]],
                              hmm_obj =  hmm_obj)
                output$threshold_ploting <- renderImage(
                    {
                        # list is returned that has the file name 
                        list(src    = file.path(bayesian_output_path,"test.png"),
                             contentType = 'image/png',
                             width  = "90%", 
                             alt    = "HMM Plot")
                    }
                )
                removeNotification(id="loadingMessage")
            }
        )
        # Plot the probablity of being normal 
        observeEvent(
            input$probNormalButton,
            {
                # Start the loading message 
                showNotification(id = "loadingMessage",
                                 ui = shiny_busy(),
                                 duration = NULL)
                
                
                run_plotNormal( obj = sub_obj()[[1]] )
                output$threshold_ploting <- renderImage(
                    {
                        # list is returned that has the file name 
                        list(src    = file.path(bayesian_output_path,"test.png"),
                             contentType = 'image/png',
                             width  = "90%",
                             alt    = "Probability Plot")
                    }
                )
                removeNotification(id="loadingMessage")
            }
        )
    }
}
