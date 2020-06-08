#!/usr/bin/env Rscript

#' Shiny Module for running the infercnv analysis
#'
#' @title runInfercnvUI(): User Interface for the run infercnv analysis potion of the application.
#'
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing the UI elements for creating an infercnv object
#'

runInfercnvUI <- function(id) {
    ns <- NS(id)

    # Start the new RunInfercnv section
    tagList(

        #-------------------------- Run Analysis tab --------------------------

        fluidRow(
            tags$h2(tags$strong("Options for running analysis"), align = "center"),
            tags$br(),
            
            tabsetPanel(
                tabPanel("Options",
                         column(width = 12,
                                
                                tags$br(),
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                #------------ Output Directory ---------------
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                wellPanel(
                                    
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Choose Directory To Output Results:")),
                                    
                                    # Output direcctory: set the output directory

                                    shinyFiles::shinyDirButton( id = ns("dir"),
                                                    label =  "Output directory",
                                                    title = "Set"),
                                    verbatimTextOutput(outputId = ns("show_dir"), placeholder = TRUE),
                                    helpText("Location for where to deposit outputs.")
                                ),
                                
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                # ------------ Basic Options -----------------
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                wellPanel(
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Basic Options:")),

                                    

                                    # cut offs
                                    numericInput(inputId = ns("cutoff"),
                                                 label   = tags$strong("Cut-off"),
                                                 value   = 1),
                                    helpText("Cut-off for the min average read counts per gene among reference cells"),

                                    tags$hr(style="border-color: grey;"),
                                    tags$br(),
                                    
                                    # num_ref_groups
                                    textInput(inputId = ns("num_ref_groups"),
                                              label = tags$strong("Number of Reference Groups"),
                                              placeholder = "NULL",
                                              value = NULL),
                                    helpText("The number of reference groups or a list of \n
                                                    indices for each group of reference indices in \n
                                                    relation to reference_obs. (default: NULL)"),
                                    
                                    tags$hr(style="border-color: grey;"),
                                    tags$br(),
                                    
                                    # cluster_by_groups
                                    checkboxInput(inputId = ns("cluster_by_groups"),
                                                  label   = tags$strong("Cluster By Groups"),
                                                  value   = TRUE),
                                    helpText("If observations are defined according to groups (ie. patients), each group \n
                                                    of cells will be clustered separately. (default=FALSE, instead will use k_obs_groups setting)"),
                                    
                                    tags$hr(style="border-color: grey;"),
                                    tags$br(),
                                    
                                    # k_obs_groups
                                    numericInput(inputId = ns("k_obs_groups"),
                                                 label = tags$strong("Number of Obsered Groups"),
                                                 value = 1),
                                    helpText("Number of groups in which to break the observations. (default: 1)"),
                                    
                                ),

                                

                                
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                # ------------ Downstream Analyses -----------------
                                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                                wellPanel(

                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Downstream Analyses: ")),

                                    # HMM
                                    h5("Runs HMM to predict CNV level"),
                                    checkboxInput(inputId = ns("HMM"),
                                                  label   = tags$strong("Run HMM"),
                                                  value   = FALSE),


                                    conditionalPanel(
                                        condition = "input.HMM == 1", # == 1 equates  to == TRUE
                                        ns = ns,

                                        wellPanel(
                                            fluidRow(
                                            # HMM_transition_prob
                                            numericInput(inputId = ns("HMM_transition_prob"),
                                                         label = h5("HMM Transition Probability"),
                                                         value = 1e-6),
                                            helpText("Sets the transition probability in Hidden Markov Model (default: 1e-6)")
                                            ),
                                            tags$hr(),

                                            fluidRow(
                                            # HMM_report_by
                                            radioButtons(inputId = ns("HMM_report_by"),
                                                         label = tags$strong("HMM Report By"),
                                                         choices = list("subcluster" = "subcluster",
                                                                        "cell" = "cell",
                                                                        "consensus"  = "consensus"),
                                                         selected = "subcluster"),
                                            helpText("Note: reporting is performed entirely separately from the HMM prediction.
                                                                So, you can predict on subclusters, but get per-cell level reporting (more voluminous output).")
                                            ),
                                            tags$hr(),
                                            fluidRow(
                                                column(width = 6,
                                                       # HMM_type
                                                       radioButtons(inputId = ns("HMM_type"),
                                                                    label = h5("HMM Model Type"),
                                                                    choices = list("i6" = "i6",
                                                                                   "i3" = "i3"),
                                                                    selected = "i6"),
                                                       helpText("HMM model type. Options: (i6 or i3): \n
                                                                    i6: infercnv 6-state model (0, 0.5, 1, 1.5, 2, >2) where state emissions are calibrated based on simulated CNV levels.
                                                                    i3: infercnv 3-state model (del, neutral, amp) configured based on normal cells and HMM_i3_pval")
                                                ),


                                                column(width = 6,
                                                       tags$div( shiny::HTML('<img src="images/infercnv_i6HMM_model.png" width="250">')),
                                                       tags$div( shiny::HTML('<img src="images/i3HMM_model.png" width="250">'))
                                                )
                                            ),

                                            tags$hr(),

                                            # IF i3 is selected, then  these options will be avalible
                                            conditionalPanel(
                                                condition = "input.HMM_type == 'i3'",
                                                ns = ns,

                                                wellPanel(
                                                    # HMM_i3_pval
                                                    numericInput(inputId = ns("HMM_i3_pval"),
                                                                 label = h5("HMM_i3_pval"),
                                                                 value = 0.05),
                                                    helpText("p-value for HMM i3 state overlap (default: 0.05)"),

                                                    # HMM_i3_use_KS
                                                    h5("Use the KS test statistic to estimate mean of amp/del distributions"),
                                                    checkboxInput(inputId = ns("HMM_i3_use_KS"),
                                                                  label = "Scale Data",
                                                                  value = TRUE)
                                                )
                                            ),





                                            #------------------ Filtering low-conf HMM preds via BayesNet P(Normal) ---------------------------

                                            # BayesMaxPNormal  maximum P(Normal) allowed for a CNV prediction according to BayesNet. (default=0.5, note zero turns it off)
                                            numericInput(inputId = ns("BayesMaxPNormal"),
                                                         label = tags$strong("Bayes Max Probability of Normal State"),
                                                         value = 0.5),
                                            helpText("p-value for HMM i3 state overlap (default: 0.05)"),


                                            tags$hr(),

                                            # reassignCNVs (boolean) Given the CNV associated probability of belonging to each possible state, reassign the state assignments made by the HMM to the state that has the highest probability. (default: TRUE)
                                            h5("Reassign CNAs depending on their probabliltiy"),
                                            checkboxInput(inputId = ns("reassignCNVs"),
                                                          label = tags$strong("reassignCNVs"),
                                                          value = TRUE),
                                            helpText("Given the CNV associated probability of belonging to each possible state, reassign the state assignments
                                                                 made by the HMM to the state that has the highest probability. (default: TRUE)")

                                        )# wellpanel
                                    )# Conditonal Panel
                                ),# wellpanel
                         )
                ), # tabsetPanel( "OPTIONS"
                
                
                
                
                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # -------------------------------------- Advanced Option Tab ----------------------------------------------
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                tabPanel("Advanced Options",
                         column(width = 12,
                                
                                tags$br(),
                                
                                #--------------------- Smoothing params ---------------------
                                wellPanel(
                                    
                                    # shiny::HTML('<hr style="border-color: purple;">'),
                                    
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Smoothing Parmeters:")),
                                    
                                    
                                    # window_length
                                    numericInput(inputId = ns("window_length"),
                                                 label   = tags$strong("Window Length"),
                                                 value   = 101),
                                    helpText("Length of the window for the moving average. \n Should be an odd integer. (default: 101)"),
                                    
                                    tags$hr(style="border-color: grey;"),
                                    
                                    # smooth_method
                                    radioButtons(inputId  = ns("smooth_method"),
                                                 label    = tags$strong("Smoothing Method"),
                                                 choices  = list("Pyramidinal" = "pyramidinal",
                                                                "Runmeans" = "runmeans"),
                                                 selected = "pyramidinal")
                                ),
                                
                                # min_cells_per_gene
                                wellPanel(
                                    # min_cells_per_gene
                                    numericInput(inputId = ns("min_cells_per_gene"),
                                                 label   = tags$strong("Minimum Cells Per Gene"),
                                                 value   = 3),
                                    helpText("Minimum number of reference cells requiring expression measurements to include the corresponding gene. (default: 3)")
                                ),
                                
                                
                                #------------------------ Reference, Clustering, And Groupings Parameters ------------------------
                                wellPanel(
                                    
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Reference, Grouping, and clustering Parmeters:")),
                                    
                                    #--------------------------- Other ---------------------------
                                    
                                    
                                    # ref_subtract_use_mean_bounds
                                    checkboxInput(inputId = ns("ref_subtract_use_mean_bounds"),
                                                  label   = tags$strong("Reference Subtract Use Mean Bounds"),
                                                  value   = TRUE),
                                    helpText("Determine means separately for each ref group, then remove intensities within bounds of means (default: TRUE) \n
                                                    Otherwise, uses mean of the means across groups."),
                                    

                                    
                                    
                                    # cluster_references
                                    checkboxInput(inputId = ns("cluster_references"),
                                                  label   = tags$strong("Cluster References"),
                                                  value   = TRUE),
                                    helpText("Whether to cluster referenc,s within their annotations or not. (dendrogram not displayed) \n
                                                    (default: TRUE)"),
                                    
                                    
                                    
                                    # hclust_method
                                    selectInput( inputId   = ns('hclust_method'),
                                                 label     = tags$strong('Hierarchical Clustering Method'),
                                                 choices   = c("ward.D2", "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid"),
                                                 selectize = FALSE,
                                                 selected  = "ward.D2"),
                                    helpText("Method used for hierarchical clustering of cells"),
                                    
                                    
                                    # max_centered_threshold
                                    numericInput(inputId = ns("max_centered_threshold"),
                                                 label   = tags$strong("Max Centered Threshold"),
                                                 value   = 3),
                                    helpText("The maximum value that a value can have after centering. \n
                                                        Also sets a lower bound of -1 * this value. (default: 3),
                                                        can set to a numeric value or \"auto\" to bound by the mean bounds across cells.
                                                        Set to NA to turn off"),
                                    
                                    
                                    br(),
                                    
                                    #  scale_data
                                    h5("Perform Z-scaling of logtransformed data"),
                                    checkboxInput(inputId = ns("scale_data"),
                                                  label   = tags$strong("Scale Data"),
                                                  value   = FALSE),
                                    
                                    helpText("perform Z-scaling of logtransformed data \n
                                                    This may be turned on if you have very different kinds of data for your normal and tumor samples.
                                                    For example, you need to use GTEx representative normal expression profiles rather than being able
                                                    to leverage normal single cell data that goes with your experiment.")
                                ),
                                
                                
                                #--------------------------- Tumor subclustering ---------------------------
                                
                                wellPanel(
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Tumor subclustering:")),
                                    fluidRow(
                                        column(width = 2,
                                               # analysis_mode
                                               radioButtons(inputId = ns("analysis_mode"),
                                                            label   = tags$strong("Analysis Mode"),
                                                            choices = list("samples" = "samples",
                                                                           "subclusters" = "subclusters",
                                                                           "cells" = "cells"),
                                                            selected = "samples")
                                        ),
                                        column(width = 10,
                                               tags$br(),
                                               helpText("Grouping level for image filtering or HMM predictions. default: samples (fastest, but subclusters is ideal)")
                                        )
                                    ),
                                    tags$blockquote("The view below shows differences obtained when performing HMM predictions at the level of whole samples as compared to subclusters."),
                                    tags$div(
                                        shiny::HTML('<img src="images/denoised_vs_sample_vs_subcluster.png" width="750">')
                                    ),
                                    
                                    
                                    tags$hr(),
                                    
                                    
                                    
                                    # tumor_subcluster_partition_method
                                    
                                    radioButtons(inputId  = ns("tumor_subcluster_partition_method"),
                                                 label    = tags$strong("Tumor Subcluster Partition Method"),
                                                 choices  = list("Random Trees" = "random_trees",
                                                                "qnorm" = "qnorm"),
                                                 selected = "random_trees"),
                                    helpText("method for defining tumor subclusters. Options('random_trees', 'qnorm')
                                                    random_trees: (default) slow but best. Uses permutation statistics w/ tree construction.
                                                    qnorm: defines tree height based on the quantile defined by the tumor_subcluster_pval"),
                                    
                                    
                                    # tumor_subcluster_pval
                                    numericInput(inputId = ns("tumor_subcluster_pval"),
                                                 label   = tags$strong("Tumor Subcluster P-value"),
                                                 value   = 0.1),
                                    helpText("max p-value for defining a significant tumor subcluster (default: 0.1)")
                                    
                                ),
                                
                                #--------------------------- de-noising parameters ---------------------------
                                
                                wellPanel(
                                    
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("De-Noising Parameters:")),
                                    
                                    # denoise
                                    checkboxInput(inputId = ns("denoise"),
                                                  label   = tags$strong("Denoise"),
                                                  value   = TRUE),
                                    helpText("If selected, turns on denoising according to options below"),
                                    
                                    
                                    
                                    # noise_filter
                                    numericInput(inputId = ns("noise_filter"),
                                                 label   = tags$strong("Noise Filter"),
                                                 value   = NA),
                                    helpText("Values +- from the reference cell mean will be set to zero (whitening effect) default(NA), instead will use sd_amplifier below"),
                                    
                                    
                                    # sd_amplifier
                                    numericInput(inputId = ns("sd_amplifier"),
                                                 label   = tags$strong("Standard Deviation Amplifier"),
                                                 value   = 1.5),
                                    helpText("Noise is defined as mean(reference_cells) +- sdev(reference_cells). (default: 1.5)"),
                                    
                                    
                                    
                                    # noise_logistic
                                    checkboxInput(inputId = ns("noise_logistic"),
                                                  label   = tags$strong("Noise Logistic"),
                                                  value   = FALSE),
                                    helpText("Use the 'Noise Filter' or 'Standard Deviation Amplifier' based threshold (whichever is invoked) as the midpoint in a
                                                 logistic model for downscaling values close to the mean. (default: FALSE)")
                                ), 
                                
                                
                                
                                
                                #--------------------------- Outlier pruning ---------------------------
                                
                                wellPanel(
                                    
                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Outlier Pruning:")),
                                    
                                    #  outlier_method_bound
                                    radioButtons(inputId  = ns("outlier_method_bound"),
                                                 label    = tags$strong("outlier_method_bound"),
                                                 choices  = list("Average Bound" = "average_bound"),
                                                 selected = "average_bound"),
                                    helpText("Method to use for bounding outlier values. (default: 'Average Bound')
                                                 Will preferentially use outlier_lower_bounda and outlier_upper_bound if set."),
                                    
                                    
                                    fluidRow(
                                        # outlier_lower_bound
                                        column(width = 6,
                                               numericInput(inputId = ns("outlier_lower_bound"),
                                                            label   = tags$strong("Outlier Lower Bound"),
                                                            value   = NA),
                                               helpText("Outliers below this lower bound will be set to this value.")
                                               
                                        ),
                                        
                                        # outlier_upper_bound
                                        column(width = 6,
                                               numericInput(inputId = ns("outlier_upper_bound"),
                                                            label   = tags$strong("Outlier Upper Bound"),
                                                            value   = NA),
                                               helpText("Outliers above this upper bound will be set to this value.")
                                               
                                        )
                                    )
                                )
                         )
                ),

                
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # --------------------------------------------- Misc ------------------------------------------------------
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                tabPanel("Misc",
                         column(width = 12,
                                
                                tags$br(),



                                #-------------------------- Misc options ------------------------------------

                                wellPanel(

                                    tags$hr(style="border-color: black;"),
                                    tags$h3(tags$strong("Misc Options:")),

                                    # final_scale_limits The scale limits for the final heatmap output by the run() method. Default "auto". Alt, c(low,high)

                                    radioButtons(inputId = ns("final_scale_limits"),
                                                 label   = tags$strong("Final Scale Limits"),
                                                 choices = list("Auto" = "auto",
                                                                "low"  = "low",
                                                                "high" = "high"),
                                                 selected = "auto"),
                                    helpText("The scale limits for the final heatmap output. Default 'auto'"),

                                    tags$hr(),

                                    #final_center_val
                                    numericInput(inputId = ns("final_center_val"),
                                                 label   = tags$strong("Final Center Value"),
                                                 value   = NULL),
                                    helpText("Center value for final heatmap output."),

                                    tags$hr(),

                                    # debug
                                    checkboxInput(inputId = ns("debug"),
                                                  label   = tags$strong("Debuging"),
                                                  value   = FALSE),
                                    helpText("Output debug level logging."),

                                    tags$hr(),

                                    # num_threads
                                    numericInput(inputId = ns("num_threads"),
                                                 label   = tags$strong("Number of Threads"),
                                                 value   = 4),
                                    helpText("Number of threads for parallel steps (default: 4)."),

                                    tags$hr(),


                                    # plot_steps
                                    checkboxInput(inputId = ns("plot_steps"),
                                                  label   = tags$strong("Plot Steps"),
                                                  value   = FALSE),
                                    helpText("Saves infercnv objects and plots data at the intermediate steps."),

                                    tags$hr(),

                                    # resume_mode
                                    checkboxInput(inputId = ns("resume_mode"),
                                                  label   = tags$strong("Resume Mode"),
                                                  value   = TRUE),
                                    helpText("leverage pre-computed and stored infercnv objects where possible."),

                                    tags$hr(),

                                    # png_res
                                    numericInput(inputId = ns("png_res"),
                                                 label   = tags$strong("Resolution for png output"),
                                                 value   = 300),
                                    tags$hr(),


                                    # no_plot
                                    checkboxInput(inputId = ns("no_plot"),
                                                  label   = tags$strong("No Plot"),
                                                  value   = FALSE),
                                    helpText("Don't make any of the images. Instead, generate all non-image outputs as part of the run. (default: FALSE)"),
                                    tags$hr(),

                                    #no_prelim_plot
                                    checkboxInput(inputId = ns("no_prelim_plot"),
                                                  label   = tags$strong("No Preliminary Plot"),
                                                  value   = FALSE),
                                    helpText("Don't make the preliminary infercnv image (default: FALSE)"),
                                    tags$hr(),


                                    # output_format
                                    selectInput(inputId  =  ns('output_format'),
                                                label    = tags$strong('Plot Output Format'),
                                                choices  = c("png","pdf",NA),
                                                selected = "png"),
                                    helpText("Output format for the figure. NA means to only write the text outputs without generating the figure itself."),
                                    tags$hr(),


                                    # useRaster
                                    checkboxInput(inputId = ns("useRaster"),
                                                  label   = tags$strong("Use Rasterization"),
                                                  value   = TRUE),
                                    helpText("Whether to use rasterization for drawing heatmap. Only disable if it produces an error as it is much faster than not using it. (default: TRUE)"),
                                    tags$hr(),


                                    # plot_probabilities
                                    checkboxInput(inputId = ns("plot_probabilities"),
                                                  label   = tags$strong("Plot Probabilities"),
                                                  value   = TRUE),
                                    helpText("Option to plot posterior probabilities (default: TRUE)"),
                                    tags$hr(),

                                    fluidRow(



                                            # diagnostics
                                            checkboxInput(inputId = ns("diagnostics"),
                                                          label   = tags$strong("Produce Diagnostics"),
                                                          value   = FALSE),
                                            helpText("Option to create diagnostic plots after running the Bayesian model (default: FALSE)",
                                                     shiny::HTML('<a href="https://github.com/broadinstitute/inferCNV/wiki/Diagnostic-Plots">More Information here</a>'))

                                    ),
                                    tags$hr(),



                                    #--------------------------------- Experimental options ---------------------------------

                                    # remove_genes_at_chr_ends
                                    checkboxInput(inputId = ns("remove_genes_at_chr_ends"),
                                                  label   = tags$strong("Remove Genes At Chromosome Ends"),
                                                  value   = FALSE),
                                    helpText("Experimental option: If true, removes the window_length/2 genes at both ends of the chromosome."),

                                    tags$hr(),

                                    # prune_outliers
                                    checkboxInput(inputId = ns("prune_outliers"),
                                                  label   = tags$strong("Prune Outliers"),
                                                  value   = FALSE),
                                    helpText("Define outliers loosely as those that exceed the mean boundaries among all cells.
                                      These are set to the bounds."),

                                    tags$hr(),

                                    #------------ experimental opts involving DE analysis ----------------------------------------------------

                                    # mask_nonDE_genes
                                    checkboxInput(inputId = ns("mask_nonDE_genes"),
                                                  label   = tags$strong("Mask non-DE Genes"),
                                                  value   = FALSE),
                                    helpText("If true, sets genes not significantly differentially expressed between tumor/normal to
                                      the mean value for the complete data set (default: 0.05)"),

                                    tags$hr(),

                                    # mask_nonDE_pval
                                    #       p-value threshold for defining statistically significant DE genes between tumor/normal
                                    numericInput(inputId = ns("mask_nonDE_pval"),
                                                 label   = tags$strong("Mask non-DE P-value"),
                                                 value   = 0.05),
                                    helpText("p-value threshold for defining statistically significant DE genes between tumor/normal"),

                                    tags$hr(),

                                    # test.use
                                    radioButtons(inputId = ns("test.use"),
                                                 label   = tags$strong("Stistical Test To Use"),
                                                 choices = list("Wilcoxon" = "wilcoxon",
                                                                "Perm" = "perm",
                                                                "t-test" = "t"),
                                                 selected = "wilcoxon"),

                                    tags$hr(),

                                    # require_DE_all_normals
                                    radioButtons(inputId = ns("require_DE_all_normals"),
                                                 label   = tags$strong("Require DE All Normals "),
                                                 choices = list("any" = "any",
                                                                "most" = "most",
                                                                "all"  = "all"),
                                                 selected = "any"),
                                    helpText("If mask_nonDE_genes is set, those genes will be masked only if they are are found as DE according to
                                      test.use and mask_nonDE_pval in each of the comparisons to normal cells."),
                                    tags$hr(),

                                    #----- other experimental opts ----------------------------------------------------

                                    # sim_method
                                    selectInput(inputId  =  ns('sim_method'),
                                                label    = tags$strong('Simulation Method'),
                                                choices  = c("meanvar"),
                                                selected = "meanvar"),
                                    helpText("method for calibrating CNV levels in the i6 HMM (default: 'meanvar')"),

                                    tags$hr(),

                                    # hspike_aggregate_normals
                                    checkboxInput(inputId = ns("hspike_aggregate_normals"),
                                                  label   = tags$strong("hspike Aggregate Normals"),
                                                  value   = FALSE),
                                    helpText("Instead of trying to model the different normal groupings individually, just merge them in the hspike.")

                                )

                         )#  column
                ) # tabPanel( "MISC"
            ),# tabsetPanel
        ), # fluid row
        fluidRow(
            alertMessage <-"",
            tags$div(id = "runAlertMessage", alertMessage))
    )

}











#' Collects user inputs from runInfercnvUI() and returns as a list.
#'
#' @title runInfercnvInputs()
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @return a list containing the UI elements for running the infercnv analysis
#'


runInfercnvInputs <- function(input, output, session){

    #-------------------------------- Output Directory selection --------------------------------
    volumes <- shinyFiles::getVolumes()
    shinyFiles::shinyDirChoose( input = input,
                                session = session,
                                id = 'dir',
                                roots = c(root="~") #volumes #c(root='.')
                                )

    # Display the output directory
    global <- reactiveValues(datapath = "~")#getwd())
    output$show_dir <- renderText({  global$datapath    })

    # what is returned by shinyFiles
    dir <- reactive(input$dir)

    # What to display when the chosen deirectory is NULL
    observeEvent(ignoreNULL = TRUE,
                 eventExpr  = {
                     input$dir
                 },
                 handlerExpr = {
                     if (!"path" %in% names(dir())) return()
                     root = "~"
                     global$datapath <- file.path(root, paste(unlist(dir()$path), collapse = .Platform$file.sep))
                 }
    )

    # path to the output directory
    path <- reactive({
        # check if the path was chosen
        if ( unlist(dir())[1] != 0 ){

            root = "~"
            pathway <- file.path(root, paste(unlist(dir()$path), collapse = .Platform$file.sep))

        } else {
            stop("Please select an output directory.")
        }
        print(paste("pathway:", pathway))
        # Check to make sure pathway exists or was created
        if (!file.exists(pathway)){
            err_message <- paste("Error in selecting the following pathway: ", pathway)
            stop(err_message)
        }
        return(pathway)
    })




    #----------------- Process the input data --------------------------------
    return(
        list(
            #------------ Needed Inputs ---------------
            dir   = path,
            cutoff   = reactive({  input$cutoff  }),
            min_cells_per_gene = reactive({  input$min_cells_per_gene   }),
            #--------------------- Smoothing params ---------------------
            window_length = reactive({  input$window_length  }),
            smooth_method = reactive({  input$smooth_method  }),
            #------------------------ Reference, Clustering, And Groupings Parameters ------------------------
            num_ref_groups = reactive({  input$num_ref_groups  }),
            ref_subtract_use_mean_bounds = reactive({ input$ref_subtract_use_mean_bounds   }),
            cluster_by_groups = reactive({ input$cluster_by_groups   }),
            cluster_references = reactive({  input$cluster_references  }),
            k_obs_groups = reactive({ input$k_obs_groups   }),
            hclust_method = reactive({  input$hclust_method  }),
            max_centered_threshold= reactive({  input$max_centered_threshold  }),
            scale_data = reactive({  input$scale_data  }),
            #--------------------------- Downstream Analyses ---------------------------
            HMM = reactive({  input$HMM  }),
            HMM_transition_prob = reactive({  input$HMM_transition_prob  }),
            HMM_report_by = reactive({  input$HMM_report_by  }),
            HMM_type = reactive({  input$HMM_type  }),
            HMM_i3_pval = reactive({  input$HMM_i3_pval  }),
            # KS = reactive({  input$KS  }),
            HMM_i3_use_KS = reactive({  input$HMM_i3_use_KS  }),
            #------------------ Filtering low-conf HMM preds via BayesNet P(Normal) ---------------------------
            BayesMaxPNormal = reactive({  input$BayesMaxPNormal  }),
            reassignCNVs = reactive({  input$reassignCNVs  }),
            #--------------------------- Tumor subclustering ---------------------------
            analysis_mode = reactive({  input$analysis_mode  }),
            tumor_subcluster_partition_method = reactive({  input$tumor_subcluster_partition_method  }),
            tumor_subcluster_pval = reactive({  input$tumor_subcluster_pval  }),
            #--------------------------- de-noising parameters ---------------------------
            denoise = reactive({  input$denoise  }),
            noise_filter = reactive({  input$noise_filter  }),
            sd_amplifier = reactive({  input$sd_amplifier  }),
            noise_logistic = reactive({  input$noise_logistic  }),
            #--------------------------- Outlier pruning ---------------------------
            outlier_method_bound = reactive({  input$outlier_method_bound  }),
            outlier_lower_bound = reactive({  input$outlier_lower_bound  }),
            outlier_upper_bound = reactive({  input$outlier_upper_bound  }),
            #-------------------------- Misc options ------------------------------------
            final_scale_limits = reactive({  input$final_scale_limits  }),
            final_center_val = reactive({  input$final_center_val  }),
            debug = reactive({  input$debug  }),
            num_threads = reactive({  input$num_threads  }),
            plot_steps = reactive({  input$plot_steps  }),
            resume_mode = reactive({  input$resume_mode  }),
            png_res = reactive({  input$png_res  }),
            no_plot = reactive({  input$no_plot  }),
            no_prelim_plot = reactive({  input$no_prelim_plot  }),
            output_format = reactive({  input$output_format  }),
            useRaster = reactive({  input$useRaster  }),
            plot_probabilities = reactive({  input$plot_probabilities  }),
            diagnostics = reactive({  input$diagnostics  }),
            #--------------------------------- Experimental options ---------------------------------
            remove_genes_at_chr_ends = reactive({  input$remove_genes_at_chr_ends  }),
            prune_outliers = reactive({  input$prune_outliers  }),
            #------------ experimental opts involving DE analysis ----------------------------------------------------
            mask_nonDE_genes = reactive({  input$mask_nonDE_genes  }),
            mask_nonDE_pval = reactive({  input$mask_nonDE_pval  }),
            test.use = reactive({  input$test.use  }),
            require_DE_all_normals = reactive({  input$require_DE_all_normals  }),
            #----- other experimental opts ----------------------------------------------------
            sim_method = reactive({  input$sim_method  }),
            hspike_aggregate_normals = reactive({  input$hspike_aggregate_normals  })

        )
    )

}



# Set the logging appender to write to a log file
message_appender <- function(file) {
    function(line) {
        cat(line)
        cat(line, file=file, append=TRUE, sep='')
    }
}





#' Run the infercnv analysis on the given data.
#'
#' @title runInfercnv()
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param  infercnv_obj, an infercnv object created by createObject().
#' @param infercnv_inputs, the list of UI inputs given by the function runInfercnvInputs().
#'
#' @return an infercnv S4 object \code{infercnv::\link[infercnv]{infercnv}}
#'


#-------------------------------- Run InferCNV Function--------------------------------

runInfercnv <- function(input,
                        output,
                        session,
                        infercnv_obj,
                        infercnv_inputs){

    options(error = function() traceback(2))

    # runInfercnv:
    #   Takes in the created infercnv object and runs the main infercnv analysis
    #   outputs the results to the given output directory

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Log file creation and set up
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if the log file already exists from previouos runs, remove it and create a new one 
    output_log_path <- file.path(infercnv_inputs$dir(),"output.log")
    if (file.exists(output_log_path)){
        unlink(output_log_path)
        file.create(output_log_path)
    }
    futile.logger::flog.appender(message_appender(output_log_path))
    
    
    #-------------------------------- Run Infercnv --------------------------------

    if (infercnv_inputs$num_ref_groups() == ""){
        num_ref_groups = NULL
    } else {
            num_ref_groups = infercnv_inputs$num_ref_groups()}

    if (is.na( infercnv_inputs$final_scale_limits() )){
        final_scale_limits = NULL }

    if (is.na( infercnv_inputs$final_center_val() )){
        final_center_val = NULL
    } else {
        final_center_val = infercnv_inputs$final_center_val()
    }


    print(infercnv_inputs$dir())

    # Function for loading message
    shiny_busy <- function() {
        # use &nbsp; for some alignment, if needed
        shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
            '<p> Running Analysis ...',
            '<span data-display-if="$(&#39;shiny::HTML&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;">',
            '<i class="fa fa-spinner fa-pulse fa-2x fa-fw" style="color:orange"></i>',
            '</span>',
            '</p>'
        ))
    }
    # ************************************ Start Runnning The Analysis ************************************
    showNotification(id = "loadingMessage",
                     ui = shiny_busy(),
                     duration = NULL)

    infercnv_object <-
        infercnv::run(
                  # infercnv object previously created
                  infercnv_obj = infercnv_obj,

                  # gene filtering settings
                  cutoff = as.integer(infercnv_inputs$cutoff()),#1,
                  min_cells_per_gene = as.integer(infercnv_inputs$min_cells_per_gene()), #3,

                  out_dir = infercnv_inputs$dir(),  #NULL,

                  ## smoothing params
                  window_length = as.integer(infercnv_inputs$window_length()), #101,
                  smooth_method = infercnv_inputs$smooth_method(), # c('pyramidinal', 'runmeans'),

                  num_ref_groups = num_ref_groups, #NULL,
                  ref_subtract_use_mean_bounds = infercnv_inputs$ref_subtract_use_mean_bounds(), #TRUE,

                  ## observation cell clustering settings
                  cluster_by_groups = infercnv_inputs$cluster_by_groups(), #FALSE,
                  cluster_references = infercnv_inputs$cluster_references(), #TRUE,
                  k_obs_groups = as.integer(infercnv_inputs$k_obs_groups()), #1,
                  hclust_method = infercnv_inputs$hclust_method(), #'ward.D2',
                  max_centered_threshold = infercnv_inputs$max_centered_threshold(), #3, # or set to a specific value or "auto", or NA to turn off
                  scale_data = infercnv_inputs$scale_data(), #FALSE,

                  ## HMM opts
                  HMM = infercnv_inputs$HMM(), #FALSE, # turn on to auto-run the HMM prediction of CNV levels

                  ## tumor subclustering opts
                  HMM_transition_prob = infercnv_inputs$HMM_transition_prob(),#1e-6,
                  HMM_report_by = infercnv_inputs$HMM_report_by(), #c("subcluster","consensus","cell"),
                  HMM_type = infercnv_inputs$HMM_type(), #c('i6', 'i3'),
                  HMM_i3_pval = infercnv_inputs$HMM_i3_pval(), #0.05,
                  HMM_i3_use_KS = infercnv_inputs$HMM_i3_use_KS(), #TRUE,
                  BayesMaxPNormal = infercnv_inputs$BayesMaxPNormal(),  #0.5,

                  ## some experimental params
                  sim_method = infercnv_inputs$sim_method(), #'meanvar', c('meanvar', 'simple', 'splatter'), only meanvar supported, others experimental
                  reassignCNVs  = infercnv_inputs$reassignCNVs(), #TRUE,


                  ## tumor subclustering options
                  analysis_mode = infercnv_inputs$analysis_mode(), #c('samples', 'subclusters', 'cells'), # for filtering and HMM
                  tumor_subcluster_partition_method = infercnv_inputs$tumor_subcluster_partition_method(), #c('random_trees', 'qnorm', 'pheight', 'qgamma', 'shc'),
                  tumor_subcluster_pval = infercnv_inputs$tumor_subcluster_pval(), #0.1,


                  ## noise settings
                  denoise = infercnv_inputs$denoise(), #FALSE,
                  # noise_filter = infercnv_inputs$noise_filter(), #NA,
                  sd_amplifier = infercnv_inputs$sd_amplifier(), #1.5,
                  noise_logistic = infercnv_inputs$noise_logistic(),  #FALSE, # if false, does complete 'noise' elimination.

                  # outlier adjustment settings
                  outlier_method_bound  = infercnv_inputs$outlier_method_bound(), #"average_bound",
                  outlier_lower_bound = infercnv_inputs$outlier_lower_bound(),  #NA,
                  outlier_upper_bound = infercnv_inputs$outlier_upper_bound(), #NA,

                  ## misc options
                  final_scale_limits = infercnv_inputs$final_scale_limits(), #NULL,
                  final_center_val = final_center_val, #NULL,
                  debug = infercnv_inputs$debug(), #FALSE, #for debug level logging
                  num_threads = infercnv_inputs$num_threads(), #4,
                  plot_steps = infercnv_inputs$plot_steps(), #FALSE,
                  resume_mode = infercnv_inputs$resume_mode(), #TRUE,
                  png_res = infercnv_inputs$png_res(), #300,
                  plot_probabilities = infercnv_inputs$plot_probabilities(), #TRUE,
                  diagnostics = infercnv_inputs$diagnostics(), #FALSE,

                  ## experimental options
                  remove_genes_at_chr_ends = infercnv_inputs$remove_genes_at_chr_ends(), #FALSE,
                  prune_outliers = infercnv_inputs$prune_outliers(), #FALSE,

                  mask_nonDE_genes = infercnv_inputs$mask_nonDE_genes(), #FALSE,
                  mask_nonDE_pval = infercnv_inputs$mask_nonDE_pval(), #0.05, # use permissive threshold
                  test.use = infercnv_inputs$test.use(), #'wilcoxon',
                  require_DE_all_normals = infercnv_inputs$require_DE_all_normals(), #"any",


                  hspike_aggregate_normals = infercnv_inputs$hspike_aggregate_normals(), #FALSE,

                  no_plot = infercnv_inputs$no_plot(), #FALSE,
                  no_prelim_plot = infercnv_inputs$no_prelim_plot(), #FALSE,
                  output_format = infercnv_inputs$output_format(), #"png",
                  useRaster = infercnv_inputs$useRaster() #TRUE
    )  #infercnv
    removeNotification(id="loadingMessage")

    return(infercnv_object)
}

