#!/usr/bin/env Rscript

################### Load the libraries and source modules ##########################

# Load libraries
library(shiny)
# source needed modules
source("createObject.R")
source("homePage.R")
source("runInfercnv.R")
source("mainAnalysisOutputTab.R")
source("bayesianAnalysisTab.R")
source("hmmAnalysisOutputTab.R")
source("adjustByProbThreshold.R")
source("diagnosticPlotTab.R")
source("medianFilterTab.R")
source("cnvCellProbsTab.R")

options(error = function() traceback(2))
options(shiny.reactlog=TRUE)

futile.logger::flog.appender(futile.logger::appender.console())




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preform when the app is closed
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove the PDF files from the www folder when the app is closed
onStop(function(){
    pdf_files <- list.files(path       = "www",
                            pattern    = "*.pdf",
                            all.files  = FALSE,
                            full.names = TRUE)
    unlink(pdf_files)
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################## Set up the User Interface (UI) #########################
#' This is the user-interface definition of a Shiny web application
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- tags$div(
    class = "navbarMain",
    shinyalert::useShinyalert(),
    navbarPage(title = paste0("Infercnv-",packageVersion("infercnv")),

               #----------- Set up the header and themes --------------
               collapsible = FALSE,
               inverse = TRUE,
               windowTitle = "Infercnv",
               position = "fixed-top",





               #############################     Home Tab     #############################
               tabPanel(title = shiny::HTML('<i class="fa fa-home fa-fw" aria-hidden="true"></i>&nbsp; Home'),
                        homePageUI("HomePage"),
               ),

               #############################     Upload and settings TAB     ########################################
               tabPanel(title = shiny::HTML('<i class="fa fa-file-text-o" aria-hidden="true"></i> &nbsp; Upload Files And Settings'),


                        #-------------------------- Create the infercnv Object --------------------------
                        createObjectInputsUI("create_object"),

                        # Add the action button
                        actionButton(inputId = "create_obj",
                                     label = "Upload Data Files",
                                     icon = icon(name = "cloud-upload-alt", lib = "font-awesome"),
                                     width = '100%',
                                     style="color: #fff; background-color: #68D77B"),



                        #-------------------------- Run Analysis --------------------------
                        tags$br(),

                        tags$hr(style="border-color: black;"),

                        tags$br(),

                        titlePanel(
                            tags$h1("Step 2: Settings And Run Analysis",
                                    align = "center",
                                    style = "background-color: #afc9eb;
                                      color: black;
                                      margin: -20px;
                                      padding: 50px")
                        ),
                        
                        tags$br(),tags$br(),
                    

                        # Run the module for infercnv analysis
                        runInfercnvUI("running_infercnv"),
                        
                        # Create the button for running the infercnv analysis
                        actionButton(inputId = "run_infercnv",
                                     label = " Run Infercnv",
                                     icon = icon("refresh", lib = "font-awesome"),
                                     width = '100%',
                                     style="color: #fff; background-color: #68D77B"),
                        
                        tags$br(),tags$br(),tags$br(),
                        
                        # Button to clear the output folder 
                        actionButton(inputId = "clear_output",
                                     label = "Clear Output",
                                     icon = icon("refresh", lib = "font-awesome"),
                                     width = '100%',
                                     style="color: #fff; background-color: red"),
                        tags$br(),tags$br(),tags$br(),

               ),





               #############################     Analysis Output tab     #############################
               tabPanel(title = shiny::HTML('<i class="fa fa-bar-chart" aria-hidden="true"></i> &nbsp; Analysis Output'),

                        tags$head(includeCSS("www/infercnv.style.css")),

                        tags$div(class = "navbarPageHome",
                                 navbarPage(
                                     title = "",
                                     #-------------------------- Main Analysis Tab --------------------------
                                     navbarMenu(
                                         title =  "Main Analysis Output",

                                         tabPanel(title =  "Main Analysis Output",
                                                  mainAnalysisTabUI("output_main_analysis")
                                         ),

                                         tabPanel(title =  "Median Filter Output",
                                                  medianFilterUI("median_filter_output")
                                         )
                                     ),
                                     #-------------------------- HMM Analysis Tab --------------------------
                                     tabPanel(title =  "HMM Analysis Output",
                                              hmmAnalysisUI("output_hmm_analysis")
                                     ),
                                     #-------------------------- Bayesian Analysis Tab --------------------------
                                     navbarMenu(
                                         title = "Bayesian Analysis Output",

                                         tabPanel(title =  "Bayesian Analysis Plots",
                                                  bayesianAnalysisUI("output_bayesian_analysis")
                                         ),

                                         tabPanel(title =  "Probability Plots",
                                                  cnvCellProbsUI("cnv_cell_probs")
                                         )
                                     ),
                                     #-------------------------- Dytnamic Analysis Tab --------------------------
                                     tabPanel(title =  "Dynamic Plots",
                                              adjustByProbThresholdUI("output_dynamic_analysis")
                                     ),
                                     
                                     #-------------------------- Supplemental Tab --------------------------
                                     navbarMenu(
                                         title = "Supplemental",
                                         tabPanel(title = "Diagnostic Plots",
                                                  diagnosticPlotsUI("diagploting")
                                         )
                                     )
                                 )
                        )


               ),
               #Add a icon to the right of the navbar
               tags$script(shiny::HTML("var header = $('.navbarMain > .navbar > .container-fluid');
                 header.append('<div style=\"float:right\"><i class=\"fa fa-dna fa-fw\" aria-hidden=\"true\" style=\"float:right; width:33px;height:41px;padding-top:20px;color:white\"></i> `</div>');
    console.log(header)")
               )


    )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################## Set up the Server #########################
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input,
                   output,session){

    # set the max upload size 1GB
    options(shiny.maxRequestSize=10000*1024^2)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ################################         Home Page         ################################
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    callModule(module = homePage,
               id     = "HomePage")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ################################ create the infercnv object ################################
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #~~~~~~~~~~~ Gather the User Inputs ~~~~~~~~~~~~
    ## adding inputs (will be list like object)
    infercnv_object_inputs <- callModule(module = createObjectInputs,
                                         id     = "create_object")

    #~~~~~~~~~~~ run create the infercnv object ~~~~~~~~~~~
    observeEvent(
        # adding inputs (will be list like object)
        input$create_obj,
        {
            tryCatch({
                infercnv_obj_1 <<- callModule(module            = createObject,
                                              id                = "create_object",
                                              createObjectVars  = infercnv_object_inputs)
            },
            error = function(e){
                shinyalert::shinyalert(title = "Error!",
                                       text  = e$message,
                                       type  = "error")
            })
        }
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ################################      Run Main Analysis      ################################
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #~~~~~~~~~~~ Gather the User Inputs ~~~~~~~~~~~~
    ## adding inputs (will be list like object)
    infercnv_analysis_inputs <<- callModule(module      = runInfercnvInputs,
                                            id          = "running_infercnv")

    #~~~~~~~~~~~ Run Analysis ~~~~~~~~~~~~
    observeEvent(
        # run infercnv button
        input$run_infercnv,
        {
            # run the infercnv analysis
            if(!exists("infercnv_obj_1")){
                print("no infercnv object")

                insertUI(
                    # ID
                    selector = "#runAlertMessage",
                    # Make a class so can remove the message
                    ui = tags$div(
                        class = "alert alert-danger alert-dismissible",
                        shiny::HTML("<span class='fa fa-times fa-fw' aria-hidden='true'> </span> \
                 Upload Data Before Running Analysis \
                 <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
                )

            }else{
                # catch any errors that are output by the following modules
                tryCatch({

                    # Run the analysis
                    infercnv_obj_2 <<- callModule(module            = runInfercnv,
                                                  id                = "running_infercnv",
                                                  infercnv_obj      = infercnv_obj_1,
                                                  infercnv_inputs   = infercnv_analysis_inputs)

                    # Module to run the analysis
                    callModule(module           = mainAnalysis,
                               id               = "output_main_analysis",
                               infercnv_inputs  = infercnv_analysis_inputs,
                               infercnv_obj     = infercnv_obj_2)

                    # Module to display the HMM analysis
                    callModule(module           = hmmAnalysis,
                               id               = "output_hmm_analysis",
                               infercnv_inputs  = infercnv_analysis_inputs)

                    # Module to display the bayesian analysis
                    callModule(module           = bayesianAnalysis,
                               id               = "output_bayesian_analysis",
                               infercnv_inputs  = infercnv_analysis_inputs)

                    # Moduel to create the diagnostic plots
                    callModule(module           = diagnosticPlots,
                               id               = "diagploting",
                               infercnv_inputs  = infercnv_analysis_inputs)

                    # Median Filter post processing option
                    callModule(module           = medianFilter,
                               id               = "median_filter_output",
                               infercnv_inputs  = infercnv_analysis_inputs,
                               infercnv_obj     = infercnv_obj_2)

                    # Dynamic Bayesian and HMM plotting
                    callModule(module           = adjustByProbThreshold,
                               id               = "output_dynamic_analysis",
                               infercnv_inputs  = infercnv_analysis_inputs)

                    # CNV and Cell probabilities
                    callModule(module           = cnvCellProbs,
                               id               = "cnv_cell_probs",
                               infercnv_inputs  = infercnv_analysis_inputs)

                    # Message to user if succesful
                    insertUI(
                        # ID
                        selector = "#runAlertMessage",
                        # Make a class so can remove the message
                        ui = tags$div(
                            class = "alert alert-success alert-dismissible",
                            shiny::HTML("<span class='fa fa-check fa-fw' aria-hidden='true'> \
                 </span> Ran Analysis! <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
                    )
                },
                error = function(e){
                    # print
                    shinyalert::shinyalert(title = "Error!",
                                           text = e$message,
                                           type = "error",
                                           imageUrl = "https://cdn.pixabay.com/photo/2017/02/12/21/29/false-2061131_960_720.png")
                })
            }
        }
    )
    
    # Option to Clear the output folder
    observeEvent(
        # run infercnv button
        input$clear_output,
        {
            # check to see if infercnv was ran 
            if(!exists("infercnv_obj_2")){
                insertUI(
                    # ID
                    selector = "#runAlertMessage",
                    # Make a class so can remove the message
                    ui = tags$div(
                        class = "alert alert-danger alert-dismissible",
                        shiny::HTML("<span class='fa fa-times fa-fw' aria-hidden='true'> </span> \
                 No Analysis was ran \
                 <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
                )
                
            }else{
                output_path <- infercnv_analysis_inputs$dir()
                print(output_path)
                print(file.exists(output_path))
                unlink(x = output_path, recursive = TRUE)
                futile.logger::flog.appender(futile.logger::appender.console())
                
                insertUI(
                    # ID
                    selector = "#runAlertMessage",
                    # Make a class so can remove the message
                    ui = tags$div(
                        class = "alert alert-success alert-dismissible",
                        shiny::HTML("<span class='fa fa-check fa-fw' aria-hidden='true'> \
                 </span> Cleared Output Folder <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
                )
                
            }
                
        }
    )
}

######################## Create App #########################
shinyApp(ui=ui, server = server)

