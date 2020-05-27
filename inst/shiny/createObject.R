#!/usr/bin/env Rscript

#' Gathers user inputs along with desplaying the upladed data.
#'
#' @title createObjectInputsUI()
#'
#' @details Shiny Module for the creation of infercnv onjects.
#' function: createObjectInputsUI, used to create the Create Infercnv Object unserinterface portion of the application
#' Gathers user inputs along with desplaying the upladed data.
#'
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing the UI elements for creating an infercnv object
#'

createObjectInputsUI <- function(id) {
    ns <- NS(id)

    # create a tag list
    tagList(

        # Title for uploading the data
        tags$style(shiny::HTML("
                          #first {
                              border: 4px solid black;
                          }")),
        h1("Step 1: Upload Files",
           align = "center",
           style = "background-color: #afc9eb;
                                      color: black;
                                      margin: -20px;
                                      padding: 50px;"),

        tags$br(), tags$br(),

        fluidRow(
            column(
                width = 5,
                #------ mandatory options -----------
                tags$br(),

                tags$h3("Input Files To Initiate InferCNV",
                        align = "center"
                        # style = "background-color: #ffc0cb;
                        # padding: 10px"
                ),

                wellPanel(
                    tabsetPanel(
                        tabPanel(title = "Input Data",

                                 br(),
                                 #----  File inputs  --------
                                 fileInput(inputId = ns("raw_counts_matrix"),
                                           label = "Raw Counts Matrix",
                                           multiple = FALSE,
                                           accept = ("")),

                                 fileInput(inputId = ns("annotations_file"),
                                           label = "Annotations File",
                                           multiple = FALSE,
                                           accept = ("")),

                                 fileInput(inputId = ns("gene_order_file"),
                                           label = "Gene Order File",
                                           multiple = FALSE,
                                           accept = ("text/comma-separated-values,text/plain")),
                                 selectInput(inputId = ns( 'delim'),
                                             label = 'File Delimination',
                                             choices = c("TSV","CSV"),
                                             selected = "TSV"),
                        ),

                        # Example data option
                        tabPanel(title = "Example Data",

                                 tags$br(),

                                 # Check box for the option to upload the example data
                                 ## set the id and class so that you only customize the one option and not everything
                                 tags$div(id='my_div_example_data',
                                          class='my_class_example_data',
                                          checkboxInput(inputId = ns("upload_example_data"),
                                                        label = "Upload Example Data",
                                                        value = FALSE),
                                          ## Customaize the check box
                                          tags$style("
                                        #my_div_example_data
                                             label {font-size: 22px; }
                                              .checkbox { /* checkbox is a div class*/
                                                line-height: 15px;
                                                margin-bottom: 15px; /*set the margin, so boxes don't overlap*/
                                              }
                                              input[type='checkbox']{ /* style for checkboxes */
                                                width: 15px; /*Desired width*/
                                                height: 15px; /*Desired height*/
                                                line-height: 15px;
                                              }
                                              span {
                                                  margin-left: 15px;  /*set the margin, so boxes don't overlap labels*/
                                                  line-height: 15px;
                                              }"
                                          )
                                 ),
                                 helpText("Option to use the Example data provided to run the infercnv analysis.")
                        )
                    )
                ),

                wellPanel(
                    textInput(inputId = ns("ref_group_names"),
                              label = "Reference Group Names",
                              placeholder = "Cell1, Cell2...",
                              value = ''),


                    #---------- optional inputs ----------------
                    tags$hr(),
                    tags$hr(style="border-color: grey;"),
                    tags$h3("Optional inputs"),

                    numericInput(inputId = ns("max_cells_per_group"),
                                 label = "Max cells per group",
                                 value = NA),

                    column(width = 6,
                           numericInput(inputId = ns("MIN_counts_per_cell"),
                                        label = "Min max counts per cell",
                                        value = NULL)
                    ),
                    column(width = 6,
                           numericInput(inputId = ns("MAX_counts_per_cell"),
                                        label = "Min max counts per cell",
                                        value = NULL)
                    ),
                    textInput(inputId = ns("chr_exclude"),
                              label = "Reference Group Names",
                              placeholder = "chrX, chrY, chrM, ...",
                              value = 'chrX, chrY, chrM')
                )

            ),
            #--------------- Output -----------------------
            column(
                width = 7,
                navbarPage(title = "Input Data",


                           tabPanel(title = "Count Data",
                                    DT::dataTableOutput(outputId = ns("count.data"))
                           ),

                           tabPanel(title = "Annotation Data",
                                    DT::dataTableOutput(outputId = ns("annotation_data"))
                           ),

                           tabPanel(title = "Gene Order",
                                    DT::dataTableOutput(outputId = ns("gene_order"))
                           )
                )
            )
        ),

        # Add message when the files are uploaded
        alertMessage <-"",
        tags$div(id = "uploadAlertMessage", alertMessage),
    )
}



#' Collects user inputs and returns as a list.
#'
#' @title createObjectInputs()
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return a list containing the UI elements for creatung an infercnv object
#'
createObjectInputs <- function(input, output, session){


    #----------------- Process the input data --------------------------------
    return(
        list(
            # Example data option
            upload_example_data = reactive({ input$upload_example_data }),

            # files
            raw_counts_matrix   = reactive({ input$raw_counts_matrix}),
            annotations_file    = reactive({ input$annotations_file }),
            gene_order_file     = reactive({ input$gene_order_file }),

            # inputs
            ref_group_names     = reactive({ input$ref_group_names }),
            delim               = reactive({ input$delim }),
            max_cells_per_group = reactive({ input$max_cells_per_group }),
            MIN_counts_per_cell = reactive({ input$MIN_counts_per_cell }),
            MAX_counts_per_cell = reactive({ input$MAX_counts_per_cell }),
            chr_exclude         = reactive({ input$chr_exclude }),
            create_obj          = reactive({ input$create_obj })
        )
    )

}






#'
#'
#' @title createObject(): Create the infercnv object from the given UI inputs.
#'
#' @param input, output, session standard \code{shiny} boilerplate
#'
#' @param createObjectVars, the list of UI inputs given by the function createObjectInputs().
#'
#' @return an infercnv S4 object \code{infercnv::\link[infercnv]{infercnv}}
#'

createObject <- function(input, output, session, createObjectVars){

    #----------------- create the infercnv object --------------------------------

    #------------------- Adjust the inputs -------------------
    if(is.na(createObjectVars$max_cells_per_group())){
        max_cells_per_group <- NULL
    } else {
        max_cells_per_group = as.integer(createObjectVars$max_cells_per_group())
    }

    chr_exclude = trimws(unlist(strsplit(createObjectVars$chr_exclude(), split = ",")))

    if ( is.na( createObjectVars$MIN_counts_per_cell() | createObjectVars$MAX_counts_per_cell() )){
        min_max_counts_per_cell = NULL
    } else {
        min_max_counts_per_cell = c(createObjectVars$MIN_counts_per_cell(), createObjectVars$MAX_counts_per_cell())
    }

    # if the example data is being used
    if (createObjectVars$upload_example_data() == TRUE){

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Example Data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # set the refernce groups for the example data
        refgroup = c("Microglia/Macrophage","Oligodendrocytes (non-malignant)")
        # set the file names for the example data
        raw_counts_matrix = system.file("extdata", "oligodendroglioma_expression_downsampled.counts.matrix.gz", package = "infercnv")
        annotations_file = system.file("extdata", "oligodendroglioma_annotations_downsampled.txt", package = "infercnv")
        gene_order_file = system.file("extdata", "gencode_downsampled.EXAMPLE_ONLY_DONT_REUSE.txt", package = "infercnv")
        # set the deliminator
        delim = "\t"

    } else {

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # User Data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # get the refernce groups
        refgroup = trimws(unlist(strsplit(createObjectVars$ref_group_names(), split = ",")))
        # get the deliminator
        if(createObjectVars$delim() == "TSV"){
            delim = "\t"
        }


        # set the file names adn check that they are not null
        if (is.null(createObjectVars$raw_counts_matrix()$datapath)){
            stop("Count data file was not uploaded.")
        }else{
            raw_counts_matrix = createObjectVars$raw_counts_matrix()$datapath
        }
        if (is.null(createObjectVars$annotations_file()$datapath)){
            stop("Annotation file was not uploaded.")
        } else {
            annotations_file = createObjectVars$annotations_file()$datapath
        }
        if (is.null(createObjectVars$gene_order_file()$datapath)){
            stop("Gene order file was not uploaded.")
        } else {
            gene_order_file = createObjectVars$gene_order_file()$datapath
        }
    }

    shiny_busy <- function() {
        # use &nbsp; for some alignment, if needed
        shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
            '<p> Uploading data ...',
            '<span data-display-if="$(&#39;shiny::HTML&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;">',
            '<i class="fa fa-spinner fa-pulse fa-2x fa-fw" style="color:orange"></i>',
            '</span>',
            '</p>'
        ))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #------------------- Create the  infercnv object ---------------------------------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # run the loading message
    showNotification(id       = "loadingMessage",
                     ui       = shiny_busy(),
                     duration = NULL,)

    # Create the infercnv object
    infercnv_obj = infercnv::CreateInfercnvObject( raw_counts_matrix       = raw_counts_matrix,
                                                   annotations_file        = annotations_file,
                                                   gene_order_file         = gene_order_file,
                                                   delim                   = delim,
                                                   ref_group_names         = refgroup,
                                                   chr_exclude             = chr_exclude,
                                                   max_cells_per_group     = max_cells_per_group,
                                                   min_max_counts_per_cell = min_max_counts_per_cell)
    # remove the loading message
    removeNotification(id = "loadingMessage")


    if( exists("infercnv_obj") ){
        # Message to display that the files have been uploaded
        insertUI(
            # ID
            selector = "#uploadAlertMessage",
            # Make a class so can remove the message
            ui = tags$div(
                class = "alert alert-success alert-dismissible",
                shiny::HTML("<span class='fa fa-check fa-fw' aria-hidden='true'> \
                 </span> Data Uploaded! <button type='button' \
                 class='close' data-dismiss='alert'>&times;</button>"))
        )



        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #--------------- Print the input data files ----------------------------
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        output$count.data <- DT::renderDataTable(DT::datatable({
            infercnv_obj@count.data
        }))

        output$annotation_data <- DT::renderDataTable(DT::datatable({
            read.table( file = annotations_file,
                        sep  = delim)
        }))

        output$gene_order <- DT::renderDataTable(DT::datatable({
            infercnv_obj@gene_order
        }))

        return(infercnv_obj)
    }
}

















