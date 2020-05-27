#!/usr/bin/env Rscript




#' User Interface for the Home Page of the infercnv shiny application 
#' 
#' @title homePageUI(): User Interface for the Home Page 
#' 
#' @param id, character string used to specify a namespace, \code{shiny::\link[shiny]{NS}}
#'
#' @return a super cool home page that provides more infomration about infercnv
#' 

homePageUI <- function(id) {
    ns <- NS(id)
    
    
    
    # set up the CSS styling for this page 
    tags$head(includeCSS("www/infercnv.style.css"))
    
    # Create the division for the navbar page for CSS 
    tags$div(class = "navbarPageHome",
             navbarPage(
                 # the footer to be added to the home page 
                 footer = includeHTML("./www/pageFooter.html"),
                 
                 title = "",
                 
                 
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 #----------------------------- About -----------------------------
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 tabPanel(title = "About",
                          fluidRow(
                              # Infercnv Logo image 
                              tags$div( HTML('<img src="images/infercnv.logo.png" width="600">'),
                                        align    = "center"
                                        )
                          ),
                          
                          
                          fluidRow(
                              column(1),
                              column(10,
                                     tags$h1("InferCNV: Inferring copy number alterations from tumor single cell RNA-Seq data",
                                             align  = "center")
                              )
                          ),
                          
                          tags$br(),
                          tags$hr(),
                          
                          fluidRow(
                              column(1),
                              column(10,
                                     tags$p( "InferCNV is used to explore tumor single cell RNA-Seq data to identify evidence for somatic 
                                                large-scale chromosomal copy number alterations, such as gains or deletions of entire chromosomes 
                                                or large segments of chromosomes. This is done by exploring expression intensity of genes 
                                                across positions of tumor genome in comparison to a set of reference 'normal' cells. A heatmap 
                                                is generated illustrating the relative expression intensities across each chromosome, and it often 
                                                becomes readily apparent as to which regions of the tumor genome are over-abundant or less-abundant 
                                                as compared to that of normal cells."),
                                     
                                     tags$p("InferCNV provides access to several residual expression filters to explore minimizing noise and further 
                                               revealing the signal supporting CNA. Additionally, inferCNV includes methods to predict CNA regions and 
                                               define cell clusters according to patterns of heterogeneity."),
                                     
                                     tags$p("InferCNV is one component of the TrinityCTAT toolkit focused on leveraging the use of RNA-Seq to better 
                                               understand cancer transcriptomes. To find out more about Trinity CTAT please visit TrinityCTAT.")
                              )
                          )
                 ),
                 
                 
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 #----------------------------- Runing Infercnv Menu-----------------------------
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 navbarMenu(
                     "Runing Infercnv",
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #----------------------------- 2-step protocol-----------------------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     tabPanel("Basics",
                              tags$div(
                                  class = "homeTabTitles",
                                  fluidRow(
                                      tags$h2("Running Infercnv")
                                  ),
                                  fluidRow(
                                      tags$br(),
                                      column(1),
                                      column(10,
                                             
                                             tags$p("InferCNV can be run via a simple 2-step protocol, or can be run step-by-step with customization for more exploratory purposes."),
                                             tags$hr(style="border-color: grey;"),
                                             tags$br(),
                                      )
                                  )
                              ),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         
                                         tags$h3("InferCNV 2-step execution:",style = "font-weight:bold"),
                                         tags$br(),
                                         tags$h4("Step 1: "),
                                         tags$p("Upload the three required inputs:"),
                                         tags$ul(
                                             tags$li("Raw Counts Matrix"),
                                             tags$li("Cell Type Annotations"),
                                             tags$li("Gene Ordering File")
                                         ),
                                         tags$p("There are several options that can be set before the files are uploaded. 
                               These options include ", tags$strong("Reference Group Names"), " is set to the various normal-cell type (non-tumor) as defined 
                               in the cellAnnotations.txt file. See the ", tags$strong("File Definitions"), " tab for more details."),
                                         tags$blockquote(
                                             "Note, if you do not have reference cells, you can leave ",tags$strong("Reference Group Names"), " blank in which case the average 
                                   signal across all cells will be used to define the baseline. This can work well when there are sufficient differences among the 
                                   cells included (ie. they do not all show a chromosomal deletion at the same place)."
                                         ),
                                         tags$blockquote(
                                             "Note, inferCNV expects that you've already filtered out low quality cells. If you need to further impose minimum/maximum read counts 
                                   per cell, you can include an additional filter, such as: Min max counts per cell = 1e5 and Min max counts per cell = 1e6"
                                         ),
                                         
                                         tags$br(),
                                         
                                         tags$h3("Step 2: "),
                                         tags$br(),
                                         tags$p(
                                             "Once the needed files are uploaded, the infercnv analysis can be ran.
                               There are many settings associated with running the analysis portion of infercnv.
                                Standard infercnv can be ran with the default settings that are automatticly. "
                                         ),
                                         tags$p("Some of the basic settings that might need to be adjusted are the following:"),
                                         
                                         tags$ul(
                                             tags$li(
                                                 tags$p(
                                                     "The ", tags$strong("Cutoff Value")," determines which genes will be used for the infercnv analysis. 
                                    Genes with a mean number of counts across cells will be excluded. For smart-seq (full-length transcript sequencing, 
                                    typically using cell plate assays rather than droplets), a value of 1 works well. For 10x (and potentially other 
                                    3'-end sequencing and droplet assays, where the count matrix tends to be more sparse), a value of 0.1 is found to generally work well."
                                                 )
                                             ),
                                             tags$li(
                                                 tags$p(
                                                     "An ", tags$strong("Output Directory")," can be selected from a dropdown menu."
                                                 )
                                             ),
                                             tags$li(
                                                 tags$p(
                                                     "The ", tags$strong("Cluster By Groups")," setting indicates to perform separate clustering for the tumor cells according to the patient type, 
                                    as defined in the cell annotations file."
                                                 )
                                             )
                                         ),
                                         
                                         tags$br(),
                                         tags$br(),
                                         tags$hr(style="border-color: grey;"),
                                         
                                         tags$h3("InferCNV step-by-step exploratory execution: ", style = "font-weight:bold"),
                                         
                                         tags$p("The general infercnv workflow as performed via the above infercnv::run() method operates as follows:"),
                                         
                                         tags$div( HTML('<img src="images/InferCNV_procedure.png" width="600">'),
                                                   align    = "center"),
                                         
                                         tags$blockquote("Setting ", tags$strong("Denoise ")," to TRUE enables the de-noising procedure. Several de-noising filters are available for exploration."),
                                         tags$blockquote("Setting ", tags$strong("HMM"), " to TRUE, enables the CNV predictions. There are multiple inferCNV HMM prediction methods available to explore as well."),
                                         
                                         tags$p("The detailed steps of the inferCNV algorithm involve the following:"),
                                         tags$ul(
                                             tags$li(
                                                 tags$p("Filtering genes: those genes found expressed in fewer than 'min_cells_per_gene' are removed from the counts matrix.")
                                             ),
                                             tags$li(
                                                 tags$p("Normalization for sequencing depth (total sum normalization): read counts per cell are scaled to sum to the median total read count across cells. Instead of a metric such as counts per million (cpm), values are counts per median sum.")
                                             ),
                                             tags$li(
                                                 tags$p("log transformation: individual matrix values (x) are transformed to log(x+1)")
                                             ),
                                             tags$li(
                                                 tags$p("Center By Normal Gene Expression: the mean value for each gene across normal (reference) cells is subtracted from all cells for corresponding genes. Since this subtraction is performed in log space, this is effectively resulting in log-fold-change values relative to the mean of the normal cells.")
                                             ),
                                             tags$li(
                                                 tags$p("Thresholding dynamic range for log-fold-change values. Any values with abs(log(x+1)) exceeding 'max_centered_threshold' (default=3) are capped at that value.")
                                             ),
                                             tags$li(
                                                 tags$p("Chromosome-Level Smoothing: for each cell, genes ordered along each chromosome have expression intensities smoothed using a weighted running average. By default, this is a window of 101 genes with a pyramidinal weighting scheme.")
                                             ),
                                             tags$li(
                                                 tags$p("Centering Cells: each cell is centered with its median expression intensity at zero under the assumption that most genes are not in CNV regions.")
                                             ),
                                             tags$li(
                                                 tags$p("Adjustment Relative To Normal Cells: The mean of the normals is once again subtracted from the tumor cells. This further compensates for differences that accrued after the smoothing process.")
                                             ),
                                             tags$li(
                                                 tags$p("The Log Transformation Is Reverted. This makes the evidence for amplification or deletion more symmetrical around the mean. (note, with loss or gain of one copy, corresponding values 0.5 and 1.5 are not symmetrical in log space. Instead, 0.5 and 2 are symmetrical in log space. Hence, we invert the log transformation to better reflect symmetry in gains and losses).")
                                             )
                                         ),
                                         tags$p("The above generates the 'preliminary infercnv object'. The most obvious signal supporting CNVs is generally apparent in this representation. Additional filtering can be applied as a way of improving the signal to noise ratio. Also, CNV regions can be predicted using HMMs.")
                                         
                                         
                                  )
                              )
                     ), # tab panel
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #----------------------------- Input File Types -----------------------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     
                     tabPanel("Input File Types",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Input File Types and Descriptions")
                                       ),
                                       fluidRow(
                                           column(1),
                                           column(10,
                                                  tags$p('There are several files that may be needed depending on the analysis. 
                                                       These files are: '),
                                                  tags$ul(
                                                      tags$li("Raw Counts Matrix"),
                                                      tags$li("Sample annotation file"),
                                                      tags$li("Gene ordering file")
                                                  ),
                                                  tags$hr(style="border-color: grey;"),
                                                  tags$br(),
                                           )
                                       )
                              ),
                              fluidRow(
                                  column(1),
                                  column(10,
                                         
                                         tags$h4('Raw Counts Matrix for Genes x Cells'),
                                         tags$p('InferCNV is compatible with both smart-seq2 and 10x single cell transcriptome data, and presumably other methods (not tested). 
                                                   The counts matrix can be generated using any conventional single cell transcriptome quantification pipeline, yielding a matrix 
                                                   of genes (rows) vs. cells (columns) containing assigned read counts.'),
                                         tags$p('The format might look like so:'),
                                         
                                         # Table output
                                         tableOutput(outputId = ns("count_matrix_example")),
                                         
                                         tags$p('The matrix can be provided as a tab-delimited file. (note, sparse matrices are also supported)'),
                                         
                                         tags$br(),
                                         tags$hr(),
                                         
                                         tags$h4('Sample annotation file'),
                                         tags$p('The sample annotation file is used to define the different cell types, and optionally, 
                                                indicating how the cells should be grouped according to sample (ie. patient). 
                                                The format is simply two columns, tab-delimited, and there is no column header.'),
                                         
                                         
                                         # Table output
                                         tableOutput(outputId = ns("annotations_example_df")),
                                         
                                         tags$br(),
                                         
                                         tags$p("The first column is the cell name, and the 2nd column indicates the known cell type. For the normal cells, 
                                                if you have different types of known normal cells (ie. immune cells, normal fibroblasts, etc.), you can give 
                                                an indication as to what the cell type is. Otherwise, you can group them all as 'normal'. If multiple 'normal' 
                                                types are defined separately, the the expression distribution for normal cells will be explored according to 
                                                each normal cell grouping, as opposed treating them all as a single normal group. They'll also be clustered 
                                                and plotted in the heatmap according to normal cell grouping."),
                                         
                                         tags$p('The sample (ie. patient) information is encoded in the attribute name as "malignant_{patient}", which allows 
                                                the tumor cells to be clustered and plotted according to sample (patient) in the heatmap.'),
                                         
                                         tags$blockquote(
                                             tags$p(
                                                 'Only those cells listed in the sample annotations file will be analyzed by inferCNV. This is useful in case 
                                                 you cells of interest are a subset of the total counts matrix, without needing create a new matrix containing 
                                                 the subset of interest.'
                                             )
                                         ),
                                         
                                         tags$h4('Gene ordering file'),
                                         
                                         tags$p('The gene ordering file provides the chromosomal location for each gene. The format is tab-delimited and has 
                                                no column header, simply providing the gene name, chromosome, and gene span:'),
                                         
                                         
                                         # Table output
                                         tableOutput(outputId = ns("gene_example_df")),
                                         
                                         tags$br(),
                                         tags$p('Every gene in the counts matrix to be analyzed should have the corresponding gene name and location info provided in this gene ordering file.'),
                                         
                                         tags$blockquote(
                                             tags$p('Note, only those genes that exist in both the counts matrix and the gene ordering file will be included in the inferCNV analysis.')
                                         ),
                                         
                                         tags$p('Some Genomic Position Files have been generated from common references and made available at ',
                                                HTML('<a href="https://data.broadinstitute.org/Trinity/CTAT/cnv/" rel="nofollow">TrinityCTAT</a>'),"."),
                                         
                                         tags$p('If you need to construct your own custom genomic positions file, see ',
                                                HTML('<a href="instructions-create-genome-position-file">instructions for creating a genomic position file</a>'), 
                                                ".")
                                  )
                              )
                     ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #----------------------------- Applying HMM -----------------------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     tabPanel("Predicting via HMM",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("InferCNV HMM based CNV Prediction methods")
                                       ),
                                       fluidRow(
                                           tags$br(),
                                           column(1),
                                           column(10,
                                                  tags$p("We currently support two models for HMM-based CNV prediction, what we refer to as the i3 and i6 models.  
                                                            These are set in step 2 of running infercnv (i6 is default). Each method 
                                                            operates on the 'preliminary infercnv object' which has been processed through the standard inferCNV 
                                                            processing routines, involving subtraction of signal corresponding to 'normal (reference)' 
                                                            cells and smoothing operations.")
                                           )
                                       ),
                                       tags$br(),
                                       tags$hr(),
                              ),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         includeMarkdown("www/inferCNV-HMM-based-CNV-Prediction-Methods.md")
                                  )
                              )
                     ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #---------------- Infercnv i3 HMM ----------------
                     tabPanel(title = "i3 HMM",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Infercnv i3 HMM")
                                       ),
                                       fluidRow(
                                           tags$br(),
                                           column(1),
                                           column(10, 
                                                  tags$p("The inferCNV i3 HMM is highly similar to that used by HoneyBADGER, containing three states representing deletion, 
                                                            neutral, and amplification. The state model is illustrated below.")
                                           )
                                       )
                              ),
                              tags$hr(style="border-color: grey;"),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         includeMarkdown("www/infercnv-i3-HMM-type.md")
                                  )
                              )
                     ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #---------------- Infercnv i6 HMM ----------------
                     tabPanel(title = "i6 HMM",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Infercnv i6 HMM")
                                       )
                              ),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         includeMarkdown("www/infercnv-i6-HMM-type.md")
                                  )
                              )
                     ),
                     
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #---------------- Bayesian Latent Mixture Model  ----------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     tabPanel(title = "Bayesian Mixture Model",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Bayesian Network Latent Mixture Model")
                                       )
                              ),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         includeMarkdown("www/Bayesian-Network-Latent-Mixture-Model.md")
                                  )
                              )
                     ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #---------------- Bayesian Latent Mixture Model  ----------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     tabPanel(title = "Tumor Subclusters",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Infercnv Tumor Subclusters")
                                       ),
                                       fluidRow(
                                           tags$br(),
                                           column(1),
                                           column(10,
                                                  tags$p("By default, inferCNV operates at the level of whole samples, such as all cells defined as a 
                                                            certain cell type derived from a single patient.  This is the fastest way to run inferCNV, but 
                                                            often not the optimal way, as a given tumor sample may have subpopulations with varied patterns of CNV.")
                                           )
                                       )
                              ),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         includeMarkdown("www/infercnv-tumor-subclusters.md")
                                  )
                              )
                     ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #---------------- Bayesian Latent Mixture Model  ----------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     tabPanel(title = "Smoothing Methods",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Smoothing Methods")
                                       ),
                                       fluidRow(
                                           tags$br(),
                                           column(1),
                                           column(10,
                                                  tags$p("Each method is applied on individual cells' data, and independently per chromosome.")
                                           )
                                       )
                              ),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         includeMarkdown("www/Smoothing_methods.md")
                                  )
                              )
                     ),
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #----------------------------- Applying Noise Filters -----------------------------
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     tabPanel("Applying Noise Filters",
                              tags$div(class = "homeTabTitles",
                                       fluidRow(
                                           tags$h2("Infercnv de-noising filters")
                                       ),
                                       fluidRow(
                                           tags$br(),
                                           column(1),
                                           column(10,
                                                  
                                                  tags$p("These de-noising filter options are available for manipulating the residual expression intensities with the goal 
                                                   of reducing the noise (residual signal in the normal cells) while retaining the signal in tumor cells that could 
                                                   be interpreted as supporting CNV.")
                                           )
                                       )
                              ),
                              tags$hr(style="border-color: grey;"),
                              fluidRow(
                                  tags$br(),
                                  column(1),
                                  column(10,
                                         
                                         tags$p("The residual normal signal is derived from the preliminary inferCNV object, which has been smoothed, centered, 
                                               and the mean of the normal (reference) cells subtracted:"),
                                         
                                         
                                         tags$div( HTML('<img src="images/denoise_filtering_methods_top.png" width="600">'),
                                                   align    = "center"),
                                         
                                         tags$br(),
                                         tags$h3("Filtering using defined thresholds:"),
                                         tags$hr(),
                                         tags$p("A specific threshold deviation from the mean can be set in the",tags$strong('De-Noising Parameters'),"section in the settings :"),
                                         tags$h5("The example bellow has the Noise Filter set to 0.1:", style = "font-weight:bold"),
                                         
                                         tags$div( HTML('<img src="images/denoise_hard_filter.png" width="600">'),
                                                   align    = "center"),
                                         tags$br(),
                                         
                                         tags$h3("Filtering using Dynamic thresholding (default setting):"),
                                         tags$hr(),
                                         tags$p("By default, the hard cutoffs for denoising are computed based on the standard deviation of the residual normal expression 
                                            values. This thresholding can be adjusted using the",tags$strong("Standard Deviation Amplifier"), "in settings. For example, 
                                            we can use ",tags$strong("1.5"), " the standard deviation for filtering like so:"),
                                         tags$div( HTML('<img src="images/denoise_dynamic_sd1.5.png" width="600">'),
                                                   align    = "center"),
                                         
                                         tags$br(),
                                         tags$h3("Adjusting intensities via sigmoidal (logistic) function:"),
                                         tags$hr(),
                                         tags$p("Instead of applying a strict threshold, you can apply a filtering gradient by applying a sigmoidal function that reduces 
                                     intensities near the mean more than intensities more distant from the mean. An example of applying this.", 
                                                tags$strong("Noise Logistic"), "is shown below."),
                                         tags$div( HTML('<img src="images/denoise_logistic_sd3.png" width="600">'),
                                                   align    = "center"),
                                         tags$p("The midpoint for the sigmoidal curve (logistic function) is set based on the ", tags$strong("Standard Deviation Amplifier"), 
                                                " (or alternative, fixed value at a ", tags$strong("Noise Filter"), "setting), and this is enabled by setting ", tags$strong("Noise Logistic.")),
                                         
                                         tags$br(),
                                         tags$h3("Add-on median filtering:"),
                                         tags$hr(),
                                         tags$p("For any of the types of denoising (even disabled) explained above, an add-on ", 
                                                HTML('<a href="https://en.wikipedia.org/wiki/Median_filter" rel="nofollow">median filtering</a>'), 
                                                "can be applied to smooth the visual output of inferCNV. To do so, you simply click on ", tags$strong("Add Median Filtering"), "in the 
                                            Main Analysis Window"),
                                         tags$p("The filtering takes into account chromosomes and the clusters or subclusters that have been defined as boundaries. It also keeps the 
                                            hierarchical clustering previously defined intact in order for it to be representative of how it was obtained."),
                                         tags$p("Using the inferCNV object obtained running the denoising via sigmoidal (see above), the resulting figure can be seen below."),
                                         tags$div( HTML('<img src="images/denoise_logistic_sd3_median_filtered.png" width="600">'),
                                                   align    = "center"),
                                  )
                              )
                              
                     ) # tab panel 
                 ),
                 
                 
                 
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 #----------------------------- Infercnv Figures -----------------------------
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 tabPanel("Infercnv Figures",
                          fluidRow(
                              tags$div(class = "homeTabTitles",
                                       tags$h2("Interpreting The Figure")
                              ),
                              
                              tags$div( HTML('<img src="images/infercnv.png" alt="infercnv_image", width="600">'),
                                        align    = "center"),
                              
                              
                              tags$br(),
                              column(1),
                              column(10,
                                     tags$p("The expression values for normal cells are plotted in the top heatmap and the tumor cells 
                                                are plotted in the bottom heatmap, with genes ordered from left to right across the chromosomes. 
                                                The normal cell expression data is effectively subtracted from the tumor cell expression data to 
                                                yield differences, where chromosomal region amplification shows up as blocks of red, and chromosomal 
                                                region deletions show up as blue blocks. 
                                                \n
                                                More precise details are provided below:"),
                                     br(),
                                     
                                     tags$h4("Rows", style = "font-weight:bold"),
                                     tags$p("The rows of the both heat maps correspond to the cells."),
                                     tags$p("The heat maps are separated horizontally between reference and non-reference cells (or observations) 
                                               with a central color bar indicating different chromosome regions (by color change). The very top heat 
                                               map is comprised of reference observations. The lower heat map contains non-reference cells (observations) 
                                               ordered using hierarchical clustering (euclidean distance, average linkage)."),
                                     tags$p("The ordering and grouping of cells is determined by specified clustering algorithm and grouping according 
                                               to the cell annotations file. Depending on whether the group_by_cluster option is used or not, there are 
                                               a few differences:"),
                                     tags$ul(
                                         tags$li("If Group By Cluster is set to FALSE, the dendrogram on the left is a hierarchical clustering of all non-reference 
                                            cells. The first color column indicates the subdivisions of the dendrogram by cutting it in k_obs_groups groups. 
                                            The second color column indicates the annotation matching to each cell from the input provided."),
                                         tags$li("If Group By Cluster is set to TRUE, the dendrogram on the left is a 'linear concatenation' of the dendrograms 
                                            for each type of non-reference cells (the root of the dendrogram is linked to the root of each type's dendrogram, 
                                            which leads to having all of them on the same level). The first color column is of a single color as k_obs_groups 
                                            is not used when clustering by annotation. The second color column indicates the annotation matching to each 
                                            cell from the input provided, and there should not be any mix since the cells have been clustered by the same 
                                            annotation."),
                                     ),
                                     tags$h4("Columns", style = "font-weight:bold"),
                                     tags$p("The columns of the figure are genes, ordered by chromosome position."),
                                     tags$p("Black vertical lines separate contigs/chromosomes. The horizontal color bar between the two heat maps also 
                                               indicates contigs/chromosomes by color change. Contigs/chromosomes are defined in and ordered by the Genomic 
                                               Position File."),
                                     
                                     tags$h4("Residual expression values", style = "font-weight:bold"),
                                     tags$p("The color intensities of the heatmap correspond to the residual expression values after performing a series 
                                               of data transformations and effectively subtracting the normal cell expression data from the tumor cell 
                                               expression data. Full details are provided here."),
                                     
                                     tags$h4("Interpretation", style = "font-weight:bold"),
                                     tags$p("The normal cells in the top heatmap define baseline expression for genes in normal cells. This baseline 
                                        distribution of normal gene expression is subtracted from both the normal cells from which was defined as well as the 
                                        tumor cells. Afterwards, the normal cell expression heatmap should be largely devoid of signal, with the exception 
                                        of certain outlier gene expression values in certain cells. Removing this baseline of normal expression signal from 
                                        the tumor cells should reveal those chromosomal regions that have significantly more or less expression than the normal 
                                        cells, highlighting likely amplified or deleted whole chromosomes or large chromosomal regions.")
                                     
                              )
                          ) # fluid Row
                 ), # tabpanel
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 #----------------------------- Example Data -----------------------------
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 tabPanel("Example Data",
                          tags$div(class = "homeTabTitles",
                                   fluidRow(
                                       tags$h2("Provided Example Data")
                                   ),
                                   fluidRow(
                                       column(1),
                                       column(10,
                                              tags$p("As part of the Infercnv package, example data has been provided. 
                                                The example data consists of scRNA-Seq expression of oligodendroglioma with hallmark chr 1p and 19q deletions")
                                       )
                                   )
                          ),
                          
                          tags$hr(style="border-color: black;"),
                          
                          fluidRow(
                              column(1),
                              column(10,
                                     tags$br(),
                                     tags$p("The following figure should be the final output figure produced by the example data."),
                                     # Figure image output 
                                     tags$div( HTML('<img src="images/infercnv.png" alt="infercnv_image" width="600">'),
                                               align    = "center"),
                                     tags$br(),
                                     tags$hr(style="border-color: black;")
                              )
                          ),
                          
                          fluidRow(
                              tags$div(align = "center",
                                       tags$span(
                                           tags$h2("Additional InferCNV Examples", style = "font-weight:bold")
                                       )
                              )
                          ),
                          fluidRow(
                              column(1),
                              column(10,
                                     tags$h4("Additional InferCNV examples can be found as indicated below:"),
                                     
                                     tags$br(),
                                     
                                     # Glioblastoma
                                     tags$ul(
                                         tags$li(
                                             HTML('<a href="https://github.com/broadinstitute/inferCNV_examples/tree/master/glioblastoma">Glioblastoma</a>'),
                                             " derived from: ",
                                             HTML('<a href="https://portals.broadinstitute.org/single_cell/study/glioblastoma-intra-tumor-heterogeneity" rel="nofollow">Single-cell RNA-seq highlights intratumoral heterogeneity in
    primary glioblastoma. Science. 2014 Jun 20;344(6190):1396-401</a>')
                                         )
                                     ),
                                     HTML('<img src="images/glioblastoma.infercnv.png" width="450">'),
                                     tags$blockquote(
                                         tags$p("In the glioblastoma data set, GTEx brain samples were used as the background signal. For the others, normal single cells were used as background.")
                                     ),
                                     tags$br(),
                                     
                                     # Melanoma
                                     tags$ul(
                                         tags$li(
                                             HTML('<a href="https://github.com/broadinstitute/inferCNV_examples/tree/master/melanoma">Melanoma</a>'),
                                             " derived from: ",
                                             HTML('<a href="https://portals.broadinstitute.org/single_cell/study/melanoma-intra-tumor-heterogeneity" rel="nofollow">Dissecting the multicellular ecosystem of metastatic melanoma by single-cell RNA-seq. Science. 2016</a>')
                                             
                                         )
                                     ),
                                     HTML('<img src="images/melanoma.infercnv.png" width="450">'),
                                     tags$br(),
                                     
                                     #Oligodendroglioma
                                     tags$ul(
                                         tags$li(
                                             HTML('<a href="https://github.com/broadinstitute/inferCNV_examples/tree/master/oligodendroglioma">Oligodendroglioma</a>'),
                                             " derived from: ",
                                             HTML('<a href="https://portals.broadinstitute.org/single_cell/study/oligodendroglioma-intra-tumor-heterogeneity" rel="nofollow">Single-cell RNA-seq supports a developmental hierarchy in human oligodendroglioma Nature, November 2016</a>')
                                         )
                                     ),
                                     HTML('<img src="images/oligodendroglioma.infercnv.png" width="450">'),
                                     tags$blockquote(
                                         tags$p("Note, a small oligodendroglioma data set is included as the example data in the ",
                                                HTML('<a href="https://github.com/broadinstitute/inferCNV">infercnv software</a>')
                                         )
                                     ),
                                     tags$br(),
                                     
                                     # Pediatric midline gliomas 
                                     tags$ul(
                                         tags$li(
                                             HTML('<a href="https://github.com/broadinstitute/inferCNV_examples/tree/master/pediatric_midline_gliomas">Pediatric midline gliomas</a>'),
                                             " derived from: ",
                                             HTML('<a href="https://portals.broadinstitute.org/single_cell/study/single-cell-analysis-in-pediatric-midline-gliomas-with-histone-h3k27m-mutation" rel="nofollow">Developmental and oncogenic programs in H3K27M gliomas
    dissected by single-cell RNA-seq. Science. 2018</a>')
                                         )
                                     ),
                                     HTML('<img src="images/pediatric_midline_gliomas.png" width="450">')
                              ),
                          ),
                 ), # Tab panel
                 
                 
                 
                 
                 
                 
                 
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 #----------------------------- Citations -----------------------------
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 
                 tabPanel("Citation",
                          fluidRow(
                              tags$div(class = "homeTabTitles",
                                       tags$h2("Citations")
                              ),
                          ),
                          
                          tags$br(),
                          
                          fluidRow(
                              column(1),
                              column(10,
                                     tags$h4("Please use the following citation:"),
                                     tags$pre(
                                         # tags$code(
                                         "inferCNV of the Trinity CTAT Project.  https://github.com/broadinstitute/inferCNV"
                                     )
                              )
                          ),
                          
                          tags$br(),
                          
                          fluidRow(
                              column(1),
                              column(10,
                                     tags$h4("This methodology was leveraged in these earlier studies, which inspired the development of the inferCNV software:"),
                                     
                                     tags$br(),
                                     
                                     tags$p(
                                         HTML('<a href="http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4123637/" 
                                         rel="nofollow">Anoop P. Patel, Itay Tirosh, et al. Single-cell RNA-seq highlights intratumoral heterogeneity in primary glioblastoma. Science. 2014 Jun 20: 1396-1401</a>')
                                     ),
                                     tags$p(
                                         HTML('<a href="http://www.ncbi.nlm.nih.gov/pubmed/27124452" 
                                         rel="nofollow">Tirosh I et al. Dissecting the multicellular ecosystem of metastatic melanoma by single-cell RNA-seq. Science. 2016 Apr 8;352(6282):189-96</a>')
                                     ),
                                     tags$p(
                                         HTML('<a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5465819/" 
                                         rel="nofollow">Tirosh I et al.  Single-cell RNA-seq supports a developmental hierarchy in human oligodendroglioma. Nature. 2016 Nov 10;539(7628):309-313. PubMed PMID: 27806376; PubMed Central PMCID: PMC5465819.</a>')
                                     ),
                                     tags$p(
                                         HTML('<a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5519096/" 
                                    rel="nofollow">Venteicher AS, Tirosh I, et al. Decoupling genetics, lineages, and microenvironment in IDH-mutant gliomas by single-cell RNA-seq. Science. 2017 Mar 31;355(6332).PubMed PMID: 28360267; PubMed Central PMCID: PMC5519096.</a>')
                                     ),
                                     tags$p(
                                         HTML('<a href="https://www.ncbi.nlm.nih.gov/pubmed/29198524" rel="nofollow">Puram SV, Tirosh I, et al. Single-Cell Transcriptomic Analysis of Primary and Metastatic Tumor Ecosystems in Head and Neck Cancer. Cell. 2017 Dec 14;171(7):1611-1624.e24. PubMed PMID: 29198524; PubMed Central PMCID: PMC5878932.</a>')
                                     )
                                     
                              )
                          )
                          
                 ),
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 #----------------------------- Session Information -----------------------------
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 
                 tabPanel("Session Info",
                          fluidRow(
                              tags$div(class = "homeTabTitles",
                                       tags$h2("Session Information")
                              ),
                          ),
                          
                          tags$br(),
                          
                          fluidRow(
                              column(width = 10, offset = 1,
                                     verbatimTextOutput(ns("sessionInfo"))
                              )
                          )
                 )
             )
    )
    
}



#' Server portion of the homePage output portion of the infercnv Shiny app
#' 
#' @title homePage()
#' 
#' @param input, output, session standard \code{shiny} 
#'
#' @return Returns information needed to create the home page output 
#' 
homePage <- function(input, output, session){
    
    
    # ____________________    Example count data     _________________________
    
    tmp_matrix <- matrix(data = c(0,0,0,0,0, 0,0,0,0,0, 0,37,30,21,0, 0,0,0,0,2, 0,0,0,0,0),ncol = 5,)
    temp_count <- data.frame(tmp_matrix,
                             row.names = c("A2M","A4GALT","AAAS","AACS","AADAT"),
                             check.names = FALSE)
    colnames(temp_count) <- c("MGH54_P16_F12","MGH54_P12_C10","MGH54_P11_C11","MGH54_P15_D06",	"MGH54_P16_A03")
    
    # render the table
    output$count_matrix_example <- renderTable({temp_count},
                                               spacing = 'xs',
                                               striped = TRUE, bordered = TRUE)
    
    # ____________________   Example anotation data     ____________________
    
    V1 <- c("MGH54_P2_C12", "MGH36_P6_F03","MGH54_P16_F12","MGH54_P12_C10","MGH36_P1_B02", "MGH36_P1_H10")
    V2 <- c("Microglia/Macrophage","Microglia/Macrophage","Oligodendrocytes (non-malignant)","Oligodendrocytes (non-malignant)","malignant_MGH36","malignant_MGH36")
    temp_annotations <- data.frame(V1,V2)
    
    output$annotations_example_df <- renderTable({temp_annotations},
                                                 spacing = 'xs',
                                                 colnames = FALSE,
                                                 striped = TRUE, bordered = TRUE)
    
    
    #____________________     Gene order example     ____________________
    
    gene_order_example <- data.frame( "V1" = c("WASH7P", "LINC00115", "NOC2L", "MIR200A", "SDF4", "UBE2J2"),
                                      "V2" = rep("chr1",6),
                                      "V3" = c(14363, 761586, 879584, 1103243, 1152288, 1189289),
                                      "V4" = c(29806, 762902, 894689, 1103332, 1167411, 1209265),
                                      check.names = F)
    
    output$gene_example_df <- renderTable({gene_order_example},
                                          spacing = 'xs',
                                          colnames = FALSE,
                                          striped = TRUE, bordered = TRUE)
    
    
    #_____________________    Session Information    _____________________
    output$sessionInfo <- renderPrint({
        sessionInfo()
    })
}
