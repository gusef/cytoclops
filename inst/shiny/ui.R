require(Biobase)
require(CIOShiny)
require(data.table)
require(DT)
require(flowCore)
require(flowVS)
require(htmltools)
require(jsonlite)
require(Rtsne)
require(shiny)
require(shinyBS)
require(shinyjs)
require(shinythemes)
require(sp)
require(V8)
require(matrixStats)

source('gating_tab_functions.R')
source('input_tab_functions.R')
source('gatingpanel_functions.R')
source('misc.R')
source('polygon.R')
source('tsne_functions.R')
source('classifier_and_qc.r')

ui <- navbarPage(theme = shinytheme("darkly"),
                 title = "Cytoclops",
                 
                 tabPanel(title = "File loading",
                          includeCSS(system.file('www','cytoclops.css',package='cytoclops')),
                          h4('Load raw input'),
                          fileInput("fcsFile", "Add .FCS file",
                                    accept = c(".fcs",".FCS")),
                          DT::dataTableOutput("file_table"),
                          h4('Load pre-gated set'),
                          fileInput("rdsFile", "Choose .RDS file",
                                    accept = c(".rds",".RDS"))
                 ),
                 tabPanel(title = "Cleanup and QC",
                      tags$div(id='ViabilityClassifier',
                        sliderInput('ViabilitySlider', 
                                    'Sensitivity threshold', 
                                    0.1, 0.9, 0.5, step = 0.1),
                        actionButton('ViabilityButton',
                                     'Run Viability Classifier',
                                     icon = icon("leaf",lib = "glyphicon")))      
                 ),
                 tabPanel(title = "FlowSOM"
                 ),
                 tabPanel(title = "Display panel",
                          shinyjs::useShinyjs(),
                          shinyjs::extendShinyjs(script = system.file('www','overlay.js',package='cytoclops')),
                          bsAlert("alert"),
                          fluidRow(
                              column(2,
                                     h4('List of gates'),
                                     tags$div(id='gatingList'),
                                     jsTreeSelectorInput('TreeGates',
                                                    df=data.table::rbindlist(list(list(id='G1',text="All"))),
                                                    selected='G1'
                                     )
                                     
                              ),
                              column(4,
                                     h3("Gating Panel", align = "center"),
                                     actionButton('PolygonButton','',icon = icon("pencil",lib = "glyphicon")),
                                     actionButton('PolygonViewButton','',icon = icon("eye-open",lib = "glyphicon")),
                                     actionButton('PolygonResetButton','',icon = icon("remove",lib = "glyphicon")),
                                     downloadButton("SaveGate", ""),
                                     shinyjs::hidden(
                                         plotOutput("GatingPanel",
                                                    height="100%",
                                                    click = "gate_click",
                                                    brush = brushOpts(
                                                        id = "GateBrush",
                                                        fill = "#FF1414", 
                                                        stroke = "#FF1414"
                                                    ))),
                                     DT::dataTableOutput("children_table")
                              ),
                              
                              column(4,align='center',
                                     h3("bh-SNE Panel", align = "center"),
                                     downloadButton("SavetSNE", ""),
                                     shinyjs::hidden(
                                         plotOutput("tSNEPanel",
                                                    height="100%",
                                                    click = "visne_click",
                                                    brush = brushOpts(
                                                        id = "visne_brush"
                                                    ))) 
                              )
                          ),
                          fluidRow(
                              column(2,
                                     downloadButton("SaveStateButton", "Save file")
                              ),
                              column(4,
                                     selectInput("PlotType",
                                                 label = "Plot type:", 
                                                 choices = list("Density plot" = "density",  
                                                                "Regular Scatter" = "regular",
                                                                "Smooth Scatter" = "smooth"), 
                                                 selected = "density")
                              ),
                              column(4,
                                     tags$div(id='AboveVisneSpace')   
                              )
                          ),
                          verbatimTextOutput('Verbose')
                          
                 ),
                 tabPanel(title = "Markers",
                          tags$div(id='CurrentPanelTab',
                                   h4("Select Markers that should be arcsinh transformed for gating:"),
                                   checkboxInput('all_none_arcsinhselect', 'All/None')
                          )
                 ),
                 tabPanel(title = "Settings",
                          numericInput(inputId = 'DownSample',
                                       value = 1000,
                                       max = 30000,
                                       min = 500,
                                       label = 'Down sample size'),
                          numericInput(inputId = 'arcsinh_par',
                                       value = 5,
                                       max = 1000,
                                       min = 0.001,
                                       label = 'arcsinh coefficient'),
                          h3('Graphical parameters'),
                          numericInput(inputId = 'graphics_cex',
                                       value = 2,
                                       max = 0.1,
                                       min = 10,
                                       label = 'Dot size'),
                          numericInput(inputId = 'plot_downsample',
                                       value = 2,
                                       max = 1,
                                       min = 5,
                                       label = 'Downsample raster'),
                          h3('tSNE parameters'),
                          numericInput(inputId = 'tSNE_iter',
                                       value = 500,
                                       max = 5000,
                                       min = 100,
                                       label = 'tSNE iterations'),
                          numericInput(inputId = 'tSNE_theta',
                                       value = 0.5,
                                       max = 0.1,
                                       min = 1,
                                       label = 'tSNE Theta'),
                          numericInput(inputId = 'tSNE_perplexity',
                                       value = 30,
                                       max = 2,
                                       min = 500,
                                       label = 'tSNE perplexity'),
                          numericInput(inputId = 'tSNE_overlay_num_columns',
                                       value = 4,
                                       max = 8,
                                       min = 1,
                                       label = 'Number of t-SNE plots next to each other in overlay'),
                          numericInput(inputId = 'tSNE_overlay_width',
                                       value = 1280,
                                       max = 1920,
                                       min = 800,
                                       label = 'Width of t-SNE overlay overview')
                 )
)
