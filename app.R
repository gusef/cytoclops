# setwd(file.path(Sys.getenv('DBPROJ'),'2017_06_14_Cytoclops_prototype/first_shiny_app'))
rm(list=ls())
gc()

library(shiny)
require(Rtsne)
require(flowCore)
require(flowVS)

GatingPanel <- setClass("GatingPanel",
                        slots = c(indices = "numeric",
                                  children = "list",
                                  parent = "GatingPanel",
                                  gates= "list",
                                  tsne = "list"))

ui <- navbarPage(title = "Cytoclops",
                 tabPanel(title = "File loading",
                          actionButton("LoadTest", "Load test set"),
                          fileInput("fcsFile", "Choose preprocessed file",
                                    accept = c(".fcs",".FCS")
                          )
                 ),
                 tabPanel(title = "Display panel",
                          fluidRow(
                              column(2,
                                  h3('')     
                              ),
                              column(4,
                                  selectInput("PlotType", label = "Plot type:", 
                                              choices = list("Density plot" = "density", 
                                                             "Smooth Scatter" = "smooth", 
                                                             "Regular Scatter" = "regular"), 
                                              selected = "density")
                              ),
                              column(4,
                                   tags$div(id='AboveVisneSpace')   
                              )
                          ),
                          fluidRow(
                              column(2,
                                     h4('List of gates'),
                                     tags$div(id='gatingList'),
                                     radioButtons('gates', label = NULL,
                                                  c(Parent='1'))
                              ),
                              column(4,
                                     h3("Gating Panel"),
                                     plotOutput("GatingPanel",
                                                height="100%",
                                                click = "gate_click",
                                                brush = brushOpts(
                                                     id = "GateBrush"
                                    ))
                              ),
                              column(4,
                                  h3("bh-SNE Panel"),
                                  plotOutput("FirstVisnePanel",
                                             height="100%",
                                             click = "visne_click",
                                             brush = brushOpts(
                                                 id = "visne_brush"
                                             )) 
                              )
                          ),
                          verbatimTextOutput('GatingVerbose')
                 ),
                 tabPanel(title = "Markers",
                          tags$div(id='CurrentPanelTab',
                                   h4("Select Markers that should be arcsinh transformed for gating:")
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
                          h3('tSNE parameters'),
                          numericInput(inputId = 'tSNE_iter',
                                       value = 1000,
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
                                       label = 'tSNE perplexity')
                 )
)

#transforms a single marker to arcsinh
arcsinTransform <- function(marker, mat, map, input){
    selected_markers <- names(map)[as.numeric(input$ArcSinhSelect)]
    current_vals <- mat[,map[marker]]
    # only transform if the checkbox is set
    if(marker %in% selected_markers){
        current_vals <- asinh(current_vals/input$arcsinh_par)
    }
    return(current_vals)
}

#helper function that determines which markers should be pre-selected for arcsinh transformation
extractArcsinhTemp <- function(values){
    markers <- names(values$markerMapping)
    # selected <- rep(TRUE,length(markers))
    # deselect <- sapply(c('event','time'),
    #                    function(x,markers)grep(x,tolower(markers)),
    #                    markers)
    # selected[deselect] <- FALSE
    selected <- rep(FALSE,length(markers))
    selected[grep('::',markers)] <- TRUE
    return(as.list((1:length(markers))[selected]))
}


#eventually these should be preselected by whatever markers are not technical markers and have not been used for gating
#currently it just deselects the standard background / DNA / Beads / .. markers
extractTSNEMarkers <- function(values){
    markers <- names(values$markerMapping)
    selected <- rep(FALSE,length(markers))
    selected[grep('::',markers)] <- TRUE
    selected[grep('_Barcode$',markers)] <- FALSE
    selected[grep('_Viability$',markers)] <- FALSE
    selected[grep('Beads$',markers)] <- FALSE
    selected[grep('_DNA[12]$',markers)] <- FALSE
    return(as.list((1:length(markers))[selected]))
}


#function that extracts the markers, makes a map and handles the proper ordering
extractMarkerPanel <- function(values){
    chan <- pData(parameters(values$flowFrame))[,c('name','desc')]
    chan <- paste(chan$name,chan$desc,sep='::')
    vals <- colnames(values$flowFrame) 
    names(vals) <- sub('::NA','',chan)
    #Katja's custom order (might be extended)
    actual <- grep('..[0-9]+',vals)
    vals <- c(vals[actual[order(sub('^..','',vals[actual]))]],sort(vals[-actual]))  
    return(vals)
}

insertGatingPanels <- function(values){
    #insert the gating and visne panel + additional buttons and inputs
    insertUI(
        selector = "#GatingPanel",
        where = "afterEnd",
        ui =  tags$div(id='GatingPanelInterface',
                       fluidRow(
                           column(6,
                                  selectInput("Select_x_channels", 
                                              label = "Select X channel",
                                              choices = names(values$markerMapping),
                                              selected = names(values$markerMapping[1]))
                           ),
                           column(6,
                                  selectInput("Select_y_channels", 
                                              label = "Select Y channel",
                                              choices = names(values$markerMapping),
                                              selected = names(values$markerMapping[2]))                         
                           )
                       ),
                       actionButton("GateButton", "Gate selected"),
                       actionButton("RunTSNE", "Run TSNE")
        )
    )
    #insert the marker panel
    if(!is.null(values$markerMapping)){
        insertUI(
            selector = "#CurrentPanelTab",
            where = "afterEnd",
            ui =  tags$div(id='MarkerPanelInterface',
                           checkboxGroupInput("ArcSinhSelect", label = NULL, 
                                              choiceNames = as.list(names(values$markerMapping)),
                                              choiceValues = as.list(1:length(values$markerMapping)),
                                              selected = extractArcsinhTemp(values))  
            )
            
        )
    }
}

plot_colorByDensity <- function(x1,x2,
                                ylim=c(min(x2),max(x2)),
                                xlim=c(min(x1),max(x1)),
                                xlab="",ylab="",main="",cex=2) {
    df <- data.frame(x1,x2)
    x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
    df$dens <- col2rgb(x)[1,] + 1L
    cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
    df$col <- cols[df$dens]
    plot(x2~x1, data=df[order(df$dens),], 
         ylim=ylim,xlim=xlim,pch=20,col=col,
         cex=cex,xlab=xlab,ylab=ylab,
         main=main)
}

map2color<-function(x,pal,limits=NULL){
    if(is.null(limits)) limits=range(x)
    pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}


server <- function(input, output, session) {
    options(shiny.maxRequestSize=500*1024^2)
    
    #reactive values
    values <- reactiveValues(gatingPanels=NULL,
                             flowFrame = NULL,
                             currentGatingPanel=NULL,
                             channels=NULL,
                             markerMapping=NULL,
                             verbatimOutput='',
                             tsne_current_marker='',
                             tsne_sample=NULL,
                             tsne_col='',
                             tSNE_markers=NULL, 
                             tsne_arcsintransform=TRUE)
    
    #file loading
    observeEvent(input$fcsFile,{ 
        #clean up
        removeUI(selector = "#GatingPanelInterface")
        removeUI(selector = "#MarkerPanelInterface")
        removeUI(selector = "#ImposeColorSelector")
        values$verbatimOutput <- ''

        #generate the underlying object that holds all information for the parent gates
        values$gatingPanels <- GatingPanel()
        values$flowFrame <- read.FCS(input$fcsFile$datapath)
        values$gatingPanels@indices <- 1:nrow(values$flowFrame)
        values$currentGatingPanel <-  values$gatingPanels
        values$markerMapping <- extractMarkerPanel(values)
        insertGatingPanels(values)
    })
    
    #file loading
    observeEvent(input$LoadTest,{ 
        #clean up
        removeUI(selector = "#GatingPanelInterface")
        removeUI(selector = "#MarkerPanelInterface")
        removeUI(selector = "#ImposeColorSelector")
        values$verbatimOutput <- ''
        
        #generate the underlying object that holds all information for the parent gates
        values$gatingPanels <- GatingPanel()
        values$flowFrame <- read.FCS('test.fcs')
        values$gatingPanels@indices <- 1:nrow(values$flowFrame)
        values$currentGatingPanel <- values$gatingPanels
        
        values$markerMapping <- extractMarkerPanel(values)
        insertGatingPanels(values)
    })
    
    #simple gating panel
    output$GatingPanel <- renderPlot({
        if (is.null(values$currentGatingPanel))
            return(NULL)
        mat <- exprs(values$flowFrame[values$currentGatingPanel@indices,])
        xVal <- arcsinTransform(input$Select_x_channels, mat, values$markerMapping, input)
        yVal <- arcsinTransform(input$Select_y_channels, mat, values$markerMapping, input)
        
        if (input$PlotType=='density'){
            plot_colorByDensity(xVal,
                                yVal,
                                xlab=input$Select_x_channels,
                                ylab=input$Select_y_channels,
                                cex=input$graphics_cex)  
        }else if(input$PlotType=='smooth'){
            smoothScatter(xVal,
                          yVal, 
                          xlab=input$Select_x_channels,
                          ylab=input$Select_y_channels,
                          colramp = colorRampPalette(c("white", 'black')),
                          pch = ".")
        }else{
            plot(xVal,
                 yVal,
                 xlab=input$Select_x_channels,
                 ylab=input$Select_y_channels,
                 pch=20,
                 cex=input$graphics_cex)    
        }
    },height = function() {
        min(700,session$clientData$output_GatingPanel_width)
    })
    
    #if you fiddle around with the markers, and the tSNE was done using arcsinh transformed data it gets dropped
    observeEvent(input$ArcSinhSelect, {
        if (values$tsne_arcsintransform){
            values$currentGatingPanel@tsne<-list()
        }
    })

    #tun the bh-SNE when the button is pressed
    observeEvent(input$RunTSNE, {
            showModal(modalDialog(
                title = "Running t-SNE",
                checkboxInput("tsne_arcsin", 
                              label='Run t-SNE using arcsinh transformed values', 
                              value = TRUE),
                checkboxGroupInput("tSNEMarkers", label = NULL, 
                                   choiceNames = as.list(names(values$markerMapping)),
                                   choiceValues = as.list(1:length(values$markerMapping)),
                                   selected = extractTSNEMarkers(values)),
                easyClose = TRUE,
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton("tsne_ok_button", "Run"))                
            ))
    })
    
    #if run tsne button has been pushed
    observeEvent(input$tsne_ok_button, {
        removeUI(selector = "#ImposeColorSelector")
        #remember whether to arcsintransform
        values$tsne_arcsintransform <- input$tsne_arcsin
        #remember which markers were selected
        values$tSNE_markers <- input$tSNEMarkers
        removeModal()
        
        #get the values
        mat <- exprs(values$flowFrame[values$currentGatingPanel@indices,])
        if (values$tsne_arcsintransform){
            map <- values$markerMapping
            selected_markers <- names(map)[as.numeric(input$ArcSinhSelect)]
            for (mark in selected_markers){
                mat[,map[mark]] <- asinh(mat[,map[mark]]/input$arcsinh_par)
            }
        }
        
        #use only the markers that were selected
        mat <- mat[,values$markerMapping[as.numeric(values$tSNE_markers)]]

        #downsampling
        set.seed(123) 
        values$tsne_sample <- sample(1:nrow(mat), input$DownSample)
        mat <- mat[values$tsne_sample,]
        values$tsne_col <- 'black'
        values$currentGatingPanel@tsne <- Rtsne(mat, 
                                          max_iter = input$tSNE_iter,
                                          perplexity = input$tSNE_perplexity,
                                          theta = input$tSNE_theta,
                                          verbose = TRUE)
        insertUI(
            selector = "#AboveVisneSpace",
            where = "afterBegin",
            ui =  tags$div(id='ImposeColorSelector',
                           selectInput("select_tsne_impose", 
                                       label = "Select marker to impose",
                                       choices = c('',names(values$markerMapping)),
                                       selected = '')
            )
            
        )
    })
    
    #display the visne panel
    output$FirstVisnePanel <- renderPlot({
        if (is.null(values$currentGatingPanel) || length(values$currentGatingPanel@tsne)==0)
            return(NULL)
        plot(values$currentGatingPanel@tsne$Y,
             col=values$tsne_col,
             xlab='tSNE1',
             ylab='tSNE2',
             pch=20,
             cex=input$graphics_cex)
    },height = function() {
        min(700,session$clientData$output_FirstVisnePanel_width)
    })
    
    #superimpose the color
    observeEvent(input$select_tsne_impose, {
        values$tsne_current_marker <- input$select_tsne_impose
        
        if (values$tsne_current_marker==''){
            return(NULL)
        }
        
        #get the values of the selected marker
        map <- values$markerMapping
        mat <- data.frame(exprs(values$flowFrame[values$currentGatingPanel@indices,]))
        vals <- mat[,map[input$select_tsne_impose]]
        
        #do an arcsinh transform is the box is checked
        arcsinh_transformed <- names(map)[as.numeric(input$ArcSinhSelect)]
        if (input$select_tsne_impose %in% arcsinh_transformed){
            vals <- asinh(vals/input$arcsinh_par)
        }
        
        #reformat the colors
        pal <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
        values$tsne_col <- map2color(vals[values$tsne_sample],pal,limits=NULL)
        
    })
    
    #color the t-SNE plot accoring to the selected gate
    observeEvent(input$GateBrush, {
        if (is.null(values$currentGatingPanel) || length(values$currentGatingPanel@tsne)==0)
            return(NULL)
        
        #set the select box to zero because the coloring is done by the brush
        values$tsne_current_marker <- ''
        
        #there are 4 combinations: both arcsinh space, both normal space, one normal the other arcsinh and vice versa
        mat <- exprs(values$flowFrame[values$currentGatingPanel@indices,])
        if (values$tsne_arcsintransform){
            map <- values$markerMapping
            selected_markers <- names(map)[as.numeric(input$ArcSinhSelect)]
            for (mark in selected_markers){
                mat[,map[mark]] <- asinh(mat[,map[mark]]/input$arcsinh_par)
            }
        }
        
        #make sure the dots are transformed if the panel is transformed
        mat <- data.frame(exprs(values$flowFrame[values$currentGatingPanel@indices,]))
        mat[,values$markerMapping[input$Select_x_channels]] <- arcsinTransform(input$Select_x_channels, mat, values$markerMapping, input)
        mat[,values$markerMapping[input$Select_y_channels]] <- arcsinTransform(input$Select_y_channels, mat, values$markerMapping, input)
        
        #select based on the drawn window
        res <- brushedPoints(mat, 
                             xvar=values$markerMapping[input$Select_x_channels],
                             yvar=values$markerMapping[input$Select_y_channels],
                             input$GateBrush, 
                             allRows = TRUE)
        
        
        values$tsne_col <- rep('grey',input$DownSample)
        values$tsne_col[res$selected_[values$tsne_sample]] <- 'black'
    })
    
    #gating on the first panel
    observeEvent(input$GateButton, {
        mat <- data.frame(exprs(values$flowFrame[values$currentGatingPanel@indices,]))
        res <- brushedPoints(mat, 
                             xvar=values$markerMapping[input$Select_x_channels],
                             yvar=values$markerMapping[input$Select_y_channels],
                             input$GateBrush, 
                             allRows = TRUE)

        values$verbatimOutput <- dim(res)
    })
    
    
    output$GatingVerbose <- renderPrint({
        names(values$markerMapping)[as.numeric(input$ArcSinhSelect)]
        #adds another column 'selected_'
    })
}

shinyApp(server = server, ui = ui)