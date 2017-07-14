require(flowCore)
GatingPanel <- setClass("GatingPanel",
                        slots = c(indices = "numeric",
                                  gate_name = "character",
                                  children = "character",
                                  parent = "character",
                                  gates= "list",
                                  tsne = "list",
                                  tsne_sample="numeric"))



insertGatingPanels <- function(values){
    #insert the gating and visne panel + additional buttons and inputs
    insertUI(
        selector = "#PolygonButton",
        where = "beforeBegin",
        ui =  tags$div(id='AboveGatingPanelInterface',
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
                       )))
    insertUI(
        selector = "#GatingPanel",
        where = "afterEnd",
        ui =  tags$div(id='BelowGatingPanelInterface',
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

instantiate_panels <- function(input, values, file){
    #clean up
    removeUI(selector = "#AboveGatingPanelInterface")
    removeUI(selector = "#BelowGatingPanelInterface")
    removeUI(selector = "#MarkerPanelInterface")
    removeUI(selector = "#ImposeColorSelector") 
    values$verbatimOutput <- ''
    
    #read the raw values
    values$flowFrame <- read.FCS(file)
    values$currentID <- 1
    values$markerMapping <- extractMarkerPanel(input, values)
    
    #generate the underlying object that holds all information for the gates
    newGatingPanel <- GatingPanel()
    newGatingPanel@gate_name <- 'All'
    newGatingPanel@indices <- 1:nrow(values$flowFrame)
    
    #generate the object that holds all gating panels
    values$gatingPanels <- list()
    values$gatingPanels[[1]] <- newGatingPanel 
    names(values$gatingPanels) <- 'G1'
    
    #add new UI elements
    insertGatingPanels(values)
    replace_gating_list(input,values)
}

loadGating <- function(input, values, file){
    #clean up
    removeUI(selector = "#AboveGatingPanelInterface")
    removeUI(selector = "#BelowGatingPanelInterface")
    removeUI(selector = "#MarkerPanelInterface")
    removeUI(selector = "#ImposeColorSelector") 
    removeUI(selector = "#ShowAllMarkersButton") 
    values$verbatimOutput <- ''
    
    #read the raw values
    temp <- readRDS(file)
    
    for (idx in names(temp)){
        values[[idx]] <- temp[[idx]]
    }

    #add new UI elements
    insertGatingPanels(values)
    replace_gating_list(input,values)
}


plot_gates <- function(input, values){
    if (is.null(values$gatingPanels) || is.null(values$currentID) || is.null(input$Select_x_channels))
        return(NULL)
    mat <- exprs(values$flowFrame[values$gatingPanels[[values$currentID]]@indices,])
    xVal <- arcsinTransform(input$Select_x_channels, mat, values$markerMapping, input)
    yVal <- arcsinTransform(input$Select_y_channels, mat, values$markerMapping, input)
    
    par(mar=c(4.1,4.1,1.1,1.1))
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
                      colramp = colorRampPalette(c("#FFFFFF","#000099", 
                                                   "#00FEFF", "#45FE4F",
                                                   "#FCFF00", "#FF9400", 
                                                   "#FF3100")),
                      pch = ".")

    }else{
        plot(xVal,
             yVal,
             xlab=input$Select_x_channels,
             ylab=input$Select_y_channels,
             pch=20,
             cex=input$graphics_cex)    
    }
}


get_current_tsne <- function(input, values){
    #there are 4 combinations: both arcsinh space, both normal space, one normal the other arcsinh and vice versa
    mat <- exprs(values$flowFrame[values$gatingPanels[[values$currentID]]@indices,])
    if (values$tsne_arcsintransform){
        map <- values$markerMapping
        selected_markers <- names(map)[as.numeric(input$ArcSinhSelect)]
        for (mark in selected_markers){
            mat[,map[mark]] <- asinh(mat[,map[mark]]/input$arcsinh_par)
        }
    }
    
    #make sure the dots are transformed if the panel is transformed
    mat <- data.frame(exprs(values$flowFrame[values$gatingPanels[[values$currentID]]@indices,]))
    mat[,values$markerMapping[input$Select_x_channels]] <- arcsinTransform(input$Select_x_channels, mat, values$markerMapping, input)
    mat[,values$markerMapping[input$Select_y_channels]] <- arcsinTransform(input$Select_y_channels, mat, values$markerMapping, input)
    return(mat)
}

gate_brush <- function(input, values){
    
    if (is.null(values$gatingPanels))
        return(NULL)
    
    #if the polygon drawing is active don't allow regular brushing
    if (!is.null(values$scaled_points))
        return(NULL)
    
    #set the select box to zero because the coloring is done by the brush
    values$tsne_current_marker <- ''
    
    #get current set of markers
    mat <- get_current_tsne(input, values)
    
    #select based on the drawn window
    values$selectedPoints <- brushedPoints(mat, 
                                           xvar=values$markerMapping[input$Select_x_channels],
                                           yvar=values$markerMapping[input$Select_y_channels],
                                           input$GateBrush, 
                                           allRows = TRUE)
    if (length(values$gatingPanels[[values$currentID]]@tsne)!=0){
        values$tsne_col <- rep('grey',input$DownSample)
        values$tsne_col[values$selectedPoints$selected_[values$gatingPanels[[values$currentID]]@tsne_sample]] <- 'black'
    }
}

