require(Rtsne)
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


#plots all possible marker overlays on the current tSNE map
plot_all_tsne_markers <- function(input, values){
    pal <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
    map <- values$markerMapping
    
    #figure out which markers should be arcsinh transformed
    arcsinh_transformed <- names(map)[as.numeric(input$ArcSinhSelect)]
    
    mat <- get_current_cells(values)
    
    #determine the number of plots
    nplots <- as.numeric(input$tSNE_overlay_num_columns) 
    op <- par(mfrow=c(ceiling(length(map)/nplots),nplots))
    for (idx in 1:length(map)){
        
        #get the values of the selected marker
        vals <- mat[,map[idx]]
        
        #do an arcsinh transform is the box is checked
        if (names(map)[idx] %in% arcsinh_transformed){
            vals <- asinh(vals/input$arcsinh_par)
        }

        #map the values to the colors
        cols <- map2color(vals[values$gatingPanels[[values$currentID]]@tsne_sample],pal,limits=NULL)
        
        #downsample the values
        tsne_vals <- values$gatingPanels[[values$currentID]]@tsne$Y
        
        df <- data.frame(x = round(tsne_vals[,1], digits = input$plot_downsample),
                         y = round(tsne_vals[,2], digits = input$plot_downsample))
        downsample <- !duplicated(df)
        
        #plot the tnse
        plot(tsne_vals[downsample,],
             col=cols[downsample],
             xlab='tSNE1',
             ylab='tSNE2',
             pch=20,
             cex=input$graphics_cex,
             main=names(map)[idx])
    }
}

map2color<-function(x,pal,limits=NULL){
    if(is.null(limits)) limits=range(x)
    pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

#adds all the tsne control inputs
add_tsne_controls <- function(input, values){
    shinyjs::show(id = "tSNEPanel", anim = TRUE)
    shinyjs::show(id = "SavetSNE")
    insertUI(
        selector = "#SavetSNE",
        where = "beforeBegin",
        ui = actionButton("ShowAllMarkersButton", "Show all"))
    insertUI(
        selector = "#ShowAllMarkersButton",
        where = "beforeBegin",
        ui =  tags$div(id='ImposeColorSelector',
                       fluidRow(
                           column(12,
                                  align='center',
                                  selectInput("select_tsne_impose", 
                                              label = "Select marker to impose",
                                              choices = c('',names(values$markerMapping)),
                                              selected = ''))),
                       bsModal("tsne_modal", 
                               "t-SNE overlays", 
                               "ShowAllMarkersButton", 
                               size = "large",
                               fluidRow(
                                   column(12,
                                          align="center",
                                          plotOutput("tSNE_overview",
                                                     width= paste0(input$tSNE_overlay_width,'px'),
                                                     height = paste0(ceiling(as.numeric(input$tSNE_overlay_width)*
                                                                                 length(values$markerMapping)/
                                                                                 as.numeric(input$tSNE_overlay_num_columns)^2),'px')
                                          )))))
        
    )

} 

show_tsne_modal <- function(input, values){
    showModal(modalDialog(
        size='l',
        title = "Running t-SNE",
        checkboxInput("tsne_arcsin", 
                      label='Run t-SNE using arcsinh transformed values', 
                      value = TRUE),
        tags$div(class = "multicol", 
            checkboxInput('all_none_tnseselect', 'All/None'),
            checkboxGroupInput("tSNEMarkers", label = NULL, 
                               choiceNames = as.list(names(values$markerMapping)),
                               choiceValues = as.list(1:length(values$markerMapping)),
                               selected = extractTSNEMarkers(values))),
            
        easyClose = TRUE,
        footer = tagList(
            modalButton("Cancel"),
            actionButton("tsne_ok_button", "Run"))                
    ))
}

tsne_ok_button_pressed <- function(input, values){
    removeUI(selector = "#ImposeColorSelector")
    removeUI(selector = "#ShowAllMarkersButton") 
    #remember whether to arcsintransform
    values$tsne_arcsintransform <- input$tsne_arcsin
    #remember which markers were selected
    values$tSNE_markers <- input$tSNEMarkers
    removeModal()
    
    #get the values
    mat <- get_current_cells(values)
    
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
    if (input$DownSample < nrow(mat)){
        values$gatingPanels[[values$currentID]]@tsne_sample <- sample(1:nrow(mat), input$DownSample)
    }else{
        values$gatingPanels[[values$currentID]]@tsne_sample <- 1:nrow(mat)
    }
    mat <- mat[values$gatingPanels[[values$currentID]]@tsne_sample,]
    values$tsne_col <- 'black'
    
    
    withProgress(message = 'Running t-SNE ... ',
                 detail = 'This may take a while...',
                 value = 0.5, {
    #capture output
    values$verbose <- NULL
    values$verbose <- capture.output(tsne <- Rtsne(mat, 
                                                   max_iter = input$tSNE_iter,
                                                   perplexity = input$tSNE_perplexity,
                                                   theta = input$tSNE_theta,
                                                   verbose = TRUE))
    })
    
    values$gatingPanels[[values$currentID]]@tsne <- tsne
    #add all of the control elements
    add_tsne_controls(input, values)
}

plot_tsne_panel <- function(input, values){
    if (is.null(values$gatingPanels) || 
        is.null(values$currentID) || 
        length(values$gatingPanels[[values$currentID]]@tsne)==0){
        return(NULL)
    }
    
    #downsample the values in the plot
    tsne_vals <- values$gatingPanels[[values$currentID]]@tsne$Y
    df <- data.frame(x = round(tsne_vals[,1], digits = input$plot_downsample),
                     y = round(tsne_vals[,2], digits = input$plot_downsample))
    downsample <- !duplicated(df)
    
    #fix the color (downsample)
    if (length(values$tsne_col)>1){
        cols <- values$tsne_col[downsample]
    }else{
        cols <- values$tsne_col
    }
    
    #plot the tSNE
    plot(tsne_vals[downsample,],
         col=cols,
         xlab='tSNE1',
         ylab='tSNE2',
         pch=20,
         cex=input$graphics_cex)
}

tsne_impose_marker <- function(input, values){
    values$tsne_current_marker <- input$select_tsne_impose
    
    if (values$tsne_current_marker==''){
        return(NULL)
    }
    
    #get the values of the selected marker
    map <- values$markerMapping
    mat <- get_current_cells(values)
    vals <- mat[,map[input$select_tsne_impose]]
    
    #do an arcsinh transform is the box is checked
    arcsinh_transformed <- names(map)[as.numeric(input$ArcSinhSelect)]
    if (input$select_tsne_impose %in% arcsinh_transformed){
        vals <- asinh(vals/input$arcsinh_par)
    }
    
    #reformat the colors
    pal <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
    values$tsne_col <- map2color(vals[values$gatingPanels[[values$currentID]]@tsne_sample],pal,limits=NULL)
    
}
