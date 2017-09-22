

getDropdowns <- function(housekeeper, values){
    vals <- values$markerMapping
    names(vals) <- NULL
    selectInput(paste0("Housekeeper_", housekeeper), 
                label = housekeeper,
                choices = as.list(names(values$markerMapping)),
                selected = names(values$markerMapping)[values$markerMapping == housekeeper])
}


init_with_markers <- function(values){
    
    classifier <- readRDS(system.file("extdata", "Viability_classifier.RDS", package="cytoclops"))
    housekeepers <- rownames(classifier$importance)
    
    #insert the housekeeping_selectors
    insertUI(
         selector = "#ViabilityClassifier",
         where = "beforeBegin",
         ui =   tags$div(h4('Confirm selected channels for cleanup'),
                         tags$div(class = "doublecol",
                         lapply(housekeepers,getDropdowns,values)))  
    )
    
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
    
    shinyjs::show(id = "GatingPanel", anim = TRUE)
    insertUI(
        selector = "#GatingPanel",
        where = "afterEnd",
        ui =  tags$div(id='BelowGatingPanelInterface',
                       actionButton("DeleteGateButton", "Delete Gate"),
                       actionButton("GateButton", "Gate cells"),
                       actionButton("RunTSNE", "Run TSNE")
        )
    )
    #insert the markers into the marker panel
    insertUI(
        selector = "#CurrentPanelTab",
        where = "afterEnd",
        ui =  tags$div(id='MarkerPanelInterface',
                       tags$div(class = "multicol",
                                checkboxGroupInput("ArcSinhSelect", label = NULL, 
                                                   choiceNames = as.list(names(values$markerMapping)),
                                                   choiceValues = as.list(1:length(values$markerMapping)),
                                                   selected = extractArcsinhTemp(values)))  
        )
    )
    
    
}


#figure out which position the flowframe had in the flowset
get_filenames_from_flowSet <- function(set){
    fset_names <- NULL
    for(i in 1:length(set)){
        fset_names <- c(fset_names, set[[i]]@description$FILENAME)
    }
    return(fset_names)
}

load_file <- function(input, values,session){
    #if this is the first file instantiate the panels and open up the file table
    if (is.null(values$file_table)){
        instantiate_panels(input, values, input$fcsFile$datapath, session)

    #if this is not the first file check if the new file has the same format
    } else {
        new_fcs <- read.FCS(input$fcsFile$datapath)
        new_markers <- extractMarkerPanel(new_fcs)
        
        #check if the file was already loaded and if it is skip
        fset_names <- get_filenames_from_flowSet(values$flowset)
        if (input$fcsFile$name %in% fset_names){
            showModal(modalDialog(
                title = "Error",
                "This file was already loaded, cannot process duplicates."
            ))
        #if the markers don't match skip the file
        }else if (any(new_markers != values$markerMapping) ||
            any(names(new_markers) != names(values$markerMapping))){
            showModal(modalDialog(
                title = "Error",
                "The markers in the new file do not match the rest of the files."
            ))
            #otherwise add it to the table
        } else {
            add_additional_flowset(new_fcs, input, values,session)
        }
    }
}

instantiate_panels <- function(input, values, file, session){
    
    #read the raw values
    values$flowset <- flowSet(read.FCS(file))
    #set the real filename
    values$flowset[[1]]@description$FILENAME <- input$fcsFile$name
    
    values$currentID <- 1
    values$markerMapping <- extractMarkerPanel(values$flowset[[1]])
    
    #generate the underlying object that holds all information for the gates
    newGatingPanel <- GatingPanel()
    newGatingPanel@gate_name <- sub('.fcs$','',input$fcsFile$name)
    newGatingPanel@filename <- input$fcsFile$name
    newGatingPanel@indices <- 1:nrow(values$flowset[[1]])
    values$gatingPanels <- list()
    values$gatingPanels[[1]] <- newGatingPanel 
    names(values$gatingPanels) <- 'G1'
    
    #generate the file table that displays all the files
    values$file_table <- data.frame(
        Filename = input$fcsFile$name,
        Cell_number = nrow(values$flowset[[1]]),
        Remove = as.character(actionButton(inputId = 'removeFile_1',
                                           label = '',
                                           icon = icon("remove",lib = "glyphicon"),
                                           onclick = 'Shiny.onInputChange(\"remove_fcs_file\",  this.id)' )),
        stringsAsFactors = FALSE,
        row.names = 1
    )    
    
    #add new UI elements
    init_with_markers(values)
    replace_gating_list(input,values,session)
}

add_additional_flowset <- function(new_fcs, input, values, session){
    
    #the real filename
    new_fcs@description$FILENAME <- input$fcsFile$name
    
    #add the new sample to the flowset
    values$flowset <- rbind2(values$flowset, new_fcs)
    
    #generate the underlying object that holds all information for the gates
    newGatingPanel <- GatingPanel()
    newGatingPanel@gate_name <- sub('.fcs$','',input$fcsFile$name)
    newGatingPanel@filename <- input$fcsFile$name
    newGatingPanel@indices <- 1:nrow(new_fcs)
    nams <- names(values$gatingPanels)
    
    #add new panel
    values$gatingPanels[[length(values$gatingPanels) + 1]] <- newGatingPanel 
    
    #figure out the current gating index / corresponding to the table index
    max_index <- max(as.numeric(rownames(values$file_table))) + 1
    sample_index <- paste0('G',max_index)
    
    #figure out the name for the new sample
    exclude <- grep('_',nams)
    if (length(exclude) > 0){
        nams <- nams[-exclude]
    }
    names(values$gatingPanels)[length(values$gatingPanels)] <- sample_index
    
    #add the new file information to the file table
    values$file_table <- rbind(
        values$file_table,
        data.frame(
            Filename = input$fcsFile$name,
            Cell_number = nrow(new_fcs),
            Remove = as.character(actionButton(inputId = paste0('removeFile_',max_index),
                                               label = '',
                                               icon = icon("remove",lib = "glyphicon"),
                                               onclick = 'Shiny.onInputChange(\"remove_fcs_file\",  this.id)' )),
            stringsAsFactors = FALSE,
            row.names = max_index
        ))    
    gc()
    
    #replace the tree
    replace_gating_list(input,values,session)
}


remove_fcs_file <- function(input,values,session){

    #if the last sample was removed
    if (length(values$flowset) > 1){
        #figure out what row was selected
        selectedRow <- as.numeric(strsplit(input$remove_fcs_file, "_")[[1]][2])
        
        #figure out what position this was in the table (even after deletes)
        index <- match(selectedRow,rownames(values$file_table))
        
        #figure out which position the flowframe had in the flowset
        fset_names <- get_filenames_from_flowSet(values$flowset)
    
        #reset the current index and update the gate
        values$currentID <- 1
        update_current_gate(session, input, values)
        
        #remove the associated flowframe
        values$flowset <- values$flowset[-match(values$file_table$Filename[index],fset_names)]
        
        #remove all gating panel objects associated with that sample
        values$gatingPanels <- values$gatingPanels[-grep(paste0('G',index,'$'),names(values$gatingPanels))]
        
        #remove the table entry
        values$file_table <- values$file_table[-index,]
        
        #reset the jstree
        replace_gating_list(input,values,session)
        
    #if the last sample was removed
    }else{
        #clean up
        removeUI(selector = "#AboveGatingPanelInterface")
        removeUI(selector = "#BelowGatingPanelInterface")
        removeUI(selector = "#MarkerPanelInterface")
        removeUI(selector = "#ImposeColorSelector") 
        
        values$flowset <- NULL
        values$gatingPanels <- NULL
        values$file_table <- NULL
        values$markerMapping <- NULL
    }
}

#Load a prior state
loadGating <- function(input, values, file, session){
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
    init_with_markers(values)
    replace_gating_list(input,values,session)
}


