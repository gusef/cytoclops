#traverses the gating tree and creates a list
replace_gating_list <- function(input, values){
    removeUI(selector = "#RadioGates")
    
    #get the names of all gates
    gate_names <- sapply(values$gatingPanels,function(x)x@gate_name)
    names(gate_names) <- NULL
    gate_names <- paste0(gsub('[^_]','',names(values$gatingPanels)),gate_names)
    
    #insert the new gating list
    insertUI(
        selector = "#gatingList",
        where = "afterEnd",
        ui = radioButtons('RadioGates',
                          label = NULL,
                          choiceNames = gate_names,
                          choiceValues = names(values$gatingPanels),
                          selected = names(values$gatingPanels)[values$currentID])
    )
}

gating_modal <- function(input, values, session){
    if(sum(values$selectedPoints$selected_)>0){
        #Popup to enter the name
        showModal(modalDialog(
            title = "New gate",
            textInput("newGateName", label = h3("Enter name for new gate"), value = "New gate"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("gate_ok_button", "Gate selected"))                
        ))
    }else{
        createAlert(session, "alert", "NoPointsSelectedAlert", title = "Warning",
                    content = "No cells selected in the gating panel.", append = FALSE)
    }
}

press_gating_ok <- function(input, values){
    
    #Add a new gatingPanel object
    newGating <- GatingPanel()
    newGating@indices <- values$gatingPanels[[values$currentID]]@indices[values$selectedPoints$selected_]
    newGating@gate_name <- input$newGateName
    parent <- names(values$gatingPanels)[values$currentID]
    newGating@parent <- parent
    
    #Derive child name and set it in children of the parent 
    kids <- values$gatingPanels[[values$currentID]]@children
    index <- length(kids)+1
    kid_name <- paste(parent,index,sep='_')
    values$gatingPanels[[values$currentID]]@children <- c(kids,kid_name)
    
    #add the new element
    new_id <- values$currentID + 1
    if(new_id > length(values$gatingPanels)){
        #if it is inserted at the end of the list
        values$gatingPanels <- c(values$gatingPanels,newGating)
        
    }else{
        #if it is inserted in the middle of the list
        values$gatingPanels <- c(values$gatingPanels[1:(new_id-1)],
                                 newGating,
                                 values$gatingPanels[new_id:length(values$gatingPanels)])
    }
    names(values$gatingPanels)[new_id] <- kid_name
    #set the currentID to the new gate
    values$currentID <- new_id
    #remove the gating modal
    removeModal()
    #remove all tSNE related UI
    removeUI(selector = "#ImposeColorSelector")
    removeUI(selector = "#ShowAllMarkersButton") 
    #replace the old gating list on the side
    replace_gating_list(input, values)
    
}

radio_gate_click <- function(input, values){
    if (is.null(values$gatingPanels) || is.null(values$currentID))
        return(NULL)
    values$currentID <- grep(paste0('^',input$RadioGates,'$'),names(values$gatingPanels))
    
    #remove all the tSNE UI
    removeUI(selector = "#ImposeColorSelector") 
    removeUI(selector = "#ShowAllMarkersButton") 
    
    #if t-SNE was run however add the controls
    if (length(values$gatingPanels[[values$currentID]]@tsne)>0){
        add_tsne_controls(input, values)
    }
}

#save the current state
press_save_gating <- function(input, values){
    showModal(modalDialog(
        title = "Saving gates",
        textInput("newFileName", label = h3("Enter file name"), value = "New gating file"),
        easyClose = TRUE,
        footer = tagList(
            modalButton("Cancel"),
            actionButton("save_ok_button", "Save Gates"))                
    ))
}

#save the current state
press_ok_gating <- function(input, values){
    filename <- input$newFileName
    filename <- paste0(sub('\\.[Rr][Dd][Ss]','',filename),'.RDS')
    
    temp <- list(flowFrame = values$flowFrame,
                 gatingPanels=values$gatingPanels,
                 currentID=values$currentID,
                 channels=values$channels,
                 markerMapping=values$markerMapping,
                 selectedPoints=values$selectedPoints,
                 verbatimOutput=values$verbatimOutput,
                 tsne_current_marker=values$tsne_current_marker,
                 tsne_col=values$tsne_col,
                 tSNE_markers=values$tSNE_markers, 
                 tsne_arcsintransform=values$tsne_arcsintransform,
                 verbose=values$verbose)
    
    saveRDS(temp,file=file.path(getwd(),filename))
    removeModal()
}



