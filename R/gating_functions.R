#traverses the gating tree and creates a list
replace_gating_list <- function(input, values, session){
    #get the names of all gates
    gate_names <- sapply(values$gatingPanels,function(x)x@gate_name)
    names(gate_names) <- NULL
    gate_names <- paste0(gsub('[^_]','',names(values$gatingPanels)),gate_names)
    dat1 <- getDataTable(gate_names,names(values$gatingPanels))
    #print(dat1)
    updateTreeTableInput(session,'TreeGates',dat1,selected='G1')
}

gating_modal <- function(input, values, session){
    if((!is.null(values$scaled_points) && 
        length(values$scaled_points$x) > 2) || 
        sum(values$selectedPoints$selected_)>0){
         
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

# From a vector of names and values (which are essentially IDs) return a suitable object for treeTable
getDataTable <- function(choiceNames,choiceValues) {
  data <- list()
  for (i in 1:length(choiceNames)) {
    parent <- NULL
    v <- unlist(strsplit(unlist(choiceValues[i]),'_'))
    if(length(v)>1) {
      parent <- paste(v[1:length(v)-1],collapse='_')
      entry <- list(id=choiceValues[i],text=choiceNames[i],parent=parent,value=choiceValues[i])
    } else {
      entry <- list(id=choiceValues[i],text=choiceNames[i],value=choiceValues[i])
    }
    data[[i]] <- entry
  }
  return(data)
}

press_gating_ok <- function(input, values, session){
    #if in polygon mode get the selected from the polygon else from brush
    if(!is.null(values$scaled_points)){
        selected <- get_polygon_selected(input, values)
    }else{
        selected <- values$selectedPoints$selected_
    }
    #Add a new gatingPanel object
    newGating <- GatingPanel()
    newGating@indices <- values$gatingPanels[[values$currentID]]@indices[selected]
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
    shinyjs::hide(id = "tSNEPanel", anim = TRUE)
    #replace the old gating list on the side
    replace_gating_list(input, values, session)
    
}

tree_gate_click <- function(input, values){
  if (is.null(values$gatingPanels) || is.null(values$currentID))
    return(NULL)
  values$currentID <- grep(paste0('^',input$TreeGates$value,'$'),names(values$gatingPanels))
  print(values$currentID)
  #remove all the tSNE UI
  removeUI(selector = "#ImposeColorSelector") 
  removeUI(selector = "#ShowAllMarkersButton") 
  
  #if t-SNE was run however add the controls
  if (length(values$gatingPanels[[values$currentID]]@tsne)>0){
    add_tsne_controls(input, values)
    shinyjs::show(id = "tSNEPanel", anim = TRUE)
  }else{
    shinyjs::hide(id = "tSNEPanel", anim = TRUE)
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
###################################################
#TODO: this needs to be updated with the latest ver
######################################################
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



