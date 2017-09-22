#traverses the gating tree and creates a list
replace_gating_list <-
    function(input, values, session , selected = NULL) {
        #get the names of all gates
        gate_names <- sapply(values$gatingPanels, function(x)
            x@gate_name)
        names(gate_names) <- NULL
        dat <- getDataTable(gate_names, names(values$gatingPanels))
        
        #if there is no selection, just use the very first gate
        if (is.null(selected)) {
            selected = dat[[1]]$id
        }
        
        updatejsTreeSelectorInput(session, 'TreeGates', dat, selected = selected)
    }

##############################################################################################
# Helperfunction to get proper names
getDataTable <- function(choiceNames, choiceValues) {
    data <- list()
    for (i in 1:length(choiceNames)) {
        parent <- NULL
        v <- unlist(strsplit(unlist(choiceValues[i]), '_'))
        if (length(v) > 1) {
            parent <- paste(v[1:length(v) - 1], collapse = '_')
            entry <-
                list(
                    id = choiceValues[i],
                    text = choiceNames[i],
                    parent = parent,
                    value = choiceValues[i]
                )
        } else {
            entry <-
                list(id = choiceValues[i],
                     text = choiceNames[i],
                     value = choiceValues[i])
        }
        data[[i]] <- entry
    }
    return(data)
}

tree_gate_click <- function(session, input, values) {
    if (is.null(values$gatingPanels) || is.null(values$currentID))
        return(NULL)
    values$currentID <-
        grep(paste0('^', input$TreeGates$value, '$'),
             names(values$gatingPanels))
    update_current_gate(session, input, values)
}


update_current_gate <- function(session, input, values) {
    #update the child table
    update_children_table(session, input, values)
    
    #set the current child to NULL
    values$current_child <- NULL
    
    #remove all the tSNE UI
    removeUI(selector = "#ImposeColorSelector")
    removeUI(selector = "#ShowAllMarkersButton")
    
    #if t-SNE was run however add the controls
    if (length(values$gatingPanels[[values$currentID]]@tsne) > 0) {
        add_tsne_controls(input, values)
        shinyjs::show(id = "tSNEPanel", anim = TRUE)
    } else{
        shinyjs::hide(id = "tSNEPanel", anim = TRUE)
    }
}

#################################################################################
#delete a gate
delete_gate_modal <- function(input, values, session) {
    showModal(
        modalDialog(
            title = "Delete current gate",
            "Are you sure you would like to delete the current gate? This cannot be undone.",
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("delete_gate_button", "Delete gate")
            )
        )
    )
}

#remove a single child based on the ID
remove_child <-
    function(input,
             values,
             session,
             parent_idx,
             childName) {
        #remove child from the parent
        child_index <-
            match(childName, names(values$gatingPanels[[parent_idx]]@children))
        values$gatingPanels[[parent_idx]]@children <-
            values$gatingPanels[[parent_idx]]@children[-child_index]
        
        #delete all children and own gate
        values$gatingPanels <-
            values$gatingPanels[-grep(childName, names(values$gatingPanels))]
        
        #replace the old gating list on the side
        replace_gating_list(input, values, session, selected = names(values$gatingPanels)[parent_idx])
    }


press_delete_gating_ok <- function(input, values, session) {
    #remove the gate remove modal
    removeModal()
    #check if this is the root
    if (length(grep('_', names(values$gatingPanels)[values$currentID])) == 0) {
        showModal(modalDialog(title = "Error",
                              "Cannot delete whole sample."))
        #if its not delete the gate and all children
    } else {
        #get current gate and name
        currentPanel <- values$gatingPanels[[values$currentID]]
        childName <- names(values$gatingPanels)[values$currentID]
        
        #get the parent ID
        parent_idx <-
            match(currentPanel@parent, names(values$gatingPanels))
        
        #remove the child
        remove_child(input, values, session, parent_idx, childName)
        
        #set the current index to the parent gate
        values$currentID <- parent_idx
        
        #update the gating panel
        update_current_gate(session, input, values)
        
    }
}


#################################################################################
#gate the currently selected population
gating_modal <- function(input, values, session) {
    if ((!is.null(values$scaled_points) &&
         length(values$scaled_points$x) > 2) ||
        sum(values$selectedPoints$selected_) > 0) {
        #Popup to enter the name
        showModal(modalDialog(
            title = "New gate",
            textInput(
                "newGateName",
                label = h5("Enter name for new gate"),
                value = "New gate"
            ),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("gate_ok_button", "Gate selected")
            )
        ))
    } else{
        createAlert(
            session,
            "alert",
            "NoPointsSelectedAlert",
            title = "Warning",
            content = "No cells selected in the gating panel.",
            append = FALSE
        )
    }
}

press_gating_ok <- function(input, values, session) {
    #if in polygon mode get the selected from the polygon else from brush
    if (!is.null(values$scaled_points)) {
        selected <- get_polygon_selected(input, values)
    } else{
        selected <- values$selectedPoints$selected_
    }
    
    #add a new child to the gating panel
    add_new_child(input, 
                  values, 
                  session,
                  indices = values$gatingPanels[[values$currentID]]@indices[selected],
                  name = input$newGateName)
    
    #remove the gating modal
    removeModal()
    #remove all tSNE related UI
    removeUI(selector = "#ImposeColorSelector")
    removeUI(selector = "#ShowAllMarkersButton")
    shinyjs::hide(id = "tSNEPanel", anim = TRUE)
    shinyjs::hide("SavetSNE")
    #replace the old gating list on the side
    replace_gating_list(input, values, session, selected = names(values$gatingPanels)[values$currentID])
    
}

add_new_child <- function(input, values, session, 
                          indices, name, 
                          xmarker = input$Select_x_channels,
                          ymarker = input$Select_y_channels){
    
    #Add a new gatingPanel object
    newGating <- GatingPanel()
    newGating@indices <- indices
    newGating@gate_name <- name
    newGating@filename <- values$gatingPanels[[values$currentID]]@filename
    parent <- names(values$gatingPanels)[values$currentID]
    newGating@parent <- parent
    kid_name <- setNewChild(input, values, parent, name, xmarker, ymarker)
    
    #add the new element
    new_id <- values$currentID + 1
    if (new_id > length(values$gatingPanels)) {
        #if it is inserted at the end of the list
        values$gatingPanels <- c(values$gatingPanels, newGating)
    } else{
        #if it is inserted in the middle of the list
        values$gatingPanels <- c(values$gatingPanels[1:(new_id - 1)],
                                 newGating,
                                 values$gatingPanels[new_id:length(values$gatingPanels)])
    }
    names(values$gatingPanels)[new_id] <- kid_name
    #set the currentID to the new gate
    values$currentID <- new_id
}

#################################################################################
#Children table function that allow to view, visualize, delete and edit gates

setNewChild <- function(input, values, parent, name, xmarker, ymarker) {
    #Derive child name and set it in children of the parent
    kids <- values$gatingPanels[[values$currentID]]@children
    index <- length(kids) + 1
    
    #generate new identifier
    if (index == 1) {
        kid_name <- paste(parent, 1, sep = '_')
    } else{
        #if there are already children just increment the last one - otherwise this might lead to duplicates
        kid_name <-
            paste(parent, as.numeric(sub('.+_', '', kids[[index - 1]]$id)) + 1, sep =
                      '_')
    }
    
    #capture all relevant information for the child
    child <- list()
    child$name <- name
    child$id <- kid_name
    child$xmarker <- xmarker
    child$ymarker <- ymarker
    
    #get the points either from the polygon
    if (!is.null(values$scaled_points)) {
        child$points <- data.frame(x = values$scaled_points$x,
                                   y = values$scaled_points$y)
        #or from the brush
    } else{
        br <- input$GateBrush
        child$points <-
            data.frame(
                x = c(br$xmin, br$xmin, br$xmax, br$xmax),
                y = c(br$ymin, br$ymax, br$ymax, br$ymin)
            )
    }
    values$gatingPanels[[values$currentID]]@children[[index]] <-
        child
    #give the children IDs so we can easily select them afterwards
    names(values$gatingPanels[[values$currentID]]@children) <-
        sapply(values$gatingPanels[[values$currentID]]@children, function(x)
            x$id)
    return(kid_name)
}

#helperfunction to make the action buttons in the child table
shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
}

update_children_table <- function(session, input, values) {
    #get the current gating object
    current <- values$gatingPanels[[values$currentID]]
    
    #get the number of children
    num_child <- length(current@children)
    
    #If there are children show the children table
    if (num_child > 0) {
        #calculate the percentages of the children in comparison to the parents
        current_cells <- length(current@indices)
        children_cells <-
            sapply(current@children, function(x)
                length(values$gatingPanels[[x$id]]@indices))
        percentage <-
            paste0(format(children_cells / current_cells * 100, digits = 3),
                   '%')
        
        #rebuild the children table
        #there is a random number included in the ID because Shiny.onInputChange
        #optimizes in a way so that it doens't allow to have that button pressed twice
        rand <- sample(1:1000, 1)
        values$children_table <-
            data.frame(
                Gatename = sapply(current@children, function(x)
                    x$name),
                Size = percentage,
                Jump_to = shinyInput(
                    actionButton,
                    num_child,
                    paste0('childjumpto', rand, '_'),
                    label = '',
                    icon = icon("share-alt", lib = "glyphicon"),
                    onclick = 'Shiny.onInputChange(\"childJumpTo\",  this.id)'
                ),
                Show = shinyInput(
                    actionButton,
                    num_child,
                    paste0('childshow', rand, '_'),
                    label = '',
                    icon = icon("eye-open", lib = "glyphicon"),
                    onclick = 'Shiny.onInputChange(\"childShow\",  this.id)'
                ),
                Remove = shinyInput(
                    actionButton,
                    num_child,
                    paste0('childremove', rand, '_'),
                    label = '',
                    icon = icon("remove", lib = "glyphicon"),
                    onclick = 'Shiny.onInputChange(\"childRemove\",  this.id)'
                ),
                Edit = shinyInput(
                    actionButton,
                    num_child,
                    paste0('childedit', rand, '_'),
                    label = '',
                    icon = icon("pencil", lib = "glyphicon"),
                    onclick = 'Shiny.onInputChange(\"childEdit\",  this.id)'
                ),
                stringsAsFactors = FALSE,
                row.names = sapply(current@children, function(x)
                    x$id)
            )
        
        #if the current gate does not have children there is nothing to display
    } else{
        values$children_table <- NULL
    }
}

#sets the markers to the current child
jumpToChild <- function(session, input, values) {
    #figure out what child was clicked
    selected_idx <-
        as.numeric(strsplit(input$childJumpTo, "_")[[1]][2])
    child_idx <- rownames(values$children_table)[selected_idx]
    child <-
        values$gatingPanels[[values$currentID]]@children[[child_idx]]
    
    #jump to current coordinates
    updateSelectInput(session, 'Select_x_channels',
                      selected = child$xmarker)
    updateSelectInput(session, 'Select_y_channels',
                      selected = child$ymarker)
    
    #this is a bit hacky, but Shiny.onInputChange allows to press each button
    #only once. So I just redraw the table every time the button was pressed.
    update_children_table(session, input, values)
}

show_child <- function(session, input, values) {
    #switch to the regular scatter plot
    updateSelectInput(session, 'PlotType',
                      selected = "regular")
    
    #change the current child which will change the current plot
    selected_idx <-
        as.numeric(strsplit(input$childShow, "_")[[1]][2])
    values$current_child <-
        rownames(values$children_table)[selected_idx]

    #this is a bit hacky, but Shiny.onInputChange allows to press each button
    #only once. So I just redraw the table every time the button was pressed.
    update_children_table(session, input, values)
}

remove_table_child <- function(session, input, values) {
    values$current_child <- NULL
    selected_idx <-
        as.numeric(strsplit(input$childRemove, "_")[[1]][2])
    child_idx <- rownames(values$children_table)[selected_idx]
    remove_child(input, values, session, values$currentID, child_idx)
    
    #this is a bit hacky, but Shiny.onInputChange allows to press each button
    #only once. So I just redraw the table every time the button was pressed.
    update_children_table(session, input, values)
    
}


#################################################################################
#save the current state
press_save_gating <- function(input, values) {
    showModal(modalDialog(
        title = "Saving gates",
        textInput(
            "newFileName",
            label = h5("Enter file name"),
            value = "New gating file"
        ),
        easyClose = TRUE,
        footer = tagList(
            modalButton("Cancel"),
            actionButton("save_ok_button", "Save Gates")
        )
    ))
}
