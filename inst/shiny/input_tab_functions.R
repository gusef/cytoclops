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
    insertGatingPanels(values)
    replace_gating_list(input,values,session)
}
