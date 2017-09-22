rm(list = ls())
gc()

server <- function(input, output, session) {
    options(shiny.maxRequestSize = 500 * 1024 ^ 2)
    
    #reactive values
    values <- reactiveValues(
        file_table = NULL,
        flowset = NULL,
        gatingPanels = NULL,
        currentID = NULL,
        channels = NULL,
        children_table = NULL,
        current_child = NULL,
        markerMapping = NULL,
        selectedPoints = NULL,
        verbatimOutput = '',
        tsne_current_marker = '',
        tsne_col = '',
        tSNE_markers = NULL,
        tsne_arcsintransform = TRUE,
        verbose = '',
        scaled_points = NULL
    )
    
    #file loading
    observeEvent(input$fcsFile, {
        load_file(input, values, session)
    })
    
    output$file_table <- DT::renderDataTable(
        values$file_table,
        server = TRUE,
        escape = FALSE,
        selection = 'none',
        options = list(
            iDisplayLength = nrow(values$file_table),
            bLengthChange = 0,
            bFilter = 0,
            bInfo = 0,
            bPaginate = 0
        )
    )
    
    #remove a single file from the fcs file table
    observeEvent(input$remove_fcs_file, {
        remove_fcs_file(input, values, session)
    })
    
    #file loading
    observeEvent(input$rdsFile, {
        loadGating(input, values, input$rdsFile$datapath, session)
    })
    

    #############################################################################
    #Cleanup and QC panel
    #############################################################################
    
    observeEvent(input$ViabilityButton, {
        run_viability(input, values, session)
    })
    
    #############################################################################
    #Display of the Gating tree
    #############################################################################
    
    #set the currentID to the newly selected gate
    observeEvent(input$TreeGates, {
        tree_gate_click(session, input, values)
    })
    
    #############################################################################
    #Save the current state
    #############################################################################
    
    #save the current state
    output$SaveStateButton <- downloadHandler(
        filename = function() {
            paste("Gatings_", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
            state <- reactiveValuesToList(values)
            saveRDS(state, file = file)
        },
        contentType = 'rds'
    )
    
    #############################################################################
    #Gating panel
    #############################################################################
    
    #simple gating panel
    output$GatingPanel <- renderPlot({
        plot_gates(input, values)
    }, height = function() {
        min(700, session$clientData$output_GatingPanel_width)
    })
    
    #update the polygon points is a new batch is drawn
    observeEvent(input$PolygonPoints, {
        update_polygon_points(input, values)
    })
    
    observeEvent(input$PolygonViewButton, {
        update_tsne_by_polygon(input, values)
    })
    
    observeEvent(input$PolygonButton, {
        js$initPolyDraw(id = "GatingPanel")
    })
    
    observeEvent(input$PolygonResetButton, {
        values$scaled_points <- NULL
        js$resetPolyDraw(id = "GatingPanel")
        
    })
    
    #saves a pdf of the current gate
    output$SaveGate <- downloadHandler(
        filename = function() {
            paste("Gating_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
            pdf(file)
            plot_gates(input, values)
            dev.off()
        },
        contentType = 'pdf'
    )
    
    #Pressing the delete gate button
    observeEvent(input$DeleteGateButton, {
        delete_gate_modal(input, values, session)
    })
    
    #Pressing the OK button in the delete gate modal
    observeEvent(input$delete_gate_button, {
        press_delete_gating_ok(input, values, session)
    })
    
    #Pressing the gating button
    observeEvent(input$GateButton, {
        gating_modal(input, values, session)
    })
    
    #Pressing the OK button in the gating modal
    observeEvent(input$gate_ok_button, {
        press_gating_ok(input, values, session)
    })
    
    #table that shows all the children for a gate
    output$children_table <- DT::renderDataTable(
        values$children_table,
        server = FALSE,
        escape = FALSE,
        selection = 'single',
        options = list(
            iDisplayLength = nrow(values$children_table),
            bLengthChange = 0,
            bFilter = 0,
            bInfo = 0,
            bPaginate = 0,
            ordering = FALSE
        ),
        rownames = FALSE
    )
    
    #Jump to child button pressed
    observeEvent(input$childJumpTo, {
        jumpToChild(session, input, values)
    })
        
    #show the gated population
    observeEvent(input$childShow, {
        show_child(session, input, values) 
    })
    
    observeEvent(input$childRemove, {
        remove_table_child(session, input, values)
    })
    
    observeEvent(input$childEdit, {
        showModal(modalDialog(
            title = "Error",
            "Edit is currently not implemented."
        ))
        #this is a bit hacky, but Shiny.onInputChange allows to press each button
        #only once. So I just redraw the table every time the button was pressed. 
        update_children_table(session, input, values)
    })
    

    ############################################################################
    #tSNE panel
    #############################################################################
    
    #tun the bh-SNE when the button is pressed
    observeEvent(input$RunTSNE, {
        show_tsne_modal(input, values)
    })
    
    #all/none selector for tSNE
    observe({
        if (!is.null(values$markerMapping) &&
            !is.null(input$all_none_tnseselect)) {
            updateCheckboxGroupInput(
                session,
                'tSNEMarkers',
                choiceNames = as.list(names(values$markerMapping)),
                choiceValues = as.list(1:length(values$markerMapping)),
                selected = if (input$all_none_tnseselect)
                    as.list(1:length(values$markerMapping))
            )
        }
    })
    
    #if run tsne button has been pushed
    observeEvent(input$tsne_ok_button, {
        tsne_ok_button_pressed(input, values)
    })
    
    #saves a pdf of the current tSNE plot
    output$SavetSNE <- downloadHandler(
        filename = function() {
            paste("tSNE_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
            if (!is.null(values$gatingPanels) &&
                !is.null(values$currentID) &&
                length(values$gatingPanels[[values$currentID]]@tsne) > 0) {
                pdf(file)
                plot_tsne_panel(input, values)
                dev.off()
            }
        },
        contentType = 'pdf'
    )
    
    #display the visne panel
    output$tSNEPanel <- renderPlot({
        plot_tsne_panel(input, values)
    }, height = function() {
        min(700, session$clientData$output_tSNEPanel_width)
    })
    
    #superimpose the color
    observeEvent(input$select_tsne_impose, {
        tsne_impose_marker(input, values)
    })
    
    #modal that shows the overviews
    output$tSNE_overview <- renderPlot({
        plot_all_tsne_markers(input, values)
    })
    
    #color the t-SNE plot accoring to the selected gate
    observeEvent(input$GateBrush, {
        gate_brush(input, values)
    })
    
    #if you click on the gating plot the selected points are cleared
    observeEvent(input$gate_click, {
        values$selectedPoints <- NULL
    })
    
    output$Verbose <- renderPrint({
        values$verbose
    })
    
    
    #############################################################################
    #Marker selection
    #############################################################################
    
    #if you fiddle around with the markers, and the tSNE was done using arcsinh transformed data it gets dropped
    observeEvent(input$ArcSinhSelect, {
        change_arcsinselect(input, values)
    })
    
    observe({
        if (!is.null(values$markerMapping)) {
            updateCheckboxGroupInput(
                session,
                'ArcSinhSelect',
                choiceNames = as.list(names(values$markerMapping)),
                choiceValues = as.list(1:length(values$markerMapping)),
                selected = if (input$all_none_arcsinhselect)
                    as.list(1:length(values$markerMapping))
            )
        }
    })
    
    # disable the tSNE downdload button on page load
    shinyjs::hide("SavetSNE")
    
}
