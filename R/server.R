rm(list=ls())
gc()
require('shiny')
require('shinyjs')
require('sp')

# Source isn't necessary when generating a package
source('misc.R')
source('gatingpanel_functions.R')
source('gating_functions.R')
source('tsne_functions.R')
source('polygon.R')


server <- function(input, output, session) {
    options(shiny.maxRequestSize=500*1024^2)
    
    #reactive values
    values <- reactiveValues(flowFrame = NULL,
                             gatingPanels=NULL,
                             currentID=NULL,
                             channels=NULL,
                             markerMapping=NULL,
                             selectedPoints=NULL,
                             verbatimOutput='',
                             tsne_current_marker='',
                             tsne_col='',
                             tSNE_markers=NULL, 
                             tsne_arcsintransform=TRUE,
                             verbose='',
                             scaled_points=NULL)
    
    #file loading
    observeEvent(input$fcsFile,{ 
        instantiate_panels(input, values, input$fcsFile$datapath)
    })
    
    #file loading - testset
    observeEvent(input$LoadTest,{
        instantiate_panels(input, values, '../inst/extdata/test.fcs') 
    })
    
    #file loading
    observeEvent(input$rdsFile,{ 
        loadGating(input, values, input$rdsFile$datapath)
    })
    
    
    #simple gating panel
    output$GatingPanel <- renderPlot({
        plot_gates(input, values)
    },height = function() {
        min(700,session$clientData$output_GatingPanel_width)
    })
    
    #update the polygon points is a new batch is drawn
    observeEvent(input$PolygonPoints,{ 
        update_polygon_points(input, values)       
    })
    
    observeEvent(input$PolygonViewButton,{
        update_tsne_by_polygon(input, values)
    })
    
    observeEvent(input$PolygonButton, {
        js$initPolyDraw(id="GatingPanel")
    })
    
    observeEvent(input$PolygonResetButton, {
        values$scaled_points <- NULL
        js$resetPolyDraw(id="GatingPanel")

    })
    
    #if you fiddle around with the markers, and the tSNE was done using arcsinh transformed data it gets dropped
    observeEvent(input$ArcSinhSelect, {
        change_arcsinselect(input, values)
    })
    
    #tun the bh-SNE when the button is pressed
    observeEvent(input$RunTSNE, {
        show_tsne_modal(input, values)
    })
    
    #if run tsne button has been pushed
    observeEvent(input$tsne_ok_button, {
        tsne_ok_button_pressed(input, values)
    })
    
    #display the visne panel
    output$tSNEPanel <- renderPlot({
        plot_tsne_panel(input, values)
    },height = function() {
        min(700,session$clientData$output_tSNEPanel_width)
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
    observeEvent(input$gate_click,{
        values$selectedPoints <- NULL
    })
    
    #Pressing the gating button
    observeEvent(input$GateButton, {
        gating_modal(input, values, session)
    })
    
    #Pressing the OK button in the gating modal
    observeEvent(input$gate_ok_button, {
        press_gating_ok(input, values)
    })
    
    #set the currentID to the newly selected gate
    observeEvent(input$RadioGates,{
        radio_gate_click(input, values)
    })
    
    output$Verbose <- renderPrint({
        values$verbose
    })

    #Pressing the save gating button
    observeEvent(input$SaveStateButton, {
        press_save_gating(input, values)
    })

    #Pressing the save gating button
    observeEvent(input$save_ok_button, {
        press_ok_gating(input, values)
    })
}