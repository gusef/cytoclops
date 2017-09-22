require(flowCore)
GatingPanel <- setClass("GatingPanel",
                        slots = c(indices = "numeric",
                                  gate_name = "character",
                                  children = "list",
                                  parent = "character",
                                  gates = "list",
                                  tsne = "list",
                                  tsne_sample="numeric",
                                  filename='character'))



#draw the actual polygon
drawPolygon <- function(x, y, idx, cols){
    #need to also add the line to the starting point
    x <- c(x,x[1])
    y <- c(y,y[1])
    lines(x,y,col=cols[idx],lwd=2.5)
    return(idx+1 %% length(cols))
}

#draw the polygon on top of the plot if it's the right markers
draw_children <- function(input, values){
    cols <- rainbow(15)[15:1]
    idx <- 1
    for (child in values$gatingPanels[[values$currentID]]@children){
        #if the markers are exactly the same
        if ((child$xmarker == input$Select_x_channels) && (child$ymarker == input$Select_y_channels)){
            idx <- drawPolygon(child$points$x, child$points$y, idx, cols)
            #if the markers are reversed
        }else if((child$ymarker == input$Select_x_channels) && (child$xmarker == input$Select_y_channels)){
            idx <- drawPolygon(child$points$y, child$points$x, idx, cols)
        }
    }
}

#contains all of the transform and color gradient  functions
plot_colorByDensity <- function(x1,x2,
                                ylim=c(min(x2),max(x2)),
                                xlim=c(min(x1),max(x1)),
                                xlab="",ylab="",main="",cex=2,
                                downsample) {
    df <- data.frame(x1,x2)
    x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
    df$dens <- col2rgb(x)[1,] + 1L
    cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
    df$col <- cols[df$dens]
    df <- df[downsample,]
    plot(x2~x1, data=df[order(df$dens),], 
         ylim=ylim,xlim=xlim,pch=20,col=col,
         cex=cex,xlab=xlab,ylab=ylab,
         main=main)
}

plot_gates <- function(input, values){
    if (is.null(values$gatingPanels) || is.null(values$currentID) || is.null(input$Select_x_channels))
        return(NULL)
    mat <- get_current_cells(values)
    xVal <- arcsinTransform(input$Select_x_channels, mat, values$markerMapping, input)
    yVal <- arcsinTransform(input$Select_y_channels, mat, values$markerMapping, input)
    
    par(mar=c(4.1,4.1,1.1,1.1))
    #either use a density plot
    if(input$PlotType=='smooth'){
        smoothScatter(xVal,
                      yVal, 
                      xlab=input$Select_x_channels,
                      ylab=input$Select_y_channels,
                      transformation = function(x) x^.25,
                      colramp = colorRampPalette(c("#FFFFFF","#000099", 
                                                   "#00FEFF", "#45FE4F",
                                                   "#FCFF00", "#FF9400", 
                                                   "#FF3100")),
                      pch = '.')
    #or downsample based on location
    }else{
        df <- data.frame(x = round(xVal, digits = input$plot_downsample),
                         y = round(yVal, digits = input$plot_downsample))
        downsample <- !duplicated(df)

        if (input$PlotType=='density'){
            plot_colorByDensity(df$x,
                                df$y,
                                xlab=input$Select_x_channels,
                                ylab=input$Select_y_channels,
                                cex=input$graphics_cex,
                                downsample=downsample)  
        }else{
            #color all cells black
            if (is.null(values$current_child)){
                col = 'black'
                #unless a child gate was selected then color the children red
            } else {
                current_ind <- values$gatingPanels[[values$currentID]]@indices
                child_ind <- values$gatingPanels[[values$current_child]]@indices
                
                col <- rep('black',length(current_ind))
                col[current_ind %in% child_ind] <- 'red'
                col <- col[downsample]
            }
            
            plot(df$x[downsample],
                 df$y[downsample],
                 xlab = input$Select_x_channels,
                 ylab = input$Select_y_channels,
                 pch = 20,
                 col = col,
                 cex = input$graphics_cex)   
        }
        
    }

    draw_children(input, values)
}


get_current_tsne <- function(input, values){
    #make sure the dots are transformed if the panel is transformed
    mat <- get_current_cells(values)
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

