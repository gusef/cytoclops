
mapLinear <- function(x, domainMin, domainMax, rangeMin, rangeMax, clip = TRUE) {
    factor <- (rangeMax - rangeMin) / (domainMax - domainMin)
    val <- x - domainMin
    newval <- (val * factor) + rangeMin
    
    if (clip) {
        maxval <- max(rangeMax, rangeMin)
        minval <- min(rangeMax, rangeMin)
        newval[newval > maxval] <- maxval
        newval[newval < minval] <- minval
    }
    newval
}

# Inverse scale val, from range to domain. If logbase is present, use inverse
# log (power) transformation.
scaleInv1D <- function(val, domainMin, domainMax, rangeMin, rangeMax,
                       logbase = NULL, clip = TRUE) {
    res <- mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip)
    if (!is.null(logbase))
        res <- logbase ^ res
    res
}

# Inverse scale x and y coordinates from range to domain, using information in
# scaleinfo.
scaleInvCoords <- function(x, y, scaleinfo) {
    if (is.null(scaleinfo))
        return(NULL)
    
    domain <- scaleinfo$domain
    range <- scaleinfo$range
    log <- scaleinfo$log
    
    list(
        x = scaleInv1D(x, domain$left, domain$right, range$left, range$right, log$x),
        y = scaleInv1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
    )
}

update_polygon_points <- function(input, values){
    #get the polygon points
    points <- matrix(input$PolygonPoints, ncol=2, byrow=TRUE)
    
    #get the image object by just grabbing the click object
    scaleinfo <- input$gate_click
    
    #scale the polygon coordinates into data space
    values$scaled_points <- scaleInvCoords(points[,1],points[,2],scaleinfo)
}

get_polygon_selected <- function(input, values){
    #squeeze the points into a SP polygon class
    poly <- Polygons(list(Polygon(values$scaled_points)),"points")
    spatpoly <- SpatialPolygons(list(poly))
    
    #extract the current data values
    mat <- exprs(values$flowFrame[values$gatingPanels[[values$currentID]]@indices,])
    xVal <- arcsinTransform(input$Select_x_channels, mat, values$markerMapping, input)
    yVal <- arcsinTransform(input$Select_y_channels, mat, values$markerMapping, input)
    data <- data.frame(x=xVal,y=yVal,id=1:nrow(mat))
    coordinates(data) = ~x+y
    
    #overlay the polygon onto the data
    selected <- over(spatpoly, data, returnList=T)
    selected <- data.frame(selected$points)
    selected <- data$id %in% selected$id
    return(selected)
}

update_tsne_by_polygon <- function(input, values){
    #if we are not in polygon mode 
    if (is.null(values$scaled_points))
        return(NULL)
    #if there are less than 3 points
    if (length(values$scaled_points$x) < 3)
        return(NULL)
    #if there is no tSNE panel
    if (length(values$gatingPanels[[values$currentID]]@tsne)==0)
        return(NULL)
    
    selected <- get_polygon_selected(input, values)        
    
    #change the color on the tsne
    values$tsne_current_marker <- ''
    values$tsne_col <- rep('grey',input$DownSample)
    values$tsne_col[selected[values$gatingPanels[[values$currentID]]@tsne_sample]] <- 'black'
    
}
