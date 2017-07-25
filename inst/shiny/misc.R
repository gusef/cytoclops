#contains all of the transform and color gradient  functions
plot_colorByDensity <- function(x1,x2,
                                ylim=c(min(x2),max(x2)),
                                xlim=c(min(x1),max(x1)),
                                xlab="",ylab="",main="",cex=2) {
    df <- data.frame(x1,x2)
    x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
    df$dens <- col2rgb(x)[1,] + 1L
    cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F","#FCFF00", "#FF9400", "#FF3100"))(256)
    df$col <- cols[df$dens]
    plot(x2~x1, data=df[order(df$dens),], 
         ylim=ylim,xlim=xlim,pch=20,col=col,
         cex=cex,xlab=xlab,ylab=ylab,
         main=main)
}

#transforms a single marker to arcsinh
arcsinTransform <- function(marker, mat, map, input){
    selected_markers <- names(map)[as.numeric(input$ArcSinhSelect)]
    current_vals <- mat[,map[marker]]
    # only transform if the checkbox is set
    if(marker %in% selected_markers){
        current_vals <- asinh(current_vals/input$arcsinh_par)
    }
    return(current_vals)
}

#helper function that determines which markers should be pre-selected for arcsinh transformation
extractArcsinhTemp <- function(values){
    markers <- names(values$markerMapping)
    selected <- rep(FALSE,length(markers))
    selected[grep('::',markers)] <- TRUE
    return(as.list((1:length(markers))[selected]))
}

#function that extracts the markers, makes a map and handles the proper ordering

extractMarkerPanel <- function(input, values){
    chan <- pData(parameters(values$flowFrame))[,c('name','desc')]
    chan <- paste(chan$name,chan$desc,sep='::')
    vals <- colnames(values$flowFrame) 
    names(vals) <- sub('::NA','',chan)
    #Katja's custom order (might be extended, by adding options into the settings
    actual <- grep('..[0-9]+',vals)
    vals <- c(vals[actual[order(sub('^..','',vals[actual]))]],sort(vals[-actual]))  
    return(vals)
}

change_arcsinselect <- function(input, values){
    if (!is.null(values$gatingPanels) && !is.null(values$currentID)){
        if (values$tsne_arcsintransform){
            values$gatingPanels[[values$currentID]]@tsne<-list()
            removeUI(selector = "#ImposeColorSelector")
        }
    }
}