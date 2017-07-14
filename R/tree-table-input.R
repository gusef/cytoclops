require(jsonlite)
require(data.table)
treeTableInput <- function(inputID,df=NULL,selected=NULL) {
  if (is.null(df)) { df = data.table::rbindlist(list(list(id="ALL")),fill=TRUE); }
  tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src="vakata-jstree-a6a0d0d/dist/jstree.js")
      )
    ),
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$link(rel="stylesheet",type="text/css",href="vakata-jstree-a6a0d0d/dist/themes/default-dark/style.css")
      )
    ),
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src="tree-table-input.js")
      )
    ),
    shiny::tags$div(id = inputID,type="treeTable"),
    shiny::tags$script(paste('initTreeTable("#',inputID,'",',"'",toJSON(df),"','",toJSON(selected),"'",')',sep=''))

    )
}

updateTreeTableInput <- function(session, inputID, df, selected=NULL) {
  message <- toJSON(list(data=df,selected=selected))
  session$sendInputMessage(inputID,message)
}
