require(shiny)
custom_tabPanel  <- function(title,...,value=title,icon=NULL){


  divTag <- tags$div(class='tab-panel',title=title,'data-value'=value,...)

  #divTag<- paste(br,divtag,sep=""),collapse='\n')
  
}