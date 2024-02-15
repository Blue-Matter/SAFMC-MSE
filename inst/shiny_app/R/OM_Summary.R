OM_SummaryServer <- function(id='om_summary') {

  moduleServer(id, function(input, output, session) {
    output$OMtable <- DT::renderDataTable({
      DT::datatable(OMdat, options=list(dom = 't',
                                        ordering=FALSE,
                                        pageLength=100))
      })
  })

}

OM_SummaryUI <- function(id='om_summary') {
  ns <- NS(id)
  tagList(DT::dataTableOutput(ns('OMtable')))
}

