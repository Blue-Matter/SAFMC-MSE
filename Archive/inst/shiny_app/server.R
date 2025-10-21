

server <- function(input, output) {



  output$OM_details <- DT::renderDataTable({
    DT::datatable(OMdat, options=list(dom = 't',
                                      ordering=FALSE,
                                      pageLength=100),
                  rownames = FALSE,
                  selection = 'none')
  })

  output$om_summary <- renderUI({
    tagList(
      br(),
      markdown('**OM Details Table:** This table describes the Base Case and five Robustness operating models (OMs) included in the analysis. The App allows users to view the MSE results for each OM, and to compare the results of two different OMs. See the [Technical Specifications Document](https://safmc-mse.bluematterscience.com/resources/TS/TS.html) for more details on the operating models.'),
      DT::dataTableOutput('OM_details')
    )
  })

  output$MP_details <- DT::renderDataTable({
    DT::datatable(MPdat, options=list(dom = 't',
                                      ordering=FALSE,
                                      pageLength=100),
                  selection = 'none',
                  rownames = FALSE)
  })

  output$mp_summary <- renderUI({
    tagList(
      br(),
      markdown('**Management Options Table:** This table describes the five management categories evaluated in the closed-loop simulation testing. The analysis included all combinations of these management options. These are described in more detail on the Home page.'),
      DT::dataTableOutput('MP_details')
    )
  })




  reconstruct_OM1 <- reactiveValues(selected_OM=OM_names[1],
                                    selected_stock=stocks[1],
                                    selected_choice='Spawning Biomass',
                                    rel_to='None',
                                    by_fleet=FALSE,
                                    free_y=FALSE,
                                    df=data.frame(),
                                    maxY=Inf,
                                    title='')

  reconstruct_OM2 <- reactiveValues(selected_OM=OM_names[2],
                                    selected_stock=stocks[1],
                                    selected_choice='Spawning Biomass',
                                    rel_to='None',
                                    by_fleet=FALSE,
                                    free_y=FALSE,
                                    maxY=Inf,
                                    title='')

  proj_OM1 <- reactiveValues(selected_OM=OM_names[1],
                             selected_stock=stocks[1],
                             selected_choice='Spawning Biomass',
                             selected_MP=MPs[1],
                             selected_recEffort=0,
                             rel_to='None',
                             by_fleet=FALSE,
                             free_y=FALSE,
                             df=data.frame(),
                             maxY=Inf,
                             title='')

  proj_OM2 <- reactiveValues(selected_OM=OM_names[2],
                             selected_stock=stocks[1],
                             selected_choice='Spawning Biomass',
                             selected_MP=MPs[1],
                             selected_recEffort=0,
                             rel_to='None',
                             by_fleet=FALSE,
                             free_y=FALSE,
                             maxY=Inf,
                             title='')


  ProbTableServer('probtables')

  OM_Select_Server('Select_Hist_OM1', 1, reconstruct_OM1)
  OM_Select_Server('Select_Hist_OM2', 2, reconstruct_OM2)

  Calc_Max_Y('calcmaxy', reconstruct_OM1, reconstruct_OM2)

  OM_Plot_Server('Plot_Hist_OM1', reconstruct_OM1)
  OM_Plot_Server('Plot_Hist_OM2', reconstruct_OM2)

  OM_Select_Server('Select_Proj_OM1', 1, proj_OM1, incMP=TRUE)
  OM_Select_Server('Select_Proj_OM2', 2, proj_OM2, incMP=TRUE)

  Calc_Max_Y_Proj('calcmaxy_proj', proj_OM1, proj_OM2)

  OM_Plot_Server('Plot_Proj_OM1', proj_OM1)
  OM_Plot_Server('Plot_Proj_OM2', proj_OM2)

}

