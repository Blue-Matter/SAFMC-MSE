

OM_ProjectUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h3('Compare the Projection Fishery Dynamics for 2 Operating Models'),
      column(6,
             fluidRow(
               OM_Select_UI('Select_Proj_OM1'),
               OM_Plot_UI('Plot_Proj_OM1')
             )),
      column(6,
             fluidRow(
               OM_Plot_UI('Plot_Proj_OM2'),
               OM_Select_UI('Select_Proj_OM2')

             ))

    )

  )
}


get_Proj_DF <- function(id='get_Proj_DF', proj_OM) {

  moduleServer(id, function(input, output, session) {

    observeEvent(list(proj_OM$selected_OM,
                      proj_OM$selected_stock,
                      proj_OM$selected_choice,
                      proj_OM$selected_MP,
                      proj_OM$rel_to,
                      proj_OM$by_fleet), {

                        ref_points <- MSE_info[[OM_numbers[which(OM_names %in% proj_OM$selected_OM)]]]$Ref_Points %>%
                          filter(Stock %in% proj_OM$selected_stock)

                        select_var <- switch(proj_OM$selected_choice,
                                             `Spawning Biomass`='Spawning Biomass',
                                             `Catch` = c('Landings', 'Discards'),
                                             `Fishing Mortality` ='Apical Fishing Mortality')


                        df <- MSE_info[[OM_numbers[which(OM_names %in% proj_OM$selected_OM)]]]$Projection %>%
                          filter(Variable%in% select_var,Stock %in% proj_OM$selected_stock)

                        df <- df %>% filter(MP%in% c(NA,proj_OM$selected_MP))

                        if (!proj_OM$by_fleet) {
                          df <- df %>% ungroup() %>% group_by(Year, Stock, MP, Sim, Variable) %>%
                            summarise(Value=sum(Value), .groups='drop')
                          df <- df %>% ungroup() %>% group_by(Year, Stock, Variable, MP) %>%
                            summarise(Value=median(Value))
                        } else {
                          df <- df %>% ungroup() %>% group_by(Year, Stock, Variable, MP, Fleet) %>%
                            summarise(Value=median(Value))
                        }


                        # Reference Points
                        do_ref <- FALSE
                        if (proj_OM$selected_choice=='Spawning Biomass') {
                          df <- left_join(df, ref_points, by = join_by(Stock)) %>%
                            tidyr::pivot_longer(., cols=c('F', 'SPR', 'SBtarg', 'MSST'),
                                                names_to = 'Reference Point') %>%
                            dplyr::filter(`Reference Point` %in% c('SBtarg', 'MSST'))
                          do_ref <- TRUE
                        }


                        if (proj_OM$selected_choice=='Fishing Mortality') {
                          df <- left_join(df, ref_points, by = join_by(Stock)) %>%
                            tidyr::pivot_longer(., cols=c('F', 'SPR', 'SBtarg', 'MSST'),
                                                names_to = 'Reference Point') %>%
                            dplyr::filter(`Reference Point` %in% c('F'))
                          df$`Reference Point` <- 'FMSY'
                          do_ref <- TRUE

                          if (proj_OM$by_fleet) do_ref <- FALSE
                        }

                        # Scale to Reference Points
                        if ( proj_OM$rel_to!='None' & proj_OM$selected_choice!='Catch') {
                          df <- df %>%  mutate(Value=Value/value[`Reference Point`== proj_OM$rel_to],
                                               value=value/value[`Reference Point`== proj_OM$rel_to])
                        }

                        if  (proj_OM$selected_choice=='Catch') {
                          df$Value <- openMSE::kg2_1000lb(df$Value)
                        }

                        proj_OM$df <- df
                      })
  })
}


Calc_Max_Y_Proj <- function(id='calcmaxy', proj_OM1, proj_OM2) {
  moduleServer(id, function(input, output, session) {

    observeEvent(list(proj_OM1$selected_OM,
                      proj_OM1$selected_stock,
                      proj_OM1$selected_choice,
                      proj_OM1$selected_MP,
                      proj_OM1$rel_to,
                      proj_OM1$by_fleet,
                      proj_OM2$selected_OM,
                      proj_OM2$selected_stock,
                      proj_OM2$selected_choice,
                      proj_OM2$selected_MP,
                      proj_OM2$rel_to,
                      proj_OM2$by_fleet), {
                        df <- bind_rows(proj_OM1$df, proj_OM2$df)

                        if (proj_OM1$selected_stock ==proj_OM2$selected_stock &
                            proj_OM1$selected_choice ==proj_OM2$selected_choice &
                            proj_OM1$rel_to ==proj_OM2$rel_to &
                            proj_OM1$by_fleet ==proj_OM2$by_fleet
                        ) {

                          df <- bind_rows(proj_OM1$df, proj_OM2$df)
                          # if ( proj_OM1$selected_choice=='Catch')
                          #   df$Value <- openMSE::kg2_1000lb(df$Value)

                          if (!is.null(df$Value)) {
                            proj_OM1$maxY <-  max(df$Value)
                            proj_OM2$maxY <- max(df$Value)
                          }

                        } else {
                          proj_OM1$maxY <-  Inf
                          proj_OM2$maxY <- Inf
                        }

                      })

    get_Proj_DF('getProj_DF1', proj_OM1)
    get_Proj_DF('getProj_DF2', proj_OM2)

  })


}


