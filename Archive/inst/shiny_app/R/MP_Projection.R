

OM_ProjectUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h2('Compare the Projection Fishery Dynamics'),
      markdown('
This page shows two time series plots showing the fishery dynamics from both the historical period and the projection period where the management actions have been implemented.


Use the selection buttons to chose the Stock, the Operating Model (OM), the Management Action and Relative Reduction in the General Recreational Effort (see Home for details), and the Plot Variable (Spawning Biomass, Catch, or Fishing Mortality) for each plot.

By default the plots are shown in absolute units, but they can be shown relative to a reference point by selecting an option under the Relative to Reference Point selection button.

Uncertainty is shown for the projection period as the median (solid line) and 25th and 75th percentiles (shading).
               '),
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
                      proj_OM$selected_recEffort,
                      proj_OM$rel_to,
                      proj_OM$by_fleet), {

                        ref_points <- MSE_info[[OM_numbers[which(OM_names %in% proj_OM$selected_OM)]]]$Ref_Points %>%
                          filter(Stock %in% proj_OM$selected_stock)

                        select_var <- switch(proj_OM$selected_choice,
                                             `Spawning Biomass`='Spawning Biomass',
                                             `Catch` = c('Landings', 'Discards'),
                                             `Fishing Mortality` ='Apical Fishing Mortality')



                        df <- MSE_info[[OM_numbers[which(OM_names %in% proj_OM$selected_OM)]]]$Projection %>%
                          filter(MP_Name %in% c(NA,proj_OM$selected_MP)) |>
                          filter(Variable%in% select_var,Stock %in% proj_OM$selected_stock) |>
                          dplyr::filter(Rec_Reduction%in%c(NA,as.numeric(proj_OM$selected_recEffort)))

                        if (!proj_OM$by_fleet) {
                          df <- df %>% ungroup() %>% group_by(Year, Stock, Sim, MP_Name, Variable) %>%
                            summarise(Value=sum(Value),
                                      Upper=sum(Upper),
                                      Lower=sum(Lower), .groups='drop')
                          # df <- df %>% ungroup() %>% group_by(Year, Stock, Variable, MP_Name)  %>%
                          #   summarise(Lower=quantile(Value,0.25),
                          #             Upper=quantile(Value,0.75),
                          #             Value=median(Value))

                        } else {
                          df <- df %>% ungroup() %>% group_by(Year, Stock, Variable, MP_Name, Fleet) %>%
                            summarise(Value=sum(Value),
                                      Upper=sum(Upper),
                                      Lower=sum(Lower), .groups='drop')
                        }


                        # Reference Points
                        do_ref <- FALSE
                        if (proj_OM$selected_choice=='Spawning Biomass') {
                          df <- left_join(df, ref_points, by = join_by(Stock)) %>%
                            tidyr::pivot_longer(., cols=c('MFMT', 'Rebuild Target', 'MSST'),
                                                names_to = 'Reference Point') %>%
                            dplyr::filter(`Reference Point` %in% c('Rebuild Target', 'MSST'))
                          do_ref <- TRUE
                        }


                        if (proj_OM$selected_choice=='Fishing Mortality') {
                          df <- left_join(df, ref_points, by = join_by(Stock)) %>%
                            tidyr::pivot_longer(., cols=c('MFMT', 'Rebuild Target', 'MSST'),
                                                names_to = 'Reference Point') %>%
                            dplyr::filter(`Reference Point` %in% c('MFMT'))
                          # df$`Reference Point` <- 'FMSY'
                          do_ref <- TRUE

                          if (proj_OM$by_fleet) do_ref <- FALSE
                        }

                        # Scale to Reference Points
                        if ( proj_OM$rel_to!='None' & proj_OM$selected_choice!='Catch') {
                          df <- df %>%  mutate(Value=Value/value[`Reference Point`== proj_OM$rel_to],
                                               Upper=Upper/value[`Reference Point`== proj_OM$rel_to],
                                               Lower=Lower/value[`Reference Point`== proj_OM$rel_to],
                                               value=value/value[`Reference Point`== proj_OM$rel_to])
                        }

                        if  (proj_OM$selected_choice=='Catch') {
                          df$Value <- openMSE::kg2_1000lb(df$Value)
                          df$Lower <- openMSE::kg2_1000lb(df$Lower)
                          df$Upper <- openMSE::kg2_1000lb(df$Upper)
                        }



                        proj_OM$df <- df
                        proj_OM$title <- paste('Stock:', proj_OM$selected_stock, 'OM:', proj_OM$selected_OM, 'MP:', proj_OM$selected_MP)
                      })
  })
}


Calc_Max_Y_Proj <- function(id='calcmaxy', proj_OM1, proj_OM2) {
  moduleServer(id, function(input, output, session) {

    observeEvent(list(proj_OM1$selected_OM,
                      proj_OM1$selected_stock,
                      proj_OM1$selected_choice,
                      proj_OM1$selected_MP,
                      proj_OM1$selected_recEffort,
                      proj_OM1$rel_to,
                      proj_OM1$by_fleet,
                      proj_OM2$selected_OM,
                      proj_OM2$selected_stock,
                      proj_OM2$selected_choice,
                      proj_OM2$selected_MP,
                      proj_OM2$selected_recEffort,
                      proj_OM2$rel_to,
                      proj_OM2$by_fleet), {
                        df <- bind_rows(proj_OM1$df, proj_OM2$df)

                        if (proj_OM1$selected_stock ==proj_OM2$selected_stock &
                            proj_OM1$selected_choice ==proj_OM2$selected_choice &
                            proj_OM1$rel_to ==proj_OM2$rel_to &
                            proj_OM1$by_fleet ==proj_OM2$by_fleet &
                            proj_OM1$free_y ==proj_OM2$free_y &
                            !proj_OM1$free_y & !proj_OM2$free_y
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


