

plot_C <- function(multiHist, units=c('1000 lb', 'mt'),
                   type=c('overall', 'byfleet')) {
  units <- match.arg(units)
  type <- match.arg(type)

  if(inherits(multiHist, 'MMSE')) {
    p <- plot_C_proj(multiHist, units, type)
  } else {
    df <- get_ts(multiHist)
    bio <- df %>% filter(Name%in%c('Landings', 'Discards'))
    bio$Name <- factor(bio$Name, levels=unique(bio$Name), ordered = TRUE)
    bio$Fleet <- factor(bio$Fleet, levels=unique(bio$Fleet), ordered = TRUE)

    if (units=='1000 lb') {
      bio$Value <- bio$Value/1000
    }
    if (units=='mt') {
      bio$Value <- lb2mt(bio$Value)
    }
    ylab <- units

    if (type=='byfleet') {

      p <- ggplot(bio, aes(x=Year, y=Value, color=Fleet)) +
        facet_grid(Stock~Name, scales='free_y') +
        geom_line(size=1.2) +
        theme_bw() +
        labs(x='Year', y=ylab)

    } else {
      bio <- bio %>% group_by(Stock, Year, Name) %>%
        summarize(Value=sum(Value), .groups = 'drop')
      p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
        facet_grid(~Name, scales='free_y') +
        geom_line(size=1.2) +
        theme_bw() +
        labs(x='Year', y=ylab) +
        scale_color_manual(values=c('red', 'blue'))
    }
  }
  p
}

plot_SB <- function(multiHist,
                    type=c('abs', 'rel'),
                    incRef=TRUE) {
  type <- match.arg(type)
  info <- get_info(multiHist)
  df <- get_ts(multiHist)
  bio <- df %>% filter(Name=='SBiomass', Fleet==info$fleets[1])
  bio <- left_join(bio, info$Ref, by='Stock')
  bio$Stock <- factor(bio$Stock, levels=unique(bio$Stock), ordered = TRUE)
  bio <- bio %>% tidyr::pivot_longer(cols=c(SBref, MSST))


  if (type=='rel') {
    bio <- bio %>% group_by(Stock) %>% mutate(Value=Value/SB0, value=value/SB0)
    ylab <- 'Relative Spawning Biomass (SB/SB0)'
  } else {
    ylab <- 'Spawning Biomass (mt and Eggs(1E8))'
  }

  p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
    geom_line(size=1.2) +
    facet_wrap(~Stock, scales='free_y') +
    expand_limits(y=c(0,1)) +
    theme_bw() +
    labs(x='Year', y=ylab) +
    scale_color_manual(values=c('red', 'blue'))
  if (incRef) {
    p <- p + geom_line(aes(y=value, color=Stock, linetype=name)) +
      scale_linetype_manual(values=c(2,3)) +
      labs(linetype='SBref')
  }
  p
}



plot_SB_proj <- function(MMSE,
                         type=c('abs', 'rel'),
                         stat=c('mean', 'median'),
                         incquants=TRUE,
                         incRef=TRUE) {
  type <- match.arg(type)
  stat <- match.arg(stat)

  info <- get_info_MMSE(MMSE)
  df <- get_ts_MMSE(MMSE)
  bio <- df %>% filter(Name=='SBiomass', Fleet==info$fleets[1])
  bio <- left_join(bio, info$Ref, by='Stock')
  bio$Stock <- factor(bio$Stock, levels=unique(bio$Stock), ordered = TRUE)
  bio <- bio %>% tidyr::pivot_longer(cols=c(SBref, MSST))

  var_stat <- switch(stat, mean='Mean', median='Median')
  bio$Value <- bio[[var_stat]]

  if (type=='rel') {
    bio <- bio %>% group_by(Stock) %>% mutate(Value=Value/SB0, MMST=value/SB0)
    ylab <- 'Relative Spawning Biomass (SB/SB0)'
  } else {
    ylab <- 'Spawning Biomass (mt and Eggs (1E8))'
  }

  p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
    facet_grid(Stock~MP, scales='free_y') +
    geom_line(size=1.2) +
    expand_limits(y=0) +
    theme_bw() +
    labs(x='Year', y=ylab) +
    scale_color_manual(values=c('red', 'blue'))
  if (incRef) {
    p <- p + geom_line(aes(y=value, linetype=name, color=Stock)) +
      scale_linetype_manual(values=c(2,3)) +
      labs(linetype='SBref')
  }
  p
}

plot_B <- function(multiHist, units=c('1000 lb', 'mt'),
                   type=c('abs', 'rel'),
                   incRef=TRUE, incLeg=TRUE) {
  units <- match.arg(units)
  type <- match.arg(type)
  info <- get_info(multiHist)
  df <- get_ts(multiHist)
  bio <- df %>% filter(Name=='Biomass', Fleet==info$fleets[1])
  bio <- left_join(bio, info$Ref, by='Stock')
  bio$Stock <- factor(bio$Stock, levels=unique(bio$Stock), ordered = TRUE)
  if (type=='rel') {
    bio <- bio %>% group_by(Stock) %>% mutate(Value=Value/B0, Bref=Bref/B0)
    ylab <- 'Relative Biomass (B/B0)'
  } else {
    if (units=='1000 lb') {
      bio$Value <- bio$Value/1000
      bio$Bref <- bio$Bref/1000
    }
    if (units=='mt') {
      bio$Value <- lb2mt(bio$Value)
      bio$Bref <- lb2mt(bio$Bref)
    }
    ylab <- paste0('Biomass (', units, ')')
  }

  p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
    geom_line(size=1.2) +
    expand_limits(y=c(0,1)) +
    theme_bw() +
    labs(x='Year', y=ylab) +
    scale_color_manual(values=c('red', 'blue'))
  if (incRef) {
    p <- p + geom_line(aes(y=Bref, color=Stock, linetype=Bref_type)) +
      scale_linetype_manual(values=c(2,3)) +
      labs(linetype='Bref')
  }
  if (!incLeg) {
    p <- p + guides(color='none')
  }
  p
}


get_info <- function(multiHist) {
  if (inherits(multiHist, 'MMSE')) {
    out <- get_info_MMSE(multiHist)
    return(out)
  }

  tt <- dim(multiHist[[1]][[1]]@AtAge$Length)
  out <- list()
  out$nsim <- tt[1]
  out$stocks <- names(multiHist)
  out$fleets <- names(multiHist[[1]])
  out$n.stocks <- length(multiHist)
  out$n.fleets <- length(multiHist[[1]])
  out$hist.yrs <- hist.yrs<- multiHist[[1]][[1]]@Data@Year

  out$nage <- nage <- tt[2]
  out$maxage <- nage - 1
  out$nyears <- nyears <- length(hist.yrs)
  out$proyears <- tt[3]-nyears

  # Reference points
  reflist <- list()
  for (s in 1:out$n.stocks) {
    stock <- out$stocks[s]
    df <- data.frame(Stock=stock,
                     B0=multiHist[[s]][[1]]@Ref$ReferencePoints$B0[1],
                     SB0=multiHist[[s]][[1]]@Ref$ReferencePoints$SSB0[1],
                     # Table 27 and Table 15 for RS and GG respectively
                     Fref=ifelse(stock=='Red Snapper', 0.21, 0.37),
                     Fref_type=ifelse(stock=='Red Snapper', 'F30%', 'FMSY'),
                     Bref=ifelse(stock=='Red Snapper', mt2lb(6530), mt2lb(4278)),
                     Bref_type=ifelse(stock=='Red Snapper', 'B_F30%', 'B_FMSY'),
                     SBref=ifelse(stock=='Red Snapper', 635426, 1563),
                     SBref_type=ifelse(stock=='Red Snapper','SB_F30%', 'SB_FMSY'),
                     MSST=ifelse(stock=='Red Snapper',476569, 1172)
    )
    reflist[[s]] <- df
  }

  out$Ref <- do.call('rbind', reflist)
  out$Stock <- factor(out$Stock, levels=unique(out$Stock), ordered=TRUE)
  out
}

get_info_MMSE <- function(MMSE) {
  out <- list()
  out$nsim <- MMSE@nsim
  out$stocks <- as.character(MMSE@Snames)
  out$fleets <- MMSE@Fnames[,1]
  out$n.stocks <- MMSE@nstocks
  out$n.fleets <- MMSE@nfleets
  all.years <- MMSE@PPD[[1]][[1]][[1]]@Year
  lh.year <- MMSE@PPD[[1]][[1]][[1]]@LHYear
  out$hist.yrs <- hist.yrs <- all.years[1:match(lh.year,all.years)]
  out$all.yrs <- all.years
  out$p.yrs <- seq(lh.year+1, by=1, length.out=MMSE@proyears)
  out$maxage <-MMSE@PPD[[1]][[1]][[1]]@MaxAge
  out$nage <- nage <- out$maxage+1

  out$nyears <- MMSE@nyears
  out$proyears <- MMSE@proyears

  out$nMPs <- MMSE@nMPs
  slotNames(MMSE)
  MMSE@RefPoint$ByYear$B0 %>% dim()
  MMSE@RefPoint$ByYear %>% length()

  # Reference points
  reflist <- list()
  for (s in 1:out$n.stocks) {
    stock <- out$stocks[s]
    # assumed no time-varying or MP-specific
    df <- data.frame(Stock=stock,
                     B0=MMSE@RefPoint$ByYear$B0[1,s,1,1],
                     SB0=MMSE@RefPoint$ByYear$SSB0[1,s,1,1],
                     # Table 27 and Table 15 for RS and GG respectively
                     Fref=ifelse(stock=='Red Snapper', 0.21, 0.37),
                     Fref_type=ifelse(stock=='Red Snapper', 'F30%', 'FMSY'),
                     Bref=ifelse(stock=='Red Snapper', mt2lb(6530), mt2lb(4278)),
                     Bref_type=ifelse(stock=='Red Snapper', 'B_F30%', 'B_FMSY'),
                     SBref=ifelse(stock=='Red Snapper', 635426, 1563),
                     SBref_type=ifelse(stock=='Red Snapper','SB_F30%', 'SB_FMSY'),
                     MSST=ifelse(stock=='Red Snapper',476569, 1172)
    )
    reflist[[s]] <- df
  }

  out$Ref <- do.call('rbind', reflist)
  out$Stock <- factor(out$Stock, levels=unique(out$Stock), ordered=TRUE)
  out
}


get_ts <- function(multiHist, info=NULL) {
  if (inherits(multiHist, 'MMSE')) {
    df <- get_ts_MMSE(multiHist)
    return(df)
  }
  if (is.null(info))
    info <- get_info(multiHist)
  outlist <- list()
  for (p in 1:info$n.stocks) {
    outlist[[p]] <- list()
    for (fl in 1:info$n.fleets) {
      nms <- c('Biomass', 'SBiomass', 'Removals', 'Landings', 'Discards')
      dflist <- list()
      for (nm_i in seq_along(nms)) {
        vals <- multiHist[[p]][[fl]]@TSdata[[nms[nm_i]]][1,,]
        if (!is.null(vals)) {
          vals <- rowSums(vals)
        } else {
          vals <- NA
        }
        dflist[[nm_i]] <- data.frame(Stock=info$stocks[p],
                                     Fleet=info$fleets[fl],
                                     Year=info$hist.yrs,
                                     Value=vals,
                                     Name=nms[nm_i])

      }

      outlist[[p]][[fl]] <- do.call('rbind', dflist)

    }
    outlist[[p]] <- do.call('rbind', outlist[[p]])
  }
  df <-  do.call('rbind', outlist)
  df$Stock <- factor(df$Stock, levels=unique(df$Stock), ordered=TRUE)
  df
}

calc_stats <- function(MMSE, slot, byfleet=TRUE, info=NULL) {
  if (slot=='Discards') {
    vals <- MMSE@Removals-MMSE@Catch
  } else {
    vals <- slot(MMSE, slot)
  }
  if (is.null(info)) info <- get_info_MMSE((MMSE))

  name <- switch(slot,
                 B='Biomass',
                 SSB='SBiomass',
                 Catch='Landings')
  if (is.null(name)) name <- slot
  dd <- dim(vals)
  if (length(dd)==4) {
    # population - not by fleet
    mean <- apply(vals, 2:4, mean)
    median <- apply(vals, 2:4, median)
    lower  <- apply(vals, 2:4, quantile, 0.05)
    upper <- apply(vals, 2:4, quantile, 0.95)
    df <- data.frame(Stock=info$stocks,
                     Fleet=info$fleets[1],
                     Year=rep(info$p.yrs,each=info$n.stocks*info$nMPs),
                     Mean=as.vector(mean),
                     Median=as.vector(median),
                     Lower=as.vector(lower),
                     Upper=as.vector(upper),
                     MP=rep(MMSE@MPs[[1]], each=info$n.stocks),
                     Name=name)

  } else {
    if (byfleet) {
      # by fleet
      mean <- apply(vals, 2:5, mean)
      median <- apply(vals, 2:5, median)
      lower  <- apply(vals, 2:5, quantile, 0.05)
      upper <- apply(vals, 2:5, quantile, 0.95)
      df <- data.frame(Stock=info$stocks,
                       Fleet=rep(info$fleets,each=info$n.stocks),
                       Year=rep(info$p.yrs,each=info$n.stocks*info$n.fleets*info$nMPs),
                       Mean=as.vector(mean),
                       Median=as.vector(median),
                       Lower=as.vector(lower),
                       Upper=as.vector(upper),
                       MP=rep(MMSE@MPs[[1]], each=info$n.stocks*info$n.fleets),
                       Name=name)
    } else {
      vals <- apply(vals, c(1,2,4,5), sum)
      mean <- apply(vals, 2:4, mean)
      median <- apply(vals, 2:4, median)
      lower  <- apply(vals, 2:4, quantile, 0.05)
      upper <- apply(vals, 2:4, quantile, 0.95)
      df <- data.frame(Stock=info$stocks,
                       Fleet=info$fleets[1],
                       Year=rep(info$p.yrs,each=info$n.stocks*info$nMPs),
                       Mean=as.vector(mean),
                       Median=as.vector(median),
                       Lower=as.vector(lower),
                       Upper=as.vector(upper),
                       MP=rep(MMSE@MPs[[1]], each=info$n.stocks),
                       Name=name)
    }
  }
  df
}

get_ts_MMSE <- function(MMSE, byfleet=TRUE) {

  info <- get_info_MMSE(MMSE)
  nMPs <- MMSE@nMPs
  outlist <- list()

  bio <- calc_stats(MMSE, 'B', byfleet = byfleet, info)
  sbio <- calc_stats(MMSE, 'SSB', byfleet = byfleet, info)
  removals <- calc_stats(MMSE, 'Removals', byfleet = byfleet, info)
  landings <- calc_stats(MMSE, 'Catch', byfleet = byfleet, info)
  discards <- calc_stats(MMSE, 'Discards', byfleet = byfleet, info)

  df <- bind_rows(bio, sbio, removals, landings, discards)
  df$Stock <- factor(df$Stock, levels=unique(df$Stock), ordered = TRUE)
  df
}

plot_C_proj <- function(MMSE, units=c('1000 lb', 'mt'),
                        type=c('overall', 'byfleet'),
                        stat=c('mean', 'median'),
                        incquants=TRUE,
                        incHist=FALSE) {

  units <- match.arg(units)
  type <- match.arg(type)
  stat <- match.arg(stat)

  if (incHist) {
    p <- plot_C_overall(MMSE, units, type, stat)
    return(p)
  }

  if (type=='byfleet') {
    byfleet <- TRUE
  } else {
    byfleet <- FALSE
  }
  df <- get_ts_MMSE(MMSE, byfleet)

  bio <- df %>% filter(Name%in%c('Landings', 'Discards'))
  bio$Name <- factor(bio$Name, levels=unique(bio$Name), ordered = TRUE)
  bio$Fleet <- factor(bio$Fleet, levels=unique(bio$Fleet), ordered = TRUE)

  var_stat <- switch(stat, mean='Mean', median='Median')
  bio$Value <- bio[[var_stat]]
  if (units=='1000 lb') {
    bio$Value <- bio$Value/1000
    bio$Upper <- bio$Upper/1000
    bio$Lower <- bio$Lower/1000
  }
  if (units=='mt') {
    bio$Value <- lb2mt(bio$Value)
    bio$Upper <- lb2mt(bio$Upper)
    bio$Lower <- lb2mt(bio$Lower)
  }
  ylab <- units

  if (type=='byfleet') {
    p <- ggplot(bio, aes(x=Year, y=Value,
                         color=Fleet, linetype=MP)) +
      facet_grid(Stock~Name, scales='free_y') +
      geom_line() +
      theme_bw() +
      labs(x='Year', y=ylab)

  } else {
    p <- ggplot(bio, aes(x=Year, y=Value, color=Stock, linetype=MP)) +
      facet_grid(Stock~Name) +
      geom_line() +
      theme_bw() +
      labs(x='Year', y=ylab) +
      scale_color_manual(values=c('red', 'blue'))

  }
  p
}

plot_C_overall <- function(MMSE, units=c('1000 lb', 'mt'),
                           type=c('overall', 'byfleet'),
                           stat=c('mean', 'median'),
                           incquants=TRUE) {

  units <- match.arg(units)
  type <- match.arg(type)
  stat <- match.arg(stat)

  if (type=='byfleet') {
    byfleet <- TRUE
  } else {
    byfleet <- FALSE
  }

  if (!inherits(MMSE, 'MMSE'))
    stop('Object must be class `MMSE`')

  if (!inherits(MMSE@multiHist, 'multiHist'))
    stop('MMSE@multiHist is empty. Re-run with `ProjectMOM(multiHist, ..., dropHist=FALSE)')

  info <- get_info_MMSE(MMSE)
  df_hist <- get_ts(MMSE@multiHist, info=info)  %>%
    filter(Name%in%c('Landings', 'Discards'))

  if (!byfleet) {
    df_hist <- df_hist %>% group_by(Stock, Year, Name) %>%
      mutate(Value=sum(Value))
  }

  df_hist$MP <- MMSE@MPs[[1]][1]
  df_proj <- get_ts_MMSE(MMSE, byfleet) %>%
    filter(Name%in%c('Landings', 'Discards'))

  var_stat <- switch(stat, mean='Mean', median='Median')
  df_proj$Value <- df_proj[[var_stat]]

  df <- bind_rows(df_hist, df_proj)

  df$Stock <- factor(df$Stock, levels=unique(df$Stock), ordered = TRUE)
  df$Name <- factor(df$Name, levels=unique(df$Name), ordered = TRUE)
  df$Fleet <- factor(df$Fleet, levels=unique(df$Fleet), ordered = TRUE)

  if (units=='1000 lb') {
    df$Value <- df$Value/1000
    df$Upper <- df$Upper/1000
    df$Lower <- df$Lower/1000
  }
  if (units=='mt') {
    df$Value <- lb2mt(df$Value)
    df$Upper <- lb2mt(df$Upper)
    df$Lower <- lb2mt(df$Lower)
  }
  ylab <- units

  if (byfleet) {
    p <- ggplot(df, aes(x=Year, y=Value,
                         color=Fleet, linetype=MP)) +
      facet_grid(Stock~Name, scales='free_y') +
      geom_line() +
      theme_bw() +
      labs(x='Year', y=ylab)
  } else {
    p <-   ggplot(df, aes(x=Year, y=Value, color=Stock, linetype=MP)) +
      facet_grid(Stock~Name) +
      geom_line() +
      theme_bw() +
      labs(x='Year', y=ylab) +
      scale_color_manual(values=c('red', 'blue'))
  }

  p

}

make_proj_df <- function(MMSE, sl) {
  info <- get_info_MMSE(MMSE)
  nstocks <- length(info$stocks)
  val <- slot(MMSE, sl)
  mps <- MMSE@MPs[[1]]
  df <- data.frame(Sim=1:info$nsim,
                   Stock=rep(info$stocks, each=info$nsim),
                   MP=rep(mps, each=info$nsim*nstocks),
                   Year=rep(info$p.yrs, each=info$nsim*nstocks*length(mps)),
                   Var=sl,
                   Val=as.vector(val))

  df

}




plot_B_proj <- function(MMSE, units=c('1000 lb', 'mt'),
                   type=c('abs', 'rel'),
                   stat=c('mean', 'median', 'all'),
                   sims=NULL,
                   mps=NA,
                   incyears=NULL,
                   maxyr,
                   incquants=TRUE,
                   incRef=TRUE) {
  units <- match.arg(units)
  type <- match.arg(type)
  stat <- match.arg(stat)

  ylab <- paste0('Biomass (', units, ')')

  # historical
  multiHist <- MMSE@multiHist

  info <- get_info(multiHist)
  df <- get_ts(multiHist) %>% filter(Name=='Biomass', Fleet=='cHL')
  df$Sim <- 1
  df$MP <- ''
  df$Phase <- 'Historical'
  bio <- df %>% filter(Name=='Biomass', Fleet==info$fleets[1])
  bio <- left_join(bio, info$Ref, by='Stock')
  bio$Stock <- factor(bio$Stock, levels=unique(bio$Stock), ordered = TRUE)

  # projection
  proj_info <- get_info_MMSE(MMSE)
  proj_df <- make_proj_df(MMSE, sl)
  proj_df$Phase <- 'Projection'

  # combine df
  df <- df %>% select(Year, Sim, Stock, Val=Value, MP, Name, Phase)

  proj_df <- proj_df %>% select(Year, Sim, Stock, Val, MP, Name=Var,Phase)
  proj_df$Name <- switch(proj_df$Name[1],
                        B='Biomass')

  # add last historical year
  df_lstyr <- df %>% filter(Year==max(df$Year))
  mm <- unique(proj_df$MP)
  templist <- list()
  for (i in 1:length(mm)) {
    tt <- df_lstyr
    tt$MP <- mm[i]
    templist[[i]] <- tt
  }
  df_lstyr <- do.call('rbind', templist)
  ss <- unique(proj_df$Sim)
  for (i in 1:length(ss)) {
    tt <- df_lstyr
    tt$Sim <- ss[i]
    templist[[i]] <- tt
  }
  df_lstyr <- do.call('rbind', templist)

  proj_df <- rbind(df_lstyr, proj_df)

  MPs <- MMSE@MPs[[1]]
  if (all(is.na(mps))) mps <- 1:length(MPs)

  if (!is.null(incyears)) {
    incyears <- c(2019, incyears)
    all.year <- unique(proj_df$Year)
    ig.year <- all.year[!all.year%in%incyears]
    proj_df$Val[proj_df$Year %in% ig.year] <- NA
  }
  proj_df  <- proj_df%>% filter(MP%in%MPs[mps])

  df$Val <- df$Val/1000
  proj_df$Val <- proj_df$Val/1000

  # plot by simulation
  if (!is.null(sims)) {
    proj_df2 <- proj_df %>% filter(Sim%in%sims)

    p <- ggplot(proj_df2) +
      geom_line(aes(x=Year, y=Val, color=Stock), data=df, size=1.2) +
      geom_line(aes(x=Year, y=Val, color=Stock, linetype=MP,
                    group=interaction(Sim, Stock, MP)),size=0.5) +
      expand_limits(y=c(0,1)) +
      theme_bw() +
      labs(x='Year', y=ylab) +
      scale_color_manual(values=c('red', 'blue')) +
      guides(color='none', linetype='none')
    lst.sim <- max(sims)

    p <- p +
      geom_line(aes(x=Year, y=Val, color=Stock, linetype=MP,
                    group=interaction(Sim, Stock, MP)), size=1.2,
                data=proj_df2%>%filter(Sim==lst.sim))

  } else {
    # summarise
    proj_df3 <- proj_df %>% group_by(Year,MP, Stock) %>%
      filter(is.na(Val)==FALSE) %>%
      summarise(Median=median(Val),
                Lower=quantile(Val,0.05),
                Upper=quantile(Val, 0.95))

    p <- ggplot(proj_df3) +
      geom_line(aes(x=Year, y=Val, color=Stock), data=df, size=1.2)
    if (incquants)
      p <- p +  geom_ribbon(aes(x=Year, ymin=Lower, ymax=Upper, fill=Stock,
                      linetype=MP,
                      group=interaction(Stock, MP)), alpha=0.2)
    p <- p +  geom_line(aes(x=Year, y=Median, color=Stock, linetype=MP,
                    group=interaction(Stock, MP)),
                size=0.8) +
      expand_limits(y=c(0,1)) +
      theme_bw() +
      labs(x='Year', y=ylab) +
      scale_color_manual(values=c('red', 'blue')) +
      scale_fill_manual(values=c('red', 'blue')) +
      guides(color='none', linetype='none', fill='none')

  }



  p
}


