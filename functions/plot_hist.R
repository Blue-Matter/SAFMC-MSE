

plot_C <- function(multiHist, type=c('overall', 'byfleet')) {
  info <- get_info(multiHist)
  df <- get_ts(multiHist)
  type <- match.arg(type)

  bio <- df %>% filter(Name%in%c('Landings', 'Discards'))
  bio$Name <- factor(bio$Name, levels=unique(bio$Name), ordered = TRUE)
  bio$Fleet <- factor(bio$Fleet, levels=unique(bio$Fleet), ordered = TRUE)
  bio$Value <- bio$Value/1000

  if (type=='byfleet') {
    p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
      facet_grid(Fleet~Name, scales='free_y') +
      geom_line() +
      theme_bw() +
      labs(x='Year', y='1000 lb') +
      scale_color_manual(values=c('blue', 'red'))
  } else {
    bio <- bio %>% group_by(Stock, Year, Name) %>%
      summarize(Value=sum(Value), .groups = 'drop')
    p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
      facet_grid(~Name, scales='free_y') +
      geom_line() +
      theme_bw() +
      labs(x='Year', y='1000 lb') +
      scale_color_manual(values=c('blue', 'red'))
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
  bio <- bio %>% tidyr::pivot_longer(cols=c(SBref, MSST))
  if (type=='rel') {
    bio <- bio %>% group_by(Stock) %>% mutate(Value=Value/SB0, value=value/SB0)
    ylab <- 'Relative Spawning Biomass (SB/SB0)'
  } else {
    ylab <- 'Spawning Biomass (mt and Eggs (1E8))'
  }

  p <- ggplot(bio, aes(x=Year, y=Value, color=Stock)) +
    geom_line() +
    facet_wrap(~Stock, scales='free_y') +
    expand_limits(y=c(0,1)) +
    theme_bw() +
    labs(x='Year', y=ylab) +
    scale_color_manual(values=c('blue', 'red'))
  if (incRef) {
    p <- p + geom_line(aes(y=value, color=Stock, linetype=name)) +
      scale_linetype_manual(values=c(2,3)) +
      labs(linetype='SBref')
  }
  p
}

plot_B <- function(multiHist, units=c('1000 lb', 'mt'),
                   type=c('abs', 'rel'),
                   incRef=TRUE) {
  units <- match.arg(units)
  type <- match.arg(type)
  info <- get_info(multiHist)
  df <- get_ts(multiHist)
  bio <- df %>% filter(Name=='Biomass', Fleet==info$fleets[1])
  bio <- left_join(bio, info$Ref, by='Stock')
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
    geom_line() +
    expand_limits(y=c(0,1)) +
    theme_bw() +
    labs(x='Year', y=ylab) +
    scale_color_manual(values=c('blue', 'red'))
  if (incRef) {
    p <- p + geom_line(aes(y=Bref, color=Stock, linetype=Bref_type)) +
      scale_linetype_manual(values=c(2,3)) +
      labs(linetype='Bref')
  }
  p
}


get_info <- function(multiHist) {
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
  out
}

get_ts <- function(multiHist) {
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
  df
}

