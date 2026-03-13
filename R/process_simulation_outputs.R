#' Aggregate replication outputs and build the simulation result list
#'
#' Takes the raw list of per-replication results from \code{parLapply} and
#' produces the full named \code{combo} list of 73 items returned by
#' \code{run_simulation()}.  This is a pure function: no Shiny calls, no
#' parallel operations, no file I/O.
#'
#' @param outputs list. Raw output from \code{parLapply(run_single_replication)}.
#' @param syst_names_single character vector. Node name strings (column 2 of
#'   \code{syst_names}).
#' @param time_unit character. Label appended to time-based metric names.
#' @param ptm proc_time. Timer started at the beginning of \code{run_simulation()}.
#' @param cap_cal_input_original data.frame. Capacity calendar before warm-up shift.
#' @param arr_cal_input_original data.frame. Arrival calendar before warm-up shift.
#'
#' @returns A named list of 73 simulation output elements (the \code{combo} object).
#' @noRd
process_simulation_outputs <- function(outputs, syst_names_single, time_unit,
                                       ptm, cap_cal_input_original,
                                       arr_cal_input_original) {
  ########## END OF SIMULATTION CODE ##########

  #### PLOTS AND SIMULATION LEVEL METRICS #########
  logger::log_debug("Making plots and simulation-level metrics.")
  nodes <- outputs[[1]][[1]]
  warm_up <- outputs[[1]][[2]]
  sim_time <- outputs[[1]][[3]]
  reps <- outputs[[1]][[4]]
  exits <- outputs[[1]][[5]]
  syst_names <- outputs[[1]][[6]]
  delay_list <- outputs[[1]][[23]]
  cap_cal_input <- outputs[[1]][[24]]
  arr_cal_input <- outputs[[1]][[25]]

  node_names <- syst_names[nodes, ]
  node_names <- rbind(node_names, c(NA, NA))
  rownames(node_names) <- c()

  node_wait <- sapply(outputs, function(x) {
    x[7]
  })
  node_active_service <- sapply(outputs, function(x) {
    x[8]
  })
  node_length_of_stay <- sapply(outputs, function(x) {
    x[9]
  })
  node_delay_to_transfer <- sapply(outputs, function(x) {
    x[10]
  })
  pat_wait <- sapply(outputs, function(x) {
    x[11]
  })
  pat_active_service <- sapply(outputs, function(x) {
    x[12]
  })
  pat_length_of_stay <- sapply(outputs, function(x) {
    x[13]
  })
  pat_delay_to_transfer <- sapply(outputs, function(x) {
    x[14]
  })
  total_time_in_system <- sapply(outputs, function(x) {
    x[15]
  })
  rejected <- sapply(outputs, function(x) {
    x[16]
  })
  ptd <- sapply(outputs, function(x) {
    x[17]
  })
  ptq <- sapply(outputs, function(x) {
    x[18]
  })
  pto <- sapply(outputs, function(x) {
    x[19]
  })
  ptt <- sapply(outputs, function(x) {
    x[20]
  })
  ptb <- sapply(outputs, function(x) {
    x[21]
  })
  multi_spread_uniform <- sapply(outputs, function(x) {
    x[22]
  })

  node_capacity_delay <- sapply(outputs, function(x) {
    x[26]
  })
  node_transition_delay <- sapply(outputs, function(x) {
    x[27]
  })

  pat_capacity_delay <- sapply(outputs, function(x) {
    x[28]
  })
  pat_transition_delay <- sapply(outputs, function(x) {
    x[29]
  })

  rm(outputs)


  ### Create the Simulation Summary Metrics ######################################################

  for (rep_fill in 1:reps) {
    if (!is.na(node_wait[[rep_fill]]$rep[1])) {
      node_wait[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(node_active_service[[rep_fill]]$rep[1])) {
      node_active_service[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(node_capacity_delay[[rep_fill]]$rep[1])) {
      node_capacity_delay[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(node_transition_delay[[rep_fill]]$rep[1])) {
      node_transition_delay[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(node_length_of_stay[[rep_fill]]$rep[1])) {
      node_length_of_stay[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(node_delay_to_transfer[[rep_fill]]$rep[1])) {
      node_delay_to_transfer[[rep_fill]]$rep <- rep_fill
    }

    if (!is.na(pat_wait[[rep_fill]]$rep[1])) {
      pat_wait[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(pat_active_service[[rep_fill]]$rep[1])) {
      pat_active_service[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(pat_capacity_delay[[rep_fill]]$rep[1])) {
      pat_capacity_delay[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(pat_transition_delay[[rep_fill]]$rep[1])) {
      pat_transition_delay[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(pat_length_of_stay[[rep_fill]]$rep[1])) {
      pat_length_of_stay[[rep_fill]]$rep <- rep_fill
    }
    if (!is.na(pat_delay_to_transfer[[rep_fill]]$rep[1])) {
      pat_delay_to_transfer[[rep_fill]]$rep <- rep_fill
    }

    if (!is.na(total_time_in_system[[rep_fill]]$rep[1])) {
      total_time_in_system[[rep_fill]]$rep <- rep_fill
    }

    if (!is.na(rejected[[rep_fill]][1, 1])) {
      rejected[[rep_fill]]$rep <- rep_fill
    }

    if (!is.na(multi_spread_uniform[[rep_fill]][1, 1])) {
      multi_spread_uniform[[rep_fill]]$rep <- paste0("rep ", rep_fill)
    }
  }



  node_wait <- data.table::rbindlist(node_wait)
  node_active_service <- data.table::rbindlist(node_active_service)
  node_capacity_delay <- data.table::rbindlist(node_capacity_delay)
  node_transition_delay <- data.table::rbindlist(node_transition_delay)
  node_length_of_stay <- data.table::rbindlist(node_length_of_stay)
  node_delay_to_transfer <- data.table::rbindlist(node_delay_to_transfer)

  pat_wait <- data.table::rbindlist(pat_wait)
  pat_active_service <- data.table::rbindlist(pat_active_service)
  pat_capacity_delay <- data.table::rbindlist(pat_capacity_delay)
  pat_transition_delay <- data.table::rbindlist(pat_transition_delay)
  pat_length_of_stay <- data.table::rbindlist(pat_length_of_stay)
  pat_delay_to_transfer <- data.table::rbindlist(pat_delay_to_transfer)

  total_time_in_system <- data.table::rbindlist(total_time_in_system)
  rejected <- data.table::rbindlist(rejected)

  node_wait_summary <-
    node_wait %>%
    group_by(node) %>%
    summarise(
      metric = "wait",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  node_active_service_summary <-
    node_active_service %>%
    group_by(node) %>%
    summarise(
      metric = "active_service",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  node_capacity_delay_summary <-
    node_capacity_delay %>%
    group_by(node) %>%
    summarise(
      metric = "capacity_delay",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  node_transition_delay_summary <-
    node_transition_delay %>%
    group_by(node) %>%
    summarise(
      metric = "transition_delay",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  node_length_of_stay_summary <-
    node_length_of_stay %>%
    group_by(node) %>%
    summarise(
      metric = "length_of_stay",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  node_delay_to_transfer_summary <-
    node_delay_to_transfer %>%
    group_by(node) %>%
    summarise(
      metric = "delay_to_transfer",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()

  pat_wait_summary <-
    pat_wait %>%
    summarise(
      metric = "wait",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  pat_active_service_summary <-
    pat_active_service %>%
    summarise(
      metric = "active_service",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  pat_capacity_delay_summary <-
    pat_capacity_delay %>%
    summarise(
      metric = "capacity_delay",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  pat_transition_delay_summary <-
    pat_transition_delay %>%
    summarise(
      metric = "transition_delay",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  pat_length_of_stay_summary <-
    pat_length_of_stay %>%
    summarise(
      metric = "length_of_stay",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()
  pat_delay_to_transfer_summary <-
    pat_delay_to_transfer %>%
    summarise(
      metric = "delay_to_transfer",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()

  total_time_in_system_summary <-
    total_time_in_system %>%
    summarise(
      metric = "total_time_in_system",
      mean = mean(mean, na.rm = T),
      sd = mean(sd, na.rm = T),
      iqr = mean(iqr, na.rm = T),
      percentile_95 = mean(percentile_95, na.rm = T)
    ) %>%
    as.data.frame()

  pat_rep_summary <-
    rbind(
      pat_wait,
      pat_active_service,
      pat_capacity_delay,
      pat_transition_delay,
      pat_length_of_stay,
      pat_delay_to_transfer
    )
  pat_total_summary <-
    rbind(
      pat_wait_summary,
      pat_active_service_summary,
      pat_capacity_delay_summary,
      pat_transition_delay_summary,
      pat_length_of_stay_summary,
      pat_delay_to_transfer_summary
    )


  rejected_summary <-
    rejected %>%
    group_by(node) %>%
    summarise(mean = mean(mean)) %>%
    as.data.frame()

  ### Create the delayed metrics ######################################################


  # Calculating the time at each delayed length##


  ptd_total <- as.data.frame(data.table::rbindlist(ptd))
  rownames(ptd_total) <- c()
  ptd_total$delayed <- as.numeric(as.character(ptd_total$delayed))
  ptd_total$time_at_delayed_level <-
    as.numeric(as.character(ptd_total$time_at_delayed_level))
  ptd_total <- ptd_total[, -4]

  ptd_time <-
    ptd_total %>%
    group_by(node, delayed) %>%
    mutate(
      time_at_delayed_level =
        sum(time_at_delayed_level) / reps
    )
  ptd_time <- as.data.frame(ptd_time)
  ptd_time <- unique(ptd_time)
  ptd_time$node <- as.numeric(as.character(ptd_time$node))
  ptd_time <- ptd_time[order(ptd_time$node, ptd_time$delayed), ]

  ptd_percent <-
    ptd_time %>%
    group_by(node) %>%
    transmute(
      delayed,
      percent_time_at_delayed_level = 100 * time_at_delayed_level / sum(time_at_delayed_level)
    )
  ptd_percent <-
    ptd_percent %>%
    group_by(node) %>%
    transmute(
      delayed,
      percent_time_at_delayed_level,
      cumulative_percent_time_at_delayed_level = cumsum(percent_time_at_delayed_level)
    )
  ptd_percent <- as.data.frame(ptd_percent)
  ptd_percent <- unique(ptd_percent)
  ptd_percent$node <- as.numeric(as.character(ptd_percent$node))
  ptd_percent <-
    ptd_percent[order(ptd_percent$node, ptd_percent$delayed), ]
  ptd_percent$node <- as.factor(ptd_percent$node)
  ptd_percent$node <-
    syst_names_single[as.numeric(as.character(ptd_percent$node))]
  ptd_percent$node <- as.factor(ptd_percent$node)


  ptd_percent$node <-
    factor(x = ptd_percent$node, levels = syst_names_single)

  ptd_plot <-
    ggplot(
      data = ptd_percent %>% mutate(node = str_replace_all(node, pattern = "_", replacement = " ")),
      aes(x = delayed, y = percent_time_at_delayed_level, fill = node)
    ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(node ~ ., labeller = label_wrap_gen(15)) +
    theme_bw() +
    # geom_text(aes(label=ifelse(signif(x = ptd_percent$percent_time_at_delayed_level,digits = 3)<100,signif(x = ptd_percent$percent_time_at_delayed_level,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
    xlab("# concurrently delayed") +
    ylab("% time at delayed level") +
    theme(legend.position = "none")

  if (max(ptd_percent$delayed) == 1) {
    ptd_plot <-
      ptd_plot + scale_x_discrete(limits = c(min(ptd_percent$delayed), max(ptd_percent$delayed)))
  }

  # ptd_plot

  # Delay Percentiles##

  dpercentiles <- matrix(nrow = length(nodes), ncol = 8)

  for (i in as.numeric(nodes)) {
    if (length(unique(
      ptd_percent$cumulative_percent_time_at_delayed_level[which(ptd_percent$node ==
        syst_names_single[i])]
    )) >= 2) {
      tmp <-
        approx(
          x = ptd_percent$cumulative_percent_time_at_delayed_level[which(ptd_percent$node ==
            syst_names_single[i])],
          y = ptd_percent$delayed[which(ptd_percent$node == syst_names_single[i])],
          xout = c(50, 80, 85, 90, 95, 99, 100),
          ties = min,
          rule = 2
        )
      tmp$y <- round(tmp$y, digits = 2)
      dpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], as.numeric(tmp$y))
    } else if (length(unique(
      ptd_percent$cumulative_percent_time_at_delayed_level[which(ptd_percent$node ==
        syst_names_single[i])]
    )) == 0) {
      dpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = NA, times = 7))
    } else {
      dpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = 0, times = 7))
    }
  }
  colnames(dpercentiles) <-
    c(
      "node",
      "50th",
      "80th",
      "85th",
      "90th",
      "95th",
      "99th",
      "100th"
    )

  # Calculating the average delayed per node per replicate & then over the simulation per node##
  #

  # cl<-parallel::makeCluster(17)
  # parallel::clusterExport(cl = cl,varlist = c("ptd","nodes","node_names"))

  avg_delayed <- lapply(
    X = ptd,
    FUN = function(ptd) {
      tmp <-
        ptd %>%
        group_by(node) %>%
        summarise(avg_delayed = sum(delayed * time_at_delayed_level) /
          sum(time_at_delayed_level)) %>%
        as.data.frame()
      tmp$node <- node_names[tmp$node, 2]
      tmp
    }
  )
  # parallel::stopCluster(cl)


  avg_delayed_summary <-
    ptd_time %>%
    group_by(node) %>%
    summarise(avg_delayed = sum(delayed * time_at_delayed_level) /
      sum(time_at_delayed_level)) %>%
    as.data.frame()
  avg_delayed_summary$node <- node_names[avg_delayed_summary$node, 2]


  # avg_delayed<-data.frame(abind(avg_delayed,along = 1))
  # avg_delayed$avg_delayed<-as.numeric(as.character(avg_delayed$avg_delayed))

  # rm(tmp_b_length,tmp_b_time,results,avg_b)




  ### Create the queue metrics ######################################################


  # Calculating the time at each queue length##



  ptq_total <- as.data.frame(data.table::rbindlist(ptq))
  rownames(ptq_total) <- c()
  ptq_total$queue <- as.numeric(as.character(ptq_total$queue))
  ptq_total$time_at_queue_length <-
    as.numeric(as.character(ptq_total$time_at_queue_length))
  ptq_total <- ptq_total[, -4]

  ptq_time <-
    ptq_total %>%
    group_by(node, queue) %>%
    mutate(time_at_queue_length = sum(time_at_queue_length) /
      reps)
  ptq_time <- as.data.frame(ptq_time)
  ptq_time <- unique(ptq_time)
  ptq_time$node <- as.numeric(as.character(ptq_time$node))
  ptq_time <- ptq_time[order(ptq_time$node, ptq_time$queue), ]

  ptq_percent <-
    ptq_time %>%
    group_by(node) %>%
    transmute(
      queue,
      percent_time_at_queue_length = 100 * time_at_queue_length / sum(time_at_queue_length)
    )
  ptq_percent <-
    ptq_percent %>%
    group_by(node) %>%
    transmute(
      queue,
      percent_time_at_queue_length,
      cumulative_percent_time_at_queue_length = cumsum(percent_time_at_queue_length)
    )
  ptq_percent <- as.data.frame(ptq_percent)
  ptq_percent <- unique(ptq_percent)
  ptq_percent$node <- as.numeric(as.character(ptq_percent$node))
  ptq_percent <-
    ptq_percent[order(ptq_percent$node, ptq_percent$queue), ]
  ptq_percent$node <- as.factor(ptq_percent$node)
  ptq_percent$node <-
    syst_names_single[as.numeric(as.character(ptq_percent$node))]
  ptq_percent$node <- as.factor(ptq_percent$node)

  ptq_percent$node <-
    factor(x = ptq_percent$node, levels = syst_names_single)

  ptq_plot <-
    ggplot(
      data = ptq_percent %>% mutate(node = str_replace_all(node, pattern = "_", replacement = " ")),
      aes(x = queue, y = percent_time_at_queue_length, fill = node)
    ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(node ~ ., scales = "free", labeller = label_wrap_gen(15)) +
    theme_bw() +
    xlab("# in queue") +
    ylab("% time at queue level") +
    theme(legend.position = "none")

  if (max(ptq_percent$queue) == 1) {
    ptq_plot <-
      ptq_plot + scale_x_discrete(limits = c(min(ptq_percent$queue), max(ptq_percent$queue)))
  }

  # ptq_plot




  # Queue Percentiles##

  qpercentiles <- matrix(nrow = length(nodes), ncol = 8)

  for (i in as.numeric(nodes)) {
    if (length(unique(
      ptq_percent$cumulative_percent_time_at_queue_length[which(ptq_percent$node ==
        syst_names_single[i])]
    )) >= 2) {
      tmp <-
        approx(
          x = ptq_percent$cumulative_percent_time_at_queue_length[which(ptq_percent$node ==
            syst_names_single[i])],
          y = ptq_percent$queue[which(ptq_percent$node == syst_names_single[i])],
          xout = c(50, 80, 85, 90, 95, 99, 100),
          ties = min,
          rule = 2
        )

      tmp$y <- round(tmp$y, digits = 2)

      qpercentiles[as.numeric(i), ] <- c(syst_names_single[i], tmp$y)
    } else if (length(unique(
      ptq_percent$cumulative_percent_time_at_queue_length[which(ptq_percent$node ==
        syst_names_single[i])]
    )) == 0) {
      qpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = NA, times = 7))
    } else {
      qpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = 0, times = 7))
    }
  }
  colnames(qpercentiles) <-
    c(
      "node",
      "50th",
      "80th",
      "85th",
      "90th",
      "95th",
      "99th",
      "100th"
    )



  # Calculating the average queue per node per replicate & then over the simulation per node##
  #

  # cl <- parallel::makeCluster(17)
  # parallel::clusterExport(cl = cl,varlist = c("ptq","nodes","node_names"))

  avg_queue <- lapply(
    X = ptq,
    FUN = function(ptq) {
      tmp <-
        ptq %>%
        group_by(node) %>%
        summarise(avg_queue = sum(queue * time_at_queue_length) /
          sum(time_at_queue_length)) %>%
        as.data.frame()
      tmp$node <- node_names[tmp$node, 2]
      tmp
    }
  )
  # parallel::stopCluster(cl)


  avg_queue_summary <-
    ptq_time %>%
    group_by(node) %>%
    summarise(avg_queue = sum(queue * time_at_queue_length) /
      sum(time_at_queue_length)) %>%
    as.data.frame()
  avg_queue_summary$node <- node_names[avg_queue_summary$node, 2]



  ### Create the occupancy metrics ###############################################


  # Calculating the time at each occupancy##


  pto_total <- as.data.frame(data.table::rbindlist(pto))
  rownames(pto_total) <- c()
  pto_total$occupancy <-
    as.numeric(as.character(pto_total$occupancy))
  pto_total$time_at_occupancy <-
    as.numeric(as.character(pto_total$time_at_occupancy))
  pto_total <- pto_total[, -4]

  pto_time <-
    pto_total %>%
    group_by(node, occupancy) %>%
    mutate(time_at_occupancy = sum(time_at_occupancy) /
      reps)
  pto_time <- as.data.frame(pto_time)
  pto_time <- unique(pto_time)
  pto_time$node <- as.numeric(as.character(pto_time$node))
  pto_time <- pto_time[order(pto_time$node, pto_time$occupancy), ]

  pto_percent <-
    pto_time %>%
    group_by(node) %>%
    transmute(
      occupancy,
      percent_time_at_occupancy = 100 * time_at_occupancy / sum(time_at_occupancy)
    )
  pto_percent <-
    pto_percent %>%
    group_by(node) %>%
    transmute(
      occupancy,
      percent_time_at_occupancy,
      cumulative_percent_time_at_occupancy = cumsum(percent_time_at_occupancy)
    )
  pto_percent <- as.data.frame(pto_percent)
  pto_percent <- unique(pto_percent)
  pto_percent$node <- as.numeric(as.character(pto_percent$node))
  pto_percent <-
    pto_percent[order(pto_percent$node, pto_percent$occupancy), ]
  pto_percent$node <- as.factor(pto_percent$node)
  pto_percent$node <-
    syst_names_single[as.numeric(as.character(pto_percent$node))]
  pto_percent$node <- as.factor(pto_percent$node)

  pto_percent$node <-
    factor(x = pto_percent$node, levels = syst_names_single)

  pto_plot <-
    ggplot(
      data = pto_percent %>% mutate(node = str_replace_all(node, pattern = "_", replacement = " ")),
      aes(x = occupancy, y = percent_time_at_occupancy, fill = node)
    ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(node ~ ., labeller = label_wrap_gen(15)) +
    theme_bw() +
    # geom_text(aes(label=ifelse(signif(x = pto_percent$percent_time_at_occupancy,digits = 3)<100,signif(x = pto_percent$percent_time_at_occupancy,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
    xlab("Patient Occupancy") +
    ylab("% time at patient occupancy level") +
    theme(legend.position = "none")

  if (max(pto_percent$occupancy) == 1) {
    pto_plot <-
      pto_plot + scale_x_discrete(limits = c(
        min(pto_percent$occupancy),
        max(pto_percent$occupancy)
      ))
  }

  # pto_plot




  # Occupancy Percentiles##

  opercentiles <- matrix(nrow = length(nodes), ncol = 8)

  for (i in as.numeric(nodes)) {
    if (length(unique(pto_percent$cumulative_percent_time_at_occupancy[which(pto_percent$node ==
      syst_names_single[i])])) >= 2) {
      tmp <-
        approx(
          x = pto_percent$cumulative_percent_time_at_occupancy[which(pto_percent$node ==
            syst_names_single[i])],
          y = pto_percent$occupancy[which(pto_percent$node == syst_names_single[i])],
          xout = c(50, 80, 85, 90, 95, 99, 100),
          ties = min,
          rule = 2
        )
      tmp$y <- round(tmp$y, digits = 2)
      opercentiles[as.numeric(i), ] <- c(syst_names_single[i], tmp$y)
    } else if (length(unique(pto_percent$cumulative_percent_time_at_occupancy[which(pto_percent$node ==
      syst_names_single[i])])) == 2) {
      opercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = NA, times = 7))
    } else {
      opercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = 0, times = 7))
    }
  }
  colnames(opercentiles) <-
    c(
      "node",
      "50th",
      "80th",
      "85th",
      "90th",
      "95th",
      "99th",
      "100th"
    )


  # Calculating the average occupancy per node per replicate##

  # Calculating the average delayed per node per replicate & then over the simulation per node##


  # cl <- parallel::makeCluster(17)
  # parallel::clusterExport(cl = cl,varlist = c("pto","nodes","node_names"))

  avg_occupancy <- lapply(
    X = pto,
    FUN = function(pto) {
      tmp <-
        pto %>%
        group_by(node) %>%
        summarise(avg_occupancy = sum(occupancy * time_at_occupancy) /
          sum(time_at_occupancy)) %>%
        as.data.frame()
      tmp$node <- node_names[tmp$node, 2]
      tmp
    }
  )
  # parallel::stopCluster(cl)


  avg_occupancy_summary <-
    pto_time %>%
    group_by(node) %>%
    summarise(avg_occupancy = sum(occupancy *
      time_at_occupancy) / sum(time_at_occupancy)) %>%
    as.data.frame()
  avg_occupancy_summary$node <-
    node_names[avg_occupancy_summary$node, 2]


  # rm(tmp_occupancy,tmp_o_time,results,avg_o)


  ### Create the transition metrics ######################################################


  # Calculating the time at each transition length##


  ptt_total <- as.data.frame(data.table::rbindlist(ptt))
  rownames(ptt_total) <- c()
  ptt_total$transition <-
    as.numeric(as.character(ptt_total$transition))
  ptt_total$time_at_transition_level <-
    as.numeric(as.character(ptt_total$time_at_transition_level))
  ptt_total <- ptt_total[, -4]

  ptt_time <-
    ptt_total %>%
    group_by(node, transition) %>%
    mutate(
      time_at_transition_level =
        sum(time_at_transition_level) / reps
    )
  ptt_time <- as.data.frame(ptt_time)
  ptt_time <- unique(ptt_time)
  ptt_time$node <- as.numeric(as.character(ptt_time$node))
  ptt_time <- ptt_time[order(ptt_time$node, ptt_time$transition), ]

  ptt_percent <-
    ptt_time %>%
    group_by(node) %>%
    transmute(
      transition,
      percent_time_at_transition_level = 100 * time_at_transition_level / sum(time_at_transition_level)
    )
  ptt_percent <-
    ptt_percent %>%
    group_by(node) %>%
    transmute(
      transition,
      percent_time_at_transition_level,
      cumulative_percent_time_at_transition_level = cumsum(percent_time_at_transition_level)
    )
  ptt_percent <- as.data.frame(ptt_percent)
  ptt_percent <- unique(ptt_percent)
  ptt_percent$node <- as.numeric(as.character(ptt_percent$node))
  ptt_percent <-
    ptt_percent[order(ptt_percent$node, ptt_percent$transition), ]
  ptt_percent$node <- as.factor(ptt_percent$node)
  ptt_percent$node <-
    syst_names_single[as.numeric(as.character(ptt_percent$node))]
  ptt_percent$node <- as.factor(ptt_percent$node)

  ptt_percent$node <-
    factor(x = ptt_percent$node, levels = syst_names_single)

  ptt_plot <-
    ggplot(
      data = ptt_percent %>% mutate(node = str_replace_all(node, pattern = "_", replacement = " ")),
      aes(x = transition, y = percent_time_at_transition_level, fill = node)
    ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(node ~ ., labeller = label_wrap_gen(15)) +
    theme_bw() +
    # geom_text(aes(label=ifelse(signif(x = ptt_percent$percent_time_at_transition_level,digits = 3)<100,signif(x = ptt_percent$percent_time_at_transition_level,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
    xlab("# concurrently in transition") +
    ylab("% time at transition level") +
    theme(legend.position = "none")

  if (max(ptt_percent$transition) == 1) {
    ptt_plot <-
      ptt_plot + scale_x_discrete(limits = c(
        min(ptt_percent$transition),
        max(ptt_percent$transition)
      ))
  }

  # ptt_plot

  # transition Percentiles##

  tpercentiles <- matrix(nrow = length(nodes), ncol = 8)

  for (i in as.numeric(nodes)) {
    if (length(unique(
      ptt_percent$cumulative_percent_time_at_transition_level[which(ptt_percent$node ==
        syst_names_single[i])]
    )) >= 2) {
      tmp <-
        approx(
          x = ptt_percent$cumulative_percent_time_at_transition_level[which(ptt_percent$node ==
            syst_names_single[i])],
          y = ptt_percent$transition[which(ptt_percent$node == syst_names_single[i])],
          xout = c(50, 80, 85, 90, 95, 99, 100),
          ties = min,
          rule = 2
        )
      tmp$y <- round(tmp$y, digits = 2)
      tpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], as.numeric(tmp$y))
    } else if (length(unique(
      ptt_percent$cumulative_percent_time_at_transition_level[which(ptt_percent$node ==
        syst_names_single[i])]
    )) == 0) {
      tpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = NA, times = 7))
    } else {
      tpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = 0, times = 7))
    }
  }
  colnames(tpercentiles) <-
    c(
      "node",
      "50th",
      "80th",
      "85th",
      "90th",
      "95th",
      "99th",
      "100th"
    )

  # Calculating the average transition per node per replicate##

  # cl <- parallel::makeCluster(17)
  # parallel::clusterExport(cl = cl,varlist = c("ptt","nodes","node_names"))

  avg_transition <- lapply(
    X = ptt,
    FUN = function(ptt) {
      tmp <-
        ptt %>%
        group_by(node) %>%
        summarise(
          avg_transition = sum(transition * time_at_transition_level) / sum(time_at_transition_level)
        ) %>%
        as.data.frame()
      tmp$node <- node_names[tmp$node, 2]
      tmp
    }
  )
  # parallel::stopCluster(cl)


  avg_transition_summary <-
    ptt_time %>%
    group_by(node) %>%
    summarise(
      avg_transition = sum(transition * time_at_transition_level) / sum(time_at_transition_level)
    ) %>%
    as.data.frame()
  avg_transition_summary$node <-
    node_names[avg_transition_summary$node, 2]




  #
  # #rm(tmp_t_length,tmp_t_time,results,avg_t)
  #


  ### Create the Bed Occupancy metrics #######################################################
  #

  #### % time at bed occupancy level###

  ptb_total <- as.data.frame(data.table::rbindlist(ptb))
  rownames(ptb_total) <- c()
  ptb_total$occ_bed <- as.numeric(as.character(ptb_total$occ_bed))
  ptb_total$time_at_occ_bed_level <-
    as.numeric(as.character(ptb_total$time_at_occ_bed_level))
  ptb_total <- ptb_total[, -4]

  ptb_time <-
    ptb_total %>%
    group_by(node, occ_bed) %>%
    mutate(
      time_at_occ_bed_level =
        sum(time_at_occ_bed_level) / reps
    )
  ptb_time <- as.data.frame(ptb_time)
  ptb_time <- unique(ptb_time)
  ptb_time$node <- as.numeric(as.character(ptb_time$node))
  ptb_time <- ptb_time[order(ptb_time$node, ptb_time$occ_bed), ]

  ptb_percent <-
    ptb_time %>%
    group_by(node) %>%
    transmute(
      occ_bed,
      percent_time_at_occ_bed_level = 100 * time_at_occ_bed_level / sum(time_at_occ_bed_level)
    )
  ptb_percent <-
    ptb_percent %>%
    group_by(node) %>%
    transmute(
      occ_bed,
      percent_time_at_occ_bed_level,
      cumulative_percent_time_at_occ_bed_level = cumsum(percent_time_at_occ_bed_level)
    )
  ptb_percent <- as.data.frame(ptb_percent)
  ptb_percent <- unique(ptb_percent)
  ptb_percent$node <- as.numeric(as.character(ptb_percent$node))
  ptb_percent <-
    ptb_percent[order(ptb_percent$node, ptb_percent$occ_bed), ]
  ptb_percent$node <- as.factor(ptb_percent$node)
  ptb_percent$node <-
    syst_names_single[as.numeric(as.character(ptb_percent$node))]
  ptb_percent$node <- as.factor(ptb_percent$node)

  ptb_percent$node <-
    factor(x = ptb_percent$node, levels = syst_names_single)

  ptb_plot <-
    ggplot(
      data = ptb_percent %>% mutate(node = str_replace_all(node, pattern = "_", replacement = " ")),
      aes(x = occ_bed, y = percent_time_at_occ_bed_level, fill = node)
    ) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(node ~ ., labeller = label_wrap_gen(15)) +
    theme_bw() +
    # geom_text(aes(label=ifelse(signif(x = ptb_percent$percent_time_at_occ_bed_level,digits = 3)<100,signif(x = ptb_percent$percent_time_at_occ_bed_level,digits = 2),"")),vjust=-0.5,position = position_dodge(width=0.9),  size=3)+ coord_cartesian(ylim = c(0,100))+
    xlab("Bed Occupancy") +
    ylab("% time at bed occupancy level") +
    theme(legend.position = "none")

  if (max(ptb_percent$occ_bed) == 1) {
    ptb_plot <-
      ptb_plot + scale_x_discrete(limits = c(min(ptb_percent$occ_bed), max(ptb_percent$occ_bed)))
  }

  # ptq_plot




  # Occ_Bed Percentiles##

  bpercentiles <- matrix(nrow = length(nodes), ncol = 8)

  for (i in as.numeric(nodes)) {
    if (length(unique(
      ptb_percent$cumulative_percent_time_at_occ_bed_level[which(ptb_percent$node ==
        syst_names_single[i])]
    )) >= 2) {
      tmp <-
        approx(
          x = ptb_percent$cumulative_percent_time_at_occ_bed_level[which(ptb_percent$node ==
            syst_names_single[i])],
          y = ptb_percent$occ_bed[which(ptb_percent$node == syst_names_single[i])],
          xout = c(50, 80, 85, 90, 95, 99, 100),
          ties = min,
          rule = 2
        )

      tmp$y <- round(tmp$y, digits = 2)

      bpercentiles[as.numeric(i), ] <- c(syst_names_single[i], tmp$y)
      # bpercentiles[as.numeric(i),]<-as.numeric(bpercentiles[as.numeric(i),])
    } else if (length(unique(
      ptb_percent$cumulative_percent_time_at_occ_bed_level[which(ptb_percent$node ==
        syst_names_single[i])]
    )) == 0) {
      bpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = NA, times = 7))
    } else {
      bpercentiles[as.numeric(i), ] <-
        c(syst_names_single[i], rep(x = 0, times = 7))
    }
  }
  colnames(bpercentiles) <-
    c(
      "node",
      "50th",
      "80th",
      "85th",
      "90th",
      "95th",
      "99th",
      "100th"
    )




  # Calculating the average occ_bed per node per replicate##
  #
  # cl <- parallel::makeCluster(17)
  # parallel::clusterExport(cl = cl,varlist = c("ptb","nodes","node_names"))

  avg_occ_bed <- lapply(
    X = ptb,
    FUN = function(ptb) {
      tmp <-
        ptb %>%
        group_by(node) %>%
        summarise(avg_occ_bed = sum(occ_bed * time_at_occ_bed_level) /
          sum(time_at_occ_bed_level)) %>%
        as.data.frame()
      tmp$node <- node_names[tmp$node, 2]
      tmp
    }
  )
  # parallel::stopCluster(cl)


  avg_occ_bed_summary <-
    ptb_time %>%
    group_by(node) %>%
    summarise(avg_occ_bed = sum(occ_bed * time_at_occ_bed_level) /
      sum(time_at_occ_bed_level)) %>%
    as.data.frame()
  avg_occ_bed_summary$node <- node_names[avg_occ_bed_summary$node, 2]



  ######  MULTI DATA TABLE ########################################################################
  logger::log_debug("Creating multi-data table.")
  multi_spread_uniform <- data.table::rbindlist(multi_spread_uniform)

  through_time_uniform <- multi_spread_uniform
  through_time_uniform$time <- through_time_uniform$time - warm_up

  rm(multi_spread_uniform)

  through_time_uniform_gather <-
    tidyr::gather(
      through_time_uniform,
      key = "metric",
      value = "value",
      occ_bed,
      delayed,
      occupancy,
      transition,
      queue
    )


  avg_through_time <- through_time_uniform_gather %>%
    group_by(time, node, metric) %>%
    summarise(
      mean = mean(value, na.rm = T),
      L99 = quantile(value, 0.005, na.rm = T),
      U99 = quantile(value, 0.995, na.rm = T),
      L95 = quantile(value, 0.025, na.rm = T),
      U95 = quantile(value, 0.975, na.rm = T),
      L50 = quantile(value, 0.25, na.rm = T),
      U50 = quantile(value, 0.75, na.rm = T)
    ) %>%
    as.data.frame()
  avg_through_time$metric <-
    factor(
      avg_through_time$metric,
      levels = c("queue", "occupancy", "occ_bed", "delayed", "transition")
    )


  avg_through_time$node <-
    factor(x = avg_through_time$node, levels = syst_names_single)

  avg_through_time_plot <- ggplot(avg_through_time %>% mutate(node = str_replace_all(node, pattern = "_", replacement = " "))) +
    geom_ribbon(aes(
      x = time,
      ymin = L99,
      ymax = U99,
      fill = "99%"
    ), alpha = 0.25) +
    geom_ribbon(aes(
      x = time,
      ymin = L95,
      ymax = U95,
      fill = "95%"
    ), alpha = 0.25) +
    geom_ribbon(aes(
      x = time,
      ymin = L50,
      ymax = U50,
      fill = "50%"
    ), alpha = 0.25) +
    scale_fill_manual(
      name = "Percentiles",
      values = c(
        "99%" = "grey75",
        "95%" = "grey60",
        "50%" = "grey45"
      ),
      breaks = c("99%", "95%", "50%")
    ) +
    geom_line(aes(
      x = time,
      y = mean,
      colour = metric
    ), linewidth = 1.1) +
    facet_grid(metric ~ node, scales = "free", labeller = label_wrap_gen(15)) +
    ylab("Mean # of patients") +
    xlab(paste0("Time (", time_unit, ")")) +
    theme_bw() +
    theme(
      panel.spacing = unit(1, "lines"),
      axis.text.x = element_text(size = 7),
      legend.position = "none"
    ) +
    expand_limits(y = 0)


  through_time_mini <-
    through_time_uniform_gather[which(
      through_time_uniform_gather$rep == "rep 1" |
        through_time_uniform_gather$rep == "rep 2" |
        through_time_uniform_gather$rep == "rep 3" |
        through_time_uniform_gather$rep == "rep 4" |
        through_time_uniform_gather$rep == "rep 5"
    ), ]
  through_time_mini$node <-
    factor(x = through_time_mini$node, levels = syst_names_single)

  total_in_system <-
    through_time_uniform_gather[which(
      through_time_uniform_gather$rep == "rep 1" |
        through_time_uniform_gather$rep == "rep 2" |
        through_time_uniform_gather$rep == "rep 3" |
        through_time_uniform_gather$rep == "rep 4" |
        through_time_uniform_gather$rep == "rep 5" |
        through_time_uniform_gather$rep ==
          "rep 6" |
        through_time_uniform_gather$rep == "rep 7" |
        through_time_uniform_gather$rep == "rep 8" |
        through_time_uniform_gather$rep == "rep 9" |
        through_time_uniform_gather$rep == "rep 10"
    ), ]
  total_in_system$node <-
    factor(x = total_in_system$node, levels = syst_names_single)
  total_in_system <-
    total_in_system[which(total_in_system$metric == "occupancy" |
      total_in_system$metric == "queue"), ]
  total_in_system <- total_in_system[, c(1, 3, 5)]

  total_in_system_dat <-
    total_in_system %>%
    group_by(time, rep) %>%
    summarise("value" = sum(value)) %>%
    as.data.frame()



  rm(through_time_uniform_gather)

  # the plot "o" ####
  o <-
    ggplot(data = through_time_mini[which(through_time_mini$metric == "occupancy"), ] %>%
      mutate(node = str_replace_all(node, pattern = "_", replacement = " "))) +
    geom_step(aes(x = time, y = value, col = node)) +
    facet_grid(node ~ rep, labeller = label_wrap_gen(15)) +
    theme_bw() +
    ylab("Occupancy") +
    theme(
      panel.spacing.x = unit(1, "lines"),
      axis.text.x = element_text(size = 7),
      legend.position = "none"
    ) +
    xlab(paste0("Time (", time_unit, ")"))

  # the plot "q" ####
  q <-
    ggplot(data = through_time_mini[which(through_time_mini$metric == "queue"), ] %>%
      mutate(node = str_replace_all(node, pattern = "_", replacement = " "))) +
    geom_step(aes(x = time, y = value, col = node)) +
    facet_grid(node ~ rep, labeller = label_wrap_gen(15)) +
    theme_bw() +
    ylab("Queue") +
    xlab(paste0("Time (", time_unit, ")")) +
    theme(
      panel.spacing.x = unit(1, "lines"),
      axis.text.x = element_text(size = 7),
      legend.position = "none"
    )

  # the plot "d" ####
  d <-
    ggplot(data = through_time_mini[which(through_time_mini$metric == "delayed"), ] %>%
      mutate(node = str_replace_all(node, pattern = "_", replacement = " "))) +
    geom_step(aes(x = time, y = value, col = node)) +
    facet_grid(node ~ rep, labeller = label_wrap_gen(15)) +
    theme_bw() +
    ylab("Delayed") +
    xlab(paste0("Time (", time_unit, ")")) +
    theme(
      panel.spacing.x = unit(1, "lines"),
      axis.text.x = element_text(size = 7),
      legend.position = "none"
    )

  # the plot "t" ####
  t <-
    ggplot(data = through_time_mini[which(through_time_mini$metric == "transition"), ] %>%
      mutate(node = str_replace_all(node, pattern = "_", replacement = " "))) +
    geom_step(aes(x = time, y = value, col = node)) +
    facet_grid(node ~ rep, labeller = label_wrap_gen(15)) +
    theme_bw() +
    ylab("Transition") +
    xlab(paste0("Time (", time_unit, ")")) +
    theme(
      panel.spacing.x = unit(1, "lines"),
      axis.text.x = element_text(size = 7),
      legend.position = "none"
    )

  # the plot "b" ####
  b <-
    ggplot(data = through_time_mini[which(through_time_mini$metric == "occ_bed"), ] %>%
      mutate(node = str_replace_all(node, pattern = "_", replacement = " "))) +
    geom_step(aes(x = time, y = value, col = node)) +
    facet_grid(node ~ rep, labeller = label_wrap_gen(15)) +
    theme_bw() +
    ylab("Bed Occupancy") +
    xlab(paste0("Time (", time_unit, ")")) +
    theme(
      panel.spacing.x = unit(1, "lines"),
      axis.text.x = element_text(size = 7),
      legend.position = "none"
    )

  # the plot "tisp" ####
  tisp <-
    ggplot(data = total_in_system_dat) +
    geom_line(aes(x = time, y = value, group = rep),
      col = "black",
      alpha = 0.4
    ) +
    theme_bw() +
    ylab("Total in System")



  # [which(avg_through_time$metric=="occupancy"),]




  # SIMULATION OUTPUT OBJECT LIST "combo" ####
  # time units added to selected metric names in tables below ####
  # these will be overwritten directly to remove underscores for the on-screen shiny outputs in some cases
  # but will still pull through to the excel file download in the format below
  logger::log_debug("Creating simulation output list (combo).")

  combo <- list(
    total_time_in_system = total_time_in_system %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    total_time_in_system_summary = total_time_in_system_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    node_wait = node_wait %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    node_wait_summary = node_wait_summary %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    pat_wait = pat_wait %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    pat_wait_summary = pat_wait_summary %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    node_active_service = node_active_service %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    node_active_service_summary = node_active_service_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    pat_active_service = pat_active_service %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    pat_active_service_summary = pat_active_service_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    node_length_of_stay = node_length_of_stay %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    node_length_of_stay_summary = node_length_of_stay_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    pat_length_of_stay = pat_length_of_stay %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    pat_length_of_stay_summary = pat_length_of_stay_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    node_delay_to_transfer = node_delay_to_transfer %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    node_delay_to_transfer_summary = node_delay_to_transfer_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    pat_delay_to_transfer = pat_delay_to_transfer %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    pat_delay_to_transfer_summary = pat_delay_to_transfer_summary %>% mutate(
      metric =
        paste0(metric, " (", time_unit, ")")
    ),
    pat_rep_summary = pat_rep_summary %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    pat_total_summary = pat_total_summary %>% mutate(metric = paste0(metric, " (", time_unit, ")")),
    ptd_percent = ptd_percent,
    ptd_plot = ptd_plot,
    avg_delayed = avg_delayed,
    avg_delayed_summary = avg_delayed_summary,
    d = d,
    ptq_percent = ptq_percent,
    ptq_plot = ptq_plot,
    avg_queue = avg_queue,
    avg_queue_summary = avg_queue_summary,
    q = q,
    pto_percent = pto_percent,
    pto_plot = pto_plot,
    avg_occupancy = avg_occupancy,
    avg_occupancy_summary = avg_occupancy_summary,
    o = o,
    ptb_percent = ptb_percent,
    ptb_plot = ptb_plot,
    avg_occ_bed = avg_occ_bed,
    avg_occ_bed_summary = avg_occ_bed_summary,
    b = b,
    ptt_percent = ptt_percent,
    ptt_plot = ptt_plot,
    avg_transition = avg_transition,
    avg_transition_summary = avg_transition_summary,
    t = t,
    dpercentiles = dpercentiles,
    qpercentiles = qpercentiles,
    opercentiles = opercentiles,
    bpercentiles = bpercentiles,
    tpercentiles = tpercentiles,
    rejected_summary = rejected_summary,
    avg_through_time_plot = avg_through_time_plot,
    reps = reps,
    ptm = ptm,
    avg_through_time = avg_through_time,
    nodes = nodes,
    warm_up = warm_up,
    sim_time = sim_time,
    exits = exits,
    syst_names = syst_names,
    delay_list = delay_list,
    cap_cal_input = cap_cal_input_original,
    arr_cal_input = arr_cal_input_original,
    node_capacity_delay = node_capacity_delay,
    node_capacity_delay_summary = node_capacity_delay_summary,
    node_transition_delay = node_transition_delay,
    node_transition_delay_summary = node_transition_delay_summary,
    pat_capacity_delay = pat_capacity_delay,
    pat_capacity_delay_summary = pat_capacity_delay_summary,
    pat_transition_delay = pat_transition_delay,
    pat_transition_delay_summary = pat_transition_delay_summary,
    tisp = tisp,

    # change - add simulation time unit for use in markdown report ####
    # needs to be saved here so it can be added to parameters list
    time_unit = time_unit # save character string time unit description for use as param in markdown
  )
}
