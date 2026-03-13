#' network_visualisation Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param var Reactive returning the wizard var_input data frame.
#' @param cal Reactive returning the wizard cal_input data frame.
#' @param parent_session The parent Shiny session, used for navbar navigation.
#'
#' @import shiny
#' @noRd
mod_network_visualisation_server <- function(id, var, cal, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded_inputs <- reactive({
      if (input$w_temp == 0) {
        req(input$file1)
        req(input$file2)
        var_input <- read.csv(input$file1$datapath, header = TRUE, sep = ",")
        rownames(var_input) <- var_input[, 1]
        var_input <- var_input[, -1]
        cal_input <- read.csv(input$file2$datapath, header = TRUE, sep = ",")
      } else {
        var_input <- as.data.frame(var())
        f <- var_input
        indx <- sapply(f, is.factor)
        f[indx] <- lapply(f[indx], function(x) as.character(x))
        var_input <- as.data.frame(f)
        f <- var_input[, 1:nrow(var_input)]
        indx <- 1:nrow(var_input)
        f[, indx] <- lapply(f[indx], function(x) as.numeric(x))
        var_input[, 1:nrow(var_input)] <- f
        var_input$ext_queue <- as.numeric(var_input$ext_queue)
        var_input$int_queue <- as.numeric(var_input$int_queue)
        cal_input <- as.data.frame(cal())
        cal_input$metric <- as.character(cal_input$metric)
        cal_input$node <- as.character(cal_input$node)
        cal_input$start <- as.numeric(as.character(cal_input$start))
        cal_input$end <- as.numeric(as.character(cal_input$end))
        cal_input$value <- as.numeric(as.character(cal_input$value))
      }
      list(var_input = var_input, cal_input = cal_input)
    })

    output$contents1 <- renderTable(
      {
        if (input$disp1 == TRUE) {
          if (input$w_temp == 0) {
            req(input$file1)
            df <- read.csv(input$file1$datapath,
              header = TRUE,
              sep = ","
            )
            rownames(df) <- df[, 1]
            df <- df[, -1]
            colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
              rownames(df)
            df
          } else {
            var()
          }
        }
      },
      rownames = TRUE,
      caption = "Variable Inputs",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
    )


    output$contents2 <- renderTable(
      {
        if (input$disp2 == TRUE) {
          if (input$w_temp == 0) {
            req(input$file2)
            df <- read.csv(input$file2$datapath,
              header = TRUE,
              sep = ","
            )
            df
          } else {
            cal()
          }
        }
      },
      caption = "Calendar Inputs",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
    )




    issues <- eventReactive(input$go_viz, {
      req(input$file1)
      req(input$file2)

      df <- read.csv(input$file1$datapath,
        header = TRUE,
        sep = ","
      )

      rownames(df) <- df[, 1]
      df <- df[, -1]
      colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
        rownames(df)


      df2 <- read.csv(input$file2$datapath,
        header = TRUE,
        sep = ","
      )

      issues <- c()

      var <- df
      cal <- df2

      x <- rownames(var[which(!is.na(var[, 1])), ])
      x <- unique(x)
      # rownames(x)<-1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      sp <- x[which(x != "")]

      x <- rownames(var[which(is.na(var[, 1])), ])
      x <- unique(x)
      # rownames(x)<-1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      exit <- x[which(x != "")]

      node_number <- length(sp)
      exit_number <- length(exit)

      node_names <- sp

      exit_names <- exit

      all_names <- c(node_names, exit_names)




      ### Testing if the names match between templates###

      cal_names <- unique(cal$node)


      if (length(node_names) != length(cal_names) |
        any(!(node_names %in% cal_names))) {
        issues <-
          c(
            issues,
            c(
              paste0("Network & Cal input"),
              "All",
              "Service point names do not match between templates"
            )
          )
      }




      ### Testing if the transition matrix has rowsums of 1###

      f <- var[1:node_number, 1:length(all_names)]
      indx <- sapply(f, is.factor)
      f[indx] <-
        lapply(f[indx], function(x) {
          as.numeric(as.character(x))
        })
      transition <- as.data.frame(f)


      if (sum(transition < 0) > 0 | sum(transition > 1) > 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            "All",
            paste(
              "Transition matrix contains value outside required range (replace with value between 0 and 1)",
              sep = ""
            )
          ))
      }

      rs <- rowSums(transition)

      for (i in 1:node_number) {
        x <- rs[i]

        if (is.na(x)) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste(
                "Transition row contains NA (replace with 0 or value)",
                sep = ""
              )
            ))
        } else if (!isTRUE(dplyr::near(x, 1))) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Row sum does not equal 1 (Currently:", x, ")", sep = "")
            ))
        }
      }



      ### Testing if the distribution parameter inputs are correct ###
      f <-
        var[1:node_number, (length(all_names) + 1):(length(all_names) + 2)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      serv_dist_param <- as.data.frame(f)



      for (i in 1:node_number) {
        x <- serv_dist_param[i, 1]

        if (is.na(x)) {
          issues <-
            c(
              issues,
              paste0("Network Input"),
              node_names[i],
              "Missing a service distribution"
            )
        }
      }

      ### Testing if the distribution parameter inputs are correct ###
      f <-
        var[1:node_number, (length(all_names) + 1):(length(all_names) + 2)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      serv_dist_param <- as.data.frame(f)



      for (i in 1:node_number) {
        if (serv_dist_param[i, 1] == "exp") {
          x <- serv_dist_param[i, 2]

          if (is.na(x)) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Missing a service distribution parameter"
              )
          }

          if ((!is.na(x)) & x <= 0) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameter is not greater than 0"
              )
          }
        } else {
          x <- serv_dist_param[i, 2]
          x <- strsplit(x, ";")[[1]]

          if ("NA" %in% x) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Missing a service distribution parameter"
              )
          }

          if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "unif") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameters are not greater than 0"
              )
          }

          if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "gamma") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameters are not greater than 0"
              )
          }

          if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "weibull") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameters are not greater than 0"
              )
          }

          # if((!("NA" %in% x))&x[2]<0&serv_dist_param[i,1]=="lnorm"){
          #
          #   issues<-c(issues,paste0("Network Input"),node_names[i],"lnorm service parameter (sdlog) is less than 0")
          #
          # }
        }
      }


      #### Testing if the Queue inputs are correct #
      iq <- var$int_queue


      eq <- var$ext_queue

      for (i in 1:node_number) {
        x <- iq[i]

        if (x == Inf) {
          x <- 9999
        }

        if (is.na(x)) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter Internal Queue Value")
            ))
        }


        if (as.numeric(x) %% 1 != 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter an integer Internal Queue Value")
            ))
        }

        if (as.numeric(x) < 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter a positive Internal Queue Value")
            ))
        }
      }

      for (i in 1:node_number) {
        x <- eq[i]

        if (x == Inf) {
          x <- 9999
        }

        if (is.na(x)) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter External Queue Value")
            ))
        }

        if (as.numeric(x) %% 1 != 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter an integer External Queue Value")
            ))
        }

        if (as.numeric(x) < 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter a positive External Queue Value")
            ))
        }
      }




      ### Testing if the delay distribution inputs are correct ###

      f <-
        var[1:node_number, (length(all_names) + 5):((2 * length(all_names)) + 4)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      delay_dist <- as.data.frame(f)


      f <- var[1:node_number, (2 * length(all_names) + 5):ncol(var)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      delay_param <- as.data.frame(f)

      for (j in 1:length(all_names)) {
        for (i in 1:node_number) {
          if (!is.na(delay_param[i, j])) {
            x <- delay_dist[i, j]

            if (is.na(x)) {
              issues <-
                c(
                  issues,
                  paste0("Network Input"),
                  node_names[i],
                  "Missing a delay distribution "
                )
            }
          }
        }
      }


      ### Testing if the delay parameter inputs are correct ###

      f <-
        var[1:node_number, (length(all_names) + 5):((2 * length(all_names)) + 4)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      delay_dist <- as.data.frame(f)


      f <- var[1:node_number, (2 * length(all_names) + 5):ncol(var)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      delay_param <- as.data.frame(f)

      for (j in 1:length(all_names)) {
        for (i in 1:node_number) {
          if (!is.na(delay_dist[i, j])) {
            if (delay_dist[i, j] == "exp") {
              x <- delay_param[i, j]

              if (is.na(x)) {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Missing a delay distribution parameter"
                  )
              }
              if ((!is.na(x)) & x <= 0) {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay parameter is not greater than 0"
                  )
              }
            } else {
              x <- delay_param[i, j]
              x <- strsplit(x, ";")[[1]]

              if ("NA" %in% x) {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Missing a delay distribution parameter"
                  )
              }

              if ((!("NA" %in% x)) & any(x <= 0) &
                delay_dist[i, j] == "unif") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay distribution parameters are not greater than 0"
                  )
              }

              if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "gamma") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay distribution parameters are not greater than 0"
                  )
              }

              if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "weibull") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay distribution parameters are not greater than 0"
                  )
              }

              # if((!("NA" %in% x))&x[2]<0&delay_dist[i,j]=="lnorm"){
              #
              #   issues<-c(issues,paste0("Network Input"),node_names[i],"lnorm service parameter (sdlog) is less than 0")
              #

              # }
            }
          }
        }
      }


      ### Testing if there is at least 1 row of capacity and ext_arrival rate for each service point###

      row_test <- as.data.frame(cal[, 1:2])

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- row_test[which(row_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) == 0) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing ", j, " input rows")
              )
          }
        }
      }

      ### Testing that every line in the calendar template has a value entry###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), 5]

          if (length(x) > 0) {
            if (any(is.na(x))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Missing ", j, " value entry in calendar")
                )
            }
            if (!any(is.na(x))) {
              if (any(x < 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0("Negative ", j, " value entry in calendar")
                  )
              }
            }
            if (!any(is.na(x))) {
              if (j == "cap" & all(x == 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0("All zero ", j, " values entered in calendar")
                  )
              }
            }
          }
        }
      }

      ### Testing that nodes that have 2+ lines in the calendar have any values in the start and end columns ###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) > 1) {
            start <- x[, 3]
            end <- x[, 4]

            if (any(is.na(start))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Missing start value(s) in ", j, " calendar")
                )
            }

            if (any(is.na(end))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Missing end value(s) in ", j, " calendar")
                )
            }
          }
        }
      }

      ### Testing that nodes that have a zero in the first start line in the calendar ###

      value_test <- as.data.frame(cal)


      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]

          if (nrow(x) != 0) {
            if (!is.na(x[1, 3])) {
              start <- x[1, 3]
              if (start != 0) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0("Non-Zero Initial Start Time in ", j, " calendar")
                  )
              }
            }
            if (is.na(x[1, 3])) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Non-Zero Initial Start Time in ", j, " calendar")
                )
            }
          }
        }
      }


      ### Testing that nodes that have 2+ lines in the calendar have matching values in the start and end columns ###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) > 1) {
            start <- x[, 3]
            end <- x[, 4]

            start_tail <- tail(start, -1)
            end_head <- head(end, -1)

            start_tail[is.na(start_tail)] <- 0
            end_head[is.na(end_head)] <- 0



            if (any(!(start_tail == end_head))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0(
                    "Start & End values don't match up sequentially in ",
                    j,
                    " calendar"
                  )
                )
            }
          }
        }
      }



      ### Testing that nodes that have ascending start and end values ###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) > 1) {
            start <- x[, 3]
            end <- x[, 4]

            if (!any(is.na(start))) {
              if (any(diff(start) <= 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0(
                      "Start values don't increase sequentially in ",
                      j,
                      " calendar"
                    )
                  )
              }
            }
            if (!any(is.na(end))) {
              if (any(diff(end) <= 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0(
                      "End values don't increase sequentially in ",
                      j,
                      " calendar"
                    )
                  )
              }
            }
          }
        }
      }

      ### Testing that there are arrivals to at least one node ###

      value_test <- as.data.frame(cal)

      x <- value_test[which(value_test[, 1] == "ext_arr"), 5]
      if (!any(is.na(x))) {
        if (all(x == 0)) {
          issues <-
            c(
              issues,
              "Calendar",
              "All",
              paste0(
                "No Arrival rates to any service point in the ext_arr calendar"
              )
            )
        }
      }



      if (length(issues) == 0) {
        issues <- c("Complete", "Complete", "Complete")
      }

      issues <- matrix(
        data = issues,
        ncol = 3,
        byrow = T
      )

      colnames(issues) <- c("Location", "Service Point", "Issue")

      issues
    })

    output$file_check_issues <- renderTable(
      {
        issues <- issues()
        issues
      },
      striped = T,
      bordered = T,
      align = "c",
      caption = "Issues Log",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
    )




    #### NETWORK VISUALISATION ####
    logger::log_debug("Creating network visualisation.")
    viz <- eventReactive(input$go_viz, {
      inp <- loaded_inputs()
      var_input <- inp$var_input
      cal_input <- inp$cal_input

      if (input$w_temp == 0) {
        issues <- issues()
        req(issues[1, 1] == "Complete")
      }

      nodes <-
        rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
          "serv_dist") - 1], na.rm = T) != 0), ])
      exits <-
        rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
          "serv_dist") - 1], na.rm = T) == 0), ])
      # ext_arr<-rownames(var_input[which(var_input$ext_arr>0),])

      ext_arr <-
        unique(cal_input$node[which(cal_input$metric == "ext_arr" &
          cal_input$value > 0)])


      delay_dist <-
        var_input[, (nrow(var_input) + 5):(nrow(var_input) + nrow(var_input) + 4)] ## Import the template in csv
      rownames(delay_dist) <- rownames(var_input)
      colnames(delay_dist) <- rownames(var_input)
      delay_dist[which(delay_dist == "", arr.ind = T)] <- NA

      delay_param <-
        var_input[, (nrow(var_input) + nrow(var_input) + 5):(ncol(var_input))] ## Import the template in csv
      rownames(delay_param) <- rownames(var_input)
      colnames(delay_param)[1:nrow(delay_param)] <- rownames(var_input)
      delay_param[which(delay_param == "", arr.ind = T)] <- NA


      from <- c(0)
      to <- c(0)


      for (i in 1:nrow(delay_dist)) {
        for (j in 1:nrow(delay_dist)) {
          if (!is.na(delay_dist[i, j])) {
            from <- c(from, i)
            to <- c(to, j)
          }
        }
      }

      delay_list <- cbind(from, to)


      tmp <- rownames(delay_dist)
      delay_exits <-
        tmp[c(delay_list[, 2])][!tmp[c(delay_list[, 2])] %in% nodes]


      var_input$serv_dist[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
        NA
      var_input$serv_dist_param[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
        NA

      cap_min <- vector()
      for (i in nodes) {
        cap_min <-
          c(cap_min, min(cal_input$value[which(cal_input$node == i &
            cal_input$metric == "cap")]))
      }


      cap_max <- vector()
      for (i in nodes) {
        cap_max <-
          c(cap_max, max(cal_input$value[which(cal_input$node == i &
            cal_input$metric == "cap")]))
      }


      cal_tooltip <- vector()

      for (i in nodes) {
        tmp <- cal_input[which(cal_input$node == i &
          cal_input$metric == "cap"), ]
        tmp2 <- vector()

        for (j in 1:nrow(tmp)) {
          tmp3 <-
            paste(
              "\n",
              "Start:",
              tmp[j, 3],
              "End:",
              tmp[j, 4],
              "Capacity:",
              tmp[j, 5],
              "//"
            )


          tmp2 <- c(tmp2, tmp3)
        }
        tmp2 <- paste(tmp2, collapse = "")
        tmp2 <- paste("Capacity Calendar //", tmp2)
        cal_tooltip <- c(cal_tooltip, tmp2)
      }



      # Create a node data frame (ndf)

      ndf1 <- DiagrammeR::create_node_df(
        n = length(nodes),
        type = "lower",
        label = c(nodes),
        fillcolor = "deepskyblue1",
        color = "black",
        fontcolor = "black",
        shape = "square",
        tooltip = cal_tooltip,
        fixedsize = FALSE
      )



      ndf2 <- DiagrammeR::create_node_df(
        n = length(exits),
        type = "lower",
        label = c(exits),
        fillcolor = "green",
        color = "black",
        fontcolor = "black",
        shape = "diamond",
        tooltip = "Exit",
        fixedsize = FALSE
      )




      ndf3 <- DiagrammeR::create_node_df(
        n = length(ext_arr),
        type = "lower",
        label = as.numeric(c(length(c(
          nodes, exits
        )) + 1):(length(c(
          nodes, exits
        )) + length(ext_arr))),
        fillcolor = "white",
        fontcolor = "white",
        shape = "square",
        color = "white"
      )

      ndf <- DiagrammeR::combine_ndfs(ndf1, ndf2, ndf3)

      # Create an edge data frame (edf)
      f <- vector()
      t <- vector()
      l <- vector()
      edge_col <- vector()
      edge_tip <- vector()

      for (i in 1:length(nodes)) {
        for (j in 1:length(c(nodes, exits))) {
          if (var_input[i, j] > 0) {
            f <- c(f, i)
            t <- c(t, j)

            if (!is.na(delay_dist[i, j])) {
              l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
              edge_col <- c(edge_col, "sienna2")

              if (delay_dist[i, j] == "exp") {
                pars <-
                  as.numeric(unlist(strsplit(
                    x = as.character(delay_param[i, j]), split = ";"
                  )))
                delay_mean <- 1 / pars[1]
              } else if (delay_dist[i, j] == "unif") {
                pars <-
                  as.numeric(unlist(strsplit(
                    x = as.character(delay_param[i, j]), split = ";"
                  )))
                delay_mean <- (pars[1] + pars[2]) / 2
              } else if (delay_dist[i, j] == "lnorm") {
                pars <-
                  as.numeric(unlist(strsplit(
                    x = as.character(delay_param[i, j]), split = ";"
                  )))
                delay_mean <- exp(pars[1] + 0.5 * (pars[2])^2)
              } else if (delay_dist[i, j] == "weibull") {
                pars <-
                  as.numeric(unlist(strsplit(
                    x = as.character(delay_param[i, j]), split = ";"
                  )))
                delay_mean <- pars[2] * (gamma(1 + (1 / pars[1])))
              } else if (delay_dist[i, j] == "gamma") {
                pars <-
                  as.numeric(unlist(strsplit(
                    x = as.character(delay_param[i, j]), split = ";"
                  )))
                delay_mean <- pars[1] / pars[2]
              } else {
                pars <-
                  as.numeric(unlist(strsplit(
                    x = as.character(delay_param[i, j]), split = ";"
                  )))
                tmp2 <-
                  do.call(get(paste0("r", delay_dist[i, j])), as.list(c(10^7, pars))) # Creates a service time
                delay_mean <- mean(tmp2)
              }

              edge_tip <-
                c(
                  edge_tip,
                  paste0(
                    "Mean Delay: ",
                    delay_mean,
                    " (Delay Dist: ",
                    delay_dist[i, j],
                    ")"
                  )
                )
            } else {
              l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
              edge_col <- c(edge_col, "black")
              edge_tip <- c(edge_tip, paste0("No Delay"))
            }
          }
        }
      }


      edf1 <- DiagrammeR::create_edge_df(
        from = f,
        to = t,
        # rel = c("leading_to"),
        label = l,
        color = edge_col,
        fontcolor = edge_col,
        tooltip = edge_tip
      )


      edf2 <-
        DiagrammeR::create_edge_df(
          from = c(length(c(nodes, exits)) + 1):(length(c(nodes, exits)) + length(ext_arr)),
          to = as.numeric(which(rownames(var_input) %in% ext_arr)),
          # rel = c("leading_to"),
          label = as.character("Arrivals"),
          color = "red",
          fontcolor = "red",
          tooltip = "Arrival"
        )

      edf <- DiagrammeR::combine_edfs(edf1, edf2)




      # Create a list of average LOS
      LOS <- vector()


      for (i in nodes) {
        arr.dist <- var_input$serv_dist[which(rownames(var_input) == i)]
        pars <-
          as.numeric(unlist(strsplit(
            as.character(var_input$serv_dist_param[which(rownames(var_input) == i)]), ";"
          )))


        if (arr.dist == "exp") {
          tmp3 <- 1 / pars
          LOS <- c(LOS, tmp3)
        } else if (arr.dist == "unif") {
          tmp3 <- (pars[1] + pars[2]) / 2
          LOS <- c(LOS, tmp3)
        } else if (arr.dist == "lnorm") {
          tmp3 <- exp(pars[1] + 0.5 * (pars[2])^2)
          LOS <- c(LOS, tmp3)
        } else if (arr.dist == "weibull") {
          tmp3 <- pars[2] * (gamma(1 + (1 / pars[1])))
          LOS <- c(LOS, tmp3)
        } else if (arr.dist == "gamma") {
          tmp3 <- pars[1] / pars[2]
          LOS <- c(LOS, tmp3)
        } else {
          tmp2 <-
            do.call(get(paste0("r", arr.dist)), as.list(c(10^7, pars))) # Creates a service time
          tmp3 <- mean(tmp2)

          LOS <- c(LOS, tmp3)
        }
      }
      LOS <- round(LOS, digits = 2)


      TAC <- vector()

      for (i in nodes) {
        tmp <- cal_input[which(cal_input$node == i &
          cal_input$metric == "cap"), ]

        if (nrow(tmp) == 1) {
          TAC <- c(TAC, tmp$value)
        }
        if (nrow(tmp) > 1) {
          tmp2 <- sum(tmp$value * (tmp$end - tmp$start)) / max(tmp$end)
          TAC <- c(TAC, tmp2)
        }
      }

      TAC <- ceiling(TAC)


      node_labels <- vector()

      for (i in 1:length(nodes)) {
        tmp1 <-
          paste0(
            nodes[i],
            "\n",
            " LOS: ",
            LOS[i],
            "\n",
            "Av Cap: ",
            TAC[i],
            "\n",
            "IQC: ",
            var_input$int_queue[i],
            "\n",
            "EQC: ",
            var_input$ext_queue[i]
          )

        node_labels <- c(node_labels, tmp1)
      }

      if (input$disp3 == TRUE) {
        ndf$label[1:length(nodes)] <- node_labels
      }


      # Create a graph with the ndf and edf
      graph <-
        DiagrammeR::create_graph(
          nodes_df = ndf,
          edges_df = edf
        )

      graph$global_attrs[1, "value"] <- "dot"
      graph$global_attrs[4, "value"] <- 20
      graph$global_attrs[6, "value"] <- "false"
      graph$global_attrs[14, "value"] <- 20
      graph$global_attrs[17, "value"] <- 1

      graph$global_attrs <-
        rbind(graph$global_attrs, c("rankdir", "LR", "graph"))
      graph$global_attrs <-
        rbind(graph$global_attrs, c("splines", "true", "graph"))

      showTab(inputId = "navbar", target = "2. Simulation Setup & Run")

      output$next_button <- renderUI({
        column(6, align = "center", actionButton(inputId = ns("j2PSR2"), label = c(tagList(
          "Next", icon("arrow-right")
        ))))
      })




      DiagrammeR::render_graph(graph)
    })

    output$network <- DiagrammeR::renderGrViz({
      viz()
    })

    observeEvent(input$jb2i, {
      updateTabsetPanel(parent_session, "navbar", selected = "Introduction")
    })

    observeEvent(input$j2PSR2, {
      updateTabsetPanel(parent_session, "navbar", selected = "2. Simulation Setup & Run")
    })

    return(list(
      viz = viz,
      inputs = loaded_inputs,
      time_unit = reactive(input$time_unit)
    ))
  })
}
