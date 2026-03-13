#' Run the PathSimR discrete event simulation
#'
#' @param var_input data.frame. The network/parameter template, with row names
#'   set to 1:n and the node name column removed.
#' @param cal_input data.frame. The calendar template with columns: metric,
#'   node, start, end, value.
#' @param sim_time numeric. Simulation time period (excluding warm-up).
#' @param warm_up numeric. Warm-up period length.
#' @param reps integer. Number of simulation replications.
#' @param syst_names matrix. Two-column matrix of (numeric index, node name string)
#'   for all nodes, including exits.
#' @param syst_names_single character vector. Node name strings only (column 2 of
#'   syst_names).
#' @param time_unit character. Label for time unit used in outputs.
#' @param session Shiny session object. When non-NULL, completion modals are
#'   shown in the UI. Pass NULL (default) to run outside Shiny (e.g. in tests).
#'
#' @returns A named list of 73 simulation output elements.
#' @noRd
run_simulation <- function(var_input, cal_input, sim_time, warm_up, reps,
                           syst_names, syst_names_single,
                           time_unit = "units", session = NULL) {

  ptm <- proc.time()
  cluster <- parallel::makeCluster(
    processor_cores_required(reps)
  )

        nodes <-
          as.numeric(rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
            "serv_dist") - 1], na.rm = T) != 0), ])) ## create a list of the service nodes
        node_names <- syst_names[nodes, ]
        node_names <- rbind(node_names, c(NA, NA))
        rownames(node_names) <- c()


        delay_dist <-
          var_input[, (nrow(var_input) + 5):(nrow(var_input) + nrow(var_input) + 4)] ## Import the template in csv
        rownames(delay_dist) <- 1:nrow(delay_dist)
        colnames(delay_dist)[1:nrow(delay_dist)] <- c(1:nrow(delay_dist))
        delay_dist[which(delay_dist == "", arr.ind = T)] <- NA

        delay_param <-
          var_input[, (nrow(var_input) + nrow(var_input) + 5):(ncol(var_input))] ## Import the template in csv
        rownames(delay_param) <- 1:nrow(delay_param)
        colnames(delay_param)[1:nrow(delay_param)] <-
          c(1:nrow(delay_param))
        delay_param[which(delay_param == "", arr.ind = T)] <- NA


        rep_bed <- list()

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

        #
        # cal_input<-read.csv(input$file2$datapath,header = TRUE,sep = ",") ## Import the template in csv
        # cal_input$node<-as.character(cal_input$node)
        #


        if (!is.null(nrow(node_names))) {
          for (i in 1:nrow(node_names)) {
            cal_input$node[as.character(cal_input$node) == node_names[i, 2]] <-
              as.numeric(i)
          }
        }

        if (is.null(nrow(node_names))) {
          cal_input$node[as.character(cal_input$node) == node_names[2]] <- 1
        }

        cap_cal_input <- cal_input[which(cal_input$metric == "cap"), ]
        cap_cal_input <- as.data.frame(cap_cal_input)

        arr_cal_input <- cal_input[which(cal_input$metric == "ext_arr"), ]
        arr_cal_input <- as.data.frame(arr_cal_input)
        cap_cal_input_original <- cap_cal_input
        arr_cal_input_original <- arr_cal_input



        ### Shifting Calendars so that the start of the sim_time is the equivalent of 0 on the calendar
        logger::log_trace("Sim shifting calendars.")
        if (warm_up != 0) {
          cap_cal_input_new <- cap_cal_input[0, ]

          for (cc in nodes) {
            cap_cal_shift <- cap_cal_input[which(cap_cal_input$node == cc), ]

            if (nrow(cap_cal_shift) > 1) {
              cap_cal_max <- max(cap_cal_shift$end)
              warm_up_modulo <- warm_up %% cap_cal_max

              if (warm_up_modulo != 0) {
                cap_cal_shift$start <- cap_cal_shift$start + warm_up_modulo
                cap_cal_shift$end <- cap_cal_shift$end + warm_up_modulo

                cap_cal_stable <-
                  cap_cal_shift[1:min(which(cap_cal_shift$end >= cap_cal_max)), ]
                cap_cal_stable$end[nrow(cap_cal_stable)] <- cap_cal_max

                cap_cal_switch <-
                  cap_cal_shift[min(which(cap_cal_shift$end > cap_cal_max)):nrow(cap_cal_shift), ]
                cap_cal_switch$start[1] <- cap_cal_max
                cap_cal_switch$start <- cap_cal_switch$start - cap_cal_max
                cap_cal_switch$end <- cap_cal_switch$end - cap_cal_max


                cap_cal_shift <- rbind(cap_cal_stable, cap_cal_switch)
                cap_cal_shift <-
                  cap_cal_shift[order(cap_cal_shift$start), ]

                cap_cal_input_new <-
                  rbind(cap_cal_input_new, cap_cal_shift)
              } else {
                cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
              }
            } else {
              cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
            }
          }


          arr_cal_input_new <- arr_cal_input[0, ]

          for (ac in nodes) {
            arr_cal_shift <- arr_cal_input[which(arr_cal_input$node == ac), ]

            if (nrow(arr_cal_shift) > 1) {
              arr_cal_max <- max(arr_cal_shift$end)
              warm_up_modulo <- warm_up %% arr_cal_max

              if (warm_up_modulo != 0) {
                arr_cal_shift$start <- arr_cal_shift$start + warm_up_modulo
                arr_cal_shift$end <- arr_cal_shift$end + warm_up_modulo

                arr_cal_stable <-
                  arr_cal_shift[1:min(which(arr_cal_shift$end > arr_cal_max)), ]
                arr_cal_stable$end[nrow(arr_cal_stable)] <- arr_cal_max

                arr_cal_switch <-
                  arr_cal_shift[min(which(arr_cal_shift$end > arr_cal_max)):nrow(arr_cal_shift), ]
                arr_cal_switch$start[1] <- arr_cal_max
                arr_cal_switch$start <- arr_cal_switch$start - arr_cal_max
                arr_cal_switch$end <- arr_cal_switch$end - arr_cal_max


                arr_cal_shift <- rbind(arr_cal_stable, arr_cal_switch)
                arr_cal_shift <-
                  arr_cal_shift[order(arr_cal_shift$start), ]

                arr_cal_input_new <-
                  rbind(arr_cal_input_new, arr_cal_shift)
              } else {
                arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
              }
            } else {
              arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
            }
          }


          cap_cal_input <- cap_cal_input_new
          arr_cal_input <- arr_cal_input_new
        }

        # Sets the timer
        logger::log_trace("Sim setting timer.")

        record_scale <- 0.8
        na_lim <- 100
        rpi <- 0.1


        t.period <- warm_up + sim_time # Sets simulation period






        parallel::clusterExport(
          cl = cluster,
          varlist = c(
            "cluster",
            "var_input",
            "syst_names",
            "syst_names_single",
            "nodes",
            "delay_dist",
            "delay_param",
            "delay_list",
            "cap_cal_input",
            "arr_cal_input",
            "cal_input",
            "record_scale",
            "na_lim",
            "rpi",
            "warm_up",
            "sim_time",
            "t.period",
            "node_names",
            "reps"
          ),
          envir = environment()
        )

        parallel::clusterSetRNGStream(cluster)

        # required to pass magrittr package to the parallel core workers,
        # which cannot be prefixed magrittr:: like other code can
        # TODO refactor to base pipe once tests are in place
        parallel::clusterEvalQ(
          cl = cluster,
          c(
            library(magrittr)
          )
        )


        ####### SIMULATION CODE ##################################################################
        logger::log_trace("Sim core simulation start.")
        outputs <- parallel::parLapply(
          cl = cluster,
          X = 1:reps,
          fun = function(j) {
            # print(paste("replicate",j))

            # shiny:: namespace required inside parallel operations
            # (shiny::req removed - not needed when called outside Shiny)


            time <- 0 # Sets time start
            patient <- 0 # Sets initial patient label


            # nodes<-as.numeric(rownames(var_input)) ##create a list of the service nodes

            colnames(var_input)[1:(which(colnames(var_input) == "serv_dist") -
              1)] <- c(1:(which(
              colnames(var_input) == "serv_dist"
            ) - 1))

            exits <-
              as.numeric(rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                "serv_dist") - 1], na.rm = T) == 0), ])) # Finds all the exit pathway nodes
            exit_names <- syst_names[exits, ]

            sch <-
              matrix(NA, ncol = 6, nrow = max(3 * sum(
                t.period * arr_cal_input$value,
                na.rm = T
              )), 25)
            colnames(sch) <-
              c(
                "time",
                "event",
                "patient",
                "current_node",
                "next_node",
                "previous_node"
              )
            # sch<-data.frame(time=numeric(0),event=character(0),patient=numeric(0),current_node=numeric(0),next_node=numeric(0),previous_node=numeric(0))    #Creates a data frame for the event schedule

            # record<-matrix(NA,ncol=6,nrow=23000)
            # colnames(record)<-c("time","event","patient","current_node","next_node","previous_node")
            # record<-data.frame(time=numeric(0),event=character(0),patient=numeric(0),current_node=numeric(0),next_node=numeric(0),previous_node=numeric(0)) #Creates a data frame to collect all records

            blocked_mat <-
              matrix(NA,
                ncol = 6,
                nrow = sum(cap_cal_input$value, na.rm = T)
              )
            colnames(blocked_mat) <-
              c(
                "time",
                "event",
                "patient",
                "current_node",
                "next_node",
                "previous_node"
              )
            # blocked_mat<-data.frame(time=numeric(0),event=character(0),patient=numeric(0),current_node=numeric(0),next_node=numeric(0),previous_node=numeric(0)) #Creates a data frame to manage blocked patients

            bed <-
              data.frame(
                time = as.numeric(0),
                bed = as.numeric(0),
                node = as.numeric(0),
                rep = as.numeric(0)
              )


            ## Create syst_ variables and assign initial zero value and exits to Inf####

            for (i in nodes) {
              tmp <- paste("syst_", i, sep = "")
              assign(tmp, 0)
            }

            for (i in exits) {
              tmp <- paste("syst_", i, sep = "")
              assign(tmp, Inf)
            }


            ## Create n_serv_ variables and assign capacity from input for cal time 0 and set exits to Inf####

            initial_cap <- rep(x = 0, times = length(nodes))
            for (i in nodes) {
              initial_cap[i] <-
                cap_cal_input$value[which(cap_cal_input$node == i &
                  cap_cal_input$start == 0)]
            }


            for (i in 1:length(nodes)) {
              tmp <- paste("n_serv_", nodes[i], sep = "")
              tmp2 <- initial_cap[i]
              assign(tmp, tmp2)
            }

            for (i in exits) {
              tmp <- paste("n_serv_", i, sep = "")
              assign(tmp, Inf)
            }


            ## Creates inward & outward nodes and probabilities####

            for (i in 1:length(nodes)) {
              tmp <- paste("inward_nodes_", nodes[i], sep = "")
              tmp2 <- rownames(var_input[var_input[, i] > 0, ])
              assign(tmp, tmp2)
            }


            onward_nodes <-
              as.numeric(colnames(var_input[, 1:(which(colnames(var_input) == "serv_dist") -
                1)]))


            for (i in 1:length(nodes)) {
              tmp <- paste("onward_nodes_prob_", nodes[i], sep = "")
              tmp2 <-
                as.vector(var_input[i, 1:(which(colnames(var_input) == "serv_dist") - 1)])

              assign(tmp, tmp2)
            }


            # Creates the service distribution and parameters lists####


            serv_dist <- var_input$serv_dist
            serv_dist_param <- var_input$serv_dist_param



            ## Creates the arrivals schedules per node and combines them to form sch ####


            # sch[1,]<-c(0,1,1,nodes[1],sample(x=onward_nodes,size = 1,prob = get(paste("onward_nodes_prob_",nodes[1],sep=""))),0)   #Adds a patient to the schedule to enter service node 1 at time 0


            for (i in 1:length(nodes)) {
              arr_cal_input_temp <-
                arr_cal_input[which(arr_cal_input$node == nodes[i]), ]

              if (nrow(arr_cal_input_temp) == 1) {
                arr_cal_input_temp$end <- t.period
              }

              for (w in 1:nrow(arr_cal_input_temp)) {
                cycle_max <- max(arr_cal_input_temp$end)

                start <- arr_cal_input_temp$start[w]
                end <- arr_cal_input_temp$end[w]
                if (end > t.period) {
                  end <- t.period
                }
                time <- start

                while (time < t.period) {
                  if (arr_cal_input_temp[w, ]$value != 0) {
                    while (time < end) {
                      tmp1 <- rexp(1, rate = arr_cal_input_temp$value[w])
                      time <- time + tmp1
                      # print(time)
                      if (time < end) {
                        patient <- patient + 1
                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            time,
                            1,
                            patient,
                            nodes[i],
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste("onward_nodes_prob_", nodes[i], sep = "")
                              )
                            ),
                            0
                          )
                      }
                      # sch<-rbind(sch,data.frame(time=time,event="arrival",patient=patient,current_node=nodes[i],next_node=sample(x=onward_nodes,size = 1,prob = get(paste("onward_nodes_prob_",nodes[i],sep = ""))),previous_node="external"))
                    }
                  }
                  start <- start + cycle_max
                  time <- start
                  end <- end + cycle_max
                }
              }
            }


            sch <- sch[1:(match(NA, sch[, "time"]) + 5), ]

            # loss_potential<-sum(sch[,"current_node"] %in% nodes[which(var_input$ext_arr>0&var_input$ext_queue!=Inf)])

            record <- matrix(NA,
              ncol = 6,
              nrow = round(record_scale * nrow(sch), 0)
            )
            colnames(record) <-
              c(
                "time",
                "event",
                "patient",
                "current_node",
                "next_node",
                "previous_node"
              )



            # Creates the service change schedules per node and combines them to form sch ####

            cap_sch <-
              data.frame(
                time = numeric(),
                event = numeric(),
                patient = numeric(),
                current_node = numeric(),
                next_node = numeric(),
                previous_node = numeric()
              )

            for (i in as.numeric(cap_cal_input$node[duplicated(cap_cal_input$node)])) {
              cap_cal_input_temp <- cap_cal_input[which(cap_cal_input$node == i), ]
              for (l in 1:sum(cap_cal_input_temp$node == i)) {
                time <- cap_cal_input_temp$start[l]
                while (time < t.period) {
                  cap_sch <- rbind(
                    cap_sch,
                    c(time, 7, 0, i, cap_cal_input_temp$value[l], 0)
                  )
                  time <- time + max(cap_cal_input_temp$end)
                }
              }
            }
            colnames(cap_sch) <-
              c(
                "time",
                "event",
                "patient",
                "current_node",
                "next_node",
                "previous_node"
              )
            cap_sch <- cap_sch[order(cap_sch$time), ]
            cap_sch <- cap_sch[-which(cap_sch$time == 0), ]
            # write.csv(cap_sch,"cap_sch.csv")

            sch <- rbind(sch, as.matrix(cap_sch))
            rownames(sch) <- c()



            ## Creates the external queue list and sets queue max ####

            for (i in 1:length(nodes)) {
              tmp <- paste("ext_queue_", nodes[i], sep = "")
              q_max <- var_input$ext_queue[i]
              if (q_max == Inf) {
                q_max <- max(nrow(sch[which(sch[, "current_node"] == nodes[i]), ]), 1)
              }
              mat <- matrix(NA, ncol = 6, nrow = q_max)
              colnames(mat) <-
                c(
                  "time",
                  "event",
                  "patient",
                  "current_node",
                  "next_node",
                  "previous_node"
                )
              assign(tmp, mat)

              tmp <- paste("ext_queue_max_", nodes[i], sep = "")
              tmp2 <- var_input$ext_queue[i]
              assign(tmp, tmp2)
            }


            ## Creates the internal queue list and sets queue max ####

            for (i in 1:length(nodes)) {
              tmp <- paste("int_queue_", nodes[i], sep = "")
              q_max <- var_input$int_queue[i]
              if (q_max == Inf) {
                q_max <- nrow(sch)
              }
              mat <- matrix(NA, ncol = 6, nrow = q_max)
              colnames(mat) <-
                c(
                  "time",
                  "event",
                  "patient",
                  "current_node",
                  "next_node",
                  "previous_node"
                )
              assign(tmp, mat)


              tmp <- paste("int_queue_max_", nodes[i], sep = "")
              tmp2 <- var_input$int_queue[i]
              assign(tmp, tmp2)
            }




            #### SIMULATION CYCLE ######################################################################################

            ### START - Simulation Cycle###
            logger::log_trace("Sim cycle start.")
            while (min(sch[, "time"], na.rm = T) < t.period) {
              # while(min(sch[,"time"],na.rm = T)<21.2) {
              # print(min(sch[,"time"],na.rm = T))

              time_test <- min(sch[, "time"], na.rm = T)



              if (sum(is.na(record[, "time"])) <= na_lim) {
                mat <- matrix(NA,
                  ncol = 6,
                  nrow = round(rpi * nrow(record), 0)
                )
                colnames(mat) <-
                  c(
                    "time",
                    "event",
                    "patient",
                    "current_node",
                    "next_node",
                    "previous_node"
                  )
                record <- rbind(record, mat)
              }



              roi_test <- 7 %in% sch[which(sch[, "time"] == time_test), "event"]

              if (roi_test == FALSE) {
                roi <- which.min(sch[, "time"])
              } else {
                roi <- which(sch[, "time"] == time_test & sch[, "event"] == 7)
                roi <- roi[1]
              }


              ### EXTERNAL ARRIVALS###
              if (sch[roi, "event"] == 1) {
                # Checks if the event at the top of the sch is an arrival

                ### EXTERNAL ARRIVAL SCENARIOS -####
                ###
                ### 1. SPACE IN THE NODE
                ###
                ### 2.   ELSE SPACE IN THE QUEUE
                ###
                ### 3.     ELSE QUEUE IS FULL ~ LOST
                ###


                if (sch[roi, "event"] != 1) {
                  print("line199-Non_arrival_event_in_arrival_section")
                }

                ### EXTERNAL ARRIVALS 1 -  SPACE IN THE NODE #############################################################

                if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                  get(paste("n_serv_", sch[roi, "current_node"], sep = ""))) {
                  # Checks if there is space at the node for an arrival


                  record[match(NA, record[, "time"]), ] <-
                    sch[roi, ] # Adds the event to the record

                  arr.dist <-
                    serv_dist[which(nodes == (sch[roi, "current_node"]))]
                  pars <-
                    as.numeric(unlist(strsplit(
                      as.character(serv_dist_param[which(nodes == (sch[roi, "current_node"]))]), ";"
                    )))

                  tmp2 <-
                    do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                  # tmp2<-do.call(paste("serv_dist_",sch$current_node[roi],sep=""),args = list())

                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 2, patient = sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], 0) # Adds a service start event to the record
                  sch[match(NA, sch[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T) + tmp2,
                      3,
                      patient = sch[roi, "patient"],
                      sch[roi, "current_node"],
                      sch[roi, "next_node"],
                      0
                    ) # Adds a service end event to the schedule

                  tmp3 <-
                    get(paste("syst_", sch[roi, "current_node"], sep = "")) + 1 # Adds 1 to the relevant node system
                  assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp3) # Assigns the increased node system value to the correct system variable

                  bed <-
                    rbind(bed, c(
                      time = sch[roi, "time"],
                      bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                      node = sch[roi, "current_node"],
                      rep = j
                    ))

                  if (get(paste("syst_", sch[roi, "current_node"], sep = "")) >
                    paste("n_serv_", sch[roi, "current_node"], sep = "")) {
                    print("line221- Added a patient to a node where there is no capacity")
                  }


                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line224- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <-
                    c(rep(NA, 6)) # Removes the event from the schedule list
                }

                ### EXTERNAL ARRIVALS 2 -  SPACE IN THE QUEUE #############################################################


                else
                # If there is not space at the node then the patient is either added to the queue or if the queue is full then the patient is lost
                if (sum(!is.na(get(
                  paste("ext_queue_", sch[roi, "current_node"], sep = "")
                ))) / 6 < get(paste("ext_queue_max_", sch[roi, "current_node"], sep = ""))) {
                  # If there is space in the queue then the patient is added to a queue

                  if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                    get(paste("n_serv_", sch[roi, "current_node"], sep = ""))) {
                    print("line232- Added a patient to a queue where there is capacity")
                  }

                  record[match(NA, record[, "time"]), ] <-
                    sch[roi, ] # Adds the arrival to the record

                  tmp4 <-
                    paste("ext_queue_", sch[roi, "current_node"], sep = "") # Finds relevant queue
                  inter <-
                    get(tmp4) # Creates copy of queue to ammend
                  inter[match(NA, inter[, "time"]), ] <-
                    sch[roi, ] # Changes the correct row in the copy
                  assign(tmp4, inter) # Ressigns the correct queue list

                  if (sum(!is.na(get(
                    paste("ext_queue_", sch[roi, "current_node"], sep = "")
                  ))) / 6 > get(paste("ext_queue_max_", sch[roi, "current_node"], sep = ""))) {
                    print(("line235-Exceeded external queue capactity"))
                  }

                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line240- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <-
                    c(rep(NA, 6)) # Removes arrival event from the schedule
                }

                ### EXTERNAL ARRIVALS 3 -  NO SPACE IN NODE OR QUEUE THEREFORE LOST #####

                else {
                  # If there isn't space in the queue then the patient is lost
                  record[match(NA, record[, "time"]), ] <-
                    sch[roi, ] # Adds the arrival to the record
                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 5, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], 0) # Adds the loss to the record

                  if (sum(!is.na(get(
                    paste("ext_queue_", sch[roi, "current_node"], sep = "")
                  ))) / 6 < get(paste("ext_queue_max_", sch[roi, "current_node"], sep = ""))) {
                    print((
                      "line245-Lost patient even though there is capacity"
                    ))
                  }

                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line251- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <- c(rep(NA, 6))
                }
              }




              ### SERVICE END###

              else if (sch[roi, "event"] == 3) {
                # Checks if the event at the top of the sch is an arrival
                if (sch[roi, "event"] != 3) {
                  print("line265-Non service_end event in service_end section")
                }

                ### SERVICE END SCENARIOS################################################################
                ###
                ### 1. NO SPACE AT ONWARD NODE OR ONWARD QUEUE & NOT AN EXIT ~~ BLOCKED
                ###
                ### 2. SPACE IN ONWARD NODE OR ONWARD QUEUE (OR EXIT)
                ###
                ###     a. PATIENT ADDED TO ONWARD NODE QUEUE
                ###
                ###         1. IF NO PATIENT WAITING UPSTREAM ~~ Add current patient departure and arrival to the record. Decrease the number in node system by 1 to allow new arrivals later
                ###
                ###         2. IF PATIENT WAITING UPSTREAM ~~
                ###               Add current patient details (departure & arrival) to the record for their new node queue
                ###               Add new patient details to the record for the waiting patient and schedule the service_end
                ###               Shift node system values to reflect moving capacity
                ###
                ###                   a. NO MORE WAITING PATIENTS
                ###
                ###                   b. while(MORE WAITING PATIENTS UP THE CHAIN)
                ###                     {Add departure, arrival and service_start time to the record for the waiting patient and schedule the service_end AND Shift node system values to reflect moving capacity}
                ###                     {Includes filling empty capacity in fixed capacity queues}
                ###                       1.service_end backfill
                ###                       2.int queue backfill
                ###                       3.ext_queue backfill
                ###
                ###
                ###
                ###
                ###     b. NEXT NODE/EXIT IS PRESCRIBED TRANSITION DELAY ~~ Check if the patient is moving to a node or exit with a prescribed transition delay
                ###
                ###
                ###
                ###     c. PATIENT ADDED TO ONWARD NODE SERVICE OR EXITS ~~ Check if there is a patient waiting in any of the inward service nodes or the queue. If multiple, then take patient with earliest service_end time
                ###
                ###         1. IF NO PATIENT WAITING ~~ Add current patient details to the record and new service_end time to the schedule. Decrease the number in node system by 1 to allow new arrivals later
                ###
                ###         2. IF PATIENT WAITING ~~
                ###               Add current patient details (departure & arrival) to the record for their new node queue
                ###               Add new patient details to the record for the waiting patient and schedule the service_end
                ###               Shift node system values to reflect moving capacity
                ###
                ###                   a. NO MORE WAITING PATIENTS
                ###
                ###                   b. while(MORE WAITING PATIENTS UP THE CHAIN)
                ###                     {Add departure, arrival and service_start time to the record for the waiting patient and schedule the service_end AND Shift node system values to reflect moving capacity}
                ###                     {Includes filling empty capacity in fixed capacity queues}
                ###                       1.service_end backfill
                ###                       2.int queue backfill
                ###                       3.ext_queue backfill
                ###


                record[match(NA, record[, "time"]), ] <-
                  sch[roi, ] # Adds the service_end to the record

                ### SERVICE END 1 -  THERE IS NO SPACE IN THE QUEUE OR THE SERVICE NODE SO THE PATIENT IS BLOCKED #############################################################
                if (sch[roi, "next_node"] %in% nodes &&
                  get(paste("syst_", sch[roi, "next_node"], sep = "")) >= get(paste("n_serv_", sch[roi, "next_node"],
                    sep =
                      ""
                  )) &&
                  sum(!is.na(get(
                    paste("int_queue_", sch[roi, "next_node"], sep = "")
                  ))) / 6 >= get(paste("int_queue_max_", sch[roi, "next_node"], sep = ""))) {
                  ## If the next node is not an exit and there is no space in the onward queue or node (i.e. no space anywhere)


                  if (get(paste("syst_", sch[roi, "next_node"], sep = "")) < get(paste("n_serv_", sch[roi, "next_node"],
                    sep =
                      ""
                  ))) {
                    print("line314-Blocked patient even though there is space in the node")
                  }
                  if (sum(!is.na(get(
                    paste("int_queue_", sch[roi, "next_node"], sep = "")
                  ))) / 6 < get(paste("int_queue_max_", sch[roi, "next_node"], sep = ""))) {
                    print("line315-Blocked patient even though there is space in the queue")
                  }


                  blocked_mat[match(NA, blocked_mat[, "time"]), ] <-
                    sch[roi, ]

                  if (time_test < min(sch[, "time"], na.rm = T)) {
                    print(
                      "line320- Event has been addded to the schedule that occurs before current event"
                    )
                  }
                  sch[roi, ] <- c(rep(NA, 6))
                } else {
                  #### SERVICE END 2a -  THERE IS NO SPACE IN THE NODE AND IT ISNT AN EXIT SO THE PATIENT IS ADDED TO THE QUEUE#########################################################

                  if (get(paste("syst_", sch[roi, "next_node"], sep = "")) >= get(paste("n_serv_", sch[roi, "next_node"],
                    sep =
                      ""
                  )) &&
                    sch[roi, "next_node"] %in% nodes) {
                    ## If the next node is not an exit and there is no space in the onward node (i.e. there is space in the queue, not the node)


                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 8, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"]) # Adds the transition start to the record

                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 4, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"]) # Adds the departure to the record

                    tmp99 <-
                      sample(
                        x = onward_nodes,
                        size = 1,
                        prob = get(paste(
                          "onward_nodes_prob_", sch[roi, "next_node"],
                          sep = ""
                        ))
                      )

                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T),
                        event = 1,
                        sch[roi, "patient"],
                        sch[roi, "next_node"],
                        tmp99,
                        sch[roi, "current_node"]
                      ) # Adds the arrival to the record


                    tmp4 <-
                      paste("int_queue_", sch[roi, "next_node"], sep = "") # Finds relevant queue
                    inter <- get(tmp4)
                    inter[match(NA, inter[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 1, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"])
                    assign(tmp4, inter) # Adds the patient arrival record to the correct queue



                    if (sum(!is.na(get(
                      paste("int_queue_", sch[roi, "next_node"], sep = "")
                    ))) / 6 > get(paste("int_queue_max_", sch[roi, "next_node"], sep = ""))) {
                      print(("line347-Exceed Internal queue capactity"))
                    }




                    tmp5 <-
                      get(paste("syst_", sch[roi, "current_node"], sep = "")) - 1 # Takes 1 from the relevant node system
                    assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp5) # Assigns the decreased node system value to the correct system variable

                    bed <-
                      rbind(bed, c(
                        time = sch[roi, "time"],
                        bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                        node = sch[roi, "current_node"],
                        rep = j
                      ))

                    if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                      0) {
                      print("line355- Lowered syst value to below zero which is impossible")
                    }




                    backfill_loop <- "TRUE"
                    backfill <-
                      rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"],
                        sep =
                          ""
                      )), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                    if (sum(!is.na(backfill[, "patient"])) > 0 &
                      get(paste("n_serv_", sch[roi, "current_node"], sep = "")) > get(paste("syst_", sch[roi, "current_node"],
                        sep =
                          ""
                      ))) {
                      while (backfill_loop == "TRUE") {
                        # Finds the next available person from the queue or blocked node
                        if (sch[roi, "event"] != 3) {
                          print("line367-Non service_end event triggering backfill loop")
                        }

                        if (backfill[which.min(backfill[, "time"]), "event"] ==
                          3) {
                          if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                            delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                            0) {
                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                8,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                4,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient

                            tmp99 <-
                              sample(
                                x = onward_nodes,
                                size = 1,
                                prob = get(
                                  paste(
                                    "onward_nodes_prob_",
                                    backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )
                                )
                              )

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                1,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              ) # Adds an arrival event to the record for the blocked patient

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                2,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              ) # Adds a service start event to the record for blocked patient


                            arr.dist <-
                              serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                            pars <-
                              as.numeric(unlist(strsplit(
                                as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                              )))

                            tmp7 <-
                              do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                            # tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())

                            sch[match(NA, sch[, "time"]), ] <-
                              c(
                                min(sch[, "time"], na.rm = T) + tmp7,
                                3,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              )

                            tmp97 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )) - 1 # Takes 1 from the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              ),
                              tmp97
                            ) # Assigns the decreased node system value to the correct system variable

                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                )
                              )


                            if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) < 0) {
                              print(
                                "line398- Lowered syst value within backfill loop to below zero which is impossible"
                              )
                            }



                            tmp_unblocked_node <-
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            tmp_filled_node <-
                              backfill[which.min(backfill[, "time"]), "next_node"]

                            tmp_blocked_remove <-
                              which(
                                blocked_mat[, "current_node"] == tmp_unblocked_node &
                                  blocked_mat[, "next_node"] == tmp_filled_node
                              )

                            blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                              c(rep(NA, 6))

                            tmp9 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )) + 1 # Adds 1 to the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ),
                              tmp9
                            ) # Assigns the increased node system value to the correct system variable


                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "next_node"],
                                  rep = j
                                )
                              )



                            if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ))) {
                              print(
                                "line413- Increased syst value within backfill loop to above capacity"
                              )
                            }



                            backfill <-
                              rbind(
                                get(
                                  paste(
                                    "int_queue_",
                                    tmp_unblocked_node,
                                    sep = ""
                                  )
                                ),
                                get(
                                  paste(
                                    "ext_queue_",
                                    tmp_unblocked_node,
                                    sep = ""
                                  )
                                ),
                                blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]
                              ) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                            if (sum(!is.na(backfill[, "patient"])) > 0) {
                              if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              } else {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              }
                            }

                            if (sum(!is.na(backfill[, "patient"])) == 0) {
                              backfill_loop <- "FALSE"
                            }
                          } else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                            delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                            0) {
                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                8,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a transfer_delay_start event to the record for the blocked patient


                            arr.dist <-
                              delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                            pars <-
                              as.numeric(unlist(strsplit(
                                as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                              )))

                            tmp2 <-
                              do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                            sch[match(NA, sch[, "time"]), ] <-
                              c(
                                min(sch[, "time"], na.rm = T) + tmp2,
                                6,
                                patient = backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              )


                            if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                              tmp5 <-
                                get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )) + 1 # Adds 1 from the relevant node system
                              assign(
                                paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                ),
                                tmp5
                              ) # Assigns the increased node system value to the correct system variable

                              bed <-
                                rbind(
                                  bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                      sep = ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "next_node"],
                                    rep = j
                                  )
                                )
                            }



                            tmp_unblocked_node <-
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            tmp_filled_node <-
                              backfill[which.min(backfill[, "time"]), "next_node"]

                            tmp_blocked_remove <-
                              which(
                                blocked_mat[, "current_node"] == tmp_unblocked_node &
                                  blocked_mat[, "next_node"] == tmp_filled_node
                              )

                            blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                              c(rep(NA, 6))

                            backfill_loop <- "FALSE"
                          }
                        } ## END OF SERVICE END PART OF BACKFILL LOOP

                        else if (backfill[which.min(backfill[, "time"]), "event"] ==
                          1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service start event to the record for the next person in the queue


                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                            )))

                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                          # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service end event to schedule for the next person in the queue

                          queue_find <- "int"

                          tmp8 <-
                            get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) # Find the queue in question
                          tmp8[which.min(tmp8[, "time"]), ] <-
                            c(rep(NA, 6)) # Remove the patient from the queue
                          assign(
                            paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ),
                            tmp8
                          ) # Reassign the queue to the correct variable name

                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) + 1 # Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ), tmp9) # Assigns the increased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              )
                            )



                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ))) {
                            print(
                              "line455- Increased syst value within backfill loop to above capacity"
                            )
                          }




                          backfill <-
                            rbind(get(paste(
                              "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )), get(paste(
                              "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) # Finds everyone who is blocked for the newly undercapacity queue

                          if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                            0) {
                            backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                8,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient


                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                4,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient


                            tmp99 <-
                              sample(
                                x = onward_nodes,
                                size = 1,
                                prob = get(
                                  paste(
                                    "onward_nodes_prob_",
                                    backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )
                                )
                              )

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                1,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              ) # Adds an arrival event to the record for the blocked patient

                            tmp4 <-
                              paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ) # Finds the correct queue for the patient to enter
                            inter <-
                              get(tmp4) # Creates copy of queue to ammend

                            inter[match(NA, inter[, "time"]), ] <-
                              c(
                                backfill[which.min(backfill[, "time"]), "time"],
                                1,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              )
                            assign(tmp4, inter) # Adds the patient arrival record to the correct queue

                            if (sum(!is.na(get(
                              paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )
                            ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ))) {
                              print((
                                "line480-Internal queue capactity exceeded"
                              ))
                            }


                            tmp9 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )) - 1 # Subtracts 1 to the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              ),
                              tmp9
                            )

                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                )
                              )


                            if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) < 0) {
                              print(
                                "line464- Lowered syst value within backfill loop to below zero"
                              )
                            }



                            tmp_unblocked_node <-
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            tmp_filled_node <-
                              backfill[which.min(backfill[, "time"]), "next_node"]

                            tmp_blocked_remove <-
                              which(
                                blocked_mat[, "current_node"] == tmp_unblocked_node &
                                  blocked_mat[, "next_node"] == tmp_filled_node
                              )

                            blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                              c(rep(NA, 6))

                            backfill <-
                              rbind(
                                get(
                                  paste("int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )
                                ),
                                get(
                                  paste("ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )
                                ),
                                blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]
                              ) # Finds everyone who is either blocked for the newly undercapacity queue

                            if (sum(!is.na(backfill[, "patient"])) > 0) {
                              if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              } else {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              }
                            }
                          } else {
                            backfill_loop <- "FALSE"
                          }
                        } ## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP

                        else if (backfill[which.min(backfill[, "time"]), "event"] ==
                          1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service start event to the record for the next person in the queue


                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                            )))

                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                          # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service end event to schedule for the next person in the queue

                          queue_find <- "ext"

                          tmp8 <-
                            get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) # Find the queue in question
                          tmp8[which.min(tmp8[, "time"]), ] <-
                            c(rep(NA, 6)) # Remove the patient from the queue
                          assign(
                            paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ),
                            tmp8
                          ) # Reassign the queue to the correct variable name

                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) + 1 # Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ), tmp9) # Assigns the increased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              )
                            )


                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ))) {
                            print(
                              "line535- Increased syst value within backfill loop to above capacity"
                            )
                          }



                          backfill_loop <- "FALSE"
                        } ## END OF ARRIVAL (External) PART OF BACKFILL LOOP

                        else {
                          backfill_loop <- "FALSE"
                        }
                        if (sum(!is.na(backfill[, "patient"])) == 0) {
                          backfill_loop <- "FALSE"
                        }
                      }
                    }
                    if (time_test < min(sch[, "time"], na.rm = T)) {
                      print(
                        "line546- Event has been addded to the schedule that occurs before current event"
                      )
                    }
                    sch[roi, ] <-
                      c(rep(NA, 6))
                  } # Removes the original service end event from the schedule



                  #### SERVICE END 2b - NEXT NODE/EXIT IS PRESCRIBED TRANSITION DELAY###########################################################


                  else if (sum(delay_list[, 1] == sch[roi, "current_node"] &
                    delay_list[, 2] == sch[roi, "next_node"]) > 0) {
                    ## Need new test for delay between service points or service point and exit


                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 8, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"]) # Adds the transfer_delay_start to the record


                    arr.dist <-
                      delay_dist[sch[roi, "current_node"], sch[roi, "next_node"]]
                    pars <-
                      as.numeric(unlist(strsplit(
                        as.character(delay_param[sch[roi, "current_node"], sch[roi, "next_node"]]), ";"
                      )))

                    tmp2 <-
                      do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                    sch[match(NA, sch[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T) + tmp2,
                        6,
                        patient = sch[roi, "patient"],
                        sch[roi, "current_node"],
                        sch[roi, "next_node"],
                        sch[roi, "previous_node"]
                      )


                    if (sch[roi, "next_node"] %in% nodes) {
                      tmp5 <-
                        get(paste("syst_", sch[roi, "next_node"], sep = "")) + 1 # Adds 1 from the relevant node system
                      assign(paste("syst_", sch[roi, "next_node"], sep = ""), tmp5) # Assigns the increased node system value to the correct system variable

                      bed <-
                        rbind(
                          bed,
                          c(
                            time = sch[roi, "time"],
                            bed = get(paste("syst_", sch[roi, "next_node"], sep = "")),
                            node = sch[roi, "next_node"],
                            rep = j
                          )
                        )
                    }

                    sch[roi, ] <- c(rep(NA, 6))
                  }


                  #### SERVICE END 2c - THERE IS SPACE IN THE ONWARD NODE OR NO DELAY EXIT #####################################################
                  else {
                    # There is an empty space in the onward node or this is an no delay exit from the system

                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 8, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"]) # Adds the transfer_delay_start to the record

                    record[match(NA, record[, "time"]), ] <-
                      c(min(sch[, "time"], na.rm = T), 4, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"]) # Adds the departure to the record

                    if (sch[roi, "next_node"] %in% nodes) {
                      # If the patient is exiting the system then they will not need an arrival, service_start or service_end to the record or sch

                      tmp99 <-
                        sample(
                          x = onward_nodes,
                          size = 1,
                          prob = get(paste(
                            "onward_nodes_prob_", sch[roi, "next_node"],
                            sep = ""
                          ))
                        ) # Finds the next node destination after moving node

                      record[match(NA, record[, "time"]), ] <-
                        c(min(sch[, "time"], na.rm = T), 1, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) # Adds arrival to the record

                      record[match(NA, record[, "time"]), ] <-
                        c(min(sch[, "time"], na.rm = T), 2, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) # Adds service_start to the record

                      arr.dist <-
                        serv_dist[which(nodes == sch[roi, "next_node"])]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(serv_dist_param[which(nodes == sch[roi, "next_node"])]), ";"
                        )))

                      tmp2 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                      # tmp2<-do.call(paste("serv_dist_",sch$next_node[roi],sep=""),args = list())

                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp2,
                          3,
                          sch[roi, "patient"],
                          sch[roi, "next_node"],
                          tmp99,
                          sch[roi, "current_node"]
                        ) # Adds service_end to the sch

                      tmp5 <-
                        get(paste("syst_", sch[roi, "next_node"], sep = "")) + 1 # Adds 1 from the relevant node system
                      assign(paste("syst_", sch[roi, "next_node"], sep = ""), tmp5) # Assigns the decreased node system value to the correct system variable

                      bed <-
                        rbind(
                          bed,
                          c(
                            time = sch[roi, "time"],
                            bed = get(paste("syst_", sch[roi, "next_node"], sep = "")),
                            node = sch[roi, "next_node"],
                            rep = j
                          )
                        )


                      if (get(paste("syst_", sch[roi, "next_node"], sep = "")) >
                        get(paste("n_serv_", sch[roi, "next_node"], sep = ""))) {
                        print("line577- Increased syst value to above capacity")
                      }
                    }


                    tmp5 <-
                      get(paste("syst_", sch[roi, "current_node"], sep = "")) - 1 # Takes 1 from the relevant node system
                    assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp5) # Assigns the decreased node system value to the correct system variable

                    bed <-
                      rbind(bed, c(
                        time = sch[roi, "time"],
                        bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                        node = sch[roi, "current_node"],
                        rep = j
                      ))

                    if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                      0) {
                      print("line585- Decreased syst value below 0")
                    }



                    backfill_loop <- "TRUE"
                    backfill <-
                      rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"],
                        sep =
                          ""
                      )), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                    if (sum(!is.na(backfill[, "patient"])) > 0 &
                      get(paste("n_serv_", sch[roi, "current_node"], sep = "")) > get(paste("syst_", sch[roi, "current_node"],
                        sep =
                          ""
                      ))) {
                      while (backfill_loop == "TRUE") {
                        # Finds the next available person from the queue or blocked node
                        if (sch[roi, "event"] != 3) {
                          print("line367-Non service_end event triggering backfill loop")
                        }

                        if (backfill[which.min(backfill[, "time"]), "event"] ==
                          3) {
                          if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                            delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                            0) {
                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                8,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a transfer delay start event to the record for the blocked patient

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                4,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient

                            tmp99 <-
                              sample(
                                x = onward_nodes,
                                size = 1,
                                prob = get(
                                  paste(
                                    "onward_nodes_prob_",
                                    backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )
                                )
                              )

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                1,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              ) # Adds an arrival event to the record for the blocked patient

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                2,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              ) # Adds a service start event to the record for blocked patient


                            arr.dist <-
                              serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                            pars <-
                              as.numeric(unlist(strsplit(
                                as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                              )))

                            tmp7 <-
                              do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                            # tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())

                            sch[match(NA, sch[, "time"]), ] <-
                              c(
                                min(sch[, "time"], na.rm = T) + tmp7,
                                3,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              )

                            tmp97 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )) - 1 # Takes 1 from the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              ),
                              tmp97
                            ) # Assigns the decreased node system value to the correct system variable

                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                )
                              )


                            if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) < 0) {
                              print(
                                "line398- Lowered syst value within backfill loop to below zero which is impossible"
                              )
                            }



                            tmp_unblocked_node <-
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            tmp_filled_node <-
                              backfill[which.min(backfill[, "time"]), "next_node"]

                            tmp_blocked_remove <-
                              which(
                                blocked_mat[, "current_node"] == tmp_unblocked_node &
                                  blocked_mat[, "next_node"] == tmp_filled_node
                              )

                            blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                              c(rep(NA, 6))

                            tmp9 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )) + 1 # Adds 1 to the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ),
                              tmp9
                            ) # Assigns the increased node system value to the correct system variable

                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "next_node"],
                                  rep = j
                                )
                              )



                            if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ))) {
                              print(
                                "line413- Increased syst value within backfill loop to above capacity"
                              )
                            }



                            backfill <-
                              rbind(
                                get(
                                  paste(
                                    "int_queue_",
                                    tmp_unblocked_node,
                                    sep = ""
                                  )
                                ),
                                get(
                                  paste(
                                    "ext_queue_",
                                    tmp_unblocked_node,
                                    sep = ""
                                  )
                                ),
                                blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]
                              ) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                            if (sum(!is.na(backfill[, "patient"])) > 0) {
                              if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              } else {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              }
                            }

                            if (sum(!is.na(backfill[, "patient"])) == 0) {
                              backfill_loop <- "FALSE"
                            }
                          } else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                            delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                            0) {
                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                8,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a transfer delay start event to the record for the blocked patient


                            arr.dist <-
                              delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                            pars <-
                              as.numeric(unlist(strsplit(
                                as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                              )))

                            tmp2 <-
                              do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                            sch[match(NA, sch[, "time"]), ] <-
                              c(
                                min(sch[, "time"], na.rm = T) + tmp2,
                                6,
                                patient = backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              )


                            if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                              tmp5 <-
                                get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )) + 1 # Adds 1 from the relevant node system
                              assign(
                                paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                ),
                                tmp5
                              ) # Assigns the increased node system value to the correct system variable

                              bed <-
                                rbind(
                                  bed,
                                  c(
                                    time = sch[roi, "time"],
                                    bed = get(paste(
                                      "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                      sep = ""
                                    )),
                                    node = backfill[which.min(backfill[, "time"]), "next_node"],
                                    rep = j
                                  )
                                )
                            }



                            tmp_unblocked_node <-
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            tmp_filled_node <-
                              backfill[which.min(backfill[, "time"]), "next_node"]

                            tmp_blocked_remove <-
                              which(
                                blocked_mat[, "current_node"] == tmp_unblocked_node &
                                  blocked_mat[, "next_node"] == tmp_filled_node
                              )

                            blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                              c(rep(NA, 6))

                            backfill_loop <- "FALSE"
                          }
                        } ## END OF SERVICE END PART OF BACKFILL LOOP

                        else if (backfill[which.min(backfill[, "time"]), "event"] ==
                          1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service start event to the record for the next person in the queue


                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                            )))

                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                          # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service end event to schedule for the next person in the queue

                          queue_find <- "int"

                          tmp8 <-
                            get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) # Find the queue in question
                          tmp8[which.min(tmp8[, "time"]), ] <-
                            c(rep(NA, 6)) # Remove the patient from the queue
                          assign(
                            paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ),
                            tmp8
                          ) # Reassign the queue to the correct variable name

                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) + 1 # Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ), tmp9) # Assigns the increased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              )
                            )


                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ))) {
                            print(
                              "line455- Increased syst value within backfill loop to above capacity"
                            )
                          }




                          backfill <-
                            rbind(get(paste(
                              "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )), get(paste(
                              "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) # Finds everyone who is either blocked for the newly undercapacity queue

                          if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                            0) {
                            backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                8,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient


                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                4,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "current_node"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                backfill[which.min(backfill[, "time"]), "previous_node"]
                              ) # Adds a departure event to the record for the blocked patient


                            tmp99 <-
                              sample(
                                x = onward_nodes,
                                size = 1,
                                prob = get(
                                  paste(
                                    "onward_nodes_prob_",
                                    backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )
                                )
                              )

                            record[match(NA, record[, "time"]), ] <-
                              c(
                                sch[roi, "time"],
                                1,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              ) # Adds an arrival event to the record for the blocked patient

                            tmp4 <-
                              paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ) # Finds the correct queue for the patient to enter
                            inter <-
                              get(tmp4) # Creates copy of queue to ammend

                            inter[match(NA, inter[, "time"]), ] <-
                              c(
                                backfill[which.min(backfill[, "time"]), "time"],
                                1,
                                backfill[which.min(backfill[, "time"]), "patient"],
                                backfill[which.min(backfill[, "time"]), "next_node"],
                                tmp99,
                                backfill[which.min(backfill[, "time"]), "current_node"]
                              )
                            assign(tmp4, inter) # Adds the patient arrival record to the correct queue

                            if (sum(!is.na(get(
                              paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )
                            ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ))) {
                              print((
                                "line480-Internal queue capactity exceeded"
                              ))
                            }


                            tmp9 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )) - 1 # Subtracts 1 to the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              ),
                              tmp9
                            )

                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "current_node"],
                                  rep = j
                                )
                              )

                            if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) < 0) {
                              print(
                                "line464- Lowered syst value within backfill loop to below zero"
                              )
                            }



                            tmp_unblocked_node <-
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            tmp_filled_node <-
                              backfill[which.min(backfill[, "time"]), "next_node"]

                            tmp_blocked_remove <-
                              which(
                                blocked_mat[, "current_node"] == tmp_unblocked_node &
                                  blocked_mat[, "next_node"] == tmp_filled_node
                              )

                            blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                              c(rep(NA, 6))

                            backfill <-
                              rbind(
                                get(
                                  paste("int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )
                                ),
                                get(
                                  paste("ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                                    sep =
                                      ""
                                  )
                                ),
                                blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]
                              ) # Finds everyone who is either blocked for the newly undercapacity queue

                            if (sum(!is.na(backfill[, "patient"])) > 0) {
                              if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              } else {
                                if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                ))) {
                                  backfill_loop <- "FALSE"
                                }
                              }
                            }
                          } else {
                            backfill_loop <- "FALSE"
                          }
                        } ## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP

                        else if (backfill[which.min(backfill[, "time"]), "event"] ==
                          1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service start event to the record for the next person in the queue


                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                            )))

                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                          # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a service end event to schedule for the next person in the queue

                          queue_find <- "ext"

                          tmp8 <-
                            get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) # Find the queue in question
                          tmp8[which.min(tmp8[, "time"]), ] <-
                            c(rep(NA, 6)) # Remove the patient from the queue
                          assign(
                            paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ),
                            tmp8
                          ) # Reassign the queue to the correct variable name

                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) + 1 # Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ), tmp9) # Assigns the increased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              )
                            )


                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ))) {
                            print(
                              "line535- Increased syst value within backfill loop to above capacity"
                            )
                          }



                          backfill_loop <- "FALSE"
                        } ## END OF ARRIVAL (External) PART OF BACKFILL LOOP

                        else {
                          backfill_loop <- "FALSE"
                        }
                        if (sum(!is.na(backfill[, "patient"])) == 0) {
                          backfill_loop <- "FALSE"
                        }
                      }
                    }

                    if (time_test < min(sch[, "time"], na.rm = T)) {
                      print(
                        "line776- Event has been addded to the schedule that occurs before current event"
                      )
                    }
                    sch[roi, ] <- c(rep(NA, 6))
                  }
                }
              }

              ### Delayed Departure/transfer###
              ### DELAYED DEPARTURE SCENARIOS ################################################################################
              ###
              ### 1. Patient finishes transition delay and moves onto next node or exit
              ###         1.service_end backfill
              ###         2.int queue backfill
              ###         3.ext_queue backfill
              ###


              else if (sch[roi, "event"] == 6) {
                record[match(NA, record[, "time"]), ] <-
                  c(min(sch[, "time"], na.rm = T), 4, sch[roi, "patient"], sch[roi, "current_node"], sch[roi, "next_node"], sch[roi, "previous_node"]) # Adds the delayed departure to the record

                tmp5 <-
                  get(paste("syst_", sch[roi, "current_node"], sep = "")) - 1 # Takes 1 from the relevant node system
                assign(paste("syst_", sch[roi, "current_node"], sep = ""), tmp5) # Assigns the decreased node system value to the correct system variable

                bed <-
                  rbind(bed, c(
                    time = sch[roi, "time"],
                    bed = get(paste("syst_", sch[roi, "current_node"], sep = "")),
                    node = sch[roi, "current_node"],
                    rep = j
                  ))

                if (sch[roi, "next_node"] %in% nodes) {
                  tmp99 <-
                    sample(
                      x = onward_nodes,
                      size = 1,
                      prob = get(paste(
                        "onward_nodes_prob_", sch[roi, "next_node"],
                        sep = ""
                      ))
                    ) # Finds the next node destination after moving node

                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 1, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) # Adds arrival to the record

                  record[match(NA, record[, "time"]), ] <-
                    c(min(sch[, "time"], na.rm = T), 2, sch[roi, "patient"], sch[roi, "next_node"], tmp99, sch[roi, "current_node"]) # Adds service_start to the record

                  arr.dist <- serv_dist[which(nodes == sch[roi, "next_node"])]
                  pars <-
                    as.numeric(unlist(strsplit(
                      as.character(serv_dist_param[which(nodes == sch[roi, "next_node"])]), ";"
                    )))

                  tmp2 <-
                    do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                  # tmp2<-do.call(paste("serv_dist_",sch$next_node[roi],sep=""),args = list())

                  sch[match(NA, sch[, "time"]), ] <-
                    c(
                      min(sch[, "time"], na.rm = T) + tmp2,
                      3,
                      sch[roi, "patient"],
                      sch[roi, "next_node"],
                      tmp99,
                      sch[roi, "current_node"]
                    ) # Adds service_end to the sch
                }



                if (get(paste("syst_", sch[roi, "current_node"], sep = "")) <
                  0) {
                  print("line585- Decreased syst value below 0")
                }



                backfill_loop <- "TRUE"
                backfill <-
                  rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"],
                    sep =
                      ""
                  )), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                if (sum(!is.na(backfill[, "patient"])) > 0 &
                  get(paste("n_serv_", sch[roi, "current_node"], sep = "")) > get(paste("syst_", sch[roi, "current_node"],
                    sep =
                      ""
                  ))) {
                  while (backfill_loop == "TRUE") {
                    # Finds the next available person from the queue or blocked node
                    if (sch[roi, "event"] != 6) {
                      print("line367-Non service_end event triggering backfill loop")
                    }

                    if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                      if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                        delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                        0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            8,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a transfer delay start event to the record for the blocked patient

                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            4,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a departure event to the record for the blocked patient

                        tmp99 <-
                          sample(
                            x = onward_nodes,
                            size = 1,
                            prob = get(
                              paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )
                            )
                          )

                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            1,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          ) # Adds an arrival event to the record for the blocked patient

                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          ) # Adds a service start event to the record for blocked patient


                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                          )))

                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                        # tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())

                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          )

                        tmp97 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) - 1 # Takes 1 from the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ), tmp97) # Assigns the decreased node system value to the correct system variable

                        bed <-
                          rbind(
                            bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "current_node"],
                              rep = j
                            )
                          )


                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) < 0) {
                          print(
                            "line398- Lowered syst value within backfill loop to below zero which is impossible"
                          )
                        }



                        tmp_unblocked_node <-
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        tmp_filled_node <-
                          backfill[which.min(backfill[, "time"]), "next_node"]

                        tmp_blocked_remove <-
                          which(
                            blocked_mat[, "current_node"] == tmp_unblocked_node &
                              blocked_mat[, "next_node"] == tmp_filled_node
                          )

                        blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                          c(rep(NA, 6))

                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          )) + 1 # Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                          sep =
                            ""
                        ), tmp9) # Assigns the increased node system value to the correct system variable

                        bed <-
                          rbind(
                            bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep = ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "next_node"],
                              rep = j
                            )
                          )


                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                          sep =
                            ""
                        )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                          sep =
                            ""
                        ))) {
                          print(
                            "line413- Increased syst value within backfill loop to above capacity"
                          )
                        }



                        backfill <-
                          rbind(get(
                            paste("int_queue_", tmp_unblocked_node, sep = "")
                          ), get(
                            paste("ext_queue_", tmp_unblocked_node, sep = "")
                          ), blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                        if (sum(!is.na(backfill[, "patient"])) > 0) {
                          if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ))) {
                              backfill_loop <- "FALSE"
                            }
                          } else {
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ))) {
                              backfill_loop <- "FALSE"
                            }
                          }
                        }

                        if (sum(!is.na(backfill[, "patient"])) == 0) {
                          backfill_loop <- "FALSE"
                        }
                      } else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                        delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                        0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            8,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a transfer delay start event to the record for the blocked patient

                        arr.dist <-
                          delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                          )))

                        tmp2 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp2,
                            6,
                            patient = backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          )


                        if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                          tmp5 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )) + 1 # Adds 1 from the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          ), tmp5) # Assigns the increased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep = ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "next_node"],
                                rep = j
                              )
                            )
                        }

                        tmp_unblocked_node <-
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        tmp_filled_node <-
                          backfill[which.min(backfill[, "time"]), "next_node"]

                        tmp_blocked_remove <-
                          which(
                            blocked_mat[, "current_node"] == tmp_unblocked_node &
                              blocked_mat[, "next_node"] == tmp_filled_node
                          )

                        blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                          c(rep(NA, 6))

                        backfill_loop <- "FALSE"
                      }
                    } ## END OF SERVICE END PART OF BACKFILL LOOP

                    else if (backfill[which.min(backfill[, "time"]), "event"] ==
                      1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                      record[match(NA, record[, "time"]), ] <-
                        c(
                          sch[roi, "time"],
                          2,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        ) # Adds a service start event to the record for the next person in the queue


                      arr.dist <-
                        serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                        )))

                      tmp7 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                      # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp7,
                          3,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        ) # Adds a service end event to schedule for the next person in the queue

                      queue_find <- "int"

                      tmp8 <-
                        get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) # Find the queue in question
                      tmp8[which.min(tmp8[, "time"]), ] <-
                        c(rep(NA, 6)) # Remove the patient from the queue
                      assign(
                        paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ),
                        tmp8
                      ) # Reassign the queue to the correct variable name

                      tmp9 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) + 1 # Adds 1 to the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                        sep =
                          ""
                      ), tmp9) # Assigns the increased node system value to the correct system variable

                      bed <-
                        rbind(
                          bed,
                          c(
                            time = sch[roi, "time"],
                            bed = get(paste(
                              "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )),
                            node = backfill[which.min(backfill[, "time"]), "current_node"],
                            rep = j
                          )
                        )


                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                        sep =
                          ""
                      )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                        sep =
                          ""
                      ))) {
                        print(
                          "line455- Increased syst value within backfill loop to above capacity"
                        )
                      }




                      backfill <-
                        rbind(get(paste(
                          "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )), get(paste(
                          "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) # Finds everyone who is either blocked for the newly undercapacity queue

                      if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                        0) {
                        backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))

                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            8,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a departure event to the record for the blocked patient


                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            4,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a departure event to the record for the blocked patient


                        tmp99 <-
                          sample(
                            x = onward_nodes,
                            size = 1,
                            prob = get(
                              paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )
                            )
                          )

                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            1,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          ) # Adds an arrival event to the record for the blocked patient

                        tmp4 <-
                          paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          ) # Finds the correct queue for the patient to enter
                        inter <-
                          get(tmp4) # Creates copy of queue to ammend

                        inter[match(NA, inter[, "time"]), ] <-
                          c(
                            backfill[which.min(backfill[, "time"]), "time"],
                            1,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            tmp99,
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          )
                        assign(tmp4, inter) # Adds the patient arrival record to the correct queue

                        if (sum(!is.na(get(
                          paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          )
                        ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"],
                          sep =
                            ""
                        ))) {
                          print((
                            "line480-Internal queue capactity exceeded"
                          ))
                        }


                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) - 1 # Subtracts 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ), tmp9)

                        bed <-
                          rbind(
                            bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "current_node"],
                              rep = j
                            )
                          )

                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) < 0) {
                          print("line464- Lowered syst value within backfill loop to below zero")
                        }



                        tmp_unblocked_node <-
                          backfill[which.min(backfill[, "time"]), "current_node"]
                        tmp_filled_node <-
                          backfill[which.min(backfill[, "time"]), "next_node"]

                        tmp_blocked_remove <-
                          which(
                            blocked_mat[, "current_node"] == tmp_unblocked_node &
                              blocked_mat[, "next_node"] == tmp_filled_node
                          )

                        blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                          c(rep(NA, 6))

                        backfill <-
                          rbind(get(paste(
                            "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )), get(paste(
                            "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) # Finds everyone who is either blocked for the newly undercapacity queue

                        if (sum(!is.na(backfill[, "patient"])) > 0) {
                          if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ))) {
                              backfill_loop <- "FALSE"
                            }
                          } else {
                            if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ))) {
                              backfill_loop <- "FALSE"
                            }
                          }
                        }
                      } else {
                        backfill_loop <- "FALSE"
                      }
                    } ## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP

                    else if (backfill[which.min(backfill[, "time"]), "event"] ==
                      1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                      record[match(NA, record[, "time"]), ] <-
                        c(
                          sch[roi, "time"],
                          2,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        ) # Adds a service start event to the record for the next person in the queue


                      arr.dist <-
                        serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                      pars <-
                        as.numeric(unlist(strsplit(
                          as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                        )))

                      tmp7 <-
                        do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                      # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                      sch[match(NA, sch[, "time"]), ] <-
                        c(
                          min(sch[, "time"], na.rm = T) + tmp7,
                          3,
                          backfill[which.min(backfill[, "time"]), "patient"],
                          backfill[which.min(backfill[, "time"]), "current_node"],
                          backfill[which.min(backfill[, "time"]), "next_node"],
                          backfill[which.min(backfill[, "time"]), "previous_node"]
                        ) # Adds a service end event to schedule for the next person in the queue

                      queue_find <- "ext"

                      tmp8 <-
                        get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) # Find the queue in question
                      tmp8[which.min(tmp8[, "time"]), ] <-
                        c(rep(NA, 6)) # Remove the patient from the queue
                      assign(
                        paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ),
                        tmp8
                      ) # Reassign the queue to the correct variable name

                      tmp9 <-
                        get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) + 1 # Adds 1 to the relevant node system
                      assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                        sep =
                          ""
                      ), tmp9) # Assigns the increased node system value to the correct system variable

                      bed <-
                        rbind(
                          bed,
                          c(
                            time = sch[roi, "time"],
                            bed = get(paste(
                              "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )),
                            node = backfill[which.min(backfill[, "time"]), "current_node"],
                            rep = j
                          )
                        )


                      if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                        sep =
                          ""
                      )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                        sep =
                          ""
                      ))) {
                        print(
                          "line535- Increased syst value within backfill loop to above capacity"
                        )
                      }



                      backfill_loop <- "FALSE"
                    } ## END OF ARRIVAL (External) PART OF BACKFILL LOOP

                    else {
                      backfill_loop <- "FALSE"
                    }
                    if (sum(!is.na(backfill[, "patient"])) == 0) {
                      backfill_loop <- "FALSE"
                    }
                  }
                }

                if (time_test < min(sch[, "time"], na.rm = T)) {
                  print(
                    "line776- Event has been addded to the schedule that occurs before current event"
                  )
                }
                sch[roi, ] <- c(rep(NA, 6))
              }



              ### CAPACITY CHANGE###
              ### CAPACITY CHANGE SCENARIOS ##########################################
              ### 1. If the capacity has increased, find all patients who can batch join the new node and then cycle through the backfills repeatedly after each batched patient has been processed.
              ###
              ### 2. If the capacity has decreased, change the capacity value of the node so no new patients can start until the occupancy decreases below the new capacity. The occupancy should fall away until the new capacity is met.
              ###

              ### CAPACITY CHANGE 1 -  CAPACITY INCREASED INVOKING BATCH ARRIVALS AND CYCLIC BACKFILL #############################################################

              else if (sch[roi, "event"] == 7) {
                tmp1 <- paste("n_serv_", sch[roi, "current_node"], sep = "")
                assign(tmp1, sch[roi, "next_node"])

                cap_node <- sch[roi, "current_node"]

                if (get(paste("n_serv_", sch[roi, "current_node"], sep = "")) >
                  get(paste("syst_", sch[roi, "current_node"], sep = ""))) {
                  x <-
                    get(paste("n_serv_", sch[roi, "current_node"], sep = "")) - get(paste("syst_", sch[roi, "current_node"],
                      sep =
                        ""
                    ))

                  backfill <-
                    rbind(get(paste("int_queue_", sch[roi, "current_node"], sep = "")), get(paste("ext_queue_", sch[roi, "current_node"],
                      sep =
                        ""
                    )), blocked_mat[c(which(blocked_mat[, "next_node"] == sch[roi, "current_node"])), ]) # Finds everyone who is either blocked or in a queue for the newly undercapacity node
                  backfill <-
                    rbind(backfill, rep(x = NA, times = 6), rep(x = NA, times = 6))
                  backfill <- backfill[order(backfill[, "time"]), ]
                  y <- sum(!is.na(backfill[, "time"]))
                  xy <- min(x, y)

                  if (xy > 0) {
                    backfill <- backfill[c(1:xy), ]
                    backfill <-
                      rbind(
                        backfill,
                        rep(x = NA, times = 6),
                        rep(x = NA, times = 6)
                      )
                    backfill_loop <- "TRUE"

                    while (backfill_loop == "TRUE") {
                      # Finds the next available person from the queue or blocked node

                      if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                        if (!sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                          delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                          0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a transfer delay start event to the record for the blocked patient

                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              4,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a departure event to the record for the blocked patient

                          tmp99 <-
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )
                              )
                            )

                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            ) # Adds an arrival event to the record for the blocked patient

                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              2,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            ) # Adds a service start event to the record for blocked patient


                          arr.dist <-
                            serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "next_node"])]), ";"
                            )))

                          tmp7 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                          # tmp7<-do.call(paste("serv_dist_",backfill$next_node[which.min(backfill[,"time"])],sep=""),args = list())

                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp7,
                              3,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            )

                          tmp97 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) - 1 # Takes 1 from the relevant node system
                          assign(
                            paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            ),
                            tmp97
                          ) # Assigns the decreased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              )
                            )


                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) < 0) {
                            print(
                              "line398- Lowered syst value within backfill loop to below zero which is impossible"
                            )
                          }



                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]

                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )

                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))

                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )) + 1 # Adds 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          ), tmp9) # Assigns the increased node system value to the correct system variable

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep = ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "next_node"],
                                rep = j
                              )
                            )


                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          ))) {
                            print(
                              "line413- Increased syst value within backfill loop to above capacity"
                            )
                          }



                          backfill <-
                            rbind(get(
                              paste("int_queue_", tmp_unblocked_node, sep = "")
                            ), get(
                              paste("ext_queue_", tmp_unblocked_node, sep = "")
                            ), blocked_mat[c(which(blocked_mat[, "next_node"] == tmp_unblocked_node)), ]) # Finds everyone who is either blocked or in a queue for the newly undercapacity node

                          if (sum(!is.na(backfill[, "patient"])) > 0) {
                            if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ))) {
                                backfill_loop <- "FALSE"
                              }
                            } else {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              ))) {
                                backfill_loop <- "FALSE"
                              }
                            }
                          }

                          if (sum(!is.na(backfill[, "patient"])) == 0) {
                            backfill_loop <- "FALSE"
                          }
                        } else if (sum(delay_list[, 1] == backfill[which.min(backfill[, "time"]), "current_node"] &
                          delay_list[, 2] == backfill[which.min(backfill[, "time"]), "next_node"]) >
                          0) {
                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a transfer delay start event to the record for the blocked patient

                          arr.dist <-
                            delay_dist[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]
                          pars <-
                            as.numeric(unlist(strsplit(
                              as.character(delay_param[backfill[which.min(backfill[, "time"]), "current_node"], backfill[which.min(backfill[, "time"]), "next_node"]]), ";"
                            )))

                          tmp2 <-
                            do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time

                          sch[match(NA, sch[, "time"]), ] <-
                            c(
                              min(sch[, "time"], na.rm = T) + tmp2,
                              6,
                              patient = backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            )


                          if (backfill[which.min(backfill[, "time"]), "next_node"] %in% nodes) {
                            tmp5 <-
                              get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )) + 1 # Adds 1 from the relevant node system
                            assign(
                              paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ),
                              tmp5
                            ) # Assigns the increased node system value to the correct system variable

                            bed <-
                              rbind(
                                bed,
                                c(
                                  time = sch[roi, "time"],
                                  bed = get(paste(
                                    "syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                    sep = ""
                                  )),
                                  node = backfill[which.min(backfill[, "time"]), "next_node"],
                                  rep = j
                                )
                              )
                          }



                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]

                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )

                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))

                          backfill_loop <- "FALSE"
                        }
                      } ## END OF SERVICE END PART OF BACKFILL LOOP

                      else if (backfill[which.min(backfill[, "time"]), "event"] ==
                        1 & backfill[which.min(backfill[, "time"]), "previous_node"] != 0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a service start event to the record for the next person in the queue


                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                          )))

                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                        # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a service end event to schedule for the next person in the queue

                        queue_find <- "int"

                        tmp8 <-
                          get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) # Find the queue in question
                        tmp8[which.min(tmp8[, "time"]), ] <-
                          c(rep(NA, 6)) # Remove the patient from the queue
                        assign(
                          paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ),
                          tmp8
                        ) # Reassign the queue to the correct variable name

                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) + 1 # Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ), tmp9) # Assigns the increased node system value to the correct system variable

                        bed <-
                          rbind(
                            bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "current_node"],
                              rep = j
                            )
                          )


                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ))) {
                          print(
                            "line455- Increased syst value within backfill loop to above capacity"
                          )
                        }




                        backfill <-
                          rbind(get(paste(
                            "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )), get(paste(
                            "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) # Finds everyone who is either blocked for the newly undercapacity queue

                        if (length(backfill[which(backfill[, "event"] == 3), "event"]) !=
                          0) {
                          backfill <- rbind(backfill[which(backfill[, "event"] == 3), ], rep(NA, 6))

                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              8,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a departure event to the record for the blocked patient


                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              4,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "current_node"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              backfill[which.min(backfill[, "time"]), "previous_node"]
                            ) # Adds a departure event to the record for the blocked patient


                          tmp99 <-
                            sample(
                              x = onward_nodes,
                              size = 1,
                              prob = get(
                                paste("onward_nodes_prob_", backfill[which.min(backfill[, "time"]), "next_node"],
                                  sep =
                                    ""
                                )
                              )
                            )

                          record[match(NA, record[, "time"]), ] <-
                            c(
                              sch[roi, "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            ) # Adds an arrival event to the record for the blocked patient

                          tmp4 <-
                            paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            ) # Finds the correct queue for the patient to enter
                          inter <-
                            get(tmp4) # Creates copy of queue to ammend

                          inter[match(NA, inter[, "time"]), ] <-
                            c(
                              backfill[which.min(backfill[, "time"]), "time"],
                              1,
                              backfill[which.min(backfill[, "time"]), "patient"],
                              backfill[which.min(backfill[, "time"]), "next_node"],
                              tmp99,
                              backfill[which.min(backfill[, "time"]), "current_node"]
                            )
                          assign(tmp4, inter) # Adds the patient arrival record to the correct queue

                          if (sum(!is.na(get(
                            paste("int_queue_", backfill[which.min(backfill[, "time"]), "next_node"],
                              sep =
                                ""
                            )
                          ))) / 6 > get(paste("int_queue_max_", backfill[which.min(backfill[, "time"]), "next_node"],
                            sep =
                              ""
                          ))) {
                            print((
                              "line480-Internal queue capactity exceeded"
                            ))
                          }


                          tmp9 <-
                            get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )) - 1 # Subtracts 1 to the relevant node system
                          assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ), tmp9)

                          bed <-
                            rbind(
                              bed,
                              c(
                                time = sch[roi, "time"],
                                bed = get(paste(
                                  "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                  sep =
                                    ""
                                )),
                                node = backfill[which.min(backfill[, "time"]), "current_node"],
                                rep = j
                              )
                            )


                          if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) < 0) {
                            print(
                              "line464- Lowered syst value within backfill loop to below zero"
                            )
                          }



                          tmp_unblocked_node <-
                            backfill[which.min(backfill[, "time"]), "current_node"]
                          tmp_filled_node <-
                            backfill[which.min(backfill[, "time"]), "next_node"]

                          tmp_blocked_remove <-
                            which(
                              blocked_mat[, "current_node"] == tmp_unblocked_node &
                                blocked_mat[, "next_node"] == tmp_filled_node
                            )

                          blocked_mat[tmp_blocked_remove[which.min(blocked_mat[tmp_blocked_remove, "time"])], ] <-
                            c(rep(NA, 6))

                          backfill <-
                            rbind(get(paste(
                              "int_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )), get(paste(
                              "ext_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                              sep =
                                ""
                            )), blocked_mat[c(which(as.vector(blocked_mat[, "next_node"]) == backfill[which.min(backfill[, "time"]), "current_node"])), ]) # Finds everyone who is either blocked for the newly undercapacity queue

                          if (sum(!is.na(backfill[, "patient"])) > 0) {
                            if (backfill[which.min(backfill[, "time"]), "event"] == 3) {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "next_node"],
                                sep =
                                  ""
                              ))) {
                                backfill_loop <- "FALSE"
                              }
                            } else {
                              if (get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )) <= get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              ))) {
                                backfill_loop <- "FALSE"
                              }
                            }
                          }
                        } else {
                          backfill_loop <- "FALSE"
                        }
                      } ## END OF ARRIVAL (Internal) PART OF BACKFILL LOOP

                      else if (backfill[which.min(backfill[, "time"]), "event"] ==
                        1 & backfill[which.min(backfill[, "time"]), "previous_node"] == 0) {
                        record[match(NA, record[, "time"]), ] <-
                          c(
                            sch[roi, "time"],
                            2,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a service start event to the record for the next person in the queue


                        arr.dist <-
                          serv_dist[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]
                        pars <-
                          as.numeric(unlist(strsplit(
                            as.character(serv_dist_param[which(nodes == backfill[which.min(backfill[, "time"]), "current_node"])]), ";"
                          )))

                        tmp7 <-
                          do.call(get(paste0("r", arr.dist)), as.list(c(1, pars))) # Creates a service time


                        # tmp7<-do.call(paste("serv_dist_",backfill$current_node[which.min(backfill[,"time"])],sep=""),args = list()) #Draws a random service time from the distribution

                        sch[match(NA, sch[, "time"]), ] <-
                          c(
                            min(sch[, "time"], na.rm = T) + tmp7,
                            3,
                            backfill[which.min(backfill[, "time"]), "patient"],
                            backfill[which.min(backfill[, "time"]), "current_node"],
                            backfill[which.min(backfill[, "time"]), "next_node"],
                            backfill[which.min(backfill[, "time"]), "previous_node"]
                          ) # Adds a service end event to schedule for the next person in the queue

                        queue_find <- "ext"

                        tmp8 <-
                          get(paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) # Find the queue in question
                        tmp8[which.min(tmp8[, "time"]), ] <-
                          c(rep(NA, 6)) # Remove the patient from the queue
                        assign(
                          paste(queue_find, "_queue_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          ),
                          tmp8
                        ) # Reassign the queue to the correct variable name

                        tmp9 <-
                          get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                            sep =
                              ""
                          )) + 1 # Adds 1 to the relevant node system
                        assign(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ), tmp9) # Assigns the increased node system value to the correct system variable

                        bed <-
                          rbind(
                            bed,
                            c(
                              time = sch[roi, "time"],
                              bed = get(paste(
                                "syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                                sep =
                                  ""
                              )),
                              node = backfill[which.min(backfill[, "time"]), "current_node"],
                              rep = j
                            )
                          )


                        if (get(paste("syst_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        )) > get(paste("n_serv_", backfill[which.min(backfill[, "time"]), "current_node"],
                          sep =
                            ""
                        ))) {
                          print(
                            "line535- Increased syst value within backfill loop to above capacity"
                          )
                        }



                        backfill_loop <- "FALSE"
                      } ## END OF ARRIVAL (External) PART OF BACKFILL LOOP

                      else {
                        backfill_loop <- "FALSE"
                      }
                      if (sum(!is.na(backfill[, "patient"])) == 0) {
                        backfill_loop <- "FALSE"
                      }
                    }
                  }
                }



                sch[roi, ] <- c(rep(NA, 6))
              } else {
                print("Major Error")
              }
            }
            ### END - Simulation Cycle###

            #### OUTPUTS--------------------------------------------------------------

            ### Create standard record###
            record <- as.data.frame(record[which(!is.na(record[, "time"])), ])
            record$event[which(record$event == 1)] <- "arrival"
            record$event[which(record$event == 2)] <- "service_start"
            record$event[which(record$event == 3)] <- "service_end"
            record$event[which(record$event == 4)] <- "departure"
            record$event[which(record$event == 5)] <- "loss"
            record$event[which(record$event == 6)] <- "delayed_departure"
            record$event[which(record$event == 7)] <- "capacity_change"
            record$event[which(record$event == 8)] <- "transition_start"
            record$previous_node[which(record$previous_node == 0)] <-
              "external"

            # record<-record[which(!is.na(record[,"time"])),]


            bed <- bed[-1, ]


            ### Create the patient & node metrics ######################################################
            logger::log_trace("Sim creating patient and node metrics.")
            all_data <-
              data.frame(
                rep = as.numeric(),
                patient = as.numeric(),
                node = as.numeric(),
                arr = as.numeric(),
                wait = as.numeric(),
                ss = as.numeric(),
                service = as.numeric(),
                se = as.numeric(),
                delayed = as.numeric(),
                tds = as.numeric(),
                transition = as.numeric(),
                dep = as.numeric()
              )


            for (i in 1:length(nodes)) {
              arr_node <-
                record[which(record$event == "arrival" &
                  record$current_node == nodes[i]), c(3, 1)]
              ss_node <-
                record[which(record$event == "service_start" &
                  record$current_node == nodes[i]), c(3, 1)]
              se_node <-
                record[which(record$event == "service_end" &
                  record$current_node == nodes[i]), c(3, 1)]
              tds_node <-
                record[which(record$event == "transition_start" &
                  record$current_node == nodes[i]), c(3, 1)]
              dep_node <-
                record[which(record$event == "departure" &
                  record$current_node == nodes[i]), c(3, 1)]


              arr_node <- arr_node[which(arr_node$time > warm_up), ]
              colnames(arr_node)[2] <- "arr"

              ss_node <- ss_node[which(ss_node$time > warm_up), ]
              colnames(ss_node)[2] <- "ss"

              se_node <- se_node[which(se_node$time > warm_up), ]
              colnames(se_node)[2] <- "se"

              tds_node <- tds_node[which(tds_node$time > warm_up), ]
              colnames(tds_node)[2] <- "tds"

              dep_node <- dep_node[which(dep_node$time > warm_up), ]
              colnames(dep_node)[2] <- "dep"


              tmp1 <- merge(arr_node, ss_node, by = "patient", all = TRUE)
              tmp2 <- merge(tmp1, se_node, by = "patient", all = TRUE)
              tmp3 <- merge(tmp2, tds_node, by = "patient", all = TRUE)
              tmp4 <- merge(tmp3, dep_node, by = "patient", all = TRUE)

              tmp4 <- cbind(0, i, tmp4)
              colnames(tmp4)[1] <- "rep"
              colnames(tmp4)[2] <- "node"

              # dplyr:: namespace required inside parallel operations
              tmp4 <- dplyr::mutate(tmp4, wait = ss - arr)
              tmp4 <- dplyr::mutate(tmp4, service = se - ss)
              tmp4 <- dplyr::mutate(tmp4, delayed = tds - se)
              tmp4 <- dplyr::mutate(tmp4, transition = dep - tds)

              tmp4 <-
                tmp4[, c(
                  "rep",
                  "patient",
                  "node",
                  "arr",
                  "wait",
                  "ss",
                  "service",
                  "se",
                  "delayed",
                  "tds",
                  "transition",
                  "dep"
                )]
              tmp4[, "node"] <- node_names[c(tmp4[, "node"]), 2]


              all_data <- data.table::rbindlist(list(all_data, tmp4))
            }


            # all_data<-data.table::rbindlist(all_data)

            # dplyr:: namespace required inside parallel operations
            rep_node_dat <- all_data %>% 
              dplyr::group_by(rep, node)

            pat_dat <- all_data %>%
              dplyr::group_by(patient, rep) %>%
              dplyr::transmute(
                wait = sum(wait),
                service = sum(service),
                delayed = sum(delayed),
                transition = sum(transition)
              ) %>%
              dplyr::ungroup() %>%
              dplyr::group_by(rep)

            # change all of the below to include time units? #####

            node_wait <-
              as.data.frame(
                dplyr::summarise(
                  rep_node_dat,
                  metric = "wait",
                  mean = mean(wait, na.rm = T),
                  sd = sd(wait, na.rm = T),
                  iqr = IQR(wait, na.rm = T),
                  percentile_95 = quantile(wait, 0.95, na.rm = T)
                )
              )

            node_active_service <-
              as.data.frame(
                dplyr::summarise(
                  rep_node_dat,
                  metric = "active_service",
                  mean = mean(service, na.rm = T),
                  sd = sd(service, na.rm = T),
                  iqr = IQR(service, na.rm = T),
                  percentile_95 = quantile(
                    x = service,
                    probs = 0.95,
                    na.rm = TRUE
                  )
                )
              )

            node_capacity_delay <-
              as.data.frame(
                dplyr::summarise(
                  rep_node_dat,
                  metric = "capacity_delay",
                  mean = mean(delayed, na.rm = T),
                  sd = sd(delayed, na.rm = T),
                  iqr = IQR(delayed, na.rm = T),
                  percentile_95 = quantile(
                    x = delayed,
                    probs = 0.95,
                    na.rm = TRUE
                  )
                )
              )

            node_transition_delay <-
              as.data.frame(
                dplyr::summarise(
                  rep_node_dat,
                  metric = "transition_delay",
                  mean = mean(transition, na.rm = T),
                  sd = sd(transition, na.rm = T),
                  iqr = IQR(transition, na.rm = T),
                  percentile_95 = quantile(
                    x = transition,
                    probs = 0.95,
                    na.rm = TRUE
                  )
                )
              )

            node_length_of_stay <-
              as.data.frame(
                dplyr::summarise(
                  rep_node_dat,
                  metric = "length_of_stay",
                  mean = mean(service + delayed + transition, na.rm = T),
                  sd = sd(service + delayed + transition, na.rm = T),
                  iqr = IQR(service + delayed + transition, na.rm = T),
                  percentile_95 = quantile(
                    x = service + delayed + transition,
                    probs = 0.95,
                    na.rm = TRUE
                  )
                )
              )

            node_delay_to_transfer <-
              as.data.frame(
                dplyr::summarise(
                  rep_node_dat,
                  metric = "delay_to_transfer",
                  mean = mean(delayed + transition, na.rm = T),
                  sd = sd(delayed + transition, na.rm = T),
                  iqr = IQR(delayed + transition, na.rm = T),
                  percentile_95 = quantile(
                    x = delayed + transition,
                    probs = 0.95,
                    na.rm = TRUE
                  )
                )
              )


            pat_wait <-
              as.data.frame(
                dplyr::summarise(
                  pat_dat,
                  metric = "wait",
                  mean = mean(wait, na.rm = T),
                  sd = sd(wait, na.rm = T),
                  iqr = IQR(wait, na.rm = T),
                  percentile_95 = quantile(wait, 0.95, na.rm = T)
                )
              )

            pat_active_service <-
              as.data.frame(
                dplyr::summarise(
                  pat_dat,
                  metric = "service",
                  mean = mean(service, na.rm = T),
                  sd = sd(service, na.rm = T),
                  iqr = IQR(service, na.rm = T),
                  percentile_95 = quantile(service, 0.95, na.rm = T)
                )
              )

            pat_capacity_delay <-
              as.data.frame(
                dplyr::summarise(
                  pat_dat,
                  metric = "capacity_delay",
                  mean = mean(delayed, na.rm = T),
                  sd = sd(delayed, na.rm = T),
                  iqr = IQR(delayed, na.rm = T),
                  percentile_95 = quantile(delayed, 0.95, na.rm = T)
                )
              )

            pat_transition_delay <-
              as.data.frame(
                dplyr::summarise(
                  pat_dat,
                  metric = "transition_delay",
                  mean = mean(transition, na.rm = T),
                  sd = sd(transition, na.rm = T),
                  iqr = IQR(transition, na.rm = T),
                  percentile_95 = quantile(transition, 0.95, na.rm = T)
                )
              )

            pat_length_of_stay <-
              as.data.frame(
                dplyr::summarise(
                  pat_dat,
                  metric = "length_of_stay",
                  mean = mean(service + delayed + transition, na.rm = T),
                  sd = sd(service + delayed + transition, na.rm = T),
                  iqr = IQR(service + delayed + transition, na.rm = T),
                  percentile_95 = quantile(service + delayed + transition, 0.95, na.rm = T)
                )
              )

            pat_delay_to_transfer <-
              as.data.frame(
                dplyr::summarise(
                  pat_dat,
                  metric = "delay_to_transfer",
                  mean = mean(delayed + transition, na.rm = T),
                  sd = sd(delayed + transition, na.rm = T),
                  iqr = IQR(delayed + transition, na.rm = T),
                  percentile_95 = quantile(delayed + transition, 0.95, na.rm = T)
                )
              )


            # dplyr:: namespace required inside parallel operations
            ttis_dat <- all_data %>%
              dplyr::group_by(patient, rep) %>%
              dplyr::transmute(ttis = max(dep) - min(arr))

            total_time_in_system <- ttis_dat %>%
              dplyr::group_by(rep) %>%
              dplyr::summarise(
                node = "ALL",
                metric = "total_time_in_system",
                mean = mean(ttis, na.rm = T),
                sd = sd(ttis, na.rm = T),
                iqr = IQR(ttis, na.rm = T),
                percentile_95 = quantile(ttis, 0.95, na.rm = T)
              )

            # all_metrics<-rbind(total_time_in_system,wait,active_service,length_of_stay,delay_to_transfer)



            rm(rep_node_dat, node_dat, pat_dat, all_data)

            ### Create the rejected rate metrics #########################################################

            rejected <-
              data.frame(
                node = numeric(),
                metric = character(),
                mean = numeric()
              )

            for (i in 1:length(nodes)) {
              rej_node <-
                record[which(record$event == "loss" &
                  record$current_node == nodes[i]), c(3, 1)]
              rej_node <- rej_node[which(rej_node$time > warm_up), ]
              colnames(rej_node) <- c("patient", "rejected")

              rejected <-
                rbind(
                  rejected,
                  data.frame(
                    node = syst_names[i, 2],
                    metric = "rejected",
                    mean = nrow(rej_node) / sim_time
                  )
                )
            }


            ### Create the delayed metrics ######################################################

            delayed <-
              data.frame(
                time = numeric(0),
                event = numeric(0),
                delayed = numeric(0),
                node = numeric(0)
              )
            # print(j)
            for (i in 1:length(nodes)) {
              rec_temp_total <-
                record[which(
                  record$current_node == nodes[i] &
                    (
                      record$event == "service_end" | record$event == "transition_start"
                    )
                ), ]
              delayed_change <- as.vector(rec_temp_total$event)
              delayed_change <-
                replace(delayed_change, delayed_change == "service_end", 1)
              delayed_change <-
                replace(
                  delayed_change,
                  delayed_change == "transition_start",
                  -1
                )
              delayed_change <- as.numeric(delayed_change)
              delayed_change <- cumsum(delayed_change)

              delayed <-
                rbind(
                  delayed,
                  data.frame(
                    time = rec_temp_total$time,
                    event = rec_temp_total$event,
                    delayed = delayed_change,
                    node = nodes[i]
                  )
                )
            }
            delayed <-
              cbind(delayed, c(diff(delayed$time), delayed$time[nrow(delayed)]))
            colnames(delayed) <-
              c(
                "time",
                "event",
                "delayed",
                "node",
                "time_at_delayed_level"
              )
            if (warm_up > 0) {
              delayed <- delayed[-which(delayed$time < warm_up), ]
            }


            # Calculating the time at each delayed length##

            ptd <-
              data.frame(
                node = numeric(0),
                delayed = numeric(),
                time_at_delayed_level = numeric(),
                percent_time_at_delayed_level = numeric()
              )


            for (i in 1:length(nodes)) {
              node_delayed <- delayed[which(delayed$node == nodes[i]), ]
              node_delayed <- node_delayed[-nrow(node_delayed), ]
              tmp <-
                data.frame(
                  node = numeric(0),
                  delayed = numeric(),
                  time_at_delayed_level = numeric()
                )

              for (k in unique(node_delayed$delayed)) {
                time_at_k <-
                  sum(node_delayed$time_at_delayed_level[which(node_delayed$delayed == k)])

                tmp <-
                  rbind(
                    tmp,
                    data.frame(
                      node = nodes[i],
                      delayed = k,
                      time_at_delayed_level = time_at_k
                    )
                  )
              }
              tmp2 <-
                cbind(tmp, (100 * tmp$time_at_delayed_level / (
                  sum(tmp$time_at_delayed_level)
                )))
              colnames(tmp2) <-
                c(
                  "node",
                  "delayed",
                  "time_at_delayed_level",
                  "percent_time_at_delayed_level"
                )

              ptd <- rbind(ptd, tmp2)
            }




            # rm(tmp_b_length,tmp_b_time,results,avg_b)

            ### Create the delayed through time data ######################################################

            if (nrow(delayed) != 0) {
              datd <-
                data.frame(
                  time = delayed$time[which(delayed$time_at_delayed_level != 0)],
                  delayed = delayed$delayed[which(delayed$time_at_delayed_level != 0)],
                  node = as.character(delayed$node[which(delayed$time_at_delayed_level != 0)]),
                  rep = paste("rep", 0)
                )

              datd$node <- as.numeric(as.character(datd$node))
              datd <- datd[order(datd$node), ]
              datd$node <- syst_names_single[as.numeric(datd$node)]
              datd$node <- as.factor(datd$node)
            } else {
              datd <- data.frame(
                time = 0,
                delayed = 0,
                node = 0,
                rep = paste("rep", 0)
              )
              datd <- datd[0, ]
            }




            ### Create the queue metrics ######################################################

            # Creating the queue tables###


            queue <-
              data.frame(
                time = numeric(0),
                event = numeric(0),
                queue_length = numeric(0),
                node = numeric(0)
              )

            for (i in 1:length(nodes)) {
              rec_temp_total <-
                record[which(
                  record$current_node == nodes[i] &
                    (
                      record$event == "arrival" |
                        record$event == "service_start" | record$event == "loss"
                    )
                ), ]
              queue_change <- as.vector(rec_temp_total$event)
              queue_change <-
                replace(queue_change, queue_change == "arrival", 1)
              queue_change <-
                replace(queue_change, queue_change == "service_start", -1)
              queue_change <- replace(queue_change, queue_change == "loss", -1)
              queue_change <- as.numeric(queue_change)
              queue_change <- cumsum(queue_change)

              queue <-
                rbind(
                  queue,
                  data.frame(
                    time = rec_temp_total$time,
                    event = rec_temp_total$event,
                    queue_length = queue_change,
                    node = nodes[i]
                  )
                )
            }
            queue <-
              cbind(queue, c(diff(queue$time), queue$time[nrow(queue)]))
            colnames(queue) <-
              c(
                "time",
                "event",
                "queue_length",
                "node",
                "time_at_queue_length"
              )
            if (warm_up > 0) {
              queue <- queue[-which(queue$time < warm_up), ]
            }



            # Calculating the time at each queue length##

            ptq <-
              data.frame(
                node = numeric(0),
                queue = numeric(),
                time_at_queue_length = numeric(),
                percent_time_at_queue_length = numeric()
              )


            for (i in 1:length(nodes)) {
              node_queue <- queue[which(queue$node == nodes[i]), ]
              node_queue <- node_queue[-nrow(node_queue), ]
              tmp <-
                data.frame(
                  node = numeric(0),
                  queue = numeric(),
                  time_at_queue_length = numeric()
                )

              for (k in unique(node_queue$queue)) {
                time_at_k <-
                  sum(node_queue$time_at_queue_length[which(node_queue$queue == k)])

                tmp <-
                  rbind(
                    tmp,
                    data.frame(
                      node = nodes[i],
                      queue = k,
                      time_at_queue_length = time_at_k
                    )
                  )
              }
              tmp2 <-
                cbind(tmp, (100 * tmp$time_at_queue_length / (
                  sum(tmp$time_at_queue_length)
                )))
              colnames(tmp2) <-
                c(
                  "node",
                  "queue",
                  "time_at_queue_length",
                  "percent_time_at_queue_length"
                )
              ptq <- rbind(ptq, tmp2)
            }


            ### Create the queue through time data ######################################################

            if (nrow(queue) != 0) {
              datq <-
                data.frame(
                  time = queue$time[which(queue$time_at_queue_length != 0)],
                  queue_length = queue$queue_length[which(queue$time_at_queue_length != 0)],
                  node = as.character(queue$node[which(queue$time_at_queue_length != 0)]),
                  rep = paste("rep", 0)
                )

              datq$node <- as.numeric(as.character(datq$node))
              datq <- datq[order(datq$node), ]
              datq$node <- syst_names_single[as.numeric(datq$node)]
              datq$node <- as.factor(datq$node)
            } else {
              datq <- data.frame(
                time = 0,
                queue_length = 0,
                node = 0,
                rep = paste("rep", 0)
              )
              datq <- datq[0, ]
            }



            ### Create the occupancy metrics ######################################################

            occupancy <-
              data.frame(
                time = numeric(0),
                event = numeric(0),
                occupancy = numeric(0),
                occupancy_prop = numeric(0),
                capacity = numeric(),
                remainder_time = numeric(0),
                node = numeric(0)
              )


            for (i in 1:length(nodes)) {
              rec_temp_total <-
                record[which(
                  record$current_node == nodes[i] &
                    (
                      record$event == "service_start" | record$event == "departure"
                    )
                ), ]
              cap_cal_input_temp <-
                cap_cal_input[which(cap_cal_input$node == i), ]
              if (nrow(cap_cal_input_temp) == 1) {
                cap_cal_input_temp$end <- sim_time
              }

              occupancy_change <- as.vector(rec_temp_total$event)
              occupancy_change <-
                replace(
                  occupancy_change,
                  occupancy_change == "service_start",
                  1
                )
              occupancy_change <-
                replace(
                  occupancy_change,
                  occupancy_change == "departure",
                  -1
                )
              occupancy_change <- as.numeric(occupancy_change)
              occupancy_change <- cumsum(occupancy_change)

              rt <-
                rec_temp_total$time %% max(cap_cal_input_temp$end[which(cap_cal_input_temp$node ==
                  i)])

              tmp <-
                data.frame(
                  time = rec_temp_total$time,
                  event = rec_temp_total$event,
                  occupancy = occupancy_change,
                  occupancy_prop = NA,
                  capacity = NA,
                  remainder_time = rt,
                  node = nodes[i]
                )

              for (time_gap in 1:nrow(cap_cal_input_temp)) {
                tmp$capacity[which(
                  tmp$remainder_time >= cap_cal_input_temp$start[time_gap] &
                    tmp$remainder_time < cap_cal_input_temp$end[time_gap]
                )] <- cap_cal_input_temp$value[time_gap]
              }

              tmp$occupancy_prop <- tmp$occupancy / tmp$capacity
              tmp$occupancy_prop[which(tmp$occupancy_prop == Inf |
                tmp$occupancy_prop >= 1)] <- 1
              tmp$occupancy_prop <- tmp$occupancy_prop * 100

              occupancy <- rbind(occupancy, tmp)
            }
            occupancy <-
              cbind(occupancy, c(diff(occupancy$time), occupancy$time[nrow(occupancy)]))
            colnames(occupancy) <-
              c(
                "time",
                "event",
                "occupancy",
                "occupancy_prop",
                "capacity",
                "remainder_time",
                "node",
                "time_at_occupancy"
              )
            if (warm_up > 0) {
              occupancy <- occupancy[-which(occupancy$time < warm_up), ]
            }




            # Calculating the time at each occupancy##


            pto <-
              data.frame(
                node = numeric(0),
                occupancy = numeric(),
                time_at_occupancy = numeric(),
                percent_time_at_occupancy = numeric()
              )


            for (i in 1:length(nodes)) {
              node_occupancy <- occupancy[which(occupancy$node == nodes[i]), ]
              node_occupancy <- node_occupancy[-nrow(node_occupancy), ]
              tmp <-
                data.frame(
                  node = numeric(0),
                  occupancy = numeric(),
                  time_at_occupancy = numeric()
                )

              for (k in unique(node_occupancy$occupancy)) {
                time_at_k <-
                  sum(node_occupancy$time_at_occupancy[which(node_occupancy$occupancy == k)])

                tmp <-
                  rbind(
                    tmp,
                    data.frame(
                      node = nodes[i],
                      occupancy = k,
                      time_at_occupancy = time_at_k
                    )
                  )
              }
              tmp2 <-
                cbind(tmp, (100 * tmp$time_at_occupancy / (sum(
                  tmp$time_at_occupancy
                ))))
              colnames(tmp2) <-
                c(
                  "node",
                  "occupancy",
                  "time_at_occupancy",
                  "percent_time_at_occupancy"
                )
              pto <- rbind(pto, tmp2)
            }


            # rm(results,tmp,node_occupancy,time_at_k,tmp2)



            ### Create the occupancy through time data ######################################################
            if (nrow(occupancy) != 0) {
              dato <-
                data.frame(
                  time = occupancy$time[which(occupancy$time_at_occupancy != 0)],
                  occupancy = occupancy$occupancy[which(occupancy$time_at_occupancy != 0)],
                  node = occupancy$node[which(occupancy$time_at_occupancy != 0)],
                  rep = paste("rep", 0)
                )

              dato$node <- as.numeric(as.character(dato$node))
              dato <- dato[order(dato$node), ]
              dato$node <- syst_names_single[as.numeric(dato$node)]
              dato$node <- as.factor(dato$node)
            } else {
              dato <- data.frame(
                time = 0,
                occupancy = 0,
                node = 0,
                rep = paste("rep", 0)
              )
              dato <- dato[0, ]
            }


            ### Create the transition metrics ######################################################


            transition <-
              data.frame(
                time = numeric(0),
                event = numeric(0),
                transition = numeric(0),
                node = numeric(0)
              )

            for (i in 1:length(nodes)) {
              rec_temp_total <-
                record[which(
                  record$current_node == nodes[i] &
                    (
                      record$event == "transition_start" | record$event == "departure"
                    )
                ), ]
              transition_change <- as.vector(rec_temp_total$event)
              transition_change <-
                replace(
                  transition_change,
                  transition_change == "transition_start",
                  1
                )
              transition_change <-
                replace(
                  transition_change,
                  transition_change == "departure",
                  -1
                )
              transition_change <- as.numeric(transition_change)
              transition_change <- cumsum(transition_change)

              transition <-
                rbind(
                  transition,
                  data.frame(
                    time = rec_temp_total$time,
                    event = rec_temp_total$event,
                    transition = transition_change,
                    node = nodes[i]
                  )
                )
            }
            transition <-
              cbind(transition, c(diff(transition$time), transition$time[nrow(transition)]))
            colnames(transition) <-
              c(
                "time",
                "event",
                "transition",
                "node",
                "time_at_transition_level"
              )
            if (warm_up > 0) {
              transition <- transition[-which(transition$time < warm_up), ]
            }


            # rm(rec_temp_total,results,transition_change)


            # Calculating the time at each transition length##



            ptt <-
              data.frame(
                node = numeric(0),
                transition = numeric(),
                time_at_transition_level = numeric(),
                percent_time_at_transition_level = numeric()
              )


            for (i in 1:length(nodes)) {
              node_transition <- transition[which(transition$node == nodes[i]), ]
              node_transition <- node_transition[-nrow(node_transition), ]
              tmp <-
                data.frame(
                  node = numeric(0),
                  transition = numeric(),
                  time_at_transition_level = numeric()
                )

              for (k in unique(node_transition$transition)) {
                time_at_k <-
                  sum(node_transition$time_at_transition_level[which(node_transition$transition ==
                    k)])

                tmp <-
                  rbind(
                    tmp,
                    data.frame(
                      node = nodes[i],
                      transition = k,
                      time_at_transition_level = time_at_k
                    )
                  )
              }
              tmp2 <-
                cbind(tmp, (100 * tmp$time_at_transition_level / (
                  sum(tmp$time_at_transition_level)
                )))
              colnames(tmp2) <-
                c(
                  "node",
                  "transition",
                  "time_at_transition_level",
                  "percent_time_at_transition_level"
                )

              ptt <- rbind(ptt, tmp2)
            }

            # rm(results,tmp,node_transition,time_at_k,tmp2)



            ### Create the transition through time data ######################################################

            if (nrow(transition) != 0) {
              datt <-
                data.frame(
                  time = transition$time[which(transition$time_at_transition_level != 0)],
                  transition = transition$transition[which(transition$time_at_transition_level !=
                    0)],
                  node = as.character(transition$node[which(transition$time_at_transition_level !=
                    0)]),
                  rep = paste("rep", 0)
                )

              datt$node <- as.numeric(as.character(datt$node))
              datt <- datt[order(datt$node), ]
              datt$node <- syst_names_single[as.numeric(datt$node)]
              datt$node <- as.factor(datt$node)
            } else {
              datt <- data.frame(
                time = 0,
                transition = 0,
                node = 0,
                rep = paste("rep", 0)
              )
              datt <- datt[0, ]
            }

            ### Create the occ_bed metrics ######################################################


            occ_bed <-
              data.frame(
                time = numeric(0),
                occ_bed = numeric(0),
                node = numeric(0)
              )

            for (i in 1:length(nodes)) {
              rec_temp_total <- bed[which(bed$node == nodes[i]), ]

              occ_bed <-
                rbind(
                  occ_bed,
                  data.frame(
                    time = rec_temp_total$time,
                    occ_bed = rec_temp_total$bed,
                    node = nodes[i]
                  )
                )
            }
            occ_bed <-
              cbind(occ_bed, c(diff(occ_bed$time), occ_bed$time[nrow(occ_bed)]))
            colnames(occ_bed) <-
              c("time", "occ_bed", "node", "time_at_occ_bed_level")
            if (warm_up > 0) {
              occ_bed <- occ_bed[-which(occ_bed$time < warm_up), ]
            }
            occ_bed


            #### % time at bed occupancy level#



            ptb <-
              data.frame(
                node = numeric(0),
                occ_bed = numeric(),
                time_at_occ_bed_level = numeric(),
                percent_time_at_occ_bed_level = numeric()
              )


            for (i in 1:length(nodes)) {
              node_occ_bed <- occ_bed[which(occ_bed$node == nodes[i]), ]
              node_occ_bed <- node_occ_bed[-nrow(node_occ_bed), ]
              tmp <-
                data.frame(
                  node = numeric(0),
                  occ_bed = numeric(),
                  time_at_occ_bed_level = numeric()
                )

              for (k in unique(node_occ_bed$occ_bed)) {
                time_at_k <-
                  sum(node_occ_bed$time_at_occ_bed_level[which(node_occ_bed$occ_bed == k)])

                tmp <-
                  rbind(
                    tmp,
                    data.frame(
                      node = nodes[i],
                      occ_bed = k,
                      time_at_occ_bed_level = time_at_k
                    )
                  )
              }
              tmp2 <-
                cbind(tmp, (100 * tmp$time_at_occ_bed_level / (
                  sum(tmp$time_at_occ_bed_level)
                )))
              colnames(tmp2) <-
                c(
                  "node",
                  "occ_bed",
                  "time_at_occ_bed_level",
                  "percent_time_at_occ_bed_level"
                )
              ptb <- rbind(ptb, tmp2)
            }



            ### Create the occ_bed through time data ######################################################


            if (nrow(occ_bed) != 0) {
              datb <-
                data.frame(
                  time = occ_bed$time[which(occ_bed$time_at_occ_bed_level != 0)],
                  occ_bed = occ_bed$occ_bed[which(occ_bed$time_at_occ_bed_level != 0)],
                  node = occ_bed$node[which(occ_bed$time_at_occ_bed_level != 0)],
                  rep = paste("rep", 0)
                )

              datb$node <- as.numeric(as.character(datb$node))
              datb <- datb[order(datb$node), ]
              datb$node <- syst_names_single[as.numeric(datb$node)]
              datb$node <- as.factor(datb$node)
            } else {
              datb <- data.frame(
                time = 0,
                occ_bed = 0,
                node = 0,
                rep = paste("rep", 0)
              )
              datb <- datb[0, ]
            }

            ### Create the multi data & through time uniform ######################################################

            dato_multi <- cbind(dato, rep(x = "occupancy", nrow(dato)))
            colnames(dato_multi) <- c("time", "value", "node", "rep", "metric")

            datd_multi <- cbind(datd, rep(x = "delayed", nrow(datd)))
            colnames(datd_multi) <- c("time", "value", "node", "rep", "metric")

            datb_multi <- cbind(datb, rep(x = "occ_bed", nrow(datb)))
            colnames(datb_multi) <- c("time", "value", "node", "rep", "metric")

            datt_multi <- cbind(datt, rep(x = "transition", nrow(datt)))
            colnames(datt_multi) <- c("time", "value", "node", "rep", "metric")

            datq_multi <- cbind(datq, rep(x = "queue", nrow(datq)))
            colnames(datq_multi) <- c("time", "value", "node", "rep", "metric")


            multi <-
              data.table::rbindlist(list(
                datb_multi,
                datd_multi,
                dato_multi,
                datt_multi,
                datq_multi
              ))


            # tidyr:: namespace required inside parallel operations
            multi_spread <- tidyr::spread(
              data = multi,
              key = metric,
              value = value
            )


            multi_spread_uniform <-
              data.frame(
                time = numeric(),
                node = numeric(),
                rep = numeric(),
                occ_bed = numeric(),
                delayed = numeric(),
                occupancy = numeric(),
                transition = numeric(),
                queue = numeric()
              )

            uniform_time <- seq(
              from = warm_up,
              to = t.period,
              by = 0.5
            )



            for (i in nodes) {
              base <-
                multi_spread[which(as.character(multi_spread$node) == node_names[i, 2]), ] ## Reassigns names

              uniform_ts <-
                data.frame(
                  time = uniform_time,
                  node = node_names[i, 2],
                  rep = NA,
                  occ_bed = NA,
                  delayed = NA,
                  occupancy = NA,
                  transition = NA,
                  queue = NA
                )

              uniform_ts <-
                data.table::rbindlist(list(base, uniform_ts),
                  fill = T,
                  use.names = T
                )

              uniform_ts <- uniform_ts[order(uniform_ts$time), ]

              # tidyr:: namespace required inside parallel operations
              uniform_ts <-
                uniform_ts %>%
                  tidyr::fill(rep, occ_bed, delayed, occupancy, transition, queue) ## tidyr::fill function changes the NA values to the previous value down the df

              uniform_ts <-
                uniform_ts %>%
                tidyr::fill(rep,
                  occ_bed,
                  delayed,
                  occupancy,
                  transition,
                  queue,
                  .direction = "up"
                ) ## tidyr::fill function changes the NA values to the pervious value up the df

              multi_spread_uniform <-
                data.table::rbindlist(list(multi_spread_uniform, uniform_ts), use.names = T)
            }

            multi_spread_uniform <-
              multi_spread_uniform[which(multi_spread_uniform$time %in% uniform_time), ]






            x <-
              list(
                nodes,
                warm_up,
                sim_time,
                reps,
                exits,
                syst_names,
                node_wait,
                node_active_service,
                node_length_of_stay,
                node_delay_to_transfer,
                pat_wait,
                pat_active_service,
                pat_length_of_stay,
                pat_delay_to_transfer,
                total_time_in_system,
                rejected,
                ptd,
                ptq,
                pto,
                ptt,
                ptb,
                multi_spread_uniform,
                delay_list,
                cap_cal_input,
                arr_cal_input,
                node_capacity_delay,
                node_transition_delay,
                pat_capacity_delay,
                pat_transition_delay
              )

            names(x) <-
              c(
                "nodes",
                "warm_up",
                "sim_time",
                "reps",
                "exits",
                "syst_names",
                "node_wait",
                "node_active_service",
                "node_length_of_stay",
                "node_delay_to_transfer",
                "pat_wait",
                "pat_active_service",
                "pat_length_of_stay",
                "pat_delay_to_transfer",
                "total_time_in_system",
                "rejected",
                "ptd",
                "ptq",
                "pto",
                "ptt",
                "ptb",
                "multi_spread_uniform",
                "delay_list",
                "node_capacity_delay",
                "node_transition_delay",
                "pat_capacity_delay",
                "pat_transition_delay"
              )


            rm(
              record,
              datd,
              datq,
              dato,
              datt,
              datb,
              datq_multi,
              datd_multi,
              dato_multi,
              datt_multi,
              datb_multi,
              multi,
              multi_spread
            )
            # gc()

            return(x)
          }
        )
        # parallel::stopCluster(cl)

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


        parallel::stopCluster(cluster)


        # change to check on number of simulation outputs ####
        # this is a manual check to see if every item in "combo", the list of items that the sim_out function
        # will return, has been created. Orignally, this list had 72 items and there was a hard-coded check
        # to see if the length of the list was 72. A further item (the time unit) has now been added, and so
        # the number being checked for is now 73
        # The reason for performing this check, and what it is intended to achieve, needs to be clarified
        if (!is.null(session)) {
          if (length(combo) == 73) {
            logger::log_debug("Simulation complete (combo item is length 73).")
            shiny::showModal(shiny::modalDialog(
              title = div(paste0("Simulation Complete \n(", format(Sys.time()), ")"), style = "font-size:200%"),
              div("Click anywhere on screen to continue", style = "font-size:200%"),
              easyClose = TRUE,
              footer = NULL,
              size = "l"
            ))
          } else {
            logger::log_error("Simulation error.")
            shiny::showModal(shiny::modalDialog(
              title = "Simulation Error",
              "",
              easyClose = TRUE,
              footer = NULL,
              size = "l"
            ))
            shiny::hideTab(inputId = "navbar", target = "3. Simulation Outputs")
            shiny::hideTab(inputId = "navbar", target = "4. Download Outputs")
          }
        }



        return(combo)
}
