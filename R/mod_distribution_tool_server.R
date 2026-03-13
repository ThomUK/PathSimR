#' distribution_tool Server Module
#'
#' @param id Internal parameter for {shiny}.
#'
#' @import shiny
#' @noRd
mod_distribution_tool_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$model_help_figure <- renderPlot(
      {
        x <- c(rexp(10000, 1))

        fe <- fitdistrplus::fitdist(data = x, distr = "exp")
        fl <- fitdistrplus::fitdist(data = x, distr = "lnorm")
        fu <- fitdistrplus::fitdist(data = x, distr = "unif")
        fw <- fitdistrplus::fitdist(data = x, distr = "weibull")
        fg <- fitdistrplus::fitdist(data = x, distr = "gamma")

        p <-
          fitdistrplus::denscomp(
            ft = list(fe, fl, fu, fw, fg),
            plotstyle = "ggplot",
            breaks = 100,
            # fitcol = c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
            fitlty = 1
          )
        p <- p + theme_bw()
        p
      },
      res = 128
    )

    #### Length of Service Model Fit Tab ####
    logger::log_debug("Creating LOS model fitting tab")
    observeEvent(input$go_distfit, {
      req(input$los_dat)
      df <- read.csv(input$los_dat$datapath,
        header = F,
        sep = ","
      )


      if (is.numeric(df[, 1])) {
        colnames(df) <- "data"

        fe <- fitdistrplus::fitdist(data = df$data, distr = "exp")
        fl <- fitdistrplus::fitdist(data = df$data, distr = "lnorm")
        fu <- fitdistrplus::fitdist(data = df$data, distr = "unif")
        fw <- fitdistrplus::fitdist(data = df$data, distr = "weibull")
        fg <- fitdistrplus::fitdist(data = df$data, distr = "gamma")


        output$los_plot <- renderPlot(
          {
            fitdistrplus::plotdist(df$data, histo = T, demp = T)
          },
          res = 128
        )

        output$los_cf <- renderPlot(
          {
            fitdistrplus::descdist(df$data, boot = 100)
          },
          res = 128
        )

        output$los_fit_plot <- renderPlot(
          {
            p <-
              fitdistrplus::denscomp(
                ft = list(fe, fl, fu, fw, fg),
                plotstyle = "ggplot",
                breaks = 100,
                # fitcol = c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                fitlty = 1
              )
            p <- p + theme_bw()
            p
          },
          res = 128
        )


        output$los_text <- renderText({
          c("Ranked Model Table")
        })

        output$los_text_help <- renderText({
          c(
            "The distributions below have been ranked in terms of best fit. The Rank 1 Distribution was found to fit closest to the provided data.
      Simply use the top ranking model and enter the details in the data entry tab.
      If the exponential distribution is the highest ranking, then there is only one parameter to copy across (rate), else there will be two. These are named in the table and should be copied
      to the relevant box on the data entry page. If the histogram appears completely flat, it may be that the uniform distribution is the best fitting model. In this case, ignore the rankings and take the parameters from that row.
      In the case where multiple distributions are found to have the same fit, some model fit lines may be obscured on the plot (i.e. plotting over eachother). These models will still be ranked but should be treated as ranking equally.
          "
          )
        })


        output$los_fit_table <- renderTable(
          {
            fes <- summary(fe)
            fls <- summary(fl)
            fus <- summary(fu)
            fws <- summary(fw)
            fgs <- summary(fg)
            aic <- c(fes$aic, fls$aic, fus$aic, fws$aic, fgs$aic)
            del_aic <- aic - min(aic, na.rm = T)
            aic_lik <- exp(-0.5 * del_aic)
            aic_weight <- aic_lik / sum(aic_lik, na.rm = T)

            means <-
              c(
                (1 / fes$estimate[1]),
                (exp(fls$estimate[1] + (
                  0.5 * (fls$estimate[2])^2
                ))),
                (0.5 * (fus$estimate[1] + fus$estimate[2])),
                (fws$estimate[2] * gamma(1 + 1 / fws$estimate[1])),
                (fgs$estimate[1] / fgs$estimate[2])
              )
            means <- unname(means)
            mean_dif <- means - mean(df$data)

            fit_table <- data.frame(
              "Distribution" = c(
                "exponential",
                "log-normal",
                "uniform",
                "weibull",
                "gamma"
              ),
              "Parameter 1 Name" = c(
                names(fes$estimate)[1],
                names(fls$estimate)[1],
                names(fus$estimate)[1],
                names(fws$estimate)[1],
                names(fgs$estimate)[1]
              ),
              "Parameter 1 Value" = c(
                fes$estimate[1],
                fls$estimate[1],
                fus$estimate[1],
                fws$estimate[1],
                fgs$estimate[1]
              ),
              "Parameter 2 Name" = c(
                names(fes$estimate)[2],
                names(fls$estimate)[2],
                names(fus$estimate)[2],
                names(fws$estimate)[2],
                names(fgs$estimate)[2]
              ),
              "Parameter 2 Value" = c(
                fes$estimate[2],
                fls$estimate[2],
                fus$estimate[2],
                fws$estimate[2],
                fgs$estimate[2]
              ),
              "AIC Score" = c(ceiling(aic)),
              "AIC Weight" = c(100 * signif(aic_weight, digits = 3)),
              "Mean" = means,
              "Diff from actual mean" = signif(mean_dif, digits = 3),
              row.names = NULL
            )

            # rownames(fit_table)<-c()
            fit_table <-
              fit_table[order(fit_table$AIC.Weight,
                decreasing = T,
                na.last = T
              ), ]
            fit_table <- cbind("Rank" = 1:5, fit_table)
            fit_table[which(fit_table$Distribution == "uniform"), c(7, 8)] <-
              "Check Graph for fit"
            colnames(fit_table) <-
              c(
                "Rank",
                "Distribution",
                "Parameter 1 Name",
                "Parameter 1 Value",
                "Parameter 2 Name",
                "Parameter 2 Value",
                "AIC Score",
                "AIC Weight (/100)",
                "Estiamted Mean",
                "Diff from data mean"
              )
            fit_table <- fit_table[, -c(7, 8, 9, 10)]
            fit_table
          },
          striped = T,
          bordered = T,
          align = "c"
        )

        output$fit_error <- renderText({
          c("")
        })


        output$mini_summary <- renderTable(
          {
            mini_summary <-
              data.frame(
                "Metric" = c(
                  "Mean",
                  "Standard Deviation",
                  "Inter-quartile range",
                  "90th Percentile"
                ),
                "Value" = c(
                  mean(df$data),
                  sd(df$data),
                  IQR(df$data),
                  quantile(df$data, probs = c(0.9))
                )
              )
            mini_summary
          },
          striped = T,
          bordered = T,
          align = "c",
          caption = "Uploaded Data",
          caption.placement = getOption("xtable.caption.placement", "top"),
          caption.width = getOption("xtable.caption.width", NULL)
        )
      } else {
        output$fit_error <- renderText({
          c(
            "Error: Ensure that the uploaded file is a csv, has only one column of numbers (No Header Required) and that they are located in the leftmost column"
          )
        })

        output$los_fit_plot <- renderPlot({
        })

        output$los_text <- renderText({
        })

        output$los_text_help <- renderText({
        })

        output$los_fit_table <- renderTable({
        })

        output$lmini_summary <- renderTable({
        })
      }
    })

    #### Length of Service Scaled Means Tab ####
    logger::log_debug("Creating LOS scaled means tab")

    # LOS distriubtion dataframe ####
    # reads in pre-caculated values from csv stored in www folder
    # mostly calcuated by interval-censored maximum likelihood distriubtion fitting on HES
    # data, with candidate distrubtion chosen by AIC
    # But with some fitted to non-interval censored regional data (where HES fits did not
    # coverge or were otherwise unavailable). n.b the HES method is NOT the same as that
    # in the "fit your own" data tab, which assumes uncensored data
    pre_fitted_data <- read.csv("inst/app/www/fits_for_pathsimr.csv",
      check.names = FALSE
    ) %>%
      dplyr::arrange(Names)

    output$treatment_select_ui <- renderUI({
      x <- as.character(pre_fitted_data$Names)

      selectInput(
        inputId = ns("treatment_select"),
        label = "Service Point Library",
        choices = x,
        selected = x[1],
        selectize = F,
        width = "150%"
      )
    })



    observeEvent(input$go_scaled_fit, {
      table <- pre_fitted_data



      req(input$treatment_mean)

      df <-
        # as.data.frame(subset(table, table$Names == input$treatment_select))
        dplyr::filter(table, Names == input$treatment_select)

      if (df$Distribution == "exponential") {
        df$`Parameter 1 Value` <- 1 / input$treatment_mean

        df$`Parameter 1 Value` <-
          as.character(signif(df$`Parameter 1 Value`, digits = 5))
      } else if (df$Distribution == "log-normal") {
        df$`Parameter 1 Value` <-
          log(input$treatment_mean) - 0.5 * (df$`Parameter 2 Value`)^2

        df$`Parameter 1 Value` <-
          as.character(signif(df$`Parameter 1 Value`, digits = 5))
        df$`Parameter 2 Value` <-
          as.character(signif(df$`Parameter 2 Value`, digits = 5))
      } else if (df$Distribution == "gamma") {
        df$`Parameter 2 Value` <- df$`Parameter 1 Value` / input$treatment_mean

        df$`Parameter 1 Value` <-
          as.character(signif(df$`Parameter 1 Value`, digits = 5))
        df$`Parameter 2 Value` <-
          as.character(signif(df$`Parameter 2 Value`, digits = 5))
      } else if (df$Distribution == "weibull") {
        df$`Parameter 2 Value` <-
          input$treatment_mean / gamma(1 + (1 / df$`Parameter 1 Value`))

        df$`Parameter 1 Value` <-
          as.character(signif(df$`Parameter 1 Value`, digits = 5))
        df$`Parameter 2 Value` <-
          as.character(signif(df$`Parameter 2 Value`, digits = 5))
      }


      output$scaled_fit <- renderTable(
        {
          df
        },
        rownames = FALSE,
        striped = T,
        bordered = T,
        align = "c"
      )


      output$scaled_fit_plot <- renderPlot(
        {
          t_mean <- input$treatment_mean

          if (df$Distribution == "exponential") {
            x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
            y <- dexp(x, rate = as.numeric(df$`Parameter 1 Value`))
            dat <- data.frame("Time" = x, "Probability" = y)

            ggplot(data = dat) +
              geom_line(aes(x = Time, y = Probability),
                linewidth = 1,
                col = "blue"
              ) +
              theme_bw()
          } else if (df$Distribution == "log-normal") {
            x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
            y <-
              dlnorm(
                x,
                meanlog = as.numeric(df$`Parameter 1 Value`),
                sdlog = as.numeric(df$`Parameter 2 Value`)
              )
            dat <- data.frame("Time" = x, "Probability" = y)

            ggplot(data = dat) +
              geom_line(aes(x = Time, y = Probability),
                linewidth = 1,
                col = "blue"
              ) +
              theme_bw()
          } else if (df$Distribution == "gamma") {
            x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
            y <-
              dgamma(
                x,
                shape = as.numeric(df$`Parameter 1 Value`),
                rate = as.numeric(df$`Parameter 2 Value`)
              )
            dat <- data.frame("Time" = x, "Probability" = y)

            ggplot(data = dat) +
              geom_line(aes(x = Time, y = Probability),
                linewidth = 1,
                col = "blue"
              ) +
              theme_bw()
          } else if (df$Distribution == "weibull") {
            x <- seq(0, (10 * as.numeric(t_mean)), length.out = 1000)
            y <-
              dweibull(
                x,
                shape = as.numeric(df$`Parameter 1 Value`),
                scale = as.numeric(df$`Parameter 2 Value`)
              )
            dat <- data.frame("Time" = x, "Probability" = y)

            ggplot(data = dat) +
              geom_line(aes(x = Time, y = Probability),
                linewidth = 1,
                col = "blue"
              ) +
              theme_bw()
          }
        },
        res = 128
      )
    })
  })
}
