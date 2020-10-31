# trying to package up the forest plot table

# packages required

library(dplyr)
library(ggplot2)
library(patchwork)
library(magick)
library(gridExtra)

# inputs

test <- readxl::read_excel(here::here("example_figure_data.xlsx"))

left_side_data <- test[,1:3]

estimate <- test$Estimate
ci_low <- test$`CI low`
ci_high <- test$`CI high`

forestable <- function(left_side_data, estimate, ci_low, ci_high,
                    right_side_data = NULL,
                    theme = NULL,
                    estimate_precision = 1,
                    ggplot_is_x_times_right_width = 1.2,
                    null_line_at = 0,
                    file_path = "forestable_plot.png",
                    dpi = 600){

  if(is.null(theme)){
    theme <- ttheme_minimal(core=list(
      fg_params = list(hjust = 0, x = 0.05, fontfamily = "mono"),
      bg_params = list(fill=c(rep(c("#eff3f2", "white"), length.out=4)))
    ),
    colhead = list(fg_params = list(hjust = 0, x = 0.05,
                                    fontfamily = "mono"),
                   bg_params = list(fill = "white"))
    )
  }

  gdata <- data.frame(estimate = estimate,
                    ci_low = ci_low,
                    ci_high = ci_high)

  tdata <- gdata

  tdata <- tdata %>% mutate_all(~sprintf(.,
                      fmt = paste0('%#.', estimate_precision,'f')
                   ))

  tdata[tdata == "NA"] <- " "

  if(is.null(right_side_data)){
    # pretty formatting for confidence intervals
    right_side_data <- data.frame(Estimate = ifelse(tdata$estimate == " ",
                                  " ", paste0(tdata$estimate, " (", tdata$ci_low,
                                      " to ", tdata$ci_high, ")")))


  }

  # checks

  assertthat::are_equal(nrow(left_side_data), nrow(right_side_data))

  # calculated quantities

  find_width <- function(data){
    num_of_rows <- nrow(data)
    num_of_cols <- ncol(data)

    print_data <- data %>% mutate_all(as.character)

    num_char_across <- 0
    width <- 0

    for(i in 1:num_of_cols){
      for(j in 1:num_of_rows){
        num_char_across[j] <- nchar(print_data[j, i])
      }
      width[i] <- max(max(num_char_across, na.rm = T), nchar(colnames(print_data)[i]), na.rm = T)
    }
    return(sum(width, na.rm = T))
  }

  left_width <- find_width(left_side_data)
  right_width <- find_width(right_side_data)

  # insert a blank column so we can put the ggplot object on top
  # and correctly order columns

  ggplot_width <- round(right_width * ggplot_is_x_times_right_width, 0)
  total_width <- left_width + right_width + ggplot_width

  tdata_print <- left_side_data
  tdata_print$` ` <- paste(rep(" ", times = ggplot_width),
                           collapse = '')
  tdata_print <- cbind(tdata_print, right_side_data)

  # function to calculate greatest common factor

  gcf <- function(a, b){
    t <- 0
    while (b != 0){
      t <- b
      b <- a %% b
      a <- t
    }
    return(a)
  }

  # make one unit in patchwork equal in width to the greatest common factor of
  # the three widths

  one_patchwork_unit <- gcf(gcf(left_width, right_width), ggplot_width)

  left_pw <- left_width/one_patchwork_unit
  right_pw <- right_width/one_patchwork_unit
  total_pw <- total_width/one_patchwork_unit

  # calculated patchwork layout

  layout <- c(area(t = 1,
                   b = nrow(gdata),
                   l = 1,
                   r = total_pw),
            area(t = 1,
                 b = (nrow(gdata) + 1),
                 l = left_pw + 1,
                 r = total_pw - right_pw + 1))

  gdata$row_num <- (nrow(gdata) - 1):0

  y_low <- -.54 - .1381 * log(nrow(gdata))
  y_high <- nrow(gdata) + -.3

  ########## the main figure - this will be overlaid on the table ##############

  center <- ggplot(data = gdata, aes(y = row_num, x = estimate)) +
    geom_point(size = 3.25) + # the point estimates, with big dots
    geom_errorbarh(aes(y = row_num,
                       xmin = ci_low,
                       xmax = ci_high),
                   height = .25) + # the CIs, with short ends
    theme_classic() + # base theme
    scale_y_continuous(expand = c(0,0), #remove padding
                       limits = c(y_low, y_high)) + # position dots
    theme(axis.title.y = element_blank(), # remove axis, make bg transparent
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.length.x = unit(.1, "in"),
          text = element_text(family = "mono", size = 12),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent")) +
    geom_vline(xintercept = null_line_at, linetype = "dashed") + # null line
    scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
    xlab("")

  ######### using patchwork, overlay the ggplot on the table ###################

  final <- wrap_elements(tableGrob(tdata_print, theme = theme, rows = NULL)) +
    center +
    plot_layout(design = layout)

  ######### save the plot as a png, then display it with magick ################

  ggsave(dpi = dpi, height = (nrow(gdata) + 3)/3.85,
         width = total_width/10 + 1, units = "in",
         filename = file_path)

  img <- magick::image_read(file_path)
  plot(img)

}

forestable(left_side_data = left_side_data[1:30,],
           estimate = estimate[1:30],
           ci_low = ci_low[1:30],
           ci_high = ci_high[1:30])



