#' Create a forest plot column in a table
#'
#' Creates an png image with the forest plot column inserted in the supplied data frame.
#'
#' @param left_side_data A data frame with the information to be displayed to the left of the forest plot.
#' @param estimate A vector containing the point estimates to be displayed in the forest plot.
#' @param ci_low A vector containing the lower confidence bounds
#' @param ci_high A vector containing the upper confidence bounds
#' @param right_side_data A data frame (optional) containing the information to be displayed on the right side of the table. If not supplied, an Estimate column is generated automatically.
#' @param theme A theme object for the table (optional)
#' @param estimate_precision Integer. The number of decimal places on the estimate (default 1)
#' @param ggplot_is_x_times_right_width The width of forest plot relative to the right side of the table (default 1.2)
#' @param null_line_at numeric, default 0. Change to 1 if using relative measures such as OR, RR.
#' @param file_path Where to save the image, default "forestable_plot.png" in the current working directory.
#' @param dpi The image resolution in dpi, default 600
#' @param display Show the table in RStudio viewer? Default TRUE
#' @param blank_na Should missing values in the left side table be displayed as blank? Default TRUE, if FALSE, NA values will be shown
#'
#' @return image
#' @importFrom rlang .data
#' @export
#'
#' @examples
forestable <- function(left_side_data, estimate, ci_low, ci_high,
                    right_side_data = NULL,
                    theme = NULL,
                    estimate_precision = 1,
                    ggplot_is_x_times_right_width = 1.2,
                    null_line_at = 0,
                    file_path = here::here("forestable_plot.png"),
                    dpi = 600,
                    display = TRUE,
                    blank_na = TRUE){

  if(is.null(theme)){
    theme <- gridExtra::ttheme_minimal(core=list(
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

  if(is.null(right_side_data)){
    tdata <- gdata

    tdata <- dplyr::mutate_all(tdata, ~sprintf(.,
        fmt = paste0('%#.', estimate_precision,'f')
    ))

    tdata[tdata == "NA"] <- " "
    # pretty formatting for confidence intervals
    right_side_data <- data.frame(Estimate = ifelse(tdata$estimate == " ",
                                  " ", paste0(tdata$estimate, " (", tdata$ci_low,
                                      " to ", tdata$ci_high, ")")))

  }

  # calculated quantities

  find_width <- function(data){
    num_of_rows <- nrow(data)
    num_of_cols <- ncol(data)

    print_data <- dplyr::mutate_all(data, as.character)

    num_char_across <- 0
    width <- 0

    for(i in 1:num_of_cols){
      for(j in 1:num_of_rows){
        num_char_across[j] <- nchar(print_data[j, i])
      }
      width[i] <- max(max(num_char_across, na.rm = T),
                      nchar(colnames(print_data)[i]), na.rm = T)
    }
    return(sum(width, na.rm = T))
  }

  if(blank_na == TRUE){
    left_side_data[is.na(left_side_data)] <- " "
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

  layout <- c(patchwork::area(t = 1,
                b = nrow(gdata),
                l = 1,
                r = total_pw),
              patchwork::area(t = 1,
                b = (nrow(gdata) + 1),
                l = left_pw + 1,
                r = total_pw - right_pw + 1))

  gdata$row_num <- (nrow(gdata) - 1):0

  y_low <- -.54 - .1381 * log(nrow(gdata))
  y_high <- nrow(gdata) + -.3

  ########## the main figure - this will be overlaid on the table ##############

  center <- ggplot2::ggplot(data = gdata, ggplot2::aes(y = .data$row_num, x = estimate)) +
    ggplot2::geom_point(size = 3.25) + # the point estimates, with big dots
    ggplot2::geom_errorbarh(ggplot2::aes(y = .data$row_num,
          xmin = ci_low,
          xmax = ci_high),
          height = .25) + # the CIs, with short ends
    ggplot2::theme_classic() + # base theme
    ggplot2::scale_y_continuous(expand = c(0,0), #remove padding
          limits = c(y_low, y_high)) + # position dots
    ggplot2::theme(axis.title.y = ggplot2::element_blank(), # remove axis, make bg transparent
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.ticks.length.x = grid::unit(.1, "in"),
          text = ggplot2::element_text(family = "mono", size = 12),
          panel.background = ggplot2::element_rect(fill = "transparent"),
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(fill = "transparent"),
          legend.box.background = ggplot2::element_rect(fill = "transparent")) +
    ggplot2::geom_vline(xintercept = null_line_at, linetype = "dashed") + # null line
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
    ggplot2::xlab("")

  ######### using patchwork, overlay the ggplot on the table ###################

  final <- patchwork::wrap_elements(gridExtra::tableGrob(tdata_print, theme = theme, rows = NULL)) +
    center +
    patchwork::plot_layout(design = layout)

  ######### save the plot as a png, then display it with magick ################

  ggplot2::ggsave(dpi = dpi, height = (nrow(gdata) + 3)/3.85,
         width = total_width/10 + 1, units = "in",
         filename = file_path)

  if(display == TRUE){
    img <- magick::image_read(file_path)
    plot(img)
  }
}


