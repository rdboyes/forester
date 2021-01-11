#' Create a forest plot column in a table
#'
#' Creates an png image with the forest plot column inserted in the supplied data frame.
#'
#' @param left_side_data Data frame (required). The information to be displayed to the left of the forest plot.
#' @param estimate Vector. The point estimates to be displayed in the forest plot.
#' @param ci_low Vector. The lower confidence bounds.
#' @param ci_high Vector. The upper confidence bounds.
#' @param right_side_data Data frame (optional). Information to be displayed on the right side of the table. If not supplied, an Estimate column is generated automatically.
#' @param estimate_precision Integer. The number of decimal places on the estimate (default 1)
#' @param ggplot_width Integer. The width of forest plot in characters (default 30)
#' @param null_line_at Numeric. Default 0. Change to 1 if using relative measures such as OR, RR.
#' @param file_path String. Where to save the image, default "forestable_plot.png" in the current working directory.
#' @param dpi Numeric. The image resolution in dpi, default 600
#' @param display Logical. Show the table in RStudio viewer? Default TRUE
#' @param blank_na Logical. Should missing values in the left side table be displayed as blank? Default TRUE, if FALSE, NA values will be shown
#' @param font_family String. The font to use for the ggplot and table. Default "mono".
#' @param estimate_col_name String. The name for the generated estimate column. Default "Estimate"
#' @param stripe_colour Hex String. Colour to use for the table stripes, default "#eff3f2"
#' @param x_scale_linear Logical. Default TRUE, change to FALSE for log scale
#' @param xlim Vector. Manually specify limits for the x axis as a vector length 2, i.e. c(low, high)
#' @param xbreaks Vector. X axis breaks to label. Specify limits in xlim if using this option.
#' @param nudge_y Numeric. Allows small changes to the vertical alignment of the forest plot points. 1 unit is approximately the height of 1 row.
#' @param arrows Logical. Should there be arrows displayed below the ggplot? Default FALSE. Specify xlim if using arrows.
#' @param arrow_labels String Vector, length 2. Labels for the arrows. Set arrows to TRUE or this will have no effect.
#' @param add_plot A ggplot object to add to the right side of the table. To align correctly with rows, 1 unit is the height of a row and y = 0 for the center of the bottom row.
#' @param add_plot_width Numeric (0 - 1). Width to display add_plot, relative to the width of the table. Default 0.3.
#'
#' @return image
#' @importFrom rlang .data
#' @importFrom graphics text
#' @export
#'
#' @examples
forester <- function(left_side_data,
                    estimate,
                    ci_low,
                    ci_high,
                    right_side_data = NULL,
                    estimate_precision = 1,
                    ggplot_width = 30,
                    null_line_at = 0,
                    file_path = here::here("forester_plot.png"),
                    dpi = 600,
                    display = TRUE,
                    blank_na = TRUE,
                    font_family = "mono",
                    estimate_col_name = "Estimate",
                    stripe_colour = "#eff3f2",
                    x_scale_linear = TRUE,
                    xlim = NULL,
                    xbreaks = NULL,
                    nudge_y = NULL,
                    arrows = FALSE,
                    arrow_labels = c("Lower", "Higher"),
                    add_plot = NULL,
                    add_plot_width = 0.3){

  theme <- gridExtra::ttheme_minimal(core=list(
    fg_params = list(hjust = 0, x = 0.05, fontfamily = font_family),
    bg_params = list(fill=c(rep(c(stripe_colour, "white"), length.out=nrow(left_side_data)), "white", "white", "white"))
  ),
    colhead = list(fg_params = list(hjust = 0, x = 0.05,
                                    fontfamily = font_family),
                   bg_params = list(fill = "white"))
  )

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

    colnames(right_side_data) <- estimate_col_name

  }

  # finds width in number of characters for monospaced font

  find_width_mono <- function(data){
    num_of_rows <- nrow(data)
    num_of_cols <- ncol(data)

    print_data <- dplyr::mutate_all(data, as.character)

    num_char_across <- 0
    width <- 0

    for(i in 1:num_of_cols){
      for(j in 1:num_of_rows){
        num_char_across[j] <- nchar(print_data[j, i])
      }
      width[i] <- max(max(num_char_across, na.rm = TRUE),
                      nchar(colnames(print_data)[i]), na.rm = TRUE)
    }
    return(sum(width, na.rm = TRUE))
  }

  # finds width using shape_string from the systemfonts package
  # if not using monospaced font

  find_width <- function(data){
    num_of_rows <- nrow(data)
    num_of_cols <- ncol(data)

    print_data <- dplyr::mutate_all(data, as.character)

    width <- 0

    names <- colnames(print_data)

    for (i in 1:num_of_cols){
      temp <- systemfonts::shape_string(print_data[[names[i]]], family = font_family)
      temp_col <- systemfonts::shape_string(names[i], family = font_family)
      width[i] <- max(max(temp$metrics$width, na.rm = TRUE),
                      temp_col$metrics$width, na.rm = TRUE)
    }
    return(sum(width, na.rm = TRUE)/7.2)
  }

  # calculate widths for each side with the appropriate function

  if(font_family == "mono"){
    left_width <- find_width_mono(left_side_data)
    right_width <- find_width_mono(right_side_data)
  }else{
    left_width <- find_width(left_side_data)
    right_width <- find_width(right_side_data)
  }

  # insert a blank column so we can put the ggplot object on top
  # and correctly order columns

  total_width <- left_width + right_width + ggplot_width

  tdata_print <- left_side_data
  tdata_print$` ` <- paste(rep(" ", times = round(ggplot_width, 0)),
                           collapse = '')
  tdata_print <- cbind(tdata_print, right_side_data)

  tdata_print <- tibble::add_row(tdata_print)
  tdata_print <- tibble::add_row(tdata_print)
  tdata_print <- tibble::add_row(tdata_print)

  if(blank_na == TRUE){
    tdata_print <- dplyr::mutate_all(tdata_print, as.character)
    tdata_print[is.na(tdata_print)] <- " "
  }

  mono_column <- function(table, col){
    col_indexes <- function(table, col, name="core-fg"){
      l <- table$layout
      which(l$l==col & l$name==name)
    }

    ind <- col_indexes(table, col, "core-fg")

    for(i in ind){
      table$grobs[i][[1]][["gp"]] <- grid::gpar(fontfamily = "mono")
    }
    return(table)
  }

  ######## calculations for the top and bottom of the plot

  gdata$row_num <- (nrow(gdata) - 1):0

  if(is.null(nudge_y)){
    h_adj <- dplyr::case_when(
      font_family == "mono" ~ 0,
      font_family == "Fira Sans" ~ .2,
      font_family == "Times New Roman" ~ .2,
      TRUE ~ 0.3
    )
  }else{
    h_adj <- 0.3 + nudge_y
  }

  y_low <- -.5 - .1381 * log(nrow(gdata)) + h_adj
  y_high <- 1.017 * nrow(gdata) - 0.6

  ########## the main figure - this will be overlaid on the table ##############

  center <- ggplot2::ggplot(data = gdata, ggplot2::aes(y = .data$row_num, x = estimate)) +
    ggplot2::geom_point(size = 3.25) + # the point estimates, with big dots
    ggplot2::geom_errorbarh(ggplot2::aes(y = .data$row_num,
          xmin = ci_low,
          xmax = ci_high),
          height = .25) +
    ggplot2::theme_classic() + # base theme
    ggplot2::scale_y_continuous(expand = c(0,0), #remove padding
          limits = c(y_low, y_high)) + # position dots
    ggplot2::theme(axis.title.y = ggplot2::element_blank(), # remove axis, make bg transparent
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.ticks.length.x = grid::unit(.07, "in"),
          text = ggplot2::element_text(family = font_family, size = 12),
          panel.background = ggplot2::element_rect(fill = "transparent"),
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(fill = "transparent"),
          legend.box.background = ggplot2::element_rect(fill = "transparent")) +
    ggplot2::geom_vline(xintercept = null_line_at, linetype = "dashed") +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
      ggplot2::xlab("")

  ######## Optional customizations here ########

  if(x_scale_linear){
    center <- center + ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
      ggplot2::xlab("")
  }else{
    center <- center + ggplot2::scale_x_log10(labels = scales::number_format(accuracy = 0.1)) +
      ggplot2::xlab("")
  }

  if(!is.null(xlim)){
    if(is.null(xbreaks)){
      center <- center + ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                                                     limits = xlim,
                                                     expand = c(0,0))
    }else{
      center <- center + ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                                                     breaks = xbreaks,
                                            limits = xlim,
                                            expand = c(0,0))
    }
  }

  ######################## Arrows ##############################
  if(arrows == TRUE){

    # this df has the text labels
    xlab_df <- data.frame(text = arrow_labels,
                          x = xlim,
                          y = c(0, 0),
                          hjust = c(0, 1))

    a_small_amount <- abs(xlim[1] - xlim[2])/35

    # this df has the arrows
    arrow_df <- data.frame(id = c(1,2),
                           xstart = c(null_line_at - a_small_amount, null_line_at + a_small_amount),
                           xend = c(xlim[1] + a_small_amount, xlim[2] - a_small_amount),
                           y = c(1, 1))

    # create the arrow/label ggplot object
    arrows_plot <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = arrow_df, ggplot2::aes(x = .data$xstart, xend = .data$xend, y = .data$y, yend = .data$y),
                 arrow = ggplot2::arrow(angle = 15, type = "closed", length = grid::unit(0.1, "in"))) +
      ggplot2::geom_text(data = xlab_df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$text, hjust = .data$hjust),
              family = font_family, size = 3) +
      ggplot2::scale_y_continuous(expand = c(0,0), limits = c(-0.5, 1.75)) +
      ggplot2::scale_x_continuous(expand = c(0,0), limits = xlim) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"),
          plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(fill = "transparent"),
          legend.box.background = ggplot2::element_rect(fill = "transparent"),
          panel.border = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.line.x = ggplot2::element_blank())
  }


  ######### using patchwork, overlay the ggplot on the table ###################

  table_final <- mono_column(gridExtra::tableGrob(tdata_print, theme = theme, rows = NULL), ncol(left_side_data) + 1)

  table_final$widths[ncol(left_side_data) + 1] <- grid::unit(ggplot_width/10, "in")

  table_final$heights <- grid::unit(rep(0.255, times = length(table_final$heights)), "in")

  final <- patchwork::wrap_elements(table_final) +
                    patchwork::inset_element(center,
                             align_to = "full",
                             left = (left_width/total_width),
                             right = ((ggplot_width + left_width)/total_width),
                             top = 1,
                             bottom = 0.35/nrow(gdata))

  if(arrows == TRUE){
    final <- final + patchwork::inset_element(arrows_plot,
                                              align_to = "full",
                                              left = (left_width/total_width),
                                              right = ((ggplot_width + left_width)/total_width),
                                              top = 1.5/nrow(gdata),
                                              bottom = 0)
  }

  if(!is.null(add_plot)){
    canvas <- ggplot(tibble(x = 0, y = 0), aes(x = x, y = y)) + geom_point() + theme_void()

    extra_plot <- canvas +
      patchwork::inset_element(final,
                               align_to = "full",
                               left = 0,
                               right = 1 - add_plot_width,
                               top = 1,
                               bottom = 0) +
      patchwork::inset_element(add_plot,
                               align_to = "full",
                               left = add_plot_width,
                               right = 1,
                               top = 1,
                               bottom = 0.35/nrow(gdata))

    final <- extra_plot
  }

  ######### save the plot as a png, then display it with magick ################

  png_width <- total_width/10 + 1

  if(!is.null(add_plot)){png_width <- png_width/(1 - add_plot_width)}

  ggplot2::ggsave(dpi = dpi,
         height = (nrow(gdata) + 3)/3.8,
         width = png_width, units = "in",
         filename = file_path)

  if(display == TRUE){
    magick::image_resize(magick::image_read(file_path),
                         paste0(grDevices::dev.size("px")[1],
                                "x",
                                grDevices::dev.size("px")[2]))
  }
}


