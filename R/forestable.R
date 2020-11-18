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
#' @param ggplot_width The width of forest plot in characters (default 26)
#' @param null_line_at numeric, default 0. Change to 1 if using relative measures such as OR, RR.
#' @param file_path Where to save the image, default "forestable_plot.png" in the current working directory.
#' @param dpi The image resolution in dpi, default 600
#' @param display Show the table in RStudio viewer? Default TRUE
#' @param blank_na Should missing values in the left side table be displayed as blank? Default TRUE, if FALSE, NA values will be shown
#' @param font_family The font to use for the ggplot and table
#' @param estimate_col_name The name for the generated estimate column. Default "Estimate"
#' @param stripe_colour Colour to use for the table stripes, default "#eff3f2"
#' @param x_scale_linear Default TRUE, change to FALSE for log scale
#' @param xlim Manually specify limits for the x axis as a vector length 2, i.e. c(low, high)
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
                    ggplot_width = 30,
                    null_line_at = 0,
                    file_path = here::here("forestable_plot.png"),
                    dpi = 600,
                    display = TRUE,
                    blank_na = TRUE,
                    font_family = "mono",
                    estimate_col_name = "Estimate",
                    stripe_colour = "#eff3f2",
                    x_scale_linear = TRUE,
                    xlim = NULL){

  if(is.null(theme)){
    theme <- gridExtra::ttheme_minimal(core=list(
      fg_params = list(hjust = 0, x = 0.05, fontfamily = font_family),
      bg_params = list(fill=c(rep(c(stripe_colour, "white"), length.out=4)))
    ),
    colhead = list(fg_params = list(hjust = 0, x = 0.05,
                                    fontfamily = font_family),
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

  if(blank_na == TRUE){
    left_side_data <- dplyr::mutate_all(left_side_data, as.character)
    left_side_data[is.na(left_side_data)] <- " "
  }

  # insert a blank column so we can put the ggplot object on top
  # and correctly order columns

  total_width <- left_width + right_width + ggplot_width

  tdata_print <- left_side_data
  tdata_print$` ` <- paste(rep(" ", times = round(ggplot_width, 0)),
                           collapse = '')
  tdata_print <- cbind(tdata_print, right_side_data)

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

  text_height <- systemfonts::shape_string("Hello", family = font_family)$metrics$height[1] + 11.16

  baseline_height <- 23.25 + 11.16

  h_adj <- ifelse(font_family == "mono",
                  1,
                  text_height/baseline_height)

  height <- h_adj * nrow(gdata)

  y_low <- -.54 - .1381 * log(nrow(gdata)) + (h_adj - 1) * 6
  y_high <- 1.017 * nrow(gdata) - 0.35 - (h_adj - 1) * 6

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
          axis.ticks.length.x = grid::unit(.1, "in"),
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
    center <- center + ggplot2::xlim(xlim)
  }

  ######### using patchwork, overlay the ggplot on the table ###################

  canvas <- ggplot2::ggplot() + ggplot2::theme_void()

  table_final <- mono_column(gridExtra::tableGrob(tdata_print, theme = theme, rows = NULL), ncol(left_side_data) + 1)

  table_final$widths[ncol(left_side_data) + 1] <- grid::unit(ggplot_width/10, "in")

  final <- canvas + patchwork::inset_element(patchwork::wrap_elements(table_final),
                             align_to = "full",
                             left = 0,
                             right = 1,
                             top = 1,
                             bottom = 0.4684 -.1155 * log(nrow(gdata))) +
                    patchwork::inset_element(center,
                             align_to = "full",
                             left = (left_width/total_width),
                             right = ((ggplot_width + left_width)/total_width),
                             top = 1,
                             bottom = 0)

  ######### save the plot as a png, then display it with magick ################

  ggplot2::ggsave(dpi = dpi,
         height = h_adj * (nrow(gdata) + 3)/3.8 + h_adj - 1,
         width = total_width/10 + 1, units = "in",
         filename = file_path)

  if(display == TRUE){
    magick::image_resize(magick::image_read(file_path),
                         paste0(grDevices::dev.size("px")[1],
                                "x",
                                grDevices::dev.size("px")[2]))
  }
}


