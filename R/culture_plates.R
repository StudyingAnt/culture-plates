################################################################################
#
# Default dimension setting
#
################################################################################


################################################################################
#
# Empty 6-96 well plate
#
################################################################################
#' Draw empty plate
#'
#' @param plate_type Plate type e.g. 6 wells
#' @param ref dimension for plates
#'
#' @return ggplot of plate
#' @export
#'
#'
#' @examples culture_plate_empty("6 wells")

culture_plate_empty <- function(plate_type, ref = default_dimension) {
  default_dimension <- list(
    `6 wells` = list(
      dim = c(2, 3),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(26.76, 25.47),
      well_well = 39.12,
      d = 35.43
    ),
    `12 wells` = list(
      dim = c(3, 4),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(24.94, 16.79),
      well_well = 26.01,
      d = 24.73
    ),
    `24 wells` = list(
      dim = c(4, 6),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(17.76, 14.47),
      well_well = 19.3,
      d = 18.26
    ),
    `48 wells` = list(
      dim = c(6, 8),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(18.76, 10.47),
      well_well = 13.38,
      d = 12.56
    ),
    `96 wells` = list(
      dim = c(8, 12),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(11.76, 10.47),
      well_well = 9.88,
      d = 9.63
    )
  )

  # dimension
  n_row         <- ref[[plate_type]][["dim"]][1]
  n_col         <- ref[[plate_type]][["dim"]][2]
  plate_length  <- ref[[plate_type]][["plate_outer"]][1]
  plate_width   <- ref[[plate_type]][["plate_outer"]][2]
  a1_row_offset <- ref[[plate_type]][["a1_offset"]][1]
  a1_col_offset <- ref[[plate_type]][["a1_offset"]][2]
  well_well     <- ref[[plate_type]][["well_well"]][1]
  d             <- ref[[plate_type]][["d"]][1]
  r             <- d/2

  row_label_offset <- (a1_row_offset-r)/2
  col_label_offset <- (a1_col_offset-r)/2

  # plate outer line
  plate_df <- data.frame(
    X = c(0, plate_length, plate_length, 0, 0),
    Y = c(0, 0, plate_width, plate_width, 0)
  )

  # might need?
  ggtext_size <- function(base_size, ratio = 1) {
    ratio * base_size / ggplot2::.pt
  }

  # draw plate
  plate_plot <- ggplot(plate_df, aes(x=X, y=Y)) +
    geom_path()

  for (i in 1:n_row) {
    for (j in 1:n_col){
      x0 <- a1_row_offset+(j-1)*well_well
      y0 <- plate_width-a1_col_offset-(i-1)*well_well
      plate_plot <- plate_plot + ggforce::geom_circle(aes(x0 = {{x0}},
                                                          y0 = {{y0}},
                                                          r=r),
                                                      inherit.aes = FALSE)

    }
  }

  for (i in 1:n_row) {
    alphabets <- c("A", "B", "C", "D", "E", "F", "G", "H")
    plate_plot <- plate_plot + annotate("text",
                                        x = {{a1_row_offset-row_label_offset-r}},
                                        y = {{plate_width-a1_col_offset-(i-1)*well_well}},
                                        label = alphabets[i])
  }

  for (j in 1:n_col) {
    plate_plot <- plate_plot + annotate("text",
                                        x = {{a1_row_offset+(j-1)*well_well}},
                                        y = {{plate_width-col_label_offset}},
                                        label = as.character(j))
  }


  plate_plot <- plate_plot +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white")
    )

  return(plate_plot)
}

################################################################################
#
# Colored 6-96 well plate
#
################################################################################
#' Draw plate and color by condition
#'
#' @param data data frame of plate
#' @param condition_columns columns name for condition
#' @param color colors for condtion to use
#' @param plate_type Plate type e.g. 6 wells
#' @param ref dimension for plates
#'
#' @return ggplot of plate
#' @export
#'
#'
#' @examples culture_plate_empty("6 wells")

# library(dplyr)
# library(tidyr)
#
# test_df <- data.frame(
#   row = c("A", "A", "A", "B", "B", "B"),
#   col = c(1, 2, 3, 1, 2, 3),
#   con1 = c("ctrl","ctrl","ctrl","treatment","treatment","treatment"),
#   con2 = c("cell1","cell2","cell3","cell1","cell2","cell3")
# )


culture_plate <- function(data,
                          condition_columns,
                          color = default_color,
                          plate_type = "96 wells",
                          ref = default_dimension) {
  default_dimension <- list(
    `6 wells` = list(
      dim = c(2, 3),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(26.76, 25.47),
      well_well = 39.12,
      d = 35.43
    ),
    `12 wells` = list(
      dim = c(3, 4),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(24.94, 16.79),
      well_well = 26.01,
      d = 24.73
    ),
    `24 wells` = list(
      dim = c(4, 6),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(17.76, 14.47),
      well_well = 19.3,
      d = 18.26
    ),
    `48 wells` = list(
      dim = c(6, 8),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(18.76, 10.47),
      well_well = 13.38,
      d = 12.56
    ),
    `96 wells` = list(
      dim = c(8, 12),
      plate_outer = c(127.76, 85.47),
      a1_offset = c(11.76, 10.47),
      well_well = 9.88,
      d = 9.63
    )
  )

  # make color
  default_color <- c(
    "#ff0000", "#ff8700", "#ffd300", "#deff0a", "#a1ff0a",
    "#0aff99", "#0aefff", "#147df5", "#580aff", "#be0aff"
  )

  if ("color" %in% colnames(data)) {

  } else {
    data.condition <- unite(data, new_condition, condition_columns)
    conditions <- data.condition %>% select(new_condition) %>% distinct()

    data.condition <- data.condition %>% rowwise() %>%
      mutate(color = color[which(conditions == new_condition)])

    data$color <- data.condition$color
  }

  # dimension
  n_row         <- ref[[plate_type]][["dim"]][1]
  n_col         <- ref[[plate_type]][["dim"]][2]
  plate_length  <- ref[[plate_type]][["plate_outer"]][1]
  plate_width   <- ref[[plate_type]][["plate_outer"]][2]
  a1_row_offset <- ref[[plate_type]][["a1_offset"]][1]
  a1_col_offset <- ref[[plate_type]][["a1_offset"]][2]
  well_well     <- ref[[plate_type]][["well_well"]][1]
  d             <- ref[[plate_type]][["d"]][1]
  r             <- d/2

  row_label_offset <- (a1_row_offset-r)/2
  col_label_offset <- (a1_col_offset-r)/2

  # plate outer line
  plate_df <- data.frame(
    X = c(0, plate_length, plate_length, 0, 0),
    Y = c(0, 0, plate_width, plate_width, 0)
  )

  # might need?
  ggtext_size <- function(base_size, ratio = 1) {
    ratio * base_size / ggplot2::.pt
  }

  # draw plate
  alphabets <- c("A", "B", "C", "D", "E", "F", "G", "H")

  plate_plot <- ggplot(plate_df, aes(x=X, y=Y)) +
    geom_path()

  for (i in 1:n_row) {
    for (j in 1:n_col){
      well_color <- data %>% rowwise() %>%
        filter(row == alphabets[i], col == j) %>% select(color)

      x0 <- a1_row_offset+(j-1)*well_well
      y0 <- plate_width-a1_col_offset-(i-1)*well_well
      plate_plot <- plate_plot + geom_circle(aes(x0 = {{x0}},
                                                 y0 = {{y0}},
                                                 r=r),
                                             fill = {{well_color$color}},
                                             inherit.aes = FALSE)
    }
  }

  for (i in 1:n_row) {
    plate_plot <- plate_plot + annotate("text",
                                        x = {{a1_row_offset-row_label_offset-r}},
                                        y = {{plate_width-a1_col_offset-(i-1)*well_well}},
                                        label = alphabets[i])
  }

  for (j in 1:n_col) {
    plate_plot <- plate_plot + annotate("text",
                                        x = {{a1_row_offset+(j-1)*well_well}},
                                        y = {{plate_width-col_label_offset}},
                                        label = as.character(j))
  }

  plate_plot <- plate_plot +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white")
    )

  return(plate_plot)
}
