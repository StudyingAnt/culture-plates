################################################################################
#
# Default dimension setting
#
################################################################################
default_dimension <- list(
  `6 well` = list(
    dim = c(2, 3),
    plate_outer = c(127.76, 85.47),
    a1_offset = c(26.76, 25.47),
    well_well = 39.12,
    d = 35.43
  ),
  `12 well` = list(
    dim = c(3, 4),
    plate_outer = c(127.76, 85.47),
    a1_offset = c(24.94, 16.79),
    well_well = 26.01,
    d = 24.73
  ),
  `24 well` = list(
    dim = c(4, 6),
    plate_outer = c(127.76, 85.47),
    a1_offset = c(17.76, 14.47),
    well_well = 19.3,
    d = 18.26
  ),
  `48 well` = list(
    dim = c(6, 8),
    plate_outer = c(127.76, 85.47),
    a1_offset = c(18.76, 10.47),
    well_well = 13.38,
    d = 12.56
  ),
  `96 well` = list(
    dim = c(8, 12),
    plate_outer = c(127.76, 85.47),
    a1_offset = c(11.76, 10.47),
    well_well = 9.88,
    d = 9.63
  )
)

################################################################################
#
# Empty 6-96 well plate
#
################################################################################
culture_plate_empty <- function(plate_type, ref = default_dimension) {
  # dimension
  n_row         <- default_dimension[[plate_type]][["dim"]][1]
  n_col         <- default_dimension[[plate_type]][["dim"]][2]
  plate_length  <- default_dimension[[plate_type]][["plate_outer"]][1]
  plate_width   <- default_dimension[[plate_type]][["plate_outer"]][2]
  a1_row_offset <- default_dimension[[plate_type]][["a1_offset"]][1]
  a1_col_offset <- default_dimension[[plate_type]][["a1_offset"]][2]
  well_well     <- default_dimension[[plate_type]][["well_well"]][1]
  d             <- default_dimension[[plate_type]][["d"]][1]
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
      plate_plot <- plate_plot + geom_circle(aes(x0 = {{x0}},
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
culture_plate <- function(plate_type, ref = default_dimension) {
  # dimension
  n_row         <- default_dimension[[plate_type]][["dim"]][1]
  n_col         <- default_dimension[[plate_type]][["dim"]][2]
  plate_length  <- default_dimension[[plate_type]][["plate_outer"]][1]
  plate_width   <- default_dimension[[plate_type]][["plate_outer"]][2]
  a1_row_offset <- default_dimension[[plate_type]][["a1_offset"]][1]
  a1_col_offset <- default_dimension[[plate_type]][["a1_offset"]][2]
  well_well     <- default_dimension[[plate_type]][["well_well"]][1]
  d             <- default_dimension[[plate_type]][["d"]][1]
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
      plate_plot <- plate_plot + geom_circle(aes(x0 = {{x0}},
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
