position_jitter_ellipse <- function(width = NULL,
                                    height = NULL,
                                    seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }
  
  ggproto(
    NULL,
    PositionJitterEllipse,
    width = width,
    height = height,
    seed = seed
  )
}


PositionJitterEllipse <-
  ggproto(
    "PositionJitterEllipse",
    Position,
    required_aes = c("x", "y"),
    
    setup_params = function(self, data) {
      list(
        width = self$width %||% (resolution(data$x, zero = FALSE) * 0.4),
        height = self$height %||% (resolution(data$y, zero = FALSE) * 0.4),
        seed = self$seed
      )
    },
    # https://stackoverflow.com/questions/5529148/algorithm-calculate-pseudo-random-point-inside-an-ellipse
    # https://stats.stackexchange.com/questions/120527/simulate-a-uniform-distribution-on-a-disc
    compute_layer = function(self, data, params, layout) {
      trans_x <-
        function(x) {
          set.seed(params$seed)
          n <- length(x)
          rho <- sqrt(runif(n)) * params$width
          theta <- runif(n, 0, 2 * pi)
          x + rho * cos(theta)
        }
      trans_y <-
        function(x) {
          set.seed(params$seed)
          n <- length(x)
          rho <- sqrt(runif(n)) * params$height
          theta <- runif(n, 0, 2 * pi)
          x + rho * sin(theta)
        }
      
      ggplot2:::with_seed_null(params$seed, transform_position(data, trans_x, trans_y))
    }
  )
