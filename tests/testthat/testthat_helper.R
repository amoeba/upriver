params_simple <- list("reaches"   = 0:6,
                      "rates"     = c(50,  50,  50,  50,  50),
                      "distances" = c(100, 100, 100, 100, 100))

params_complex <- list("reaches"   = 0:6,
                       "rates"     = c(43.5,  48.9,  36.2,  37.1,  50),
                       "distances" = c(102.8, 212,   76.8,  24.2,  190))


arrival_single <- data.frame(
  day = 5,
  proportion = 1
)

arrival_simple <- data.frame(
  day = 1:10,
  proportion = c(0, 0, 0.1, 0.2, 0.2, 0.2, 0.2, 0.1, 0, 0)
)
