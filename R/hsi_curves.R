# Functions for calculating HSI for Chinook salmon ("chnk") and steelhead ("sthd")
# and for the juvenile ("juv") and spawning ("spw") life stages
# from Maret et al. 2006

## Chinook salmon, juvenile, depth
chnk_juv_d <- function(x) {
  y = if_else(x <= 0.061, 0,
              if_else(x > 0.061 & x <= 0.1219, 0.8202 * x -0.03,
                      if_else(x > 0.1219 & x <= 0.1829, 1.4764 * x -0.11,
                              if_else(x > 0.1829 & x <= 0.2438, 1.6404 * x -0.14,
                                      if_else(x > 0.2438 & x <= 0.3048, 1.8045 * x -0.18,
                                              if_else(x > 0.3048 & x <= 0.3658, 1.8045 * x -0.18, 
                                                      if_else(x > 0.3658 & x <= 0.4267, 1.8045 * x -0.18,
                                                              if_else(x > 0.4267 & x <= 0.4877, 1.4764 * x -0.04, 
                                                                      if_else(x > 0.4877 & x <= 0.5486, 1.3123 * x + 0.04,
                                                                              if_else(x > 0.5486 & x <= 0.6096, 3.937 * x - 1.4,
                                                                                      if_else(x > 0.6096, 1,
                                                                                              as.numeric(NA))))))))))))
  return(y)
} # end chnk_juv_d
                                                                                            
# Chinook salmon, juvenile, velocity
chnk_juv_v <- function(x) {
  y = if_else(x >= 0 & x <= 0.0305, 2.2966 * x + 0.88,
              if_else(x > 0.0305 & x <= 0.0610, 1.3123 * x + 0.91,
                      if_else(x > 0.0610 & x <= 0.0914, 0.3281 * x + 0.97,
                              if_else(x > 0.0914 & x <= 0.1219, -0.3281 * x + 1.03,
                                      if_else(x > 0.1219 & x <= 0.1524, -1.3123 * x + 1.15,
                                              if_else(x > 0.1524 & x <= 0.3962, -2.9364 * x + 1.4205,
                                                      if_else(x > 0.3962 & x <= 0.4572, -1.8045 * x + 0.9733,
                                                              if_else(x > 0.4572 & x <= 0.5182, -1.3123 * x + 0.75,
                                                                      if_else(x > 0.5182 & x <= 0.5791, -0.6562 * x + 0.41, 
                                                                              if_else(x > 0.5791 & x <= 0.6401, -0.3281 * x + 0.22,
                                                                                      if_else(x > 0.6401 & x <= 0.6706, 0.01,
                                                                                              if_else(x > 0.6706 & x <= 0.7010, -0.3281 * x + 0.23,
                                                                                                      if_else(x > 0.7010, 0,
                                                                                                             as.numeric(NA))))))))))))))
  return(y)
} # end chnk_juv_v

# Chinook salmon, spawning, depth
chnk_spw_d <- function(x) {
  y = if_else(x < 0.06096, 0,
              if_else(x > 0.06096 & x <= 0.09144, 6.5617 * x - 0.4, 
                      if_else(x > 0.09144 & x <= 0.17618, 2.3435 * x - 0.0143,
                              if_else(x > 0.17618 & x <= 0.24384, 5.9652 * x - 0.6546,
                                      if_else(x > 0.24384 & x <= 0.28956, 4.3745 * x - 0.2667,
                                              if_else(x > 0.28956, 1, as.numeric(NA)))))))
  return(y)
} # end chnk_spw_d
                                                      
# Chinook salmon, spawning, velocity
chnk_spw_v <- function(x) { 
  y = if_else(x <= 0.15240, 0,
              if_else(x > 0.15240 & x <= 0.3048, 6.5617 * x - 1,
                      if_else(x > 0.3048 & x <= 0.9144, 1,
                              if_else(x > 0.9144 & x <= 1.2192, -3.2808 * x + 4,
                                      if_else(x > 1.2192, 0,
                                              as.numeric(NA))))))
  return(y)
} # end chnk_spw_v
                                              
# Steelhead, juvenile, depth                                      
sthd_juv_d <- function(x) {
  y = if_else(x >= 0 & x <= 0.0914, 2.1872 * x,
              if_else(x > 0.0914 & x <= 0.1829, 4.9213 * x - 0.25,
                      if_else(x > 0.1829 & x <= 0.3048, 2.5427 * x + 0.185,
                              if_else(x > 0.3048 & x <= 0.3658, 0.6562 * x + 0.76,
                                      if_else(x > 0.3658, 1,
                                              as.numeric(NA))))))
  return(y)
} # end sthd_juv_d

# Steelhead, juvenile, velocity
sthd_juv_v <- function(x) {
  y = if_else(x >= 0 & x <= 0.061, 2.4606 * x,
              if_else(x > 0.061 & x <= 0.0914, 24.606 * x - 1.35,
                      if_else(x > 0.0914 & x <= 0.1524, 1.1483 * x + 0.795,
                              if_else(x > 0.1524 & x <= 0.2134, 0.4921 * x + 0.895,
                                      if_else(x > 0.2134 & x <= 0.3658, 1,
                                              if_else(x > 0.3658 & x <= 0.6096, -4.101 * x + 2.5,
                                                      if_else(x > 0.6096, 1,
                                                              as.numeric(NA))))))))
  return(y)
} # end sthd_juv_v

# Steelhead, spawning, depth
sthd_spw_d <- function(x) {
  y = if_else(x <= 0.061, 0,
              if_else(x > 0.061 & x <= 0.0914, 6.5617 * x - 0.4,
                      if_else(x > 0.0914 & x <= 0.1768, 2.3435 * x + -0.0143,
                              if_else(x > 0.1768 & x <= 0.2438, 5.9652 * x - 0.6545,
                                      if_else(x > 0.2438 & x <= 0.2896, 4.3745 * x - 0.2667,
                                              if_else(x > 0.2896, 1,
                                                      as.numeric(NA)))))))
  return(y)
} # end sthd_spw_d

# Steelhead, spawning, velocity
sthd_spw_v <- function(x) {
  y = if_else(x <= 0.1524, 0,
              if_else(x > 0.1524 & x <= 0.3048, 6.5617 * x - 1,
                      if_else(x > 0.3048 & x <= 0.9144, 1,
                              if_else(x > 0.9144 & x <= 1.2192, -3.2808 * x + 4,
                                      if_else(x < 1.2192, 0,
                                              as.numeric(NA))))))
  return(y)
} # end sthd_spw_v

## END hsi_curves.R