suppressMessages(library(dplyr))

test_that("kB factors are correct for normal distribution", {
  cmh17_factors <- matrix(
    c(2, 20.581, 36, 1.725, 70, 1.582, 104, 1.522,
      3, 6.157, 37, 1.718, 71, 1.579, 105, 1.521,
      4, 4.163, 38, 1.711, 72, 1.577, 106, 1.519,
      5, 3.408, 39, 1.704, 73, 1.575, 107, 1.518,
      6, 3.007, 40, 1.698, 74, 1.572, 108, 1.517,
      7, 2.756, 41, 1.692, 75, 1.570, 109, 1.516,
      8, 2.583, 42, 1.686, 76, 1.568, 110, 1.515,
      9, 2.454, 43, 1.680, 77, 1.566, 111, 1.513,
      10, 2.355, 44, 1.675, 78, 1.564, 112, 1.512,
      11, 2.276, 45, 1.669, 79, 1.562, 113, 1.511,
      12, 2.211, 46, 1.664, 80, 1.560, 114, 1.510,
      13, 2.156, 47, 1.660, 81, 1.558, 115, 1.509,
      14, 2.109, 48, 1.655, 82, 1.556, 116, 1.508,
      15, 2.069, 49, 1.650, 83, 1.554, 117, 1.507,
      16, 2.034, 50, 1.646, 84, 1.552, 118, 1.506,
      17, 2.002, 51, 1.642, 85, 1.551, 119, 1.505,
      18, 1.974, 52, 1.638, 86, 1.549, 120, 1.504,
      19, 1.949, 53, 1.634, 87, 1.547, 121, 1.503,
      20, 1.927, 54, 1.630, 88, 1.545, 122, 1.502,
      21, 1.906, 55, 1.626, 89, 1.544, 123, 1.501,
      22, 1.887, 56, 1.623, 90, 1.542, 124, 1.500,
      23, 1.870, 57, 1.619, 91, 1.540, 125, 1.499,
      24, 1.854, 58, 1.616, 92, 1.539, 126, 1.498,
      25, 1.839, 59, 1.613, 93, 1.537, 127, 1.497,
      26, 1.825, 60, 1.609, 94, 1.536, 128, 1.496,
      27, 1.812, 61, 1.606, 95, 1.534, 129, 1.495,
      28, 1.800, 62, 1.603, 96, 1.533, 130, 1.494,
      29, 1.789, 63, 1.600, 97, 1.531, 131, 1.493,
      30, 1.778, 64, 1.597, 98, 1.530, 132, 1.492,
      31, 1.768, 65, 1.595, 99, 1.529, 133, 1.492,
      32, 1.758, 66, 1.592, 100, 1.527, 134, 1.491,
      33, 1.749, 67, 1.589, 101, 1.526, 135, 1.490,
      34, 1.741, 68, 1.587, 102, 1.525, 136, 1.489,
      35, 1.733, 69, 1.584, 103, 1.523, 137, 1.488),
    ncol = 2, byrow = TRUE
  ) %>%
    as.data.frame() %>%
    rename(n = V1) %>%
    rename(kb = V2) %>%
    filter(n <= 95) %>%
    rowwise() %>%
    mutate(calc_kb = k_factor_normal(n, p = 0.90, conf = 0.95)) %>%
    mutate(check = expect_lte(abs(calc_kb - kb), expected = 0.002,
                              label = paste0("Validation failure for ", n, ".",
                                             "CMH-17 gives kB=", kb, ",",
                                             "library gives kB=", calc_kb)))
})

test_that("kA factors are correct for normal distribution", {
  cmh17_factors <- matrix(
    c(2, 37.094, 36, 2.983, 70, 2.765, 104, 2.676,
      3, 10.553, 37, 2.972, 71, 2.762, 105, 2.674,
      4, 7.042, 38, 2.961, 72, 2.758, 106, 2.672,
      5, 5.741, 39, 2.951, 73, 2.755, 107, 2.671,
      6, 5.062, 40, 2.941, 74, 2.751, 108, 2.669,
      7, 4.642, 41, 2.932, 75, 2.748, 109, 2.667,
      8, 4.354, 42, 2.923, 76, 2.745, 110, 2.665,
      9, 4.143, 43, 2.914, 77, 2.742, 111, 2.663,
      10, 3.981, 44, 2.906, 78, 2.739, 112, 2.662,
      11, 3.852, 45, 2.898, 79, 2.736, 113, 2.660,
      12, 3.747, 46, 2.890, 80, 2.733, 114, 2.658,
      13, 3.659, 47, 2.883, 81, 2.730, 115, 2.657,
      14, 3.585, 48, 2.876, 82, 2.727, 116, 2.655,
      15, 3.520, 49, 2.869, 83, 2.724, 117, 2.654,
      16, 3.464, 50, 2.862, 84, 2.721, 118, 2.652,
      17, 3.414, 51, 2.856, 85, 2.719, 119, 2.651,
      18, 3.370, 52, 2.850, 86, 2.716, 120, 2.649,
      19, 3.331, 53, 2.844, 87, 2.714, 121, 2.648,
      20, 3.295, 54, 2.838, 88, 2.711, 122, 2.646,
      21, 3.263, 55, 2.833, 89, 2.709, 123, 2.645,
      22, 3.233, 56, 2.827, 90, 2.706, 124, 2.643,
      23, 3.206, 57, 2.822, 91, 2.704, 125, 2.642,
      24, 3.181, 58, 2.817, 92, 2.701, 126, 2.640,
      25, 3.158, 59, 2.812, 93, 2.699, 127, 2.639,
      26, 3.136, 60, 2.807, 94, 2.697, 128, 2.638,
      27, 3.116, 61, 2.802, 95, 2.695, 129, 2.636,
      28, 3.098, 62, 2.798, 96, 2.692, 130, 2.635,
      29, 3.080, 63, 2.793, 97, 2.690, 131, 2.634,
      30, 3.064, 64, 2.789, 98, 2.688, 132, 2.632,
      31, 3.048, 65, 2.785, 99, 2.686, 133, 2.631,
      32, 3.034, 66, 2.781, 100, 2.684, 134, 2.630,
      33, 3.020, 67, 2.777, 101, 2.682, 135, 2.628,
      34, 3.007, 68, 2.773, 102, 2.680, 136, 2.627,
      35, 2.995, 69, 2.769, 103, 2.678, 137, 2.626),
    ncol = 2, byrow = TRUE
  ) %>%
    as.data.frame() %>%
    rename(n = V1) %>%
    rename(ka = V2) %>%
    filter(n <= 75) %>%
    rowwise() %>%
    mutate(calc_ka = k_factor_normal(n, p = 0.99, conf = 0.95)) %>%
    mutate(check = expect_lte(abs(calc_ka - ka), expected = 0.002,
                              label = paste0("Validation failure for ", n, ".",
                                             "CMH-17 gives kA=", ka, ",",
                                             "library gives kA=", calc_ka)))
})
