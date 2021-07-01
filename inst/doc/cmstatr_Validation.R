## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(cmstatr)
library(dplyr)
library(purrr)
library(tidyr)
library(testthat)

## -----------------------------------------------------------------------------
expect_equal(10, 10.1, tolerance = 0.01)

## -----------------------------------------------------------------------------
head(carbon.fabric)

## -----------------------------------------------------------------------------
res <- carbon.fabric %>%
  filter(test == "WT") %>%
  filter(condition == "RTD") %>%
  ad_ksample(strength, batch)

expect_equal(res$ad / (res$k - 1), 0.456, tolerance = 0.002)
expect_false(res$reject_same_dist)

res

## -----------------------------------------------------------------------------
res <- carbon.fabric %>%
  filter(test == "WT") %>%
  filter(condition == "ETW") %>%
  ad_ksample(strength, batch)

expect_equal(res$ad / (res$k - 1), 1.604, tolerance = 0.002)
expect_false(res$reject_same_dist)

res

## -----------------------------------------------------------------------------
dat_8_3_11_1_1 <- tribble(
  ~batch, ~strength, ~condition,
  1, 118.3774604, "CTD", 1, 84.9581364, "RTD", 1, 83.7436035, "ETD",
  1, 123.6035612, "CTD", 1, 92.4891822, "RTD", 1, 84.3831677, "ETD",
  1, 115.2238092, "CTD", 1, 96.8212659, "RTD", 1, 94.8030433, "ETD",
  1, 112.6379744, "CTD", 1, 109.030325, "RTD", 1, 94.3931537, "ETD",
  1, 116.5564277, "CTD", 1, 97.8212659, "RTD", 1, 101.702222, "ETD",
  1, 123.1649896, "CTD", 1, 100.921519, "RTD", 1, 86.5372121, "ETD",
  2, 128.5589027, "CTD", 1, 103.699444, "RTD", 1, 92.3772684, "ETD",
  2, 113.1462103, "CTD", 2, 93.7908212, "RTD", 2, 89.2084024, "ETD",
  2, 121.4248107, "CTD", 2, 107.526709, "RTD", 2, 100.686001, "ETD",
  2, 134.3241906, "CTD", 2, 94.5769704, "RTD", 2, 81.0444192, "ETD",
  2, 129.6405117, "CTD", 2, 93.8831373, "RTD", 2, 91.3398070, "ETD",
  2, 117.9818658, "CTD", 2, 98.2296605, "RTD", 2, 93.1441939, "ETD",
  3, 115.4505226, "CTD", 2, 111.346590, "RTD", 2, 85.8204168, "ETD",
  3, 120.0369467, "CTD", 2, 100.817538, "RTD", 3, 94.8966273, "ETD",
  3, 117.1631088, "CTD", 3, 100.382203, "RTD", 3, 95.8068520, "ETD",
  3, 112.9302797, "CTD", 3, 91.5037811, "RTD", 3, 86.7842252, "ETD",
  3, 117.9114501, "CTD", 3, 100.083233, "RTD", 3, 94.4011973, "ETD",
  3, 120.1900159, "CTD", 3, 95.6393615, "RTD", 3, 96.7231171, "ETD",
  3, 110.7295966, "CTD", 3, 109.304779, "RTD", 3, 89.9010384, "ETD",
  3, 100.078562, "RTD", 3, 99.1205847, "RTD", 3, 89.3672306, "ETD",
  1, 106.357525, "ETW", 1, 99.0239966, "ETW2",
  1, 105.898733, "ETW", 1, 103.341238, "ETW2",
  1, 88.4640082, "ETW", 1, 100.302130, "ETW2",
  1, 103.901744, "ETW", 1, 98.4634133, "ETW2",
  1, 80.2058219, "ETW", 1, 92.2647280, "ETW2",
  1, 109.199597, "ETW", 1, 103.487693, "ETW2",
  1, 61.0139431, "ETW", 1, 113.734763, "ETW2",
  2, 99.3207107, "ETW", 2, 108.172659, "ETW2",
  2, 115.861770, "ETW", 2, 108.426732, "ETW2",
  2, 82.6133082, "ETW", 2, 116.260375, "ETW2",
  2, 85.3690411, "ETW", 2, 121.049610, "ETW2",
  2, 115.801622, "ETW", 2, 111.223082, "ETW2",
  2, 44.3217741, "ETW", 2, 104.574843, "ETW2",
  2, 117.328077, "ETW", 2, 103.222552, "ETW2",
  2, 88.6782903, "ETW", 3, 99.3918538, "ETW2",
  3, 107.676986, "ETW", 3, 87.3421658, "ETW2",
  3, 108.960241, "ETW", 3, 102.730741, "ETW2",
  3, 116.122640, "ETW", 3, 96.3694916, "ETW2",
  3, 80.2334815, "ETW", 3, 99.5946088, "ETW2",
  3, 106.145570, "ETW", 3, 97.0712407, "ETW2",
  3, 104.667866, "ETW",
  3, 104.234953, "ETW"
)
dat_8_3_11_1_1

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW" & batch == 2) %>%
  maximum_normed_residual(strength, alpha = 0.05)

expect_equal(res$mnr, 2.008, tolerance = 0.001)
expect_equal(res$crit, 2.127, tolerance = 0.001)
expect_equal(res$n_outliers, 0)

res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW" & batch == 3) %>%
  maximum_normed_residual(strength, alpha = 0.05)

expect_equal(res$mnr, 2.119, tolerance = 0.001)
expect_equal(res$crit, 2.020, tolerance = 0.001)
expect_equal(res$n_outliers, 1)

res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW") %>%
  ad_ksample(strength, batch)

expect_equal(res$ad / (res$k - 1), 0.793, tolerance = 0.003)
expect_false(res$reject_same_dist)

res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW2") %>%
  ad_ksample(strength, batch)

expect_equal(res$ad / (res$k - 1), 3.024, tolerance = 0.001)
expect_true(res$reject_same_dist)

res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW") %>%
  anderson_darling_normal(strength)
expect_equal(res$osl, 0.006051, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW") %>%
  anderson_darling_lognormal(strength)
expect_equal(res$osl, 0.000307, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW") %>%
  anderson_darling_weibull(strength)
expect_equal(res$osl, 0.0219, tolerance = 0.002)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition != "ETW" & condition != "ETW2") %>%
  levene_test(strength, condition)
expect_equal(res$f, 0.058, tolerance = 0.01)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW2") %>%
  levene_test(strength, batch)
expect_equal(res$f, 0.123, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "CTD") %>%
  levene_test(strength, batch)
expect_equal(res$f, 3.850, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW") %>%
  basis_hk_ext(strength, method = "woodward-frawley", p = 0.99, conf = 0.95,
               override = "all")

expect_equal(res$basis, 13.0, tolerance = 0.001)

res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW") %>%
  basis_hk_ext(strength, method = "optimum-order", p = 0.90, conf = 0.95,
               override = "all")

expect_equal(res$basis, 37.9, tolerance = 0.001)

res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW2") %>%
  basis_anova(strength, batch, override = "number_of_groups",
              p = 0.99, conf = 0.95)
expect_equal(res$basis, 34.6, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_1 %>%
  filter(condition == "ETW2") %>%
  basis_anova(strength, batch, override = "number_of_groups")
expect_equal(res$basis, 63.2, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
dat_8_3_11_1_2 <- tribble(
  ~batch, ~strength, ~condition,
  1, 79.04517, "CTD", 1, 103.2006, "RTD", 1, 63.22764, "ETW", 1, 54.09806, "ETW2",
  1, 102.6014, "CTD", 1, 105.1034, "RTD", 1, 70.84454, "ETW", 1, 58.87615, "ETW2",
  1, 97.79372, "CTD", 1, 105.1893, "RTD", 1, 66.43223, "ETW", 1, 61.60167, "ETW2",
  1, 92.86423, "CTD", 1, 100.4189, "RTD", 1, 75.37771, "ETW", 1, 60.23973, "ETW2",
  1, 117.218,  "CTD", 2, 85.32319, "RTD", 1, 72.43773, "ETW", 1, 61.4808,  "ETW2",
  1, 108.7168, "CTD", 2, 92.69923, "RTD", 1, 68.43073, "ETW", 1, 64.55832, "ETW2",
  1, 112.2773, "CTD", 2, 98.45242, "RTD", 1, 69.72524, "ETW", 2, 57.76131, "ETW2",
  1, 114.0129, "CTD", 2, 104.1014, "RTD", 2, 66.20343, "ETW", 2, 49.91463, "ETW2",
  2, 106.8452, "CTD", 2, 91.51841, "RTD", 2, 60.51251, "ETW", 2, 61.49271, "ETW2",
  2, 112.3911, "CTD", 2, 101.3746, "RTD", 2, 65.69334, "ETW", 2, 57.7281,  "ETW2",
  2, 115.5658, "CTD", 2, 101.5828, "RTD", 2, 62.73595, "ETW", 2, 62.11653, "ETW2",
  2, 87.40657, "CTD", 2, 99.57384, "RTD", 2, 59.00798, "ETW", 2, 62.69353, "ETW2",
  2, 102.2785, "CTD", 2, 88.84826, "RTD", 2, 62.37761, "ETW", 3, 61.38523, "ETW2",
  2, 110.6073, "CTD", 3, 92.18703, "RTD", 3, 64.3947,  "ETW", 3, 60.39053, "ETW2",
  3, 105.2762, "CTD", 3, 101.8234, "RTD", 3, 72.8491,  "ETW", 3, 59.17616, "ETW2",
  3, 110.8924, "CTD", 3, 97.68909, "RTD", 3, 66.56226, "ETW", 3, 60.17616, "ETW2",
  3, 108.7638, "CTD", 3, 101.5172, "RTD", 3, 66.56779, "ETW", 3, 46.47396, "ETW2",
  3, 110.9833, "CTD", 3, 100.0481, "RTD", 3, 66.00123, "ETW", 3, 51.16616, "ETW2",
  3, 101.3417, "CTD", 3, 102.0544, "RTD", 3, 59.62108, "ETW",
  3, 100.0251, "CTD",                     3, 60.61167, "ETW",
                                          3, 57.65487, "ETW",
                                          3, 66.51241, "ETW",
                                          3, 64.89347, "ETW",
                                          3, 57.73054, "ETW",
                                          3, 68.94086, "ETW",
                                          3, 61.63177, "ETW"
)

## -----------------------------------------------------------------------------

res <- basis_pooled_sd(dat_8_3_11_1_2, strength, condition,
                         override = "all")

expect_equal(res$basis$value[res$basis$group == "CTD"],
           93.64, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
           87.30, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
           54.33, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW2"],
           47.12, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- basis_pooled_sd(dat_8_3_11_1_2, strength, condition,
                       p = 0.99, conf = 0.95,
                       override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
           86.19, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
           79.86, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
           46.84, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW2"],
           39.69, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_2 %>%
  filter(condition != "ETW2") %>%
  basis_pooled_sd(strength, condition, modcv = TRUE, override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
             92.25, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
             85.91, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
             52.97, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_2 %>%
  filter(condition != "ETW2") %>%
  basis_pooled_sd(strength, condition,
                  p = 0.99, conf = 0.95, modcv = TRUE, override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
             83.81, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
             77.48, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
             44.47, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- basis_pooled_cv(dat_8_3_11_1_2, strength, condition, override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
             90.89, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
             85.37, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
             56.79, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW2"],
             50.55, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- basis_pooled_cv(dat_8_3_11_1_2, strength, condition,
                       p = 0.99, conf = 0.95, override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
             81.62, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
             76.67, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
             50.98, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW2"],
             45.40, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_2 %>%
  filter(condition != "ETW2") %>%
  basis_pooled_cv(strength, condition, modcv = TRUE, override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
             90.31, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
             84.83, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
             56.43, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- dat_8_3_11_1_2 %>%
  filter(condition != "ETW2") %>%
  basis_pooled_cv(strength, condition, modcv = TRUE,
                  p = 0.99, conf = 0.95, override = "all")
expect_equal(res$basis$value[res$basis$group == "CTD"],
             80.57, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "RTD"],
             75.69, tolerance = 0.001)
expect_equal(res$basis$value[res$basis$group == "ETW"],
             50.33, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
dat <- data.frame(
  strength = c(
    137.4438, 139.5395, 150.89, 141.4474, 141.8203, 151.8821, 143.9245,
    132.9732, 136.6419, 138.1723, 148.7668, 143.283, 143.5429,
    141.7023, 137.4732, 152.338, 144.1589, 128.5218
  )
)
res <- anderson_darling_normal(dat, strength)

expect_equal(res$osl, 0.465, tolerance = 0.001)

res

## -----------------------------------------------------------------------------
dat <- data.frame(
  strength = c(
    137.4438, 139.5395, 150.89, 141.4474, 141.8203, 151.8821, 143.9245,
    132.9732, 136.6419, 138.1723, 148.7668, 143.283, 143.5429,
    141.7023, 137.4732, 152.338, 144.1589, 128.5218
  )
)

res <- anderson_darling_lognormal(dat, strength)

expect_equal(res$osl, 0.480, tolerance = 0.001)

res

## -----------------------------------------------------------------------------
dat <- data.frame(
  strength = c(
    137.4438, 139.5395, 150.89, 141.4474, 141.8203, 151.8821, 143.9245,
    132.9732, 136.6419, 138.1723, 148.7668, 143.283, 143.5429,
    141.7023, 137.4732, 152.338, 144.1589, 128.5218
  )
)

res <- anderson_darling_weibull(dat, strength)

expect_equal(res$osl, 0.179, tolerance = 0.002)

res

## -----------------------------------------------------------------------------
dat <- c(
  137.4438, 139.5395, 150.8900, 141.4474, 141.8203, 151.8821, 143.9245,
  132.9732, 136.6419, 138.1723, 148.7668, 143.2830, 143.5429, 141.7023,
  137.4732, 152.3380, 144.1589, 128.5218
)

res <- basis_normal(x = dat, p = 0.99, conf = 0.95, override = "all")
expect_equal(res$basis, 120.336, tolerance = 0.0005)
res

## -----------------------------------------------------------------------------
res <- basis_normal(x = dat, p = 0.9, conf = 0.95, override = "all")
expect_equal(res$basis, 129.287, tolerance = 0.0005)
res

## -----------------------------------------------------------------------------
dat <- c(
  137.4438, 139.5395, 150.8900, 141.4474, 141.8203, 151.8821, 143.9245,
  132.9732, 136.6419, 138.1723, 148.7668, 143.2830, 143.5429, 141.7023,
  137.4732, 152.3380, 144.1589, 128.5218
)

res <- basis_lognormal(x = dat, p = 0.99, conf = 0.95, override = "all")
expect_equal(res$basis, 121.710, tolerance = 0.0005)
res

## -----------------------------------------------------------------------------
res <- basis_lognormal(x = dat, p = 0.9, conf = 0.95, override = "all")
expect_equal(res$basis, 129.664, tolerance = 0.0005)
res

## -----------------------------------------------------------------------------
dat <- c(
  137.4438, 139.5395, 150.8900, 141.4474, 141.8203, 151.8821, 143.9245,
  132.9732, 136.6419, 138.1723, 148.7668, 143.2830, 143.5429, 141.7023,
  137.4732, 152.3380, 144.1589, 128.5218
)

res <- basis_weibull(x = dat, p = 0.99, conf = 0.95, override = "all")
expect_equal(res$basis, 109.150, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
res <- basis_weibull(x = dat, p = 0.9, conf = 0.95, override = "all")
expect_equal(res$basis, 125.441, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
dat <- c(
  137.4438, 139.5395, 150.8900, 141.4474, 141.8203, 151.8821, 143.9245,
  132.9732, 136.6419, 138.1723, 148.7668, 143.2830, 143.5429, 141.7023,
  137.4732, 152.3380, 144.1589, 128.5218
)

res <- basis_hk_ext(x = dat, p = 0.99, conf = 0.95,
                    method = "woodward-frawley", override = "all")
expect_equal(res$basis, 99.651, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
res <- basis_hk_ext(x = dat, p = 0.9, conf = 0.95,
                    method = "optimum-order", override = "all")
expect_equal(res$basis, 124.156, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
dat <- c(
  139.6734, 143.0032, 130.4757, 144.8327, 138.7818, 136.7693, 148.636,
  131.0095, 131.4933, 142.8856, 158.0198, 145.2271, 137.5991, 139.8298,
  140.8557, 137.6148, 131.3614, 152.7795, 145.8792, 152.9207, 160.0989,
  145.1920, 128.6383, 141.5992, 122.5297, 159.8209, 151.6720, 159.0156
)

## -----------------------------------------------------------------------------
res <- basis_hk_ext(x = dat, p = 0.9, conf = 0.95,
                    method = "optimum-order", override = "all")
expect_equal(res$basis, 122.36798, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- basis_hk_ext(x = head(dat, 26), p = 0.9, conf = 0.95,
                    method = "optimum-order", override = "all")
expect_equal(res$basis, 121.57073, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- basis_hk_ext(x = head(dat, 22), p = 0.9, conf = 0.95,
                    method = "optimum-order", override = "all")
expect_equal(res$basis, 128.82397, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
dat <- c(
  137.3603, 135.6665, 136.6914, 154.7919, 159.2037, 137.3277, 128.821,
  138.6304, 138.9004, 147.4598, 148.6622, 144.4948, 131.0851, 149.0203,
  131.8232, 146.4471, 123.8124, 126.3105, 140.7609, 134.4875, 128.7508,
  117.1854, 129.3088, 141.6789, 138.4073, 136.0295, 128.4164, 141.7733,
  134.455,  122.7383, 136.9171, 136.9232, 138.8402, 152.8294, 135.0633,
  121.052,  131.035,  138.3248, 131.1379, 147.3771, 130.0681, 132.7467,
  137.1444, 141.662,  146.9363, 160.7448, 138.5511, 129.1628, 140.2939,
  144.8167, 156.5918, 132.0099, 129.3551, 136.6066, 134.5095, 128.2081,
  144.0896, 141.8029, 130.0149, 140.8813, 137.7864
)

res <- basis_nonpara_large_sample(x = dat, p = 0.9, conf = 0.95,
                                  override = "all")
expect_equal(res$basis, 122.738297, tolerance = 0.005)
res

## -----------------------------------------------------------------------------
res <- equiv_mean_extremum(alpha = 0.05, mean_qual = 141.310, sd_qual = 6.415,
                           n_sample = 9)
expect_equal(res$threshold_min_indiv, 123.725, tolerance = 0.001)
expect_equal(res$threshold_mean, 137.197, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- equiv_mean_extremum(alpha = 0.05, mean_qual = 141.310, sd_qual = 6.415,
                           n_sample = 9, modcv = TRUE)
expect_equal(res$threshold_min_indiv, 117.024, tolerance = 0.001)
expect_equal(res$threshold_mean, 135.630, tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02,
                         sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
                         sd_qual = 0.162)
expect_equal(res$threshold, c(9.115, 9.365), tolerance = 0.001)
res

## -----------------------------------------------------------------------------
res <- equiv_change_mean(alpha = 0.05, n_sample = 9, mean_sample = 9.02,
                         sd_sample = 0.15785, n_qual = 28, mean_qual = 9.24,
                         sd_qual = 0.162, modcv = TRUE)
expect_equal(res$threshold, c(8.857, 9.623), tolerance = 0.001)
res

## -----------------------------------------------------------------------------
dat_ss1987 <- data.frame(
  smoothness = c(
    38.7, 41.5, 43.8, 44.5, 45.5, 46.0, 47.7, 58.0,
    39.2, 39.3, 39.7, 41.4, 41.8, 42.9, 43.3, 45.8,
    34.0, 35.0, 39.0, 40.0, 43.0, 43.0, 44.0, 45.0,
    34.0, 34.8, 34.8, 35.4, 37.2, 37.8, 41.2, 42.8
  ),
  lab = c(rep("A", 8), rep("B", 8), rep("C", 8), rep("D", 8))
)
dat_ss1987

## -----------------------------------------------------------------------------
res <- ad_ksample(dat_ss1987, smoothness, lab)

expect_equal(res$ad, 8.3926, tolerance = 0.001)
expect_equal(res$sigma, 1.2038, tolerance = 0.001)
expect_equal(res$p, 0.00226, tolerance = 0.01)

res

## -----------------------------------------------------------------------------
tribble(
  ~n, ~kB_published,
  2, 20.581, 36, 1.725, 70, 1.582, 104, 1.522,
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
  35, 1.733, 69, 1.584, 103, 1.523, 137, 1.488
) %>%
  arrange(n) %>%
  mutate(kB_cmstatr = k_factor_normal(n, p = 0.9, conf = 0.95)) %>%
  rowwise() %>%
  mutate(diff = expect_equal(kB_published, kB_cmstatr, tolerance = 0.001)) %>%
  select(-c(diff))

## -----------------------------------------------------------------------------
tribble(
  ~n, ~kA_published,
  2, 37.094, 36, 2.983, 70, 2.765, 104, 2.676,
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
  35, 2.995, 69, 2.769, 103, 2.678, 137, 2.626
) %>%
  arrange(n) %>%
  mutate(kA_cmstatr = k_factor_normal(n, p = 0.99, conf = 0.95)) %>%
  rowwise() %>%
  mutate(diff = expect_equal(kA_published, kA_cmstatr, tolerance = 0.001)) %>%
  select(-c(diff))

## -----------------------------------------------------------------------------
tribble(
  ~n, ~z,
  3,  28.820048,
  5,  6.1981307,
  7,  3.4780112,
  9,  2.5168762,
  11, 2.0312134,
  13, 1.7377374,
  15, 1.5403989,
  17, 1.3979806,
  19, 1.2899172,
  21, 1.2048089,
  23, 1.1358259,
  25, 1.0786237,
  27, 1.0303046,
) %>%
  rowwise() %>%
  mutate(
    z_calc = hk_ext_z(n, 1, ceiling(n / 2), p = 0.90, conf = 0.95)
  ) %>%
  mutate(diff = expect_equal(z, z_calc, tolerance = 0.0001)) %>% 
  select(-c(diff))

## -----------------------------------------------------------------------------
tribble(
  ~n, ~k,
  2, 80.0038,
  4, 9.49579,
  6, 5.57681,
  8, 4.25011,
  10, 3.57267,
  12, 3.1554,
  14, 2.86924,
  16, 2.65889,
  18, 2.4966,
  20, 2.36683,
  25, 2.131,
  30, 1.96975,
  35, 1.85088,
  40, 1.75868,
  45, 1.68449,
  50, 1.62313,
  60, 1.5267,
  70, 1.45352,
  80, 1.39549,
  90, 1.34796,
  100, 1.30806,
  120, 1.24425,
  140, 1.19491,
  160, 1.15519,
  180, 1.12226,
  200, 1.09434,
  225, 1.06471,
  250, 1.03952,
  275, 1.01773
) %>%
  rowwise() %>%
  mutate(z_calc = hk_ext_z(n, 1, n, 0.99, 0.95)) %>%
  mutate(diff = expect_lt(abs(k - z_calc), 0.0001)) %>% 
  select(-c(diff))

## -----------------------------------------------------------------------------
tribble(
  ~n, ~r, ~k,
  2, 2, 35.177,
  3, 3, 7.859,
  4, 4, 4.505,
  5, 4, 4.101,
  6, 5, 3.064,
  7, 5, 2.858,
  8, 6, 2.382,
  9, 6, 2.253,
  10, 6, 2.137,
  11, 7, 1.897,
  12, 7, 1.814,
  13, 7, 1.738,
  14, 8, 1.599,
  15, 8, 1.540,
  16, 8, 1.485,
  17, 8, 1.434,
  18, 9, 1.354,
  19, 9, 1.311,
  20, 10, 1.253,
  21, 10, 1.218,
  22, 10, 1.184,
  23, 11, 1.143,
  24, 11, 1.114,
  25, 11, 1.087,
  26, 11, 1.060,
  27, 11, 1.035,
  28, 12, 1.010
) %>% 
  rowwise() %>%
  mutate(r_calc = hk_ext_z_j_opt(n, 0.90, 0.95)$j) %>%
  mutate(k_calc = hk_ext_z_j_opt(n, 0.90, 0.95)$z)

## -----------------------------------------------------------------------------
tribble(
  ~n, ~rb,
  29, 1,
  46, 2,
  61, 3,
  76, 4,
  89, 5,
  103, 6,
  116, 7,
  129, 8,
  142, 9,
  154, 10,
  167, 11,
  179, 12,
  191, 13,
  203, 14
) %>%
  rowwise() %>%
  mutate(r_calc = nonpara_binomial_rank(n, 0.9, 0.95)) %>%
  mutate(test = expect_equal(rb, r_calc)) %>%
  select(-c(test))

## -----------------------------------------------------------------------------
tribble(
  ~n, ~ra,
  299, 1,
  473, 2,
  628, 3,
  773, 4,
  913, 5
) %>%
  rowwise() %>%
  mutate(r_calc = nonpara_binomial_rank(n, 0.99, 0.95)) %>%
  mutate(test = expect_equal(ra, r_calc)) %>%
  select(-c(test))

## -----------------------------------------------------------------------------
read.csv(system.file("extdata", "k1.vangel.csv",
                     package = "cmstatr")) %>%
  gather(n, k1, X2:X10) %>%
  mutate(n = as.numeric(substring(n, 2))) %>%
  inner_join(
    read.csv(system.file("extdata", "k2.vangel.csv",
                         package = "cmstatr")) %>%
      gather(n, k2, X2:X10) %>%
      mutate(n = as.numeric(substring(n, 2))),
    by = c("n" = "n", "alpha" = "alpha")
  ) %>%
  filter(n >= 5 & (alpha == 0.01 | alpha == 0.05)) %>% 
  group_by(n, alpha) %>%
  nest() %>%
  mutate(equiv = map2(alpha, n, ~k_equiv(.x, .y))) %>%
  mutate(k1_calc = map(equiv, function(e) e[1]),
         k2_calc = map(equiv, function(e) e[2])) %>%
  select(-c(equiv)) %>% 
  unnest(cols = c(data, k1_calc, k2_calc)) %>% 
  mutate(check = expect_equal(k1, k1_calc, tolerance = 0.0001)) %>%
  select(-c(check)) %>% 
  mutate(check = expect_equal(k2, k2_calc, tolerance = 0.0001)) %>%
  select(-c(check))

## -----------------------------------------------------------------------------
sessionInfo()

