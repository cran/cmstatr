## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6
)

# If any of the required packages are unavailable,
# don't re-run the code
required <- c("dplyr", "ggplot2", "tidyr", "cmstatr")
if (!all(unlist(lapply(required, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)}
  )))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(cmstatr)

## -----------------------------------------------------------------------------
dat <- carbon.fabric.2 %>%
  filter(test == "WT") %>%
  mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

dat %>%
  head(10)

## -----------------------------------------------------------------------------
b_basis_pooled <- dat %>%
  basis_pooled_cv(strength, condition, batch,
                  override = c("between_group_variability",
                               "normalized_variance_equal"))

b_basis_pooled

## -----------------------------------------------------------------------------
b_basis_pooled$basis

## -----------------------------------------------------------------------------
dat %>%
  ggplot(aes(x = batch, y = strength)) +
  geom_boxplot() +
  geom_jitter(width = 0.25) +
  geom_hline(aes(yintercept = value),
             data = b_basis_pooled$basis %>% rename(condition = group),
             color = "blue") +
  facet_grid(. ~ condition) +
  theme_bw() +
  ggtitle("Batch Plot")

## -----------------------------------------------------------------------------
dat %>%
  ggplot(aes(x = strength, color = condition)) +
  stat_ecdf(geom = "point") +
  coord_flip() +
  theme_bw() +
  ggtitle("Quantile Plot")

## -----------------------------------------------------------------------------
dat %>%
  ggplot(aes(x = strength, color = condition)) +
  stat_normal_surv_func() +
  stat_esf() +
  theme_bw() +
  ggtitle("Normal Survival Function Plot")

## -----------------------------------------------------------------------------
dat %>%
  group_by(condition) %>%
  mutate(norm.score = scale(strength)) %>%
  ggplot(aes(x = norm.score, y = strength, colour = condition)) +
  geom_point() +
  ggtitle("Normal Scores Plot") +
  theme_bw()

## -----------------------------------------------------------------------------
dat %>%
  ggplot(aes(sample = strength, colour = condition)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Q-Q Plot") +
  theme_bw()

## -----------------------------------------------------------------------------
b_basis_fcn <- tribble(
  ~condition, ~fcn, ~args,
  "CTD", "basis_normal", list(override = c("between_batch_variability")),
  "RTD", "basis_normal", list(override = c("between_batch_variability")),
  "ETW", "basis_hk_ext", NULL,
  "ETW2", "basis_normal", list(override = c("between_batch_variability"))
)

a_basis_fcn <- tribble(
  ~condition, ~fcn, ~args,
  "CTD", "basis_normal", list(override = c("between_batch_variability")),
  "RTD", "basis_normal", list(override = c("between_batch_variability")),
  "ETW", "basis_hk_ext", list(method = "woodward-frawley"),
  "ETW2", "basis_normal", list(override = c("between_batch_variability"))
)

## -----------------------------------------------------------------------------
single_point_fcn <- function(group_x, group_batch, cond, basis_fcn, p) {
  fcn <- basis_fcn$fcn[basis_fcn$condition == cond[1]]
  extra_args <- basis_fcn$args[basis_fcn$condition == cond[1]]

  args <- c(
    list(x = group_x, batch = group_batch, p = p),
    unlist(extra_args))
  basis <- do.call(fcn, args)
  basis$basis
}

single_point_results <- dat %>%
  group_by(condition) %>%
  summarise(single_point_b_basis = single_point_fcn(
              strength, batch, condition, b_basis_fcn, 0.90),
            single_point_a_basis = single_point_fcn(
              strength, batch, condition, a_basis_fcn, 0.99),
            minimum = min(strength),
            mean = mean(strength)) %>%
  mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

single_point_results

## -----------------------------------------------------------------------------
a_basis_pooled <- dat %>%
  basis_pooled_cv(strength, condition, batch, p = 0.99,
                  override = c("between_group_variability",
                               "normalized_variance_equal"))

a_basis_pooled

## -----------------------------------------------------------------------------
a_basis_pooled$basis

## -----------------------------------------------------------------------------
a_basis_pooled$basis %>%
  rename(condition = group,
         b_basis_pooled = value)

## -----------------------------------------------------------------------------
a_basis_pooled_results <- a_basis_pooled$basis %>%
  rename(condition = group,
         a_basis_pooled = value) %>%
  mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

a_basis_pooled_results

## -----------------------------------------------------------------------------
b_basis_pooled_results <- b_basis_pooled$basis %>%
  rename(condition = group,
         b_basis_pooled = value) %>%
  mutate(condition = ordered(condition, c("CTD", "RTD", "ETW", "ETW2")))

b_basis_pooled_results

## -----------------------------------------------------------------------------
single_point_results %>%
  inner_join(b_basis_pooled_results, by = "condition") %>%
  inner_join(a_basis_pooled_results, by = "condition")

## -----------------------------------------------------------------------------
single_point_results %>%
  inner_join(b_basis_pooled_results, by = "condition") %>%
  inner_join(a_basis_pooled_results, by = "condition") %>%
  pivot_longer(cols = single_point_b_basis:a_basis_pooled)

## -----------------------------------------------------------------------------
single_point_results %>%
  inner_join(b_basis_pooled_results, by = "condition") %>%
  inner_join(a_basis_pooled_results, by = "condition") %>%
  pivot_longer(cols = single_point_b_basis:a_basis_pooled) %>%
  ggplot(aes(x = condition, y = value)) +
  geom_boxplot(aes(y = strength), data = dat) +
  geom_point(aes(shape = name, color = name)) +
  ggtitle("Property Graph") +
  theme_bw()

