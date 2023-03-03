#'  Estimate Average Treatment Effect (ATE) using from AIPW estimator using cross-fit algorithm with parallelizing
#'
#' @param data a data frame of tibble
#' @param exposure name of exposure variable
#' @param outcome name of outcome variable
#' @param covarsT a vector of names of covaraites for treatment model
#' @param covarsO a vector of names of covaraites for outcome model
#' @param family.y it is the family for outcome model. It can `binomial() (default)` or `"gaussian"`
#' @param learners similar as \code{SL.library()} in `SuperLearner` package.
#' @param control similar as \code{cvControl()} in `SuperLearner` package.
#' @param num_cf number of repetition done. The default is 5.
#' @param n_split number of splits used, default `n_split = 3`
#' @param rand_split logical value; if be FALSE `(default)`, discordant splits for exposure and outcome model are chosen systematically; otherwise chosen randomly.
#' @param gbound value between (0,1) for truncation of predicted probabilities. The defaults are 0.025 and 0.975. See \code{tmle::tmle()} for more information.
#' @param alpha used to keep predicted initial values bounded away from (0,1) for logistic fluctuation. The defaults are 1e-17 and 1-1e-17.
#' @param seed numeric value to reproduce the splits distribution
#' @param conf.level confidence limit for confidence interval, `default = 0.95`.
#' @return a tibble of the estimates
#'
#' @import dplyr tibble tidyr purrr furrr
#'
#' @export
#'
#'
#' @examples
#'
#' # See the README file for details
#'
#' sum(1:5)
#'
#'
DC_aipw_k <- function(data,
                      exposure,
                      outcome,
                      covarsT,
                      covarsO,
                      family.y="binomial",
                      learners,
                      control,
                      n_split,
                      num_cf,
                      rand_split,
                      gbound = 0.025,
                      alpha = 1e-17,
                      seed=146,
                      conf.level=0.95){

  runs <- list()
  #Run on num_cf splits
  set.seed(seed)
  cf_seed = sample(num_cf)
  for(cf in 1:num_cf){
    seed1 = cf_seed[cf]

    fit_sngle = aipw_single_p(data,
                              exposure,
                              outcome,
                              covarsT,
                              covarsO,
                              family.y,
                              learners,
                              control,
                              n_split,
                              rand_split,
                              gbound,
                              alpha,
                              seed=seed1)




    runs[[cf]] <- fit_sngle

  }

  res = dplyr::bind_rows(lapply(runs, function(x) x$results))

  #runs1 <- bind_rows(runs)

  weight1 = dplyr::bind_rows(lapply(runs, function(x) x$weight))
  weight = weight1 %>% group_by(model, split) %>%
    summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
    group_by(model) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))  %>% select(!split)

  medians <- apply(res, 2, median)

  res <- res %>% mutate(var0 = var + (rd - medians[1])^2)


  results <- apply(res, 2, median)

  t.value = qt((1-conf.level)/2, nrow(data), lower.tail = F)

  l_ci = results[1] - t.value*sqrt(results[3])
  u_ci = results[1] + t.value*sqrt(results[3])

  res = tibble(rd=results[1], se = sqrt(results[3]), lower.ci = l_ci, upper.ci = u_ci)

  fit <- list()

  fit$ATE = res
  fit$weight = weight
  fit

}


