# crossfitk: An R package to apply sample splitting (crossfit) to AIPW and TMLE in causal inference
---
Author: Momenul Haque Mondol & Mohammad Ehsanul Karim
---

## How to install

```{r}
remotes::install_github("momenulhaque/crossfitk", auth_token = "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") # it will install the package
library(crossfitk) 
```
Now the package is ready to use. It supports applying both AIPW and TMLE. 

 1. Install the required R packages

```{r}
require(SuperLearner)
```
 
 2. Defining the data you want to use
 
```{r}
# Read the data set that you want to use. An example data set "statin_sim_data" can be found in this package.
data = statin_sim_data 
```
 3. Defining the model parameters

```{r}
exposure="statin"
outcome="Y"
covarsT = c("age", "ldl_log", "risk_score") # covariate for exposure model
covarsO = c("age", "ldl_log", "risk_score") # covariate for outcome model
family.y = "binomial"
learners=c("SL.glm", "SL.glmnet", "SL.xgboost")
control=list(V = 3, stratifyCV = FALSE, shuffle = TRUE, validRows = NULL)
num_cf = 5 # number of repetitions
n_split = 4 # number of splits
rand_split = FALSE # splits' crossing pattern is not random
gbound = 0.025
alpha = 1e-17
seed = 156
```

 4. Estimating the average treatment effect (ATE)

```{r}
fit_tmle <- DC_tmle_k(data,
                        exposure,
                        outcome,
                        covarsT,
                        covarsO,
                        family.y,
                        learners,
                        control,
                        num_cf, 
                        n_split ,
                        rand_split,
                        gbound,
                        alpha,
                        seed)

```

  5. Understanding the results

The object `fit_tmle` contains a vector of two elements, `rd` and `se`. The first element `rd` shows the average treatment effect, and its standard error (`se`). The AIPW can be implemented using  `DC_aipw_k()` function.



# References
Zivich PN, and Breskin A. "Machine learning for causal inference: on the use of cross-fit estimators." Epidemiology 32.3 (2021): 393-401
