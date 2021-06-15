README
================

Quick installation

``` r
# install.packages("remotes")
remotes::install_github("TatsumotoH/abcihelpr")
```

Getting started

``` r
library(abcihelpr)

# set your group account
set_abci_group_account(abci_group_account = "gcaXXXXX")

# set your user account
set_abci_user_account(abci_user_account = "acaXXXXXtm")

# set your identity file for ssh
set_ssh_identity(path_ssh_identity = "~/.ssh/id_rsa")

abci_set_work_directory()


print_config()


library(tidymodels)
library(workflows)
library(workflowsets)

library(tidymodels)
library(embed)



#for parallel processing
doParallel::registerDoParallel()

data(ames)

data_split = initial_split(ames, strata = "Sale_Price", prop = 0.75)
data_train = training(data_split)
data_test  = testing(data_split)


tune_folds = vfold_cv(data_train, v=5)

# at least one parameter to be tuned for a parameter grid
data_rec =
  recipe(Sale_Price ~ ., data = data_train ) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_umap(all_numeric_predictors(),
            neighbors = tune(),
            min_dist = tune())


mod_spec =  linear_reg() %>%
  set_engine('lm') %>% # adds lm implementation of linear regression
  set_mode('regression')


tune_wf = workflow() %>%
  add_recipe(data_rec) %>%
  add_model(mod_spec)

#check parameters to tune
tune_args(tune_wf)

# make the paramater grid
tune_params = parameters(tune_wf)
param_finalized = finalize(tune_params, x=data_train)
param_grid = grid_random(param_finalized, size=5)

#
trg_grid_tune_id = 1004

#upload a parameter grid to abci
abci_upload_params(grid_tune_id = trg_grid_tune_id,
                   tune_wf = tune_wf,
                   tune_folds = tune_folds,
                   param_grid = param_grid)

#submit job to batch
abci_submit_job_for_workers(grid_tune_id = trg_grid_tune_id,
                            num_workers = 1)

# wait for a couple of minutes
# you will recieve an email from abci when the submitted job is done.
#

# collect tuning results from abci
tune_res_set = abci_collect_tune_res(grid_tune_id = trg_grid_tune_id)

# convert tuning results as workflow_set
tune_wfs = as_workflow_set(!!!tune_res_set)

# evaluate workflows
tune_wfs %>% rank_results(rank_metric = "rmse", select_best = TRUE) %>%
  filter(rank == 1, .metric == "rmse")

wfid = tune_wfs %>%
  rank_results(rank_metric = "rmse", select_best = TRUE) %>%
  filter(rank == 1,  .metric == "rmse") %>%
  pull(wflow_id)

# extract best workflow
tune_res = tune_wfs %>%
  pull_workflow_set_result(wfid)



## display results of performances
tune_res %>%
  collect_metrics()



## show best model
tune_res %>%
  show_best(metric = "rmse")


# best model
best_rmse = tune_res %>%
  select_best("rmse")

# finalize paramaers in workflow
final_wf = finalize_workflow(
  tune_wf,
  best_rmse
)


# fit with final model
final_model = workflows:::fit.workflow(final_wf, ames)


# prediction with final model
pred = predict(final_model, new_data= ames)
```

Remove package

``` r
remove.packages("abcihelpr")
```
