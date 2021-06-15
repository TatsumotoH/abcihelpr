#チューニングのためだけのRスクリプト


library(tidymodels)
library(embed)

#source("RecipeSteps.R")
#source("AdditionalTunes.R")

#tune_wf, tune_folds, param_gridがロードされる
#load("./.param/tune.RData")

#cat(getwd())

args = commandArgs(trailingOnly = T)

f_grid_tune_id = args[1]
grid_tune_start = args[2]
grid_tune_end = args[3]


param_file = paste0("./params/tune_", f_grid_tune_id, ".Rdata")

#load("tune.Rdata")
load(param_file)

# set all the param_grid to target if grid_tune is -1
if( grid_tune_end == -1){
  n_param_grid = dim(param_grid)[1]
  grid_tune_end = n_param_grid
}

#extract target grid_param
trg_grid_param = param_grid[grid_tune_start:grid_tune_end, ]


#for parallel processing
doParallel::registerDoParallel()


tune_res = tune_grid(
  tune_wf,
  resamples = tune_folds,             #tuneするときに使うデータセット
  grid = trg_grid_param,               #グリッド計画
  control = control_grid(save_workflow = TRUE)
)

timestr = format(Sys.time(), "%H%M%OS")

saveRDS(
  tune_res,
  file = paste0("./output/tune_res_",
                grid_tune_id,
                "_",
                timestr,
                ".obj")
)
