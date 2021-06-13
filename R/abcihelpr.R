# abci helper関数群



abci_group_account = "your_group_account"
ssh_config_file = "~/.ssh/config"
ssh_identity_file = "~/.ssh/id_rsa"

abci_ssh_cmd = "ssh"
abci_rsync_cmd = "rsync"

abci_rsync_sshoption = glue::glue("{abci_ssh_cmd} -F {ssh_config_file} -i {ssh_identity_file}",
                                  abci_ssh_cmd=abci_ssh_cmd,
                                  ssh_config_file = ssh_config_file,
                                  ssh_identity_file = ssh_identity_file)


abci_local_dir = "~/.tune"
abci_remote_dir = "~/.tune"

abci_remote_params_dir = glue::glue("{abci_remote_dir}/params", abci_remote_dir=abci_remote_dir)
abci_remote_output_dir = glue::glue("{abci_remote_dir}/output", abci_remote_dir=abci_remote_dir)

abci_local_params_dir = glue::glue("{abci_local_dir}/params", abci_remote_dir=abci_local_dir)
abci_local_output_dir = glue::glue("{abci_local_dir}/output", abci_remote_dir=abci_local_dir)



#' set abci group account
#'
#'
set_abci_group_account = function(){


}


#' create .tune directory in home
#'
abci_init = function(){

  #local_dirの準備
  ret = system(
    glue::glue("mkdir -p {{ {abci_local_params_dir} {abci_local_output_dir} }}",
               abci_local_params_dir = abci_local_params_dir,
               abci_local_output_dir = abci_local_output_dir ),
    intern = TRUE  )

  #remote_dirの準備
  ret = system(
    glue::glue("{abci_ssh_cmd} -F {ssh_config_file} -i {ssh_identity_file} -t -t es-abci 'mkdir -p {{ {abci_remote_params_dir}  {abci_remote_output_dir} }}'",
               abci_ssh_cmd=abci_ssh_cmd,
               ssh_config_file = ssh_config_file,
               ssh_identity_file = ssh_identity_file,
               abci_remote_params_dir = abci_remote_params_dir,
               abci_local_params_dir = abci_local_params_dir),
    intern = TRUE)

  ret

}



#' upload a parameter grid to abci
#'
#' @param grid_tune_id id for a parameter grid
#' @param tune_wf  workflow object for tuning with a parameter grid
#' @param tune_folds resample object (rest() object) for tuning
#' @param param_grid A data frame of tuning combinations
#' @export
abci_upload_params = function(grid_tune_id = 1001, tune_wf, tune_folds, param_grid){

  #チューニングに必要なオブジェクトを保存する
  save(list = c("grid_tune_id","tune_wf", "tune_folds", "param_grid"),
       file = glue::glue("{abci_local_params_dir}/tune_{grid_tune_id}.Rdata", grid_tune_id = grid_tune_id)
  )

  #uploadする
  ret = system(glue::glue("{abci_rsync_cmd} -av {abci_local_dir}/ es-abci:{abci_remote_dir}",
                          abci_rsync_cmd = abci_rsync_cmd,
                          abci_local_dir = abci_local_dir,
                          abci_remote_dir = abci_remote_dir),
               intern = TRUE)

  ret
}




#' submit job to qsub query
#'
#' @param grid_tune_id id for tuning with a parameter grid
#' @param grid_tune_start start no. for a subset of a parameter grid to tune
#' @param grid_tune_end end no. for a subset of a parameter grid to tune. -1 means the end of a parameter grid
#' @export
abci_submit_job_to_qsub = function(grid_tune_id, grid_tune_start=1, grid_tune_end=-1){


  #base_dir = "/home/aca10085tm/rstudio_projects/tidymodelsによるハイパーパラメータサーチ/.param/"

  # ret = system(paste0("ssh -t -t es-abci qsub -wd ",base_dir ," -l rt_C.small=1 -g gca50022 -M hirofumi.tatsumoto@gmail.com ",
  #                     base_dir,
  #                     "do_tune.sh"),
  #              intern = TRUE)
  ret = system(
    glue::glue("{abci_ssh_cmd} -t -t es-abci qsub -wd '{abci_remote_dir}' -e '{abci_remote_output_dir}' -o  '{abci_remote_output_dir}' -l rt_C.small=1 -g {abci_group_account} -m e '{abci_remote_dir}/do_tune.sh {grid_tune_id} {grid_tune_start} {grid_tune_end}'",
               abci_ssh_cmd = abci_ssh_cmd,
               abci_remote_dir = abci_remote_dir,
               abci_group_account = abci_group_account,
               abci_remote_dir = abci_remote_dir,
               grid_tune_id = grid_tune_id,
               grid_tune_start = grid_tune_start,
               grid_tune_end = grid_tune_end),
    intern = TRUE)

  ret
}





#' collect tuning results from abci and transfer them to local environment
#'
#' @param grid_tune_id id for tuning with a parameter grid
#' @export
#'
abci_collect_tune_res = function(grid_tune_id = 1001){


  ret = system( glue::glue("{abci_rsync_cmd}  -av  es-abci:'{abci_remote_output_dir}/*.obj' {abci_local_output_dir}",
                           abci_rsync_cmd = abci_rsync_cmd,
                           abci_rsync_sshoption = abci_rsync_sshoption,
                           abci_remote_output_dir = abci_remote_output_dir,
                           abci_local_output_dir = abci_local_output_dir),
                intern = TRUE)

  #ret


  #library(fs)
  tune_res = dir_ls(path = glue::glue("{abci_local_output_dir}", abci_local_output_dir=abci_local_output_dir),
                    regrep = glue::glue("tune_res_{grid_tune_id}_*.obj",
                                        abci_local_output_dir = abci_local_output_dir,
                                        grid_tune_id = grid_tune_id)
  ) %>%
    map( ~ readRDS(.x))


  #チューニング結果を１つにまとめたres
  tune_res

}
