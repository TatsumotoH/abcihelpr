# abci helper関数群




#' init abci config vals
#'
#' @export abci_init
#'
abci_init = function(){
  abci_group_account <<- "your_group_account"
  abci_user_account <<- "your_user_account"

  ssh_config_file <<- "~/.ssh/config"
  ssh_identity_file <<- "~/.ssh/id_rsa"

  abci_ssh_cmd <<- "ssh"
  abci_scp_cmp <<- "scp"



  abci_local_dir <<- "~/.tune"
  abci_remote_dir <<- "~/.tune"

  #
  abci_remote_params_dir <<- glue::glue("{abci_remote_dir}/params", abci_remote_dir=abci_remote_dir)
  abci_remote_output_dir <<- glue::glue("{abci_remote_dir}/output", abci_remote_dir=abci_remote_dir)

  abci_local_params_dir <<- glue::glue("{abci_local_dir}/params", abci_remote_dir=abci_local_dir)
  abci_local_output_dir <<- glue::glue("{abci_local_dir}/output", abci_remote_dir=abci_local_dir)
}

#' set abci group account
#'
#' @param abci_group_account group_account for abci
#' @export
#'
set_abci_group_account = function(abci_group_account){

  abci_group_account <<- abci_group_account

}

#' set abci group account
#'
#' @param abci_group_account group_account for abci
#' @export set_abci_user_account
set_abci_user_account = function(abci_user_account){

  abci_user_account <<- abci_user_account

}





#' set ssh identify file
#' @param path_ssh_identity path to ssh identity
#' @export set_ssh_identity
#'
set_ssh_identity = function(path_ssh_identity){

  ssh_identity_file <<- path_ssh_identity

}

#' print current configuration for abci
#'
#' @export print_config
#'
print_config = function(){

  cat("abci_group_account: ", abci_group_account, "\n")
  cat("abci_user_account: ", abci_user_account, "\n")

  cat("ssh_identity_file: ", ssh_identity_file, "\n")

  cat("abci_local_dir: ", abci_local_dir, "\n")
  cat("abci_remote_dir: ", abci_remote_dir, "\n")


  cat("abci_ssh_cmd: ", abci_ssh_cmd, "\n")
  cat("abci_scp_cmd: ", abci_scp_cmd, "\n")

}



#' set work directory(.tune)  in home
#'
#' set work directories in remote and local. if work directories do not exist, the function create them.
#' @param abci_remote_dir Remote work directory for tuning. Default is .tune
#' @param abci_local_dir Local work directory for tuning. Default is .tune
#' @export abci_set_work_directory
#'
abci_set_work_directory = function(abci_remote_dir=NULL, abci_local_dir=NULL){

  if(FALSE == is.null(abci_remote_dir)){
    abci_remote_dir <<- abci_remote_dir
    abci_remote_params_dir <<- glue::glue("{abci_remote_dir}/params", abci_remote_dir=abci_remote_dir)
    abci_remote_output_dir <<- glue::glue("{abci_remote_dir}/output", abci_remote_dir=abci_remote_dir)
  }

  if(FALSE == is.null(abci_local_dir)){
    abci_local_dir <<- abci_local_dir
    abci_local_params_dir <<- glue::glue("{abci_local_dir}/params", abci_local_dir=abci_local_dir)
    abci_local_output_dir <<- glue::glue("{abci_local_dir}/output", abci_local_dir=abci_local_dir)
  }


  #local_dirの準備
  # ret = system(
  #   glue::glue("mkdir -p {abci_local_params_dir} {abci_local_output_dir} ",
  #              abci_local_params_dir = abci_local_params_dir,
  #              abci_local_output_dir = abci_local_output_dir ),
  #   intern = TRUE  )

  fs::dir_create(abci_local_params_dir)
  fs::dir_create(abci_local_output_dir)



  #remote_dirの準備
  ret = system(
    glue::glue("{abci_ssh_cmd} -F {ssh_config_file} -i {ssh_identity_file} es-abci 'mkdir -p  {abci_remote_params_dir}  {abci_remote_output_dir} '",
               abci_ssh_cmd=abci_ssh_cmd,
               ssh_config_file = ssh_config_file,
               ssh_identity_file = ssh_identity_file,
               abci_remote_params_dir = abci_remote_params_dir,
               abci_local_params_dir = abci_local_params_dir),
    intern = TRUE)


  #do_tune.R, do_tune.shファイルのコピー処理
  # ret = system(
  #   glue::glue("cp {do_tune_R} {do_tune_sh} {ssh_config} {abci_local_dir}",
  #              do_tune_R = system.file("do_tune.R", package = "abcihelpr"),
  #              do_tune_sh = system.file("do_tune.sh", package = "abcihelpr"),
  #              ssh_config = system.file("ssh_config", package = "abcihelpr"),
  #              abci_local_dir = evalq(abci_local_dir, parent.frame())
  #              ),
  #   intern = TRUE)

  do_tune_R = system.file("do_tune.R", package = "abcihelpr")
  do_tune_sh = system.file("do_tune.sh", package = "abcihelpr")
  ssh_config = system.file("ssh_config", package = "abcihelpr")
  abci_local_dir = evalq(abci_local_dir, parent.frame())

  fs::file_copy(do_tune_R, abci_local_dir, overwrite = TRUE )
  fs::file_copy(do_tune_sh, abci_local_dir, overwrite = TRUE  )
  fs::file_copy(ssh_config, abci_local_dir, overwrite = TRUE  )


  #ssh_config内容を修正する
  path_ssh_config = paste0(evalq(abci_local_dir, parent.frame()),"/ssh_config")
  lines = stringr::str_replace_all(readr::read_lines(path_ssh_config),
                                   "path_ssh_identity",
                                   ssh_identity_file)

  lines =  stringr::str_replace_all(lines, "abci_user_account", abci_user_account)

  readr::write_lines(x = lines,
                     file=path_ssh_config)

  ssh_config_file <<- path_ssh_config

  #configure ssh and scp commands
  abci_ssh_cmd <<- glue::glue("ssh -F {ssh_config_file}", ssh_config_file=ssh_config_file)
  abci_scp_cmd <<- glue::glue("scp -F {ssh_config_file}", ssh_config_file=ssh_config_file)



  # do_tune.R, do_tune.shをremoteにコピーする



}



#' upload a parameter grid to abci
#'
#' @param grid_tune_id id for a parameter grid
#' @param tune_wf  workflow object for tuning with a parameter grid
#' @param tune_folds resample object (rest() object) for tuning
#' @param param_grid A data frame of tuning combinations
#' @export abci_upload_params
#'
abci_upload_params = function(grid_tune_id = 1001, tune_wf, tune_folds, param_grid){

  #file for tuning parameters
  param_file = glue::glue("{abci_local_params_dir}/tune_{grid_tune_id}.Rdata",
                          abci_local_params_dir = abci_local_params_dir,
                          grid_tune_id = grid_tune_id)

  tune_r_file = glue::glue("{abci_local_dir}/do_tune.R",
                           abci_local_dir = abci_local_dir)

  tune_sh_file = glue::glue("{abci_local_dir}/do_tune.sh",
                            abci_local_dir = abci_local_dir)


  #save object for tuning into the param file
  save(list = c("grid_tune_id","tune_wf", "tune_folds", "param_grid"),
       file = param_file
  )

  #uploadする
  #後ほど、ここの処理をrsyncでなくscpを使うようにする
  # ret = system(glue::glue("{abci_rsync_cmd} -av {abci_local_dir}/ es-abci:{abci_remote_dir}",
  #                         abci_rsync_cmd = abci_rsync_cmd,
  #                         abci_local_dir = abci_local_dir,
  #                         abci_remote_dir = abci_remote_dir),
  #              intern = TRUE)


  # ret = system(glue::glue("{abci_scp_cmd} -F {ssh_config_file} {param_file} es-abci:{abci_remote_dir}",
  #                         abci_scp_cmd = abci_scp_cmd,
  #                         ssh_config_file = ,
  #                         param_file = param_file,
  #                         abci_remote_dir = abci_remote_dir),
  #              intern = TRUE)


  #tune fileをリモートにコピーする
  ret = system(glue::glue("{abci_scp_cmd}  {tune_r_file} {tune_sh_file}  es-abci:{abci_remote_dir}",
                          abci_scp_cmd = abci_scp_cmd,
                          tune_r_file = tune_r_file,
                          tune_sh_file = tune_sh_file,
                          abci_remote_dir = abci_remote_dir),
               intern = TRUE)

  #param fileをリモートにコピーする
  ret = system(glue::glue("{abci_scp_cmd} {param_file} es-abci:{abci_remote_params_dir}",
                          abci_scp_cmd = abci_scp_cmd,
                          param_file = param_file,
                          abci_remote_params_dir = abci_remote_params_dir),
               intern = TRUE)

  #clean output directory in local machine
  prev_obj_files = fs::dir_ls(
    path = abci_local_output_dir,
    regexp = glue::glue("tune_res_{grid_tune_id}_.*\\.obj", grid_tune_id=grid_tune_id)
    )

  fs::file_delete(prev_obj_files)

  #clean output directory in remote machine
  ret = system(glue::glue("{abci_ssh_cmd} es-abci rm -f '{abci_remote_output_dir}/tune_res_{grid_tune_id}_*.obj'",
                          abci_ssh_cmd = abci_ssh_cmd,
                          abci_remote_output_dir = abci_remote_output_dir,
                          grid_tune_id = grid_tune_id
                          )
  )


  ret
}




#' submit a job to qsub (batch query)
#'
#' @param grid_tune_id id for tuning with a parameter grid
#' @param grid_tune_start start no. for a subset of a parameter grid to tune
#' @param grid_tune_end end no. for a subset of a parameter grid to tune. -1 means the end of a parameter grid
#' @export abci_submit_job_to_qsub
#'
abci_submit_job_to_qsub = function(grid_tune_id, grid_tune_start=1, grid_tune_end=-1){

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


#' split a job into n works and submit them to qsub (batch query)
#'
#' @param grid_tune_id grid_tune_id
#' @param num_workers number of workers. integer.
#' @export abci_submit_job_for_workers
abci_submit_job_for_workers = function(grid_tune_id = NULL, num_workers=1) {

  x =  seq(1, dim(param_grid)[1]) #index for a whole grid
  d = num_workers # num_workers
  y = split(x, sort(rep_len(1:d, length(x)))) # splitted index

  for(ind in 1:length(y)){
    start = range(y[[ind]])[1]
    end = range(y[[ind]])[2]

    abci_submit_job_to_qsub(grid_tune_id = grid_tune_id,
                            grid_tune_start=start,
                            grid_tune_end=end)
  }


}

#' collect tuning results from abci and transfer them to local environment
#'
#' @param grid_tune_id id for tuning with a parameter grid
#' @export abci_collect_tune_res
#'
abci_collect_tune_res = function(grid_tune_id = 1001){

  #
  # ret = system( glue::glue("{abci_rsync_cmd}  -av  es-abci:'{abci_remote_output_dir}/*.obj' {abci_local_output_dir}",
  #                          abci_rsync_cmd = abci_rsync_cmd,
  #                          abci_rsync_sshoption = abci_rsync_sshoption,
  #                          abci_remote_output_dir = abci_remote_output_dir,
  #                          abci_local_output_dir = abci_local_output_dir),
  #               intern = TRUE)

  ret = suppressWarnings(
        system( glue::glue("{abci_scp_cmd} -q  es-abci:'{abci_remote_output_dir}/tune_res_{grid_tune_id}_*.obj' {abci_local_output_dir}",
                           abci_scp_cmd = abci_scp_cmd,
                           abci_remote_output_dir = abci_remote_output_dir,
                           grid_tune_id = grid_tune_id,
                           abci_local_output_dir = abci_local_output_dir),
                intern = TRUE,
                ignore.stderr = TRUE,
                ignore.stdout = TRUE)
    )

  # capture tune_res from obj files
  tune_res = fs::dir_ls(path = glue::glue("{abci_local_output_dir}", abci_local_output_dir=abci_local_output_dir),
                      regexp = glue::glue("tune_res_{grid_tune_id}_.+\\.obj", grid_tune_id = grid_tune_id)
                    ) %>%
    map( ~ readRDS(.x))


  #tune_res from several tuning workflows
  tune_res

}




