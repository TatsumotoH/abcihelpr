# abci helper関数群




#' init abci config vals
#'
#' @export abci_init
#'
abci_init = function(){

  abci_config <<- list()



  abci_config$abci_group_account <<- "your_group_account"
  abci_config$abci_user_account <<- "your_user_account"

  abci_config$ssh_config_file <<- fs::path_expand("~/.ssh/config")
  abci_config$ssh_identity_file <<- fs::path_expand("~/.ssh/id_rsa")

  abci_config$emailaddr <<- ""

  abci_config$abci_ssh_cmd <<- "ssh"
  abci_config$abci_scp_cmd <<- "scp"

  abci_config$abci_remote_host <<- "es-abci"

  abci_config$abci_remote_dir <<- "~/.tune"
  abci_config$abci_local_dir <<- fs::path_expand("~/.tune")



  #remoteはabciでlinux決め打ち
  abci_config$abci_remote_params_dir <<- glue::glue("{abci_remote_dir}/params",
                                                  abci_remote_dir = abci_config$abci_remote_dir)
  abci_config$abci_remote_output_dir <<- glue::glue("{abci_remote_dir}/output",
                                                  abci_remote_dir = abci_config$abci_remote_dir)

  #localはmac,windows,linuxの可能性があるので、path展開しておく
  abci_config$abci_local_params_dir <<- fs::path_expand(glue::glue("{abci_local_dir}/params",
                                                                 abci_local_dir = abci_config$abci_local_dir))
  abci_config$abci_local_output_dir <<- fs::path_expand(glue::glue("{abci_local_dir}/output",
                                                                 abci_local_dir = abci_config$abci_local_dir))

  # number of parallel processing
  abci_config$ppn <<- 4
}

#' set abci group account
#'
#' @param abci_group_account group_account for abci
#' @export
#'
set_abci_group_account = function(abci_group_account){

  abci_config$abci_group_account <<- abci_group_account

}

#' set abci group account
#'
#' @param abci_group_account group_account for abci
#' @export
#'
set_abci_group_account = function(abci_group_account){

  abci_config$abci_group_account <<- abci_group_account

}

#' set abci group account
#'
#' @param abci_group_account group_account for abci
#' @export set_abci_user_account
set_abci_user_account = function(abci_user_account){

  abci_config$abci_user_account <<- abci_user_account

}





#' set ssh identify file
#' @param path_ssh_identity path to ssh identity
#' @export set_ssh_identity
#'
set_ssh_identity = function(path_ssh_identity){

  abci_config$ssh_identity_file <<- fs::path_expand(path_ssh_identity)

}

#' change ssh config file
#' @param path_ssh_config path to ssh identity
#' @param abci_remote_host remote host to connect
#' @export set_ssh_config
#'
set_ssh_config = function(path_ssh_config, abci_remote_host){

  abci_config$ssh_config_file <<- path_ssh_config
  abci_config$abci_remote_host <<- abci_remote_host

}

#' set mail addr to notify the end of the task
#' @param emailaddr mail address eg. xxxx@yyy.com
#' @export
#'
set_mailaddr = function(mailaddr){

  abci_config$emailaddr <<- mailaddr

}

#' set the number of parallel processing per job
#' @param ppn number of parallel processing
#' @export
#'
set_ppn = function(ppn){

  abci_config$ppn <<- ppn

}


#' print current configuration for abci
#'
#' @export print_config
#'
print_config = function(){

  cat("abci_group_account: ", abci_config$abci_group_account, "\n")
  cat("abci_user_account: ", abci_config$abci_user_account, "\n")

  cat("ssh_config_file:", abci_config$ssh_config_file, "\n")
  cat("ssh_identity_file: ", abci_config$ssh_identity_file, "\n")

  cat("abci_remote_host: ", abci_config$abci_remote_host, "\n")
  cat("abci_local_dir: ", abci_config$abci_local_dir, "\n")
  cat("abci_remote_dir: ", abci_config$abci_remote_dir, "\n")


  cat("abci_ssh_cmd: ", abci_config$abci_ssh_cmd, "\n")
  cat("abci_scp_cmd: ", abci_config$abci_scp_cmd, "\n")

  cat("ppn:", abci_config$ppn, "\n")
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
    abci_config$abci_remote_dir <<- abci_remote_dir
    abci_config$abci_remote_params_dir <<- glue::glue("{abci_remote_dir}/params",
                                                    abci_remote_dir = abci_remote_dir)
    abci_config$abci_remote_output_dir <<- glue::glue("{abci_remote_dir}/output",
                                                    abci_remote_dir = abci_remote_dir)
  }

  if(FALSE == is.null(abci_local_dir)){
    abci_config$abci_local_dir <<- fs::path_expand(abci_local_dir)
    abci_config$abci_local_params_dir <<- fs::path_expand(
                                            glue::glue("{abci_local_dir}/params",
                                                       abci_local_dir = abci_local_dir))
    abci_config$abci_local_output_dir <<- fs::path_expand(
                                            glue::glue("{abci_local_dir}/output",
                                                        abci_local_dir = abci_local_dir))
  }


  #prepare for local_dir
  fs::dir_create(fs::path_expand(abci_config$abci_local_params_dir))
  fs::dir_create(fs::path_expand(abci_config$abci_local_output_dir))



  #copy tune files(do_tune.R, do_tune.sh)ファイルのコピー処理
  do_tune_R = system.file("do_tune.R", package = "abcihelpr")
  do_tune_sh = system.file("do_tune.sh", package = "abcihelpr")
  do_tune_custom_sh =system.file("do_tune_custom.sh", package = "abcihelpr")
  ssh_config = system.file("ssh_config", package = "abcihelpr")

    abci_local_dir = fs::path_expand(
                    evalq(abci_config$abci_local_dir, parent.frame()))

  fs::file_copy(do_tune_R, abci_config$abci_local_dir, overwrite = TRUE )
  fs::file_copy(do_tune_sh, abci_config$abci_local_dir, overwrite = TRUE  )
  fs::file_copy(do_tune_custom_sh, abci_config$abci_local_dir, overwrite = TRUE  )
  fs::file_copy(ssh_config, abci_config$abci_local_dir, overwrite = TRUE  )


  #modify ssh_config file
  path_ssh_config = fs::path_expand(
                            paste0(evalq(abci_config$abci_local_dir, parent.frame()),"/ssh_config"))

  lines = stringr::str_replace_all(readr::read_lines(path_ssh_config),
                                   "path_ssh_identity",
                                   abci_config$ssh_identity_file)

  lines =  stringr::str_replace_all(lines,
                                    "abci_user_account",
                                    abci_config$abci_user_account)

  readr::write_lines(x = lines,
                     file = path_ssh_config)

  #store the location of ssh file to abci_config
  abci_config$ssh_config_file <<- path_ssh_config


  #create work directory in remote machine
   ret = system2(abci_config$abci_ssh_cmd,
          c("-t -t",
            "-F",
            abci_config$ssh_config_file,
            abci_config$abci_remote_host,
            "mkdir -p",
            shQuote(abci_config$abci_remote_params_dir),
            shQuote(abci_config$abci_remote_output_dir)
            ),
          stdout = NULL,
          stderr = NULL
          )

}



#' upload a parameter grid to abci
#'
#' @param grid_tune_id id for a parameter grid
#' @param tune_wf  workflow object for tuning with a parameter grid
#' @param tune_folds resample object (rest() object) for tuning
#' @param pkg_names  additional packages to load in the tuning script
#' @param param_grid A data frame of tuning combinations
#' @param fns character vector of function names to be exported
#' @export abci_upload_params
#'
abci_upload_params = function(grid_tune_id = 1001, tune_wf, tune_folds, pkg_names=NULL, param_grid, fns=NULL){

  #file for tuning parameters in local
  param_file = fs::path_expand(glue::glue("{abci_local_params_dir}/tune_{grid_tune_id}.Rdata",
                          abci_local_params_dir = abci_config$abci_local_params_dir,
                          grid_tune_id = grid_tune_id))

  tune_r_file = fs::path_expand(glue::glue("{abci_local_dir}/do_tune.R",
                           abci_local_dir = abci_config$abci_local_dir))

  # tuning shell file for abci
  tune_sh_file = fs::path_expand(glue::glue("{abci_local_dir}/do_tune.sh",
                            abci_local_dir = abci_config$abci_local_dir))

  # tuning shell file for original
  tune_sh_file = fs::path_expand(glue::glue("{abci_local_dir}/do_tune_custom.sh",
                                            abci_local_dir = abci_config$abci_local_dir))

  #save functions in envrionment variable e1
  e1 = new_environment()

  for (n in fns) {
    assign(n, get(n, env_parent()), e1)
  }

  #save object for tuning into the param file
  saveRDS(list(grid_tune_id = grid_tune_id,
               tune_wf = tune_wf,
               tune_folds = tune_folds,
               pkg_names = pkg_names,
               param_grid = param_grid,
               e1 = e1),
       file = param_file
  )





  #copy tune files to remote machine
  ret = system(glue::glue("{abci_scp_cmd} {tune_r_file} {tune_sh_file}  {abci_remote_host}:{abci_remote_dir}",
                          abci_remote_host = abci_config$abci_remote_host,
                          abci_scp_cmd = abci_config$abci_scp_cmd,
                          tune_r_file = tune_r_file,
                          tune_sh_file = tune_sh_file,
                          abci_remote_dir = abci_config$abci_remote_dir),
               intern = TRUE,
               ignore.stdout = TRUE, ignore.stderr = TRUE)


  #copy a param file to remote machine
  ret = system(glue::glue("{abci_scp_cmd} {param_file} {abci_remote_host}:{abci_remote_params_dir}",
                          abci_remote_host = abci_config$abci_remote_host,
                          abci_scp_cmd = abci_config$abci_scp_cmd,
                          param_file = param_file,
                          abci_remote_params_dir = abci_config$abci_remote_params_dir),
               intern = TRUE,
               ignore.stdout = TRUE, ignore.stderr = TRUE)

  #clean output directory in local machine
  prev_obj_files = fs::dir_ls(
    path = abci_config$abci_local_output_dir,
    regexp = glue::glue("tune_res_{grid_tune_id}_.*\\.obj", grid_tune_id = grid_tune_id)
    )

  fs::file_delete(prev_obj_files)

  #clean output directory in remote machine
  ret = system(glue::glue("{abci_ssh_cmd} {abci_remote_host} rm -f '{abci_remote_output_dir}/tune_res_{grid_tune_id}_*.obj'",
                          abci_remote_host = abci_config$abci_remote_host,
                          abci_ssh_cmd = abci_config$abci_ssh_cmd,
                          abci_remote_output_dir = abci_config$abci_remote_output_dir,
                          grid_tune_id = grid_tune_id
                          ),
               intern = TRUE,
               ignore.stdout = TRUE, ignore.stderr = TRUE)


  # ret
}




#' submit a job to qsub (batch query)
#'
#' @param grid_tune_id id for tuning with a parameter grid
#' @param grid_tune_start start no. for a subset of a parameter grid to tune
#' @param grid_tune_end end no. for a subset of a parameter grid to tune. -1 means the end of a parameter grid
#' @export abci_submit_job_to_qsub
#'
abci_submit_job_to_qsub = function(grid_tune_id, grid_tune_start=1, grid_tune_end=-1){

  if(abci_config$abci_remote_host == "es-abci"){

    #submit job to qsub queue
    ret = system2(abci_config$abci_ssh_cmd,
                  c("-t -t",
                    "-F",
                    abci_config$ssh_config_file,
                    abci_config$abci_remote_host,
                    "qsub",
                    "-wd",
                    shQuote(abci_config$abci_remote_dir),
                    "-e",
                    shQuote(abci_config$abci_remote_output_dir),
                    "-o",
                    shQuote(abci_config$abci_remote_output_dir),
                    "-l rt_C.small=1",
                    "-g",
                    abci_config$abci_group_account,
                    "-m e",
                    "-N",
                    paste0("gti_",grid_tune_id),
                    shQuote(paste0(abci_config$abci_remote_dir,"/", "do_tune.sh")),
                    grid_tune_id,
                    grid_tune_start,
                    grid_tune_end
                  ),
                stdout = NULL,
                stderr = NULL
            )

  }else{
    # user-defined host to connect
    # submit job to qsub queue
    ret = system2(abci_config$abci_ssh_cmd,
                  c("-t -t",
                    "-F",
                    abci_config$ssh_config_file,
                    abci_config$abci_remote_host,
                    "qsub",
                    "-d",
                    shQuote(abci_config$abci_remote_dir),
                    "-e",
                    shQuote(abci_config$abci_remote_output_dir),
                    "-o",
                    shQuote(abci_config$abci_remote_output_dir),
                    # "-l rt_C.small=1",
                    paste0("-l ", "nodes=1:ppn=", abci_config$ppn),
                    # "-g", abci_config$abci_group_account,
                    "-m e",
                    ifelse(abci_config$emailaddr != "", paste0("-M ", abci_config$emailaddr), ""),
                    "-N",
                    paste0("gti_",grid_tune_id),
                    "-v",
                    paste0("grid=", grid_tune_id, ",start=", grid_tune_start, ",end=", grid_tune_end),
                    shQuote(paste0(abci_config$abci_remote_dir,"/", "do_tune_custom.sh"))
                  ),
                  stdout = NULL,
                  stderr = NULL
    )
  }


  return(ret)

}


#' split a job into n works and submit them to qsub (batch query)
#'
#' @param grid_tune_id grid_tune_id
#' @param num_workers number of workers. integer.
#' @param param_grid parameter grid
#' @export abci_submit_job_for_workers
abci_submit_job_for_workers = function(grid_tune_id = NULL, param_grid, num_workers=1) {

  x =  seq(1, dim(param_grid)[1]) #index for a whole grid
  d = num_workers # num_workers
  y = split(x, sort(rep_len(1:d, length(x)))) # splitted index

  for(ind in 1:length(y)){
    start = range(y[[ind]])[1]
    end = range(y[[ind]])[2]

    ret = abci_submit_job_to_qsub(grid_tune_id = grid_tune_id,
                            grid_tune_start = start,
                            grid_tune_end = end)

    cat(ret)
  }


}

#' collect tuning results from abci and transfer them to local environment
#'
#' @param grid_tune_id id for tuning with a parameter grid
#' @export abci_collect_tune_res
#'
abci_collect_tune_res = function(grid_tune_id = 1001){


  #copy tune_res obj files from remote to local
  ret = system2(abci_config$abci_scp_cmd,
                c("-F",
                  abci_config$ssh_config_file,
                  shQuote(paste0(abci_config$abci_remote_host,":", abci_config$abci_remote_output_dir,"/","tune_res_", grid_tune_id,"_*.obj")),
                  abci_config$abci_local_output_dir
                ),
              stdout = NULL,
              stderr = NULL
  )



  # capture tune_res from obj files
  tune_res = fs::dir_ls(path = glue::glue("{abci_local_output_dir}",
                                          abci_local_output_dir = abci_config$abci_local_output_dir),
                      regexp = glue::glue("tune_res_{grid_tune_id}_.+\\.obj", grid_tune_id = grid_tune_id)
                    ) %>%
    map( ~ readRDS(.x))


  #tune_res from several tuning workflows
  tune_res

}

# report status of job submitted in abci

# abci_status_job = function(grid_tune_id){
#
#   #submit job to qsub queue
#   ret = system2(abci_config$abci_ssh_cmd,
#                 c("-t -t",
#                   "-F",
#                   abci_config$ssh_config_file,
#                   "es-abci",
#                   "qstat"
#                 ),
#                 stdout = NULL,
#                 stderr = NULL
#   )
#
# }


#
# ライブラリやオブジェクトを追加する場合の関数
#
# abci_config = new_abci_config(multidplyr_n_cluster)
# cluster_library(abci_config, "purrr")
# cluster_copy(abci_config, "f")
#
