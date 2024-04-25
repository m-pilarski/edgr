.onLoad <- function(...){

  options(edgr.n_workers=1)
  options(edgr.user_agent=sample(user_agent_list)[[1]])

}
