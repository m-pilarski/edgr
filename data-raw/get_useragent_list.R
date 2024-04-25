# https://user-agents.net/download?browser=safari&platform=ios&platform_maker=apple-inc&device_name=iphone

user_agent_response <-
  httr2::request("https://user-agents.net") |>
  httr2::req_url_path_append("download") |>
  httr2::req_body_form(
    browser="safari",
    platform="ios",
    platform_maker="apple-inc",
    device_name="iphone",
    download="json"
  ) |>
  httr2::req_method("POST") |>
  httr2::req_perform()

user_agent_list <-
  user_agent_response |>
  httr2::resp_body_string() |>
  jsonlite::fromJSON()

usethis::use_data(user_agent_list, internal=TRUE, overwrite=TRUE)
