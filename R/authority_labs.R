
#' Set Callback
#'
#' @param keyword Keyword we want to analysis
#' @param callback_url Callback URL where we can get the results
#' @param auth_token API Key
#' @param locale  Locale for request, as defined in documenation
#' @param mobile  Set to TRUE to get mobile results
#'
#' @return A list with the response
#' @export
#'
#' @examples
#'
#' set_callback_authority_labs('capital one', 'http://mhairihmcneill.com/callback')
set_callback_authority_labs <- function(keyword,
                                        callback_url = 'http://mhairihmcneill.com/callback',
                                        auth_token = 'LmJWkiGmVjQwA1JIIx3o',
                                        locale     = 'en-uk',
                                        mobile     =  FALSE){

  parameters <- list(
    'keyword'    = keyword,
    'callback'   = callback_url,
    'auth_token' = auth_token,
    'locale'     = locale
  )

  if (mobile) parameters$mobile <- 'true'

  url <- 'http://api.authoritylabs.com/keywords/priority'
  r = httr::POST(url,
             body = parameters,
             encode = "json")

  return(list(r = r, status_code = httr::status_code(r), content = httr::content(r)))
}


#' Get data, using callback url
#'
#' @param json_callback_url URL from callback
#' @param auth_token API key
#'
#' @return A list containing the content of the response
#' @export
#'
#' @examples
#' raw_data <- get_authority_labs_data("http://api.authoritylabs.com/keywords/get.json?keyword=capital%20one&rank_date=2016-09-13&locale=en-uk&engine=google&pages_from=false&lang_only=false&geo=&autocorrect=&mobile=false&ppl_id=&papi_id=14738015579796")
get_authority_labs_data <- function(json_callback_url,
                                    auth_token = 'LmJWkiGmVjQwA1JIIx3o'){

  response <- httr::GET(json_callback_url, query = list(auth_token = auth_token))

  httr::http_error(response)

  return(httr::content(response))
}


### Parsing data helpers ###

#' Extract URL Data
#'
#' Helper function - extract a variable from list data
#'
#' @param var a variable
#' @param list_data a list
#'
#' @return A character vector
#' @export
#'
#' @examples \donotrun{
#' extract_url_data(var, list_data)
#' }
extract_url_data <- function(var, list_data){
  l <- purrr::map_chr(list_data, var)
  l <- dplyr::na_if(l, 'NULL')
  l <- dplyr::na_if(l, 'list()')
  return(l)
}

#' Get URL variables
#'
#' Helper function for converting to data frame
#'
#' @param return_data
#'
#' @return Data Frame
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples \donotrun{
#' get_url_vars(return_data)
#' }
get_url_vars <- function(return_data){

  all_url_vars <-
  purrr::map(return_data$serp, names) %>%
    purrr::reduce(union)

  variables <-
  purrr::map(all_url_vars,
             purrr::possibly(extract_url_data, NULL),
             return_data$serp)

  df <- purrr::discard(variables, is.null)
  df <- purrr::map(df, type.convert)
  df <- purrr::map_if(df, is.factor, as.character)
  df <- purrr::map(df, dplyr::data_frame)

  df <- dplyr::bind_cols(df)

  names(df) <- all_url_vars[purrr::map_lgl(variables, ~ !is.null(.x))]

  return(df)
}

#' Convert list returned from authority
#' labs to data frame
#'
#' @param return_data A list from `get_authority_labs_data`
#'
#' @return A dataframe with one row per url
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' raw_data <- get_authority_labs_data("http://api.authoritylabs.com/keywords/get.json?keyword=mhairi&rank_date=2016-09-13&locale=en-uk&engine=google&pages_from=false&lang_only=false&geo=&autocorrect=&mobile=false&ppl_id=&papi_id=14737953718458")
#' to_data_frame_authority_labs(raw_data)
to_data_frame_authority_labs <- function(return_data){

  df <- get_url_vars(return_data)

  n_urls <- nrow(df)

  keyword_vars <-
    purrr::keep(return_data, ~ !is.list(.x) & length(.x) == 1) %>%
    purrr::map(~ rep(.x, n_urls))

  df <- dplyr::bind_cols(df, keyword_vars)

  return(df)
}

#' Get account data
#'
#' @param account_id Account number
#' @param auth_token API key
#'
#' @return List with response data
#' @export
#'
#' @examples
#' get_account_data()
get_account_data <- function(account_id = '331', auth_token =  'LmJWkiGmVjQwA1JIIx3o'){
  url <- paste0('https://api.authoritylabs.com/account/', account_id, '.json?auth_token=', auth_token)
  r <- httr::GET(url)
  httr::content(r)
}

#return_data <- get_authority_labs_data('http://api.authoritylabs.com/keywords/get.json?keyword=scottish%20salmon&rank_date=2016-08-05&locale=en-uk&engine=google&pages_from=false&lang_only=false&geo=&autocorrect=&mobile=false&ppl_id=&papi_id=57a4853adab89e4a07000009')
#set_callback_authority_labs('scottish salmon', 'https://requestb.in/xvyd8axv', priority = TRUE)


#
# set_callback_authority_labs('scottish salmon', 'http://requestb.in/1a00l6u1', priority = TRUE)
# return_data <- get_authority_labs_data('http://api.authoritylabs.com/keywords/get.json?keyword=scottish%20salmon&rank_date=2016-08-29&locale=en-uk&engine=google&pages_from=false&lang_only=false&geo=&autocorrect=&mobile=false&ppl_id=&papi_id=14724708816196')
# df <- to_data_frame_authority_labs(return_data)
#
# readr::write_rds(df, 'scottish_salmon2.rds')

