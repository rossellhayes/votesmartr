#' Read data tables from VoteSmart
#'
#' This function allows you to read data tables from the VoteSmart website.
#' @param url URL of the VoteSmart page you wish to scrape data from
#' @keywords VoteSmart table
#' @importFrom dplyr bind_rows mutate select
#' @importFrom magrittr divide_by
#' @importFrom janitor clean_names
#' @importFrom purrr map_chr map2_chr pluck
#' @importFrom rvest html_nodes html_table html_text
#' @importFrom stringr str_extract str_replace
#' @importFrom tibble as_tibble
#' @importFrom tidyselect everything
#' @importFrom xml2 read_html
#' @export
#' @examples
#' read_votesmart()

read_votesmart <- function(url) {
  url <- url %>% str_remove("\\?p=.*")

  pages <- url %>%
    read_html() %>%
    html_nodes(
      xpath =
        '//*[@id="main"]/section/div[2]/div/div[4]/div/div[2]/div[1]/span/ul/li'
    ) %>%
    html_text() %>%
    str_extract("\\S") %>%
    map_chr(~ paste0("?p=", .)) %>%
    map2_chr(url, ., paste0)

  year <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="main"]/section/div[2]/div/div[1]/h4') %>%
    html_text() %>%
    str_extract("\\d{4}(-\\d{4})?") %>%
    str_extract("\\d{4}$") %>%
    as.numeric()

  org <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="main"]/section/div[2]/div/div[1]/h3/a') %>%
    html_text()

  map(pages, ~ read_votesmart_table(., year, org)) %>%
    bind_rows()
}

read_votesmart_table <- function(url, year, org) {
  url %>%
    read_html() %>%
    html_nodes(
      xpath = '//*[@id="main"]/section/div[2]/div/div[4]/div/div[2]/table'
    ) %>%
    html_table() %>%
    pluck(1) %>%
    clean_names() %>%
    as_tibble() %>%
    mutate(
      rating = rating %>%
        str_replace("%", "") %>%
        as.numeric() %>%
        divide_by(100),
      org = org,
      year = year
    )
}
