# install.packages("pacman")
pacman::p_load(httr,
               xml2,
               curl,
               rvest,
               glue,
               lubridate,
               tidyverse,
               zoo)

#save(sessionToken, file =  'sessionToken')
load('sessionToken') # secret

getHeaders <- function() {
  c("accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
    "accept-language" = "en-US,en;q=0.9",
    "cache-control" = "max-age=0",
    "sec-ch-ua" = "\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"96\", \"Microsoft Edge\";v=\"96\"",
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = "\"Windows\"",
    "sec-fetch-dest" = "document",
    "sec-fetch-mode" = "navigate",
    "sec-fetch-site" = "same-origin",
    "sec-fetch-user" = "?1",
    "upgrade-insecure-requests" = "1",
    "cookie" = glue("session={sessionToken}"),
    "Referer" = "https://adventofcode.com/2021/day/1",
    "Referrer-Policy" = "strict-origin-when-cross-origin"
  )
}

getProblemDay <- function(day, year=2021) {
  url <- glue("https://adventofcode.com/{year}/day/{day}")
  httr.headers <- httr::add_headers(.headers=getHeaders())
  resp <- httr::GET(url, httr.headers)
  resp %>% 
    read_html() %>% 
    html_element('main') %>% 
    html_text()
}

getInputDay <- function(day, year=2021) {
  url <- glue("https://adventofcode.com/{year}/day/{day}/input")
  httr.headers <- httr::add_headers(.headers=getHeaders())
  resp <- httr::GET(url, httr.headers)
  text <- content(resp, as='text', encoding='UTF-8')
  text
}

# NOT WORKING... Best to just solve manually.
# solveInputDay1 <- function(day, answer) {
#   url <- glue("https://adventofcode.com/2021/day/{day}/answer")
#   httr.headers <- httr::add_headers(.headers=getHeaders())
#   body <- list(level=as.character(day),
#                answer=as.character(answer))
#   resp.to.post <- httr::POST(url=url, 
#                              config=httr.headers,
#                              body=body)
#   result
# }


