#= include the library
library(tidyverse)
library(rvest)
#=see this link for documentation: http://rvest.tidyverse.org/
#=open a browser and link to the page=
thermostat.url <- "https://www.amazon.com/Nest-T3007ES-Thermostat-Temperature-Generation/product-reviews/B0131RG6VK"
thermostat.session <- session(thermostat.url)
thermostat <- thermostat.session



#= Scrape one specific review with link u
ScrapArticle <- function(u){
  
  #= jumps to link u
  temp.html <- thermostat %>%
    session_jump_to(u)
  
  user.name <- temp.html %>%
    html_nodes(css=".a-profile-name") %>%
    html_text()%>%
    unique()
  
  review.title <- temp.html %>%
    html_nodes(css=".a-text-bold span") %>%
    html_text()
  
  review.date <- temp.html %>%
    html_nodes(css=".review-date") %>%
    html_text() %>%
    str_extract("[A-z]+ [0-9]+, [0-9]+")%>%
    as.Date("%B %d, %Y")
  
  review.rating <- temp.html %>%
    html_nodes(css=".review-rating")%>%
    html_node("span")%>%
    html_text()%>%
    str_extract("[0-5].[0-9]")%>%
    as.double()
  
  review.content <- temp.html %>%
    html_nodes(css=".review-text-content span") %>%
    html_text()
  
  
  #combine into tibble
  article.table <- tibble(user=user.name,
                          title = review.title,
                          date = review.date,
                          rating = review.rating,
                          review = review.content,
                          url = links.reviews[1])
  return(article.table)
}

#= Scrape one page
ScrapePage <- function(pageNumber){
  link <- str_c(thermostat.url, "/ref=cm_cr_arp_d_paging_btm_next_", pageNumber, "?ie=UTF8&reviewerType=all_reviews&pageNumber=", pageNumber)
  links.reviews <- thermostat %>%
    session_jump_to(link) %>% # move session to the most recent page
    html_nodes(css="#cm_cr-review_list .celwidget .a-row:nth-child(2)") %>%
    html_nodes("a") %>% # extract article <a>
    html_attr("href") %>% # extract article <a> `href` attributes
    unique() %>%
    str_c("https://amazon.com",.)
  scraped <- links.reviews %>%
    map(ScrapArticle)%>%
    bind_rows()
  return(scraped)
}

#= Code that scrapes pages number of page starting from page.latest
ScrapeWeb <- function(pages, page.latest){
  start <- page.latest-pages+1
  test <- start:page.latest
  match_key <- tibble(n = test,
                      key = sample(test,length(test)))
  lapply(test, function(i){
    j <- match_key[match_key$n==i,]$key
    message("Getting page ",i, " of ",length(test), "; Actual: page ",j)
    Sys.sleep(runif(1, 1.0,5.0))
    if((i%%3)==0){
      Sys.sleep(runif(1,1.0,5.0))
    }
    ScrapePage(j)
  })->scraped
  article.table.all <- scraped%>%
    bind_rows()
  Sys.sleep(runif(1, 0.0,3.0))
  message("took a break")
  return(article.table.all)
}

#= Code that scrapes the whole website
ScrapeAll <- function(){
  #= find the last page number
  page.latest <- thermostat %>%
    html_element("#filter-info-section") %>%
    html_text() %>%
    str_replace_all(",","")%>%
    str_extract("[0-9]+ with reviews\n") %>%
    str_extract("[0-9]+") %>%
    strtoi()
  page.latest <- ceiling(page.latest/10)
  if (page.latest>500){
    page.latest=500
  }
  ans <- ScrapeWeb(10, page.latest)
  page.latest <- page.latest-10
  while(page.latest>0){
    temp <- ScrapeWeb(10,page.latest)
    ans <- bind_rows(ans, temp)
    page.latest <- page.latest-10
    Sys.sleep(runif(1,8.0,12.0))
  }
  write_csv(ans, "/Users/dylan/OneDrive/Desktop/df.csv")
  return(ans)
}

