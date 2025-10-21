library(tidyverse)
library(rvest)
library(robotstxt)

url_nfl_reference = "https://www.pro-football-reference.com/years/2024/passing.htm"

paths_allowed(url_nfl_reference)

passing_data_2024 = read_html(url_nfl_reference)

passing_data_2024 |> html_element("table") |> html_table()



cuny_sps_url = "https://sps.catalog.cuny.edu/courses"

paths_allowed(cuny_sps_url)

cuny_catalog = cuny_sps_url |> read_html()

cuny_catalog |> html_element("table")

cuny_catalog = cuny_sps_url |> read_html_live("https://sps.catalog.cuny.edu/courses?page=2")


cuny_catalog =  read_html_live("https://sps.catalog.cuny.edu/courses?page=2")


cuny_catalog |> html_element("table") |> html_table()



url_stem = "https://sps.catalog.cuny.edu/courses?page="

urls = 1:37 |> map_chr(\(x) str_c(url_stem,x))

urls

process_catalog_page = function(url){
  
  catalog_page =  read_html_live(url)
  
  Sys.sleep(1)
  
  catalog_data = catalog_page |> 
    html_element("table") |> 
    html_table()
  
  if (class(catalog_data$Credits) == "character"){
    catalog_data = catalog_data |> mutate(Credits = parse_integer(Credits))
  }
  
  rm(catalog_page)
  
  return(catalog_data)
  
}


process_catalog_insistently = insistently(process_catalog_page, rate = rate_delay(pause = 5,max_times = 10))



catalog_data = urls |> map_dfr(process_catalog_page)


process_catalog_page(urls[4]) 



#

url_imdb = "https://www.imdb.com/chart/top/"

imdb_page = read_html_live(url_imdb)

imdb_page |> html_element("table")

movie_sel = ".cli-children"

imdb_page |>   html_elements("span.ipc-rating-star--rating") |> html_text2() |> parse_number()

# Title

title = imdb_page |> html_elements(".with-margin .ipc-title__text--reduced") |> html_text2()
title

# URL

hrefs = imdb_page |> html_elements(movie_sel) |> html_elements("a") |> html_attr("href")

ind = 1:250
hrefs = hrefs[2*ind -1]

# Rating
rating = imdb_page |>   html_elements("span.ipc-rating-star--rating") |> html_text2() |> parse_number()
rating
# Vote Count

vote_count = imdb_page |> 
  html_elements("span.ipc-rating-star--voteCount") |> 
  html_text2() |> str_remove_all("\\(|\\)")
vote_count

#


info_vec = imdb_page |> html_elements(".cli-title-metadata") |> html_elements("span") |> html_text2()

top_250 = imdb_page |> html_elements(movie_sel)

top_250[1] |> html_elements(".cli-title-metadata") |> html_elements("span") |> html_text2()

process_movie = function(movie){
  info_vec = movie |> 
    html_elements(".cli-title-metadata") |> 
    html_elements("span") |> 
    html_text2()
  
  year = info_vec[1]
  run_time = info_vec[2]
  content_rating = info_vec[3]
  
  tibble(year = year,
         run_time = run_time,
         content_rating = content_rating)
  
}

movie_data = top_250 |> map_dfr(process_movie)

movie_data = movie_data |> mutate(title = title, rating= rating, vote_count = vote_count,url= hrefs)
movie_data = movie_data |> select(title,rating,year,run_time,vote_count,content_rating,url)
movie_data
