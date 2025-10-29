# Libraries

library(tidyverse)
library(httr2)
library(jsonlite)
library(tibblify)
library(lubridate)

# EPA api description https://aqs.epa.gov/aqsweb/documents/data_api.html


# EPA authentication details

epa_email = "georgehagstrom@gmail.com"
epa_key = read_csv("/home/georgehagstrom/work/Teaching/DATA607Fall2025/website/meetups/Meetup10/epa_key.csv") |> 
  pull(key)



# Create a blank request

epa_url = "https://aqs.epa.gov/data/api/"

epa_req = epa_url |> 
  request() |> req_url_query(
    email = epa_email,
    key = epa_key
  )

# Stems for creating requests

epa_classes_url = "list/classes"

epa_counties_url = "list/countiesByState"

epa_states_url = "list/states"

epa_pars_url = "list/parametersByClass"

county_endpoint = "monitors/byCounty"

# Identifying NY State

epa_req_states = epa_req |> 
  req_url_path_append(epa_states_url) 


states_list = epa_req_states |> req_perform() |> resp_body_json()

states_list$Data |> tibblify() |> print(n=60)

NY_code = "36"


# Finding Manhattan/NY County

epa_req_counties = epa_req |> 
  req_url_path_append(epa_counties_url) |> 
  req_url_query(
    state = NY_code
  )

counties_list = epa_req_counties |> req_perform() |> resp_body_json()

Manhattan_Code = counties_list$Data |> 
  tibblify() |> 
  filter(str_detect(value_represented,"New York")) |> pull(code)

Manhattan_Code

# Finding PM 2.5 class

epa_classes_list = epa_req |> 
  req_url_path_append(epa_classes_url) |> 
  req_perform() |> 
  resp_body_json() |> 
  get_Data()


get_Data = function(data){
  return(data$Data |> tibblify())
}


epa_classes_list |> print(n=30)

pm25_code = "PM2.5 CONT NONREF"

# Finding ideal PM 2.5 measurement

epa_pm25_pars_list = epa_req |> 
  req_url_path_append(epa_pars_url) |> 
  req_url_query(
    pc = pm25_code
  ) |> 
  req_perform() |> 
  resp_body_json() |> 
  get_Data()



epa_pm25_pars_list

pm25_raw = "88501"


# Finding a monitor that has this data

epa_req_county_monitors = epa_req |> 
  req_url_path_append(county_endpoint) |> 
  req_url_query(
    param = pm25_raw,
    county = Manhattan_Code,
    state = NY_code,
    bdate = "20150101",
    edate = "20250930"
  )

county_monitor_list = epa_req_county_monitors |> 
  req_perform() |> 
  resp_body_json() |> 
  get_Data()


county_monitor_list |> print(n=100)


monitor_code = "0135"

# Getting the data from that monitor

data_by_site_url = "sampleData/bySite"

epa_req_sample = epa_req |> 
  req_url_path_append(data_by_site_url) |> 
  req_url_query(
    param = pm25_raw,
    county = Manhattan_Code,
    state = NY_code,
    site = monitor_code,
    bdate = "20070101",
    edate = "20250930"
    
  )


epa_sample_data = epa_req_sample |> 
  req_perform() |> 
  resp_body_json() |> 
  get_Data()

resp = last_response()

resp |> resp_body_json()


epa_req_sample = epa_req |> 
  req_url_path_append(data_by_site_url) |> 
  req_url_query(
    param = pm25_raw,
    county = Manhattan_Code,
    state = NY_code,
    site = monitor_code,
    bdate = "20080101",
    edate = "20081231"
    
  )


epa_sample_data = epa_req_sample |> 
  req_perform() |> 
  resp_body_json() |> 
  get_Data()


epa_sample_data |> 
  group_by(date_gmt) |> 
  summarise(mean_pm25 = mean(sample_measurement,na.rm=TRUE)) |> 
  ggplot(aes(x=as_datetime(date_gmt),y=mean_pm25)) +
  geom_point() + 
  geom_smooth() +
  scale_x_date() +
  theme_bw()


# Getting the data for all the years

year_list = as.character(2007:2025)
year_list


year_function = function(year){
  bdate = str_c(year,"0101")
  edate = str_c(year,"1231")
  
  epa_req_sample = epa_req |> 
    req_url_path_append(data_by_site_url) |> 
    req_url_query(
      param = pm25_raw,
      county = Manhattan_Code,
      state = NY_code,
      site = monitor_code,
      bdate = bdate,
      edate = edate
    ) |> 
    req_throttle(rate = 1/10)
  
  return(epa_req_sample)
  
}

epa_req_list = year_list |> map(year_function)

epa_sample_data_full = 
  epa_req_list |> 
  req_perform_sequential(
    progress = TRUE
  )

sample_data_full =  epa_sample_data_full |> map(resp_body_json)

sample_data_full |> map(get_Data)

sample_df = sample_data_full[1:17] |> 
  map(get_Data) |>
  map(select,!starts_with("date_of_last_change")) |> 
  list_rbind()
   

sample_df

# Showing an example of how to select multiple things using .multi


req_weather = request("https://api.weather.gov/")
req_weather |> req_dry_run()

alert_url = "alerts/active"
request_weather_alerts = req_weather |> req_url_path_append(alert_url)
flny_alerts = request_weather_alerts |> 
  req_url_query(area = c("FL","NY"),.multi = "comma")

alerts= flny_alerts |> req_perform() |> resp_body_json()
alerts

