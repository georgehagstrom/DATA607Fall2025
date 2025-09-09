library(tidyverse)

who2

who2 |> pivot_longer(cols = !c("country","year"),
                     names_to = "category",
                     values_to = "cases",
                     values_drop_na = TRUE)

who2 |> pivot_longer(cols = !c("country","year"),
                     names_sep = "_",
                     names_to = c("diagnosis_method","gender","age"),
                     values_to = "cases",
                     values_drop_na = TRUE)



household |> pivot_longer(cols = !"family",
                          names_sep = "_",
                          names_to = c(".value","child_number"),
                          values_drop_na = TRUE) |> 
  mutate(child_number = parse_number(child_number))



cms_patient_experience |> 
  pivot_wider(names_from = measure_cd,
              values_from = prf_rate)



cms_patient_experience |> 
  pivot_wider(names_from = measure_cd,
              values_from = prf_rate,
              id_cols = starts_with("org"))


weather = read_csv("/home/georgehagstrom/work/Teaching/DATA607Fall2025/website/meetups/Meetup3/weather.csv")

weather_tidy = weather |> pivot_longer(cols = d1:d31,
                        names_to = "day",
                        values_to = "temp",
                        values_drop_na = TRUE) |> 
  mutate(day = parse_number(day)) |> 
  select(id,year, month, day, element, temp) |> 
  pivot_wider(names_from = element,
              values_from = temp)

weather_tidy


