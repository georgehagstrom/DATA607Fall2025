library(tidyverse)
library(ellmer)
library(vitals)

anthropic_key = read_csv("/home/georgehagstrom/anthropic_key.csv")  |> 
  pull(ANTHROPIC_API_KEY)

gemini_key = read_csv("/home/georgehagstrom/gemini_api_key.csv") |> 
  pull(GEMINI_API_KEY)

Sys.setenv(ANTHROPIC_API_KEY = anthropic_key)
Sys.setenv(GEMINI_API_KEY = gemini_key)


glimpse(are)

are |> slice_head(n=1) |> pull(input) |> cat()

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

are |> slice_head(n=1) |> pull(target) |> cat()

ggplot(data = diamonds) + 
  geom_bar(aes(x = cut, y = after_stat(count) / sum(after_stat(count)), fill = clarity))


are_task = Task$new(
  dataset = are,
  solver = generate(chat_anthropic(model = "claude-haiku-4-5-20251001")),
  scorer = model_graded_qa(partial_credit = TRUE, scorer_chat = chat_anthropic(model = "claude-sonnet-4-5-20250929")),
  name = "haiku Eval"
)

are_task

are_task$eval()

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

cat(are_task$get_samples()$result[1])

cat(are_task$get_samples()$scorer_chat[[1]]$last_turn()@text)


are_task_data = vitals_bind(are_task)

are_task_data

are_task_data |> ggplot(aes(x=score)) + geom_bar()

are_task_flash = are_task$clone()

are_task_flash$eval(solver_chat = chat_google_gemini(model = "gemini-2.5-flash"))


are_task_flash_lite = are_task$clone()

are_task_flash_lite$eval(solver_chat = chat_google_gemini(model = "gemini-2.5-flash-lite"))


system_prompt = "You are an expert R programmer with a deep knowledge of ggplot2, the fundamentals of the
grammar of graphics, and the tidyverse"


are_task_haiku_prompt = are_task$clone()
are_task_haiku_prompt$eval(solver_chat = chat_anthropic(system_prompt = system_prompt,
                                                                    model = "claude-haiku-4-5-20251001"))

are_task_flash_prompt = are_task$clone()
are_task_flash_prompt$eval(solver_chat = chat_google_gemini(system_prompt = system_prompt,
                                                        model = "gemini-2.5-flash"))


are_task_sonnet = are_task$clone()
are_task_sonnet$eval(solver_chat = chat_anthropic(system_prompt = system_prompt, 
                                                  model = "claude-sonnet-4-5-20250929"))

models_anthropic()

are_task_opus = are_task$clone()
are_task_opus$eval(solver_chat = chat_anthropic(system_prompt = system_prompt, 
                                                  model = "claude-opus-4-1-20250805"))


are_task_eval = vitals_bind(are_task,
                            are_task_flash_lite,
                            are_task_flash,
                            are_task_haiku_prompt,
                            are_task_flash_prompt,
                            are_task_sonnet,
                            are_task_opus) |> 
  mutate(
    task = case_when(task == "are_task" ~ "haiku",
                     task == "are_task_flash_lite" ~ "flash_lite",
                     task == "are_task_flash" ~ "flash",
                     task == "are_task_haiku_prompt" ~ "haiku_prompt",
                     task == "are_task_flash_prompt" ~ "flash_prompt",
                     task == "are_task_sonnet" ~ "sonnet",
                     task == "are_task_opus" ~ "opus") )|> 
      rename(model = task)
  
are_task_eval |> mutate(score = factor(
  case_when( score == "I" ~ "Incorrect",
             score == "P" ~ "Partially Correct",
             score == "C" ~ "Correct"), levels = c("Incorrect","Partially Correct","Correct"),
  ordered = TRUE
)) |> ggplot(aes(y=model,fill=score)) + geom_bar() +
  scale_fill_brewer(breaks = rev, palette = "RdYlGn")

library(ordinal)

are_mod = clm(score ~ model, data = are_task_eval)

are_mod

confint(are_mod)
