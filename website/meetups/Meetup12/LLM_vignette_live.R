library(tidyverse)
library(ellmer)
library(mall)

anthropic_key = read_csv("/home/georgehagstrom/anthropic_key.csv") |> pull(ANTHROPIC_API_KEY)

gemini_key = read_csv("/home/georgehagstrom/gemini_api_key.csv") |> pull(GEMINI_API_KEY)

Sys.setenv(ANTHROPIC_API_KEY = anthropic_key)
Sys.setenv(GEMINI_API_KEY = gemini_key)

spam = read_delim("work/Teaching/DATA607Fall2025/website/meetups/Meetup12/SMSSpamCollection",delim="\t")

spam

system_prompt = "You are an expert spam detection system. You will receive emails in text stringsd. Classify each 
email as a legitimate email (TRUE) or a smap email (FALSE). Respond with one word only."

models_anthropic()

chat_haiku_basic = chat_anthropic(system_prompt = system_prompt, model = "claude-haiku-4-5-20251001")

models_google_gemini()

chat_flash_lite_basic = chat_google_gemini(system_prompt = system_prompt, model = "gemini-flash-lite-latest")

set.seed(5102)

testing_examples = spam |> slice_sample(n=100)
testing_examples

email_prompts = testing_examples |> pull(text) |> as.list()

flash_response = parallel_chat(chat_flash_lite_basic,email_prompts)

flash_response |> map_chr(~.x$last_turn()@text)


flash_response = parallel_chat(chat_flash_lite_basic,interpolate("Is the following email spam or real: {{email}}",email = email_prompts))

flash_answers = flash_response  |> map_chr(~.x$last_turn()@text)

flash_answers

haiku_response = parallel_chat(chat_haiku_basic,interpolate("Is the following email spam or real: {{email}}",email = email_prompts))

haiku_answers = haiku_response |>  map_chr(~.x$last_turn()@text)

haiku_answers


testing_examples = testing_examples |> 
  mutate(class = if_else(class == "ham", TRUE, FALSE),
         flash_answers = flash_answers,
         haiku_answers = haiku_answers) 
 
testing_examples = testing_examples |> 
  mutate(flash_answers = as.logical(flash_answers),
         haiku_answers = as.logical(haiku_answers)) 

testing_examples |> summarise(flash_accuracy = mean(class == flash_answers,na.rm=TRUE),
                              haiku_accuracy = mean(class == haiku_answers, na.rm = TRUE))


training_examples_spam = spam |> 
  filter(class == "spam") |> 
  slice_sample(n=5) |> 
  anti_join(testing_examples,by = join_by(text==text))

training_examples_spam

training_examples_ham = spam |> 
  filter(class == "ham") |> 
  slice_sample(n=5) |> 
  anti_join(testing_examples,by = join_by(text==text))

training_examples_ham

training_examples = training_examples_ham |> bind_rows(training_examples_spam)
training_examples

advanced_prompt = interpolate_file("/home/georgehagstrom/work/Teaching/DATA607Fall2025/website/meetups/Meetup12/spam_prompt.md")

advanced_prompt

chat_haiku_adv = chat_anthropic(system_prompt = advanced_prompt, model = "claude-haiku-4-5-20251001")


chat_flash_lite_adv = chat_google_gemini(system_prompt = advanced_prompt, model = "gemini-flash-lite-latest")



flash_response_adv = parallel_chat(chat_flash_lite_adv,interpolate("Is the following email spam or real: {{email}}",email = email_prompts))

flash_answers_adv = flash_response_adv  |> map_chr(~.x$last_turn()@text)

flash_answers_adv

haiku_response_adv = parallel_chat(chat_haiku_adv,interpolate("Is the following email spam or real: {{email}}",email = email_prompts))

haiku_answers_adv = haiku_response_adv |>  map_chr(~.x$last_turn()@text)

haiku_answers_adv = haiku_answers_adv |> str_extract("TRUE|FALSE")

haiku_answers_adv_v2 = parallel_chat_structured(chat_haiku_adv,interpolate("Is the following email spam or real: {{email}}",email = email_prompts),
                                                type = type_object(class = type_boolean()))

haiku_answers_adv_v2

testing_examples

testing_examples |> mutate(flash_answers_adv = as.logical(flash_answers_adv),
                           haiku_answers_adv = as.logical(haiku_answers_adv),
                           haiku_answers_adv_v2 = haiku_answers_adv_v2)

testing_examples |> summarise(flash_accuracy = mean(class == flash_answers,na.rm=TRUE),
                              haiku_accuracy = mean(class == haiku_answers, na.rm = TRUE),
                              flash_accuracy_adv = mean(class == flash_answers_adv),
                              haiku_accuracy_adv = mean(class == haiku_answers_adv),
                              haiku_accuracy_adv_v2 = mean(class == haiku_answers_adv_v2))


advanced_prompt_mall = interpolate_file("work/Teaching/DATA607Fall2025/website/meetups/Meetup12/spam_prompt_mall.md")

chat_haiku_mall = chat_anthropic(system_prompt = advanced_prompt_mall, model = "claude-haiku-4-5-20251001")

llm_use(chat_haiku_mall)

email_prompts_vec = as.vector(email_prompts)

email_prompts_vec

emails = tibble(emails = email_prompts_vec) |> unnest_longer(emails)
emails

answers_haiku_mall = emails |>  llm_custom(emails,advanced_prompt_mall)

answers_haiku_mall = answers_haiku_mall |> mutate(.pred = case_when( .pred == "NOT_SPAM" ~ TRUE,
                                                .pred == "SPAM" ~ FALSE,
                                                .default = NA))
testing_examples

testing_examples = testing_examples |> mutate(mall_answers = answers_haiku_mall)

testing_examples |> summarise(flash_accuracy = mean(class == flash_answers,na.rm=TRUE),
                              haiku_accuracy = mean(class == haiku_answers, na.rm = TRUE),
                              flash_accuracy_adv = mean(class == flash_answers_adv),
                              haiku_accuracy_adv = mean(class == haiku_answers_adv),
                              haiku_accuracy_adv_v2 = mean(class == haiku_answers_adv_v2),
                              mall_accuracy = mean(class == mall_answers,na.rm=TRUE))


