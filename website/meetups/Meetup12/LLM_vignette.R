library(tidyverse)
library(ellmer)
library(mall)
library(vitals)

# LLMs with R coding Vignette

# For the first task we are going to try text classification with a
# few different methods. We are using the UCI spam email dataset.

# Get our API keys

anthropic_key = read_csv("/home/georgehagstrom/anthropic_key.csv")  |> 
  pull(ANTHROPIC_API_KEY)

gemini_key = read_csv("/home/georgehagstrom/gemini_api_key.csv") |> 
  pull(GEMINI_API_KEY)

Sys.setenv(ANTHROPIC_API_KEY = anthropic_key)
Sys.setenv(GEMINI_API_KEY = gemini_key)



# Read in the data

spam = read_delim("work/Teaching/DATA607Fall2025/website/meetups/Meetup12/SMSSpamCollection",delim="\t")

# For the first attempt, we are just going to try to classify the dataset with a basic prompt

sys_prompt_spam_basic = "You are an expert spam detection system. You will receive emails in text strings.
Classify each email as a legitimate email (TRUE) or a spam email (FALSE). Respond with only one word"

chat_haiku_basic = chat_anthropic(system_prompt = sys_prompt_spam_basic,model = "claude-haiku-4-5-20251001")

chat_flash_lite = chat_google_gemini(system_prompt = sys_prompt_spam_basic, model = "gemini-flash-lite-latest")

chat_llama3 = chat_ollama(system_prompt = sys_prompt_spam_basic,model = "llama3")

# Let us select a subset of 100 emails on which to test

set.seed(5102)

testing_examples = spam |> slice_sample(n=100)




# Now lets extract the text separately:

email_prompts = testing_examples |> pull(text) |> as.list()

class_flash = parallel_chat(chat_flash_lite,interpolate("Is the following email spam or real: {{email}}", email = email_prompts))


class_haiku = parallel_chat(chat_haiku_basic,interpolate("Is the following email spam or real: {{email}}", email = email_prompts))


class_llama3 = parallel_chat(chat_llama3,interpolate("Is the following email spam or real: {{email}}", email = email_prompts))


# Ok, let's test the accuracy

flash_answers = class_flash |> map_chr(~.x$last_turn()@text)

haiku_answers = class_haiku |> map_chr(~.x$last_turn()@text)

llama3_answers = class_llama3 |> map_chr(~.x$last_turn()@text)


testing_examples = testing_examples |> 
  mutate(class = if_else(class == "ham",TRUE,FALSE),
         class_flash = flash_answers)
  

testing_examples = testing_examples |> 
  mutate(class_flash = flash_answers,
         class_haiku = haiku_answers,
         class_llama3 = llama3_answers)

testing_examples |> summarise(flash_accuracy = mean(class == class_flash),
                              haiku_accuracy = mean(class == class_haiku),
                              llama3_accuracy = mean(class == class_llama3))

haiku_answers

training_examples_spam = spam |> 
  filter(class == "spam") |> 
  slice_sample(n=5) |> 
  anti_join(testing_examples, by = join_by(text==text))
training_examples_spam

training_examples_ham = spam |> 
  filter(class == "ham") |> 
  slice_sample(n=5) |> 
  anti_join(testing_examples, by = join_by(text==text))

training_examples = training_examples_ham |> bind_rows(training_examples_spam)
training_examples |> mutate(class = if_else(class == "ham",TRUE,FALSE)) |> 
  write_csv("work/Teaching/DATA607Fall2025/website/meetups/Meetup12/spam_prompt.md")

advanced_prompt = interpolate_file("work/Teaching/DATA607Fall2025/website/meetups/Meetup12/spam_prompt.md")

chat_haiku_basic = chat_anthropic(system_prompt = advanced_prompt,model = "claude-haiku-4-5-20251001")

chat_flash_lite = chat_google_gemini(system_prompt = advanced_prompt, model = "gemini-flash-lite-latest")

chat_llama3 = chat_ollama(system_prompt = advanced_prompt,model = "llama3")

class_flash_adv = parallel_chat(chat_flash_lite,interpolate("Is the following email spam or real: {{email}}", email = email_prompts))


class_haiku_adv = parallel_chat(chat_haiku_basic,interpolate("Is the following email spam or real: {{email}}", email = email_prompts))


class_llama3_adv = parallel_chat(chat_llama3,interpolate("Is the following email spam or real: {{email}}", email = email_prompts))

flash_answers_adv = class_flash_adv |> map_chr(~.x$last_turn()@text)

haiku_answers_adv = class_haiku_adv |> map_chr(~.x$last_turn()@text)

llama3_answers_adv = class_llama3_adv |> map_chr(~.x$last_turn()@text)

testing_examples = testing_examples |> 
  mutate(class_flash = flash_answers,
         class_haiku = haiku_answers,
         class_llama3 = llama3_answers,
         class_flash_adv = flash_answers_adv,
         class_haiku_adv = haiku_answers_adv)


testing_examples |> summarise(flash_accuracy = mean(class == class_flash),
                             haiku_accuracy = mean(class == class_haiku),
                             llama3_accuracy = mean(class == class_llama3),
                             flash_accuracy_adv = mean(class == class_flash_adv),
                             haiku_accuracy_adv = mean(class == class_haiku_adv))

haiku_answers_adv



class_haiku_adv_v2 = parallel_chat_structured(chat_haiku_basic,
                                              interpolate("Is the following email spam or real: {{email}}", email = email_prompts),
                                              type = type_object(class = type_boolean()))



chat_llama3_adv = map(email_prompts, ~{
  chat_llama3$chat_structured(interpolate("Is the following email spa or real: {{x}}",x=.x),
                              type = type_object(class = type_boolean()))
})

chat_llama3_adv <- map(email_prompts, ~{
  chat_llama3$chat_structured(
    interpolate("Is the following email spam or real: {{x}}", x=.x),
    type = type_boolean()
  )
})


class_llama3_adv 

haiku_answers_adv = class_haiku_adv_v2 
haiku_answers_adv

testing_examples = testing_examples |> 
  mutate(class_flash = flash_answers,
         class_haiku = haiku_answers,
         class_llama3 = llama3_answers,
         class_flash_adv = flash_answers_adv,
         class_haiku_adv = haiku_answers_adv)


testing_examples |> summarise(flash_accuracy = mean(class == class_flash),
                              haiku_accuracy = mean(class == class_haiku),
                              llama3_accuracy = mean(class == class_llama3),
                              flash_accuracy_adv = mean(class == class_flash_adv),
                              haiku_accuracy_adv = mean(class == class_haiku_adv))



advanced_prompt_mall = interpolate_file("work/Teaching/DATA607Fall2025/website/meetups/Meetup12/spam_prompt_mall.md")

chat_haiku_mall = chat_anthropic(system_prompt = advanced_prompt_mall,model = "claude-haiku-4-5-20251001")



llm_use(chat_haiku_mall)

email_prompts_vec = as.vector(email_prompts) 


emails = tibble(emails = email_prompts_vec) |> unnest_longer(emails)
emails
class_haiku_mall = llm_classify(emails, emails, c("SPAM", "NOT_SPAM"))
class_haiku_mall


data("reviews")
emails

emails |> slice_head(n=1) |> llm_classify(emails,c("SPAM","NOT_SPAM"),additional_prompt = "Is this a spam email or a real email?")

reviews |>
  llm_classify(review, c("appliance", "computer"))

class_haiku_mall = emails  |>  llm_custom(emails, advanced_prompt_mall)

class_haiku_mall = class_haiku_mall |> mutate(.pred = case_when(.pred == "NOT_SPAM" ~ TRUE,
                                                                .pred == "SPAM" ~ FALSE,
                                                                .default = NA))

class_haiku_mall_adv = class_haiku_mall$.pred

testing_examples = testing_examples |> 
  mutate(class_haiku_mall = class_haiku_mall_adv)


testing_examples = testing_examples |> 
  mutate(class_llama3_adv = llama3_answers_adv)

testing_examples |> summarise(flash_accuracy = mean(class == class_flash),
                              haiku_accuracy = mean(class == class_haiku),
                              llama3_accuracy = mean(class == class_llama3),
                              flash_accuracy_adv = mean(class == class_flash_adv),
                              haiku_accuracy_adv = mean(class == class_haiku_adv),
                              llama3_accuracy_adv = mean(class == class_llama3_adv),
                              haiku_mall_accuracy_adv = mean(class == class_haiku_mall,na.rm=TRUE)) |> 
  pivot_longer(cols = everything(),names_to = "model",values_to = "accuracy")

testing_examples$class_haiku_mall
testing_examples$class
