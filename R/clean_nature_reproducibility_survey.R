clean_nature_reproducibility_survey <- function(path) {
  questions_raw <- read_survey_file(path = path, range = 1:2) %>%
    create_new_table_for_multi_answer_questions()
  results_raw <- read_survey_file(path = path, range = dplyr::combine(3, NA)) %>%
    create_new_table_for_multi_answer_questions()
  single_answer_questions <- reshape_single_answer_questions(survey_questions = questions_raw)
  single_answer_results <- reshape_single_answer_results(survey_results = results_raw)
  tidy_single_answer_survey(single_answer_questions = single_answer_questions, single_answer_results = single_answer_results)
}

read_survey_file <- function(path, range) {
  readxl::read_xlsx(
    path = path,
    col_names = FALSE,
    col_types = "text",
    range = readxl::cell_rows(range),
  )
}

create_new_table_for_multi_answer_questions <- function(survey) {
  `%>%` <- dplyr::`%>%`
  multi_answer_questions <- dplyr::combine(
    glue::glue("X__{seq(7, 10)}"),
    glue::glue("X__{seq(13, 19)}")
  )

  list(
    "single_answer" = dplyr::select(survey, -multi_answer_questions),
    "multi_answer" = dplyr::select(survey, multi_answer_questions)
  )
}

reshape_single_answer_questions <- function(survey_questions) {
  `%>%` <- dplyr::`%>%`
  questions_only <- survey_questions %>%
    purrr::pluck("single_answer") %>%
    dplyr::slice(1) %>%
    tidyr::gather(X__3:X__115, key = "key", value = "question") %>%
    dplyr::select(-X__1, -X__2)
  choices_only <- survey_questions %>%
    purrr::pluck("single_answer") %>%
    dplyr::slice(2) %>%
    tidyr::gather(X__3:X__115, key = "key", value = "statement") %>%
    dplyr::select(-X__1, -X__2)
  questions_only %>%
    dplyr::left_join(choices_only) %>%
    tidyr::fill(question)
}

reshape_single_answer_results <- function(survey_results) {
  `%>%` <- dplyr::`%>%`
  survey_results %>%
    purrr::pluck("single_answer") %>%
    tidyr::gather(X__3:X__115, key = "key", value = "response")
}

tidy_single_answer_survey <- function(single_answer_questions, single_answer_results) {
  `%>%` <- dplyr::`%>%`
  long_survey <- single_answer_results %>%
    dplyr::left_join(single_answer_questions) %>%
    dplyr::select(-X__1) %>%
    dplyr::rename(respid = X__2)
  table_single_answer_questions <- long_survey %>%
    dplyr::select(key, question, statement) %>%
    dplyr::distinct()
  tidy_single_answer_survey <- long_survey %>%
    dplyr::select(-question, -statement) %>%
    tidyr::spread(key = "key", value = "response") %>%
    select(
      respid,
      glue::glue("X__{seq(3, 6)}"),
      glue::glue("X__{seq(11, 12)}"),
      glue::glue("X__{seq(20, 115)}")
    )

  list(
    "questions" = table_single_answer_questions,
    "survey" = tidy_single_answer_survey
  )
}

# Prototype figure
# one_ques <- tidy_survey$questions %>% filter(question == "Please use the scale below to indicate how much each of the following factors contributes to a failure to reproduce results:")
# one_ques_key <- tidy_survey$questions %>% filter(question == "Please use the scale below to indicate how much each of the following factors contributes to a failure to reproduce results:") %>% pull(key)
# tidy_survey$survey %>% select(one_ques_key) %>% gather(X__52:X__65, key = "key", value = "response") %>% left_join(one_ques) %>% select(-question, -key) %>% mutate(response = recode(response, `Always contributes` = "Always/often contribute", `Very often contributes` = "Always/often contribute", `Sometimes contributes` = "Sometimes contribute")) %>% filter(response == "Always/often contribute" | response == "Sometimes contribute") %>% count(statement, response) %>% ggplot() + geom_col(aes(forcats::fct_reorder2(statement, response == "Always/often contribute", n, .desc = FALSE), n, fill = forcats::fct_relevel(response, "Sometimes contribute"))) + coord_flip()
