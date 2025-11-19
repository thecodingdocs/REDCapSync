# clean_for_cli ( Internal )
test_that("clean_for_cli  works!", {
})
# cli_alert_wrap ( Internal )
test_that("cli_alert_wrap  works!", {
})
# now_time ( Internal )
test_that("now_time  works!", {
  checkmate::expect_class(now_time(),"POSIXct")
  checkmate::expect_class(now_time(),"POSIXt")
})
# process_df_list ( Internal )
test_that("process_df_list  works!", {
})
# is_something ( Internal )
test_that("is_something  works!", {
  expect_true(is_something("a"))
  expect_true(is_something(list(one = NA)))
  expect_true(is_something(mtcars))
  expect_true(is_something(1))
  expect_false(is_something(NA))
  expect_false(is_something(NULL))
  expect_false(is_something(list()))
  expect_false(is_something(data.frame()))
  expect_false(is_something(tibble()))
})
# sanitize_path ( Internal )
test_that("sanitize_path  works!", {
})
# all_character_cols ( Internal )
test_that("all_character_cols  works!", {
  form <- mtcars
  form$name <- form %>% rownames()
  form$cyl_more_then_4 <- form$cyl >4
  col_types <- form %>% lapply(class) %>% unlist() %>% unique()
  expect_vector(col_types, size = 3)
  expect_equal(col_types, c("numeric", "character", "logical"))
  form <- all_character_cols(form)
  col_types <- form %>% lapply(class) %>% unlist() %>% unique()
  expect_vector(col_types, size = 1)
  expect_equal(col_types, "character")
})
# all_character_cols_list ( Internal )
test_that("all_character_cols_list  works!", {
  form <- mtcars
  form$name <- form %>% rownames()
  form$cyl_more_then_4 <- form$cyl >4
  form_list <- list(
    one = form,
    two = airquality
  )
  col_types1 <- form_list$one %>% lapply(class) %>% unlist() %>% unique()
  col_types2 <- form_list$two %>% lapply(class) %>% unlist() %>% unique()
  expect_contains(col_types1,"numeric")
  expect_contains(col_types2,"numeric")
  form_list <- all_character_cols_list(form_list)
  col_types1 <- form_list$one %>% lapply(class) %>% unlist() %>% unique()
  col_types2 <- form_list$two %>% lapply(class) %>% unlist() %>% unique()
  expect_equal(col_types1,"character")
  expect_equal(col_types2,"character")
})
# vec1_in_vec2 ( Internal )
# vec1_not_in_vec2 ( Internal )
test_that("vec1_in_vec2 and vec1_not_in_vec2 works!", {
  vec1 <- c("a","b","c")
  vec2 <- c("b","d","c")
  out <- vec1 %>% vec1_in_vec2(vec2)
  expect_contains(out,"b")
  expect_contains(out,"c")
  expect_false("a"%in%out)
  out <- vec1 %>% vec1_not_in_vec2(vec2)
  expect_contains(out,"a")
  expect_false("b"%in%out)
  expect_false("c"%in%out)
})
# unique_which ( Internal )
test_that("unique_which  works!", {
  expect_equal(unique_which(c("a","a","b","b","c","c","c")),3)
  expect_equal(unique_which(c("a","a","b","b","c","d")),4)
})
# length_which ( Internal )
test_that("length_which  works!", {
})
# drop_nas ( Internal )
test_that("drop_nas  works!", {
})
# excel_to_list ( Internal )
test_that("excel_to_list  works!", {
})
# is_named_df_list ( Internal )
test_that("is_named_df_list  works!", {
})
# is_named_list ( Internal )
test_that("is_named_list  works!", {
})
# wb_to_list ( Internal )
test_that("wb_to_list  works!", {
})
# form_to_wb ( Internal )
test_that("form_to_wb  works!", {
})
# list_to_wb ( Internal )
test_that("list_to_wb  works!", {
})
# rename_list_names_excel ( Internal )
test_that("rename_list_names_excel  works!", {
})
# unique_trimmed_strings ( Internal )
test_that("unique_trimmed_strings  works!", {
})
# list_to_excel ( Internal )
test_that("list_to_excel works!", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "cars.xlsx")
  test_file_one <- file.path(test_dir, "cars_one.xlsx")
  test_file_two <- file.path(test_dir, "cars_two.xlsx")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_excel(df_list,dir = test_dir,file_name = "cars")
  list_to_excel(df_list,dir = test_dir,file_name = "cars",separate = TRUE)
  expect_true(file.exists(test_file_one))
  expect_true(file.exists(test_file_two))
  one <- readxl::read_excel(test_file_one)
  two <- readxl::read_excel(test_file_two)
  checkmate::expect_data_frame(one, nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_equal(colnames(one),colnames(mtcars))
  checkmate::expect_data_frame(two, nrows = nrow(cars), ncols = ncol(cars))
  expect_equal(colnames(two),colnames(cars))
})
# list_to_csv ( Internal )
test_that("list_to_csv  works!", {
  test_dir <- withr::local_tempdir() %>% sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "cars.csv")
  test_file_one <- file.path(test_dir, "cars_one.csv")
  test_file_two <- file.path(test_dir, "cars_two.csv")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_csv(df_list,dir = test_dir,file_name = "cars")
  expect_true(file.exists(test_file_one))
  expect_true(file.exists(test_file_two))
  one <- read.csv(test_file_one)
  two <- read.csv(test_file_two)
  checkmate::expect_data_frame(one, nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_equal(colnames(one),colnames(mtcars))
  checkmate::expect_data_frame(two, nrows = nrow(cars), ncols = ncol(cars))
  expect_equal(colnames(two),colnames(cars))
})
# save_wb ( Internal )
test_that("save_wb  works!", {
})
# save_csv ( Internal )
test_that("save_csv  works!", {
})
# which_duplicated ( Internal )
test_that("which_duplicated  works!", {
})
# is_consecutive_srt_1 ( Internal )
test_that("is_consecutive_srt_1  works!", {
})
# remove_html_tags ( Internal )
test_that("remove_html_tags  works!", {
})
# choice_vector_string ( Internal )
test_that("choice_vector_string  works!", {
})
# object_size ( Internal )
test_that("object_size  works!", {
})
# file_size ( Internal )
test_that("file_size  works!", {
})
# drop_if ( Internal )
test_that("drop_if  works!", {
})
# list_files_real ( Internal )
test_that("list_files_real  works!", {
})
# clean_env_names ( Internal )
test_that("clean_env_names  works!", {
})
# is_df_list ( Internal )
test_that("is_df_list  works!", {
})
# check_match ( Internal )
test_that("check_match  works!", {
})
# is_env_name ( Internal )
test_that("is_env_name  works!", {
})
# is_nested_list ( Internal )
test_that("is_nested_list  works!", {
})
# generate_hex ( Internal )
test_that("generate_hex  works!", {
  expect_true(is_hexadecimal(generate_hex(10)))
})
