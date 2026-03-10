tempdir_file <- sanitize_path(withr::local_tempdir())
withr::local_envvar(REDCAPSYNC_CACHE_OVERRIDE = tempdir_file)
# excel_to_list (Internal)
test_that("excel_to_list works!", {
  expect_directory_exists(tempdir_file)
  test_file <- file.path(tempdir_file, "cars.xlsx")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_excel(df_list, dir = tempdir_file, file_name = "cars")
  expect_file_exists(test_file)
  df_list_saved <- excel_to_list(test_file)
  expect_list(df_list, len = 2L)
  expect_named(df_list, names(df_list_saved))
  expect_identical(nrow(df_list$one), nrow(df_list_saved$one))
  expect_identical(nrow(df_list$two), nrow(df_list_saved$two))
  expect_error(list_to_excel(file.path(tempdir_file, "fake_cars.xlsx")))
})
# form_to_wb (Internal)
test_that("form_to_wb works!", {
})
# list_to_csv (Internal)
test_that("list_to_csv works!", {
  expect_directory_exists(tempdir_file)
  test_file <- file.path(tempdir_file, "cars.csv")
  test_file_one <- file.path(tempdir_file, "cars_one.csv")
  test_file_two <- file.path(tempdir_file, "cars_two.csv")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_csv(df_list, dir = tempdir_file, file_name = "cars")
  expect_file_exists(test_file_one)
  expect_file_exists(test_file_two)
  one <- read.csv(test_file_one)
  two <- read.csv(test_file_two)
  expect_data_frame(one, nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_identical(colnames(one), colnames(mtcars))
  expect_data_frame(two, nrows = nrow(cars), ncols = ncol(cars))
  expect_identical(colnames(two), colnames(cars))
})
# list_to_excel (Internal)
test_that("list_to_excel works!", {
  tempdir_test <- sanitize_path(withr::local_tempdir())
  expect_directory_exists(tempdir_test)
  test_file <- file.path(tempdir_test, "cars.xlsx")
  test_file_one <- file.path(tempdir_test, "cars_one.xlsx")
  test_file_two <- file.path(tempdir_test, "cars_two.xlsx")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_excel(df_list, dir = tempdir_test, file_name = "cars")
  expect_file_exists(test_file)
  list_to_excel(df_list,
                dir = tempdir_test,
                file_name = "cars",
                separate = TRUE)
  expect_file_exists(test_file_one)
  expect_file_exists(test_file_two)
  one <- readxl::read_excel(test_file_one)
  two <- readxl::read_excel(test_file_two)
  expect_data_frame(one, nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_identical(colnames(one), colnames(mtcars))
  expect_data_frame(two, nrows = nrow(cars), ncols = ncol(cars))
  expect_identical(colnames(two), colnames(cars))
})
# list_to_wb (Internal)
test_that("list_to_wb works!", {
})
# rename_list_names_excel (Internal)
test_that("rename_list_names_excel works!", {
  list_names <- c("data", "metadata", "fields", "forms", "choices")
  expect_identical(rename_list_names_excel(list_names), list_names)
  list_names <- c(
    "data",
    "metadata",
    "fields",
    "forms",
    "choices",
    "long_var_name_that_is_not_unique_until_the_very_end1"
  )
  list_names_match <- unique_trimmed_strings(strings = list_names,
                                             max_length = 31L)
  expect_identical(rename_list_names_excel(list_names), list_names_match)
  list_names <- list_names |>
    append("long_var_name_that_is_not_unique_until_the_very_end2")
  expect_false(anyDuplicated(list_names) > 0L)
  expect_message({
    new_names <- rename_list_names_excel(list_names)
  }, "Duplicated names when trimmed from right")
  expect_all_true(str_length(new_names) <= 31L)
})
# save_csv (Internal)
test_that("save_csv works!", {
  expect_directory_exists(tempdir_file)
  test_file <- file.path(tempdir_file, "cars.csv")
  expect_false(file.exists(test_file))
  save_csv(
    form = mtcars,
    dir = tempdir_file,
    file_name = "cars",
    overwrite = TRUE
  )
  expect_file_exists(test_file)
  test_saved_csv <- read.csv(test_file)
  expect_data_frame(test_saved_csv,
                    nrows = nrow(mtcars),
                    ncols = ncol(mtcars))
  expect_message(
    save_csv(
      form = cars,
      dir = tempdir_file,
      file_name = "cars",
      overwrite = FALSE
    ),
    "Already a file"
  )
  test_saved_csv <- read.csv(test_file)
  expect_data_frame(test_saved_csv,
                    nrows = nrow(mtcars),
                    ncols = ncol(mtcars))
})
# save_wb (Internal)
test_that("save_wb works!", {
})
