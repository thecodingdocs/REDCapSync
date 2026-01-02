# clean_for_cli ( Internal )
test_that("clean_for_cli works!", {
})
# cli_alert_wrap ( Internal )
test_that("cli_alert_wrap works!", {
})
# now_time ( Internal )
test_that("now_time works!", {
  checkmate::expect_class(now_time(), "POSIXct")
  checkmate::expect_class(now_time(), "POSIXt")
})
# process_df_list ( Internal )
test_that("process_df_list works!", {
})
# is_something ( Internal )
test_that("is_something works!", {
  expect_true(is_something("a"))
  expect_true(is_something(list(one = NA)))
  expect_true(is_something(mtcars))
  expect_true(is_something(1L))
  expect_false(is_something(NA))
  expect_false(is_something(NULL))
  expect_false(is_something(list()))
  expect_false(is_something(data.frame()))
  expect_false(is_something(tibble()))
})
# sanitize_path ( Internal )
test_that("sanitize_path works!", {
})
# all_character_cols ( Internal )
test_that("all_character_cols works!", {
  form <- mtcars
  form$name <- rownames(form)
  form$cyl_more_then_4 <- form$cyl > 4L
  col_types <- form |> lapply(class) |> unlist() |> unique()
  expect_vector(col_types, size = 3L)
  expect_identical(col_types, c("numeric", "character", "logical"))
  form <- all_character_cols(form)
  col_types <- form |> lapply(class) |> unlist() |> unique()
  expect_vector(col_types, size = 1L)
  expect_identical(col_types, "character")
})
# all_character_cols_list ( Internal )
test_that("all_character_cols_list works!", {
  form <- mtcars
  form$name <- rownames(form)
  form$cyl_more_then_4 <- form$cyl > 4L
  form_list <- list(one = form, two = airquality)
  col_types1 <- form_list$one |> lapply(class) |> unlist() |> unique()
  col_types2 <- form_list$two |> lapply(class) |> unlist() |> unique()
  expect_contains(col_types1, "numeric")
  expect_contains(col_types2, "numeric")
  form_list <- all_character_cols_list(form_list)
  col_types1 <- form_list$one |> lapply(class) |> unlist() |> unique()
  col_types2 <- form_list$two |> lapply(class) |> unlist() |> unique()
  expect_identical(col_types1, "character")
  expect_identical(col_types2, "character")
})
# vec1_in_vec2 ( Internal )
# vec1_not_in_vec2 ( Internal )
test_that("vec1_in_vec2 and vec1_not_in_vec2 works!", {
  vec1 <- c("a", "b", "c")
  vec2 <- c("b", "d", "c")
  out <- vec1_in_vec2(vec1, vec2)
  expect_contains(out, "b")
  expect_contains(out, "c")
  expect_false("a" %in% out)
  out <- vec1_not_in_vec2(vec1, vec2)
  expect_contains(out, "a")
  expect_false("b" %in% out)
  expect_false("c" %in% out)
})
# length_unique ( Internal )
test_that("length_unique works!", {
  expect_identical(length_unique(c("a", "a", "b", "b", "c", "c", "c")), 3L)
  expect_identical(length_unique(c("a", "a", "b", "b", "c", "d")), 4L)
})
# length_which ( Internal )
test_that("length_which works!", {
})
# drop_nas ( Internal )
test_that("drop_nas works!", {
})
# excel_to_list ( Internal )
test_that("excel_to_list works!", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "cars.xlsx")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_excel(df_list, dir = test_dir, file_name = "cars")
  expect_true(file.exists(test_file))
  df_list_saved <- excel_to_list(test_file)
  checkmate::expect_list(df_list, len = 2L)
  expect_named(df_list, names(df_list_saved))
  expect_identical(nrow(df_list$one), nrow(df_list_saved$one))
  expect_identical(nrow(df_list$two), nrow(df_list_saved$two))
  expect_error(list_to_excel(file.path(test_dir, "fake_cars.xlsx")))
})
# is_named_df_list ( Internal )
test_that("is_named_df_list works!", {
})
# is_named_list ( Internal )
test_that("is_named_list works!", {
})
# wb_to_list ( Internal )
test_that("wb_to_list works!", {
})
# form_to_wb ( Internal )
test_that("form_to_wb works!", {
})
# list_to_wb ( Internal )
test_that("list_to_wb works!", {
})
# rename_list_names_excel ( Internal )
test_that("rename_list_names_excel works!", {
})
# trim_string ( Internal )
test_that("trim_string works!", {
  expect_identical(trim_string("one", 5L), "one")
  expect_identical(trim_string("one123", 5L), "one12")
  expect_identical(trim_string("one123", 3L), "one")
})
# unique_trimmed_strings ( Internal )
test_that("unique_trimmed_strings works!", {
  expect_error(unique_trimmed_strings())
  expect_identical(unique_trimmed_strings("one", 5L), "one")
  test_vector <- c("one_version_1", "one_version_2", "one_version_3")
  expect_identical(unique_trimmed_strings(test_vector, 6L),
               c("one_ve", "one_v1", "one_v2"))
  expect_identical(unique_trimmed_strings(test_vector, 20L), test_vector)
})
# list_to_excel ( Internal )
test_that("list_to_excel works!", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "cars.xlsx")
  test_file_one <- file.path(test_dir, "cars_one.xlsx")
  test_file_two <- file.path(test_dir, "cars_two.xlsx")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_excel(df_list, dir = test_dir, file_name = "cars")
  expect_true(file.exists(test_file))
  list_to_excel(df_list,
                dir = test_dir,
                file_name = "cars",
                separate = TRUE)
  expect_true(file.exists(test_file_one))
  expect_true(file.exists(test_file_two))
  one <- readxl::read_excel(test_file_one)
  two <- readxl::read_excel(test_file_two)
  checkmate::expect_data_frame(one, nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_identical(colnames(one), colnames(mtcars))
  checkmate::expect_data_frame(two, nrows = nrow(cars), ncols = ncol(cars))
  expect_identical(colnames(two), colnames(cars))
})
# list_to_csv ( Internal )
test_that("list_to_csv works!", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "cars.csv")
  test_file_one <- file.path(test_dir, "cars_one.csv")
  test_file_two <- file.path(test_dir, "cars_two.csv")
  expect_false(file.exists(test_file))
  df_list <- list(one = mtcars, two = cars)
  list_to_csv(df_list, dir = test_dir, file_name = "cars")
  expect_true(file.exists(test_file_one))
  expect_true(file.exists(test_file_two))
  one <- read.csv(test_file_one)
  two <- read.csv(test_file_two)
  checkmate::expect_data_frame(one, nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_identical(colnames(one), colnames(mtcars))
  checkmate::expect_data_frame(two, nrows = nrow(cars), ncols = ncol(cars))
  expect_identical(colnames(two), colnames(cars))
})
# save_wb ( Internal )
test_that("save_wb works!", {
})
# save_csv ( Internal )
test_that("save_csv works!", {
  test_dir <- withr::local_tempdir() |> sanitize_path()
  expect_true(file.exists(test_dir))
  test_file <- file.path(test_dir, "cars.csv")
  expect_false(file.exists(test_file))
  save_csv(
    form = mtcars,
    dir = test_dir,
    file_name = "cars",
    overwrite = TRUE
  )
  expect_true(file.exists(test_file))
  test_saved_csv <- read.csv(test_file)
  checkmate::expect_data_frame(test_saved_csv,
                               nrows = nrow(mtcars),
                               ncols = ncol(mtcars))
  expect_message(
    save_csv(
      form = cars,
      dir = test_dir,
      file_name = "cars",
      overwrite = FALSE
    ),
    "Already a file"
  )
  test_saved_csv <- read.csv(test_file)
  checkmate::expect_data_frame(test_saved_csv,
                               nrows = nrow(mtcars),
                               ncols = ncol(mtcars))
})
# which_duplicated ( Internal )
test_that("which_duplicated works!", {
})
# is_consecutive_srt_1 ( Internal )
test_that("is_consecutive_srt_1 works!", {
  expect_true(is_consecutive_srt_1(1L))
  expect_true(is_consecutive_srt_1(1L:4L))
  expect_true(is_consecutive_srt_1(c(1L, 2L, 3L)))
  expect_false(is_consecutive_srt_1(c(2L, 3L)))    # doesn't start at 1
  expect_false(is_consecutive_srt_1(c(1L, 3L)))    # gap present
  expect_false(is_consecutive_srt_1(c(1L, 2L, 4L))) # non-consecutive later
})
# remove_html_tags ( Internal )
test_that("remove_html_tags works!", {
  # basic tag removal
  expect_identical(remove_html_tags("<b>bold</b>"), "bold")
  expect_identical(remove_html_tags("no tags"), "no tags")
  # vectorized input
  expect_identical(
    remove_html_tags(c("<p>one</p>", "<div>two</div>")),
    c("one", "two")
  )
  # tags with attributes and surrounding text
  expect_identical(remove_html_tags("<a href='x'>link</a> and text"),
               "link and text")
  # nested tags
  expect_identical(remove_html_tags("<div><span>nested</span></div>"), "nested")
  # empty string preserved, NA preserved
  expect_identical(remove_html_tags(""), "")
  expect_true(is.na(remove_html_tags(NA_character_)))
  # tags with newlines preserved inside text
  expect_identical(remove_html_tags("<p>line\nbreak</p>"), "line\nbreak")
})
# object_size ( Internal )
test_that("object_size works!", {
})
# file_size ( Internal )
test_that("file_size works!", {
})
# drop_if ( Internal )
test_that("drop_if works!", {
  abc <- c("a", "b", "c")
  expect_identical(drop_if(abc, "b"), c("a", "c"))
  expect_identical(drop_if(c(1L, 2L, 3L, 4L), c(2L, 4L)), c(1L, 3L))
  expect_identical(drop_if(c("a", "b"), character(0L)), c("a", "b"))
  expect_identical(drop_if(c("a", "b"), c("a", "b")), character(0L))
  expect_identical(drop_if(character(0L), "a"), character(0L))
  expect_identical(drop_if(factor(abc), "b"), factor(c("a", "c"), levels = abc))
})
# clean_env_names ( Internal )
test_that("clean_env_names works!", {
  # valid names preserved
  expect_identical(clean_env_names(c("one", "two")), c("one", "two"))
  # invalid characters are cleaned and lowercased
  expect_identical(
    clean_env_names(c("My Name", "Another-Name")),
    c("my_name", "anothername")
  )
  # duplicates produce unique cleaned names
  out <- clean_env_names(c("Dup Name", "Dup-Name", "dup_name"))
  expect_length(unique(out), length(out))
  # all results should be valid environment names
  expect_true(all(vapply(out, is_env_name, logical(1L))))
  # empty input returns empty character vector
  expect_identical(clean_env_names(character(0L)), character(0L))
})
# is_df_list ( Internal )
test_that("is_df_list works!", {
  # non-list inputs
  expect_false(is_df_list(NULL))
  expect_false(is_df_list(1L))
  expect_false(is_df_list(data.frame()))
  # empty list -> FALSE
  expect_false(is_df_list(list()))
  # list with no data.frames -> FALSE
  expect_false(is_df_list(list(a = 1L, b = "x")))
  # pure list of data.frames -> TRUE
  df1 <- mtcars
  df2 <- iris
  expect_true(is_df_list(list(df1, df2)))
  expect_true(is_df_list(list(one = df1, two = df2)))
  # mixed list -> TRUE when strict = FALSE, FALSE when strict = TRUE
  mixed <- list(df1, list())
  # expect_true(is_df_list(mixed))
  expect_false(is_df_list(mixed, strict = TRUE))
  mixed2 <- list(df1, 1L)
  expect_true(is_df_list(mixed2))
  expect_false(is_df_list(mixed2, strict = TRUE))
})
# check_match ( Internal )
test_that("check_match works!", {
  # numeric vectors in different orders -> match
  expect_true(check_match(list(
    c(1L, 2L, 3L),
    c(3L, 2L, 1L),
    c(2L, 1L, 3L)
  )))
  # character vectors in different orders -> match
  expect_true(check_match(list(
    c("a", "b", "c"),
    c("c", "b", "a")
  )))
  # differing contents -> no match
  expect_false(check_match(list(
    c("a", "b"),
    c("a", "c")
  )))
  # different lengths -> no match
  expect_false(check_match(list(
    c(1L, 2L),
    c(1L, 2L, 3L)
  )))
  # empty vectors match
  expect_true(check_match(list(
    integer(0L),
    integer(0L)
  )))
})
# is_env_name ( Internal )
test_that("is_env_name works!", {
  # valid names
  expect_true(is_env_name("one"))
  expect_true(is_env_name("A"))
  expect_true(is_env_name("a1_b2"))
  # invalid inputs
  expect_false(is_env_name(NULL))
  expect_false(is_env_name(""))
  expect_false(is_env_name("1start"))    # starts with digit
  expect_false(is_env_name("bad-char!")) # invalid character
  # expect_false(is_env_name("_start"))    # starts with underscore
  # messaging behavior
  expect_message(is_env_name("1start", silent = FALSE),
                 "Short name cannot start with a number")
  expect_silent(is_env_name("1start", silent = TRUE))
})
# is_nested_list ( Internal )
test_that("is_nested_list works!", {
  # non-list inputs -> FALSE
  expect_false(is_nested_list(NULL))
  expect_false(is_nested_list(1L))
  expect_false(is_nested_list("a"))
  # data.frame -> FALSE even though data.frames are lists
  expect_false(is_nested_list(data.frame()))
  # empty list -> TRUE (treated as nested)
  expect_true(is_nested_list(list()))
  # simple flat list -> FALSE
  expect_false(is_nested_list(list(a = 1L, b = "x", c = TRUE)))
  # nested list -> TRUE
  # expect_true(is_nested_list(list(list(1))))
  # expect_true(is_nested_list(list(a = list(b = 1))))
  expect_true(is_nested_list(list(a = list(), b = 1L)))
})
# generate_hex ( Internal )
test_that("generate_hex works!", {
 expect_true(is_hexadecimal(generate_hex(10L)))
})
