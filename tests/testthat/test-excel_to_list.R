# Tests for excel_to_list()

# Note: We use .rdata files to store class information. The list_to_excel() will
#  export all columns as 'text' class, which are imported as 'character' class,
#  so we will convert the expected objects' columns to character class.

test_that("Excel elements match list's", {

  # Create temp directory and temp file
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "mtcarsReport.xlsx")

  # Export mtcars list of one df named "sheet_name"
  mtcars_list <- list(sheet_name = mtcars)
  REDCapSync:::list_to_excel(
    mtcars_list,
    dir = temp_dir,
    file_name = "mtcarsReport"
  )

  # Import mtcars list from temp file
  mtcars_import <- REDCapSync:::excel_to_list(temp_file)

  # Convert expected list df columns to character class
  mtcars_char_df <- as.data.frame(
    lapply(
      mtcars,
      as.character
    ),
    stringsAsFactors = FALSE)
  mtcars_list_char <- list(sheet_name = mtcars_char_df)

  # Test that imported list matches exported list containing character vectors
  expect_equal(mtcars_import, mtcars_list_char)

  # Test that number of rows and columns match
  expect_equal(
    nrow(mtcars_import$sheet_name),
    nrow(mtcars_list_char$sheet_name)
  )
  expect_equal(
    ncol(mtcars_import$sheet_name),
    ncol(mtcars_list_char$sheet_name)
  )

  # Test that column names match
  expect_equal(
    names(mtcars_import$sheet_name),
    names(mtcars_list_char$sheet_name)
  )

})


test_that("Lists with multiple elements import correctly", {

  # Set up temp directory and temp file 2
  temp_dir <- tempdir()
  temp_file_2 <- file.path(temp_dir, "testReport.xlsx")

  # Export list with multiple data.frames (sheets)
  test_list <- list(
    mtcars_sheet = mtcars,
    iris_sheet = iris,
    tooth_sheet = ToothGrowth
  )
  REDCapSync:::list_to_excel(
    test_list,
    dir = temp_dir,
    file_name = "testReport"
  )

  # Read in test report Excel file
  test_list_import <- REDCapSync:::excel_to_list(temp_file_2)

  # Convert each vector in each data.frame to character class
  expected_list <- lapply(test_list, function(df) {
    as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  })

  # Test that data.frame (sheet) names match
  expect_equal(names(test_list_import), names(expected_list))

  # Compare each imported sheet with respective exported data.frame
  for (sheet_name in names(expected_list)) {
    imported_df <- imported_list[[sheet_name]]
    expected_df <- expected_list[[sheet_name]]

    # Check number of rows and columns match
    expect_equal(nrow(imported_df), nrow(expected_df),
                 info = paste("Row mismatch in sheet:", sheet_name))
    expect_equal(ncol(imported_df), ncol(expected_df),
                 info = paste("Column mismatch in sheet:", sheet_name))

    # Check column names match
    expect_equal(names(imported_df), names(expected_df),
                 info = paste("Column names mismatch in sheet:", sheet_name))

    # Check content matches
    expect_equal(imported_df, expected_df,
                 info = paste("Data mismatch in sheet:", sheet_name))
  }

})
