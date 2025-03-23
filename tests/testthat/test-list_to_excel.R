# Test for list_to_excel() from RosyUtils

# Note: list_to_excel() treats all variables as character. All columns are
#  converted to character class before comparing actual vs. expected. .rdata
#  files are typically used to store character class data.


test_that("list_to_excel() creates Excel file", {

  # Required packages
  library(openxlsx) # For reading reports

  # Create a temporary directory and file name
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "testReport.xlsx")

  # Create sample list of data frames
  df1 <- data.frame(
    age = c(34, 7, 101),
    full_name = c("Anita Break", "Gene Poole", "Gus Undheit")
  )
  df2 <- data.frame(
    diagnosis = c("Chronic Fatigue", "Huntington's Disease", "Rhinitis"),
    first_visit_year = c(2005, 2018, 1967)
  )
  test_list <- list(Sheet1 = df1, Sheet2 = df2)

  # Export as Excel file
  list_to_excel(
    test_list,
    dir = temp_dir,
    file_name = "testReport"
    )

  # Verify report is created
  expect_true(file.exists(temp_file))

  # Check if the file contains the expected sheets
  output_sheets <- openxlsx::getSheetNames(temp_file)
  expect_equal(sort(output_sheets), sort(names(test_list)))

  # Check if the data in the file matches the input data
  df1_read <- openxlsx::read.xlsx(temp_file, sheet = "Sheet1")
  df2_read <- openxlsx::read.xlsx(temp_file, sheet = "Sheet2")

  # Convert df1 and df2 to character type because the Excel columns are read
  #   as character type
  df1_char <- df1
  df1_char$age <- as.character(df1_char$age)

  df2_char <- df2
  df2_char$first_visit_year <- as.character(df2_char$first_visit_year)

  # Compare the read data to expected data.frames with character vectors
  expect_equal(df1_read, df1_char)
  expect_equal(df2_read, df2_char)

  unlink(temp_file)
})


# Test creating separate Excel files
test_that("list_to_excel() creates multiple Excel files", {

  # Required packages
  library(openxlsx) # For reading reports

  # Create temporary directory and file names
  temp_dir <- tempdir()
  temp_files <- file.path(
    temp_dir,
    c("testReports_Sheet1.xlsx", "testReports_Sheet2.xlsx")
  )

  # Export as Excel files with separate = TRUE
  list_to_excel(
    test_list,
    dir = temp_dir,
    file_name = "testReports",
    separate = TRUE
  )

  # Check if Excel files exist
  expect_true(all(file.exists(temp_files)))

  # Verify that files can be read
  wb_read_1 <- openxlsx::read.xlsx(temp_files[1])
  wb_read_2 <- openxlsx::read.xlsx(temp_files[2])

  # Convert original list's data.frame to character class only
  test_list_char <- lapply(test_list, function(df) {
    as.data.frame(lapply(df, as.character))
  })

  # Compare original data frames with imported sheets
  expect_equal(wb_read_1, test_list_char[[1]])
  expect_equal(wb_read_2, test_list_char[[2]])

})



