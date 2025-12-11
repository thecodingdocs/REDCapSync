TEST_CLASSIC_denormalized <- read.csv("data-raw/TEST_CLASSIC_denormalized.csv")
saveRDS(
  TEST_CLASSIC_denormalized,
  "tests/testthat/fixtures/TEST_CLASSIC_denormalized.rds"
)
TEST_CLASSIC <- load_project("TEST_CLASSIC")$.internal()
rcon_list <- rcon_result(TEST_CLASSIC)
rcon_list

# TEST_CLASSIC_denormalized <-
#   readRDS(test_path("fixtures", "TEST_CLASSIC_denormalized.rds"))
