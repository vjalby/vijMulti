# jmvtools::prepare()/install() don't copy DESCRIPTION's Version into jamovi/0000.yaml,
# so both must be bumped by hand on every release.
test_that("DESCRIPTION and jamovi/0000.yaml versions match", {
  descFile <- test_path("..", "..", "DESCRIPTION")
  yamlFile <- test_path("..", "..", "jamovi", "0000.yaml")
  skip_if_not(file.exists(descFile) && file.exists(yamlFile), "source tree not available")

  descVersion <- read.dcf(descFile, fields = "Version")[[1]]
  yamlLine <- grep("^version:", readLines(yamlFile), value = TRUE)
  yamlVersion <- trimws(gsub("['\"]", "", sub("^version:", "", yamlLine)))

  expect_identical(yamlVersion, descVersion)
})
