library(roadR)
library(data.table)

test_that("Visual distress percentage", {

  df <- data.frame(section_id = c(1,1,2,3,4,4),
                   loc_from = c(0, 80, 0, 0, 0, 120),
                   loc_to = c(80, 150, 120, 150, 100, 220),
                   width = c(6.5, 5, 3.7, 8, 9, 10),
                   croc_crax = c(20, 0, 33, 444, 2, 25))

  dt <- as.data.table(df)

  pcts <- rr_get_distress_pct(df, "croc_crax", "loc_from", "loc_to")

  expect_equal(pcts[1], 25)
  expect_equal(pcts[2], 0)
  expect_equal(pcts[3], 27.5)
  expect_equal(pcts[4], 296)   # greater than 100 is possible!
  expect_equal(pcts[5], 2)
  expect_equal(pcts[6], 25)


  pcts <- rr_get_distress_pct(df, "croc_crax", "loc_from", "loc_to",
                              "width", area_based = TRUE)

  expect_equal(round(pcts[1],3), 3.846)
  expect_equal(round(pcts[2],3), 0)
  expect_equal(round(pcts[3],3), 7.432)
  expect_equal(round(pcts[4],3), 37)   # greater than 100 is possible!
  expect_equal(round(pcts[5],3), 0.222)
  expect_equal(round(pcts[6],3), 2.5)



})

test_that("Visual pothole percentage", {

  df <- data.frame(section_id = c(1,1,2,3,4,4),
                   loc_from = c(0, 80, 0, 0, 0, 120),
                   loc_to = c(80, 150, 120, 150, 100, 220),
                   width = c(6.5, 5, 3.7, 8, 9, 10),
                   potholes = c(20, 0, 33, 444, 2, 25))

  dt <- as.data.table(df)

  pcts <- rr_get_pothole_pct(df, "potholes", 0.25, "loc_from", "loc_to",
                              "width")

  expect_equal(round(pcts[1],3), 0.962)
  expect_equal(round(pcts[2],3), 0)
  expect_equal(round(pcts[3],3), 1.858)
  expect_equal(round(pcts[4],3), 9.250)   # greater than 100 is possible!
  expect_equal(round(pcts[5],3), 0.056)
  expect_equal(round(pcts[6],3), 0.625)

})
