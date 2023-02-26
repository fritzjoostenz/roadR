library(roadR)
library(data.table)

test_that("Clean Deficit Set", {

  df <- data.frame(section_id = c(1,2,3,4),
                   loc_from = c(0, 30, 100, 150),
                   loc_to = c(10, 60, 150, 150),
                   deficit = runif(4, 0, 10),
                   data_code = rep("test", 4)
                   )


  # Clean the set by adjusting observations too long or too short
  cleaned <- rr_Clean_deficit_set(df)

  # First row should be unchanged
  expect_equal(cleaned$loc_from[1], 0)
  expect_equal(cleaned$loc_to[1], 10)

  # Second row should be unchanged
  expect_equal(cleaned$loc_from[2], 30)
  expect_equal(cleaned$loc_to[2], 60)

  # Third row was too long, should be shortened to 30m around the mioddle
  expect_equal(cleaned$loc_from[3], 110)
  expect_equal(cleaned$loc_to[3], 140)

  # Fourth row was zero length, should have a length of 1 m now
  expect_equal(cleaned$loc_from[4], 150)
  expect_equal(cleaned$loc_to[4], 151)


})
