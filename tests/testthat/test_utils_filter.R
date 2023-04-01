
library(roadR)
library(data.table)

test_that("Filter on Treatment Length", {

  df <- data.frame(section_id = c(1,1,2,3,4,4),
                   loc_from = c(0, 80, 0, 0, 0, 120),
                   loc_to = c(80, 150, 120, 150, 100, 220),
                   lane = c("R1", "L1", "all", "all", "all", "R1"),
                   values = c(1, 2, 3, 4, 5, 6))

  dt <- df # as.data.table(df)

  # Check for treatment length having 'all' lanes - should find 2 rows
  treat_len <- data.frame(tl_id = 1, section_id = 1, loc_from = 75,
                          loc_to = 120, lane = "all")
  rows <- rr_get_tl_data(treat_len, dt)
  expect_equal(nrow(rows), 2)


  # Check for treatment length having 'L1' lane - should find one row
  treat_len <- data.frame(tl_id = 1, section_id = 1, loc_from = 75,
                          loc_to = 120, lane = "L1")

  rows <- rr_get_tl_data(treat_len, dt)
  expect_equal(nrow(rows), 1)

  # Check for treatment length having 'R2' lane (should find no matches)
  treat_len <- data.frame(tl_id = 1, section_id = 1, loc_from = 75,
                          loc_to = 120, lane = "R2")

  rows <- rr_get_tl_data(treat_len, dt)
  expect_equal(nrow(rows), 0)

  # Check for some variations of location
  treat_len <- data.frame(tl_id = 1, section_id = 4, loc_from = 175,
                          loc_to = 200, lane = "all")

  rows <- rr_get_tl_data(treat_len, dt)
  expect_equal(nrow(rows), 1)

  # Check for some variations of location
  treat_len <- data.frame(tl_id = 1, section_id = 2, loc_from = 175,
                          loc_to = 200, lane = "all")

  rows <- rr_get_tl_data(treat_len, dt)
  expect_equal(nrow(rows), 0)

  # Check for some variations of location
  treat_len <- data.frame(tl_id = 1, section_id = 2, loc_from = 75,
                          loc_to = 200, lane = "all")

  rows <- rr_get_tl_data(treat_len, dt)
  expect_equal(nrow(rows), 1)


})


test_that("Filter on Segment with minimum overlap", {

  df <- data.frame(section_id = c(1,1,2,3,4,4),
                   loc_from = c(0, 80, 0, 0, 0, 120),
                   loc_to = c(80, 150, 120, 150, 100, 220),
                   lane = c("R1", "L1", "all", "all", "all", "R1"),
                   values = c(1, 2, 3, 4, 5, 6))

  # Check for treatment length having 'all' lanes - should find 2 rows
  treat_len <- data.frame(tl_id = 1, section_id = 1, loc_from = 75,
                          loc_to = 120, lane = "all")

  row <- rr_get_seg_data_ovlp(treat_len, df, min_overlap = 0.8)

  # row with maximum overlap should be row 2, with locFrom = 80 and locTo = 150
  # thus overlap for segment from 75 to 120 is 80 to 120 = 40 m which is
  # 40 / 45 = 0.888 thus passing the threshold minimum of 0.8
  expect_equal(row[["values"]], 2)

  # No overlap for minimu overlap of 0.9
  row <- rr_get_seg_data_ovlp(treat_len, df, min_overlap = 0.9)
  expect_equal(NULL, row)


})
