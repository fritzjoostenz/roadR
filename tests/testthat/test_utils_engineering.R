

library(roadR)

test_that("Overlap between Segments", {

  start1 <- 1
  end1 <- 4
  start2 <- 2
  end2 <- 6
  ovlp <- rr_overlap_length(start1, end1, start2, end2)
  expect_equal(ovlp, 2)

  start1 <- 1
  end1 <- 4
  start2 <- 2
  end2 <- 3.5
  ovlp <- rr_overlap_length(start1, end1, start2, end2)
  expect_equal(ovlp, 1.5)


  start1 <- 1
  end1 <- 4
  start2 <- 4
  end2 <- 9
  ovlp <- rr_overlap_length(start1, end1, start2, end2)
  expect_equal(ovlp, 0)

  start1 <- 1.3
  end1 <- 4.6
  start2 <- 4.2
  end2 <- 9.7
  ovlp <- rr_overlap_length(start1, end1, start2, end2)
  expect_equal(ovlp, 0.4)

  start1 <- 10.5
  end1 <- 20.6
  start2 <- 0.5
  end2 <- 40.5
  ovlp <- rr_overlap_length(start1, end1, start2, end2)
  expect_equal(ovlp, 10.1)

  start1 <- 10
  end1 <- 44
  start2 <- 4
  end2 <- 9
  ovlp <- rr_overlap_length(start1, end1, start2, end2)
  expect_equal(ovlp, 0)

})

test_that("Overlap between Segments - Vectorised", {

  start1 <- 1
  end1 <- 4
  starts <- c(2)
  ends <- c(6)
  ovlps <- rr_overlap_lengths(start1, end1, starts, ends)
  expect_equal(ovlps[1], 2)

  start1 <- 1
  end1 <- 4
  starts <- c(2, 2, 4, -1, -1)
  ends <- c(6, 3.5, 9, 10, 2.2)
  ovlps <- rr_overlap_lengths(start1, end1, starts, ends)
  expect_equal(ovlps[1], 2)
  expect_equal(ovlps[2], 1.5)
  expect_equal(ovlps[3], 0)
  expect_equal(ovlps[4], 3)
  expect_equal(ovlps[5], 1.2)

})
