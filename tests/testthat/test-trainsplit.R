df = data.frame(a = letters, b = 1L:26L)
dt = data.table::as.data.table(df)
tb = tibble::as_tibble(df)

test_that("package splits dataset correctly", {

   split = trainsplit(df, ntrain = 10, seed = 123)
   expect_equal(nrow(split$train), 10)
   expect_equal(names(df), names(split$train))

   split = trainsplit(df, trainpct = .75, seed = 123)
   expect_equal(nrow(split$train), 20)
   expect_equal(names(df), names(split$train))

   split = trainsplit(df, trainpct = .01, seed = 123)
   expect_equal(nrow(split$train), 0)
   expect_equal(names(df), names(split$train))

   split = trainsplit(df, trainpct = .01, seed = 123, round_ntrain = 'ceiling')
   expect_equal(nrow(split$train), 1)
   expect_equal(names(df), names(split$train))

})

test_that("package works with dt and tb", {

   split = trainsplit(dt, trainpct = .75, seed = 123)
   expect_equal(nrow(split$train), 20)
   expect_equal(names(df), names(split$train))

   split = trainsplit(tb, trainpct = .75, seed = 123)
   expect_equal(nrow(split$train), 20)
   expect_equal(names(df), names(split$train))

})



