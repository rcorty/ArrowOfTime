test_df <- data.frame(t = 1:6)
test_df$x <- 1.3^test_df$t

# a very simple way to predict is to average all past observations
test_that(
  desc = 'works with base::mean',
  {
    expect_equal(
      object = predict_the_future(f = function(t, x) base::mean(x),
                                  data = test_df),
      expected = c(1.3, 1.495, 1.729, 2.010775, 2.351206, 2.7638065)
    )

    # it should be OK to get the order of argumements "wrong"
    expect_equal(
      object = predict_the_future(f = function(x, t) base::mean(x),
                                  data = test_df),
      expected = c(1.3, 1.495, 1.729, 2.010775, 2.351206, 2.7638065)
    )
  }
)

test_that(
  desc = 'works with stats::lm',
  {

    lm_predictor <- function(t, x) {
      predict(object = stats::lm(x ~ t),
              newdata = data.frame(t = max(t) + 1))
    }

    expect_equal(
      object = predict_the_future(f = lm_predictor,
                                  data = test_df,
                                  start_at_nth = 2),
      expected = c(NA, 2.08, 2.626, 3.3046, 4.148794, 5.2))
  }
)
