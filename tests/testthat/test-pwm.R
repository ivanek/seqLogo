test_that("make PWM works", {
  expect_s4_class(makePWM(m), "pwm")
})
