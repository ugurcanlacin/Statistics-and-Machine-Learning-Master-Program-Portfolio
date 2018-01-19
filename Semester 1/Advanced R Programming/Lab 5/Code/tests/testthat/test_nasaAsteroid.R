context("nasaAsteroid")

test_that("class is correct", {
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_true(class(nasa)[1] == "nasaAsteroid")
})

test_that("Hazardous asteroid is correct", {
     nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  
    expect_output(nasa$hazardousAsteroids(), "Total asteroid is( )*[0-9]*")  
    expect_output(nasa$hazardousAsteroids(), "Total hazardous asteroid number is( )*[0-9]*")
  })

test_that("Wrong input throws an error.", {
  
  expect_error(nasaAsteroid(100) )
  expect_error(nasaAsteroid(TRUE) )
  expect_error(nasaAsteroid(asasa)) 
})
 
test_that("is data frame",{
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_that(is.data.frame(nasa$near_earth_objects),
              is_true())
})

test_that("is getAsteroidsAsDataFrame data frame",{
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_that(nasa$near_earth_objects, equals(nasa$getAsteroidsAsDataFrame()))
})

test_that("residuals output is correct", {
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_output(nasa$residuals(), "Residuals:")  
})

test_that("meanSummary output is correct", {
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_output(nasa$meanSummary(), "absolute_magnitude_h mean for asteroids is( )*[0-9]*")  
  expect_output(nasa$meanSummary(), "estimated_diameter_kilometers_max mean for asteroids is( )*[0-9]*")
  expect_output(nasa$meanSummary(), "estimated_diameter_kilometers_min mean for asteroids is( )*[0-9]*")
})

test_that("medianSummary output is correct", {
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_output(nasa$medianSummary(), "absolute_magnitude_h median for asteroids is( )*[0-9]*")  
  expect_output(nasa$medianSummary(), "estimated_diameter_kilometers_max median for asteroids is( )*[0-9]*")
  expect_output(nasa$medianSummary(), "estimated_diameter_kilometers_min median for asteroids is( )*[0-9]*")
})

test_that("regressionCoefficients output is correct", {
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_output(nasa$regressionCoefficients(), "Regression Coefficients:")  
  expect_output(nasa$regressionCoefficients(), "absolute_magnitude_h estimated_diameter_kilometers_max")
  expect_output(nasa$regressionCoefficients(), "[0-9]*( )*[0-9]*")
})

test_that("fittedValues output is correct", {
  nasa <- nasaAsteroid$new("NWqzvodQTYMM5cyNd5753dOtfqvzvbsQB73o42xB")
  expect_output(nasa$fittedValues(), "Fitted Values:")  
})

