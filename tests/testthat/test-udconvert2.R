context("Unit conversion")

test_that("Unit conversion works as expected", {
  x <- 10
  ytrue <- x * biogas::molMass("C") / biogas::molMass("CO2")
  y <- ud_convert2(x, "kg [CO2]", "kg [C]")
  expect_identical(y, ytrue)
  
  
  # Make sure that N2O conversion works as expected. 
   conversion_table <- read.csv(system.file('variable-conversion.csv', package = 'hector.rcmip'), 
                                stringsAsFactors = FALSE)
   n2o_entry <- conversion_table[conversion_table$hector_variable == " N2O_emissions", ]
   
   ztrue <- x * (biogas::molMass("N2O") / (biogas::molMass("N") * 2))
   # Divide by 100 to force back to arbitrary units 
   z     <- ud_convert2(x, from = n2o_entry$hector_udunits, to = n2o_entry$rcmip_udunits)/1000
   expect_equal(z, ztrue)
   
})
