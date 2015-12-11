test_that("test stuff",{
expect_that(is_valid_input(JM_input),is_true()) 
expect_that(is_valid_input(GM_input),is_true())
expect_that(is_valid_input(GO_input),is_true())
expect_that(is_valid_input(DSS_input),is_true())
expect_that(is_valid_input(Wei_input),is_true())

expect_that(is_valid_string(JM_methods),is_true())



#expect_that(is_valid_result())


expect_that(1 ^ 1, equals(1))
#expect_that(2 ^ 2, equals(2))
if(1 == 1){
quit("no",status = -1)
}
 
expect_that(2 + 2 == 4, is_true())
expect_that(2 == 1, is_false())
 
expect_that(1, is_a('numeric'))
 

expect_that(print('Hello World!'), prints_text('Hello World!'))
 
expect_that(log('a'), throws_error())
 
expect_that(factorial(16), takes_less_than(1))	

expect_that(factorial(145), takes_less_than(1))
})
