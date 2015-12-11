expect_that(1 ^ 1, equals(1))
expect_that(2 ^ 2, equals(2))
if(2^2 == 4){
?stop
}

 
expect_that(2 + 2 == 4, is_true())
expect_that(2 == 1, is_false())
 
expect_that(1, is_a('numeric'))
 
expect_that(print('Hello World!'), prints_text('Hello World!'))
 
expect_that(log('a'), throws_error())
 
expect_that(factorial(16), takes_less_than(1))	

expect_that(factorial(145), takes_less_than(1))
