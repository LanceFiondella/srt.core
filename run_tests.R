library('testthat')

source('sample.R')
source('tests/test_functions.R')

test_dir('tests', reporter='Summary')

