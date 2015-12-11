library('testthat')

source('sample.R')
source('tests/test_functions.R')
source('models/Model_specifications.R')
#source("test.1.R")

test_dir('tests', reporter='Summary')

