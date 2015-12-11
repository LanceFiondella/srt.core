library('testthat')

source('sample.R')
source('tests/needed.R')
source('models/Model_specifications.R')
#source("test.1.R")

test_dir('tests', reporter='Summary')

