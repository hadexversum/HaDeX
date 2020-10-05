#' Functions to help with checks
#' 
#' 
#' assert_start_end
#' This function asserts values of parameters. Start (of the sequence) should be 
#' bigger than 0 and smaller than end of the sequence. The end of the sequence 
#' should be bigger than start.

assert_start_end <- function(start, 
                             end){
  
  assert_number(start, lower = 0, upper = end)
  assert_number(end, lower = start)
  
}

#' assert_time
#' This function asserts that time_0 is bigger than 0 and smaller than time_100 
#' and time_100 is bigger than time_0.

assert_time <- function(time_0,
                        time_100) {
  
  assert_number(time_0, lower = 0, upper = time_100)
  assert_number(time_100, lower = time_0)
  
}

assert_time_t <- function(time_0,
                          time_t, 
                          time_100){
  
  assert_number(time_0, lower = 0, upper = time_100)
  assert_number(time_t, lower = time_0, upper = time_100)
  assert_number(time_100, lower = time_0)
  
}

#' for confidence_limit and deut_part

assert_0_1 <- function(x){
  
  assert_number(x, lower = 0, upper = 1)
  
}

