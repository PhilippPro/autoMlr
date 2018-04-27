require(R.utils)

## function that can take a long time
fn1 <- function(x)
{
  for (i in 1:x^x)
  {
    rep(x, 1000)
  }
  return("finished")
}

## test timeout
evalWithTimeout(fn1(3), timeout = 1, onTimeout = "error") # should be fine
evalWithTimeout(fn1(8), timeout = 1, onTimeout = "error") # should timeout


try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf)
{
  y <- try({setTimeLimit(cpu, elapsed); expr}, silent = TRUE) 
  if(inherits(y, "try-error")) NULL else y 
}

try_with_time_limit(sqrt(1:10), 1)                   #value returns as normal
try_with_time_limit(for(i in 1:1e7) sqrt(1:10), 1)   #returns NULL

# ist das auch anwendbar für RF, etc.?

library(ranger)
mod <- evalWithTimeout(ranger(Species ~ ., data = iris, num.trees = 300000), timeout = 2, onTimeout = "error")
mod = NULL

mod <- try_with_time_limit(ranger(Species ~ ., data = iris, num.trees = 300000), 100)

# scheint für ranger zu gehen.
# 

# with mcparallel?
# http://blog.revolutionanalytics.com/2014/10/r-in-production-controlling-runtime.html

eval_fork <- function(..., timeout = 60) {
  myfork <- parallel::mcparallel({
    eval(...)
  }, silent = FALSE)
  
  # wait max n seconds for a result.
  myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
  # kill fork after collect has returned
  tools::pskill(myfork$pid, tools::SIGKILL)
  tools::pskill(-1 * myfork$pid, tools::SIGKILL)
  
  # clean up:
  parallel::mccollect(myfork, wait = FALSE)
  # timeout?
  if (is.null(myresult))
    stop("reached elapsed time limit")
  
  # move this to distinguish between timeout and NULL returns
  myresult <- myresult[[1]]
  
  # send the buffered response
  return(myresult)
}
system.time(mod <- eval_fork(ranger(Species ~ ., data = iris, num.trees = 500000), timeout = 3))
mod
mod = NULL
## Error in eval_fork(nnetar(as.ts(taylor)), timeout = 4): reached elapsed time limit

# geht einigermaßen, aber nur auf Linux :)

## Timing stopped at: 0.005 0.023 4.032