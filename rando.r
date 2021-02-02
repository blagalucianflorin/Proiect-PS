rando <- function (n)
{
    tried <- 0
    
    while (tried != 3)
    {
        curr_time   <- Sys.time ()
        t1          <- strtoi (gsub (":", "", format (curr_time, "%X")), base = 10L)
        result      <- t1 %% 17
        hour        <- strtoi (strsplit (format (curr_time, "%X"), ":")[[1]][1],  base = 10L)
        minutes     <- strtoi (strsplit (format (curr_time, "%X"), ":")[[1]][2],  base = 10L)
        seconds     <- strtoi (strsplit (format (curr_time, "%X"), ":")[[1]][3],  base = 10L)
        
        if (result == 0)
        {
            x1 <- rnorm (n = 1, mean = minutes, sd = seconds); break
        }
        
        if (result == 3)
        {
            y1 <- runif (n = 1, min = -1, max = 1)
            x1 <- rpois (n = 1, lambda = minutes) + y1; break
        }
        
        if (result == 5)
        {
            x1 <- rexp (n = 1, rate = hour); break
        }
        
        if (result == 7)
        {
            y1 <- runif (n = 1, min = 0, max = 5)
            x1 <- rbinom (n = 1, size = hour, prob = 1 / minutes) + y1; break
        }
        
        if (result == 8)
        {
            x1 <- runif (n = 1, min = -5, max = 7); break
        }
        
        if (result == 11)
        {
            y1 <- rhyper (nn = 1, m = (hour + 1), n = (minutes + 1), k = ((hour + 1) + (minutes + 1)) / 2)
            x1 <- rgamma (n = 1, shape = (hour + 1), scale = 1:(minutes + 2)) + y1; break
        }
        tried = tried + 1
        # Sys.sleep(1)
    }
    
    if (tried == 3)
    {
        x1 <- rnorm (n = 1, mean = 2, sd = 1)
    }
    
    ret_vec <- x1
    last_x  = x1
    a       <- rexp (n = 1, rate = 5)
    b       <- rnorm (n = 1, mean = 2, sd = 1)
    for (i in 2:n)
    {
        next_x <- a * last_x + b
        ret_vec <- c (ret_vec, next_x)
        last_x <- next_x
    }
    
    return (ret_vec)
}