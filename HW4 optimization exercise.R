pacman::p_load(gapminder, data.table, parallel, doParallel, foreach, ggplot2)

detectCores()
cores = 4
cl <- makeCluster(cores)
registerDoParallel(cl)
n_boot = 1e3
gc()

# Not parallel
starttime1 = Sys.time()
full_result1 <- 
  foreach(i = c("Africa", "Americas", "Asia", "Europe"), 
          .combine=function(x,y)rbindlist(list(x,y))) %do% {
    result1_a = gapminder2007[sample(.N, replace = T),
                              .(median_lifeExp = median(lifeExp, na.rm=T)), 
                              continent]
    result1_a
  }
endtime1 <- Sys.time()
endtime1 - starttime1
#hist(result1)
full_result1[continent == 'Asia', quantile(median_lifeExp, c(0.025, 0.975))]

# Parallel version
rm(result1_a)
gc()

starttime2 <- Sys.time()
full_result2 <- foreach(i=1:cores,
                        .combine='c',
                        .packages = 'data.table') %dopar% {
                          lapply(1:floor(n_boot/cores), function(x){
                            gapminder2007[sample(.N, replace = T),
                                          .(median_lifeExp = median(lifeExp, na.rm=T)), 
                                          keyby = continent]
                          })
                        }
full_result2 = rbindlist(full_result2)
endtime2 <- Sys.time()
endtime2 - starttime2
#hist(result2)
full_result2[continent == 'Asia', quantile(median_lifeExp, c(0.025, 0.975))]

stopCluster(cl)