# Для встроенных в R данных AirPassengers рассчитайте 
# скользящее среднее с интервалом сглаживания равным 10. 
# Напечатайте получившийся результат 
# (первым значением в выводе должно быть среднее для элементов 1:10, 
#  во втором значении - среднее для элементов 2:11 и т.д., в последнем  - 
# среднее для элементов 135 :144)
moving_average <- function(df, interval) {
  (cumsum(df)[interval:length(df)] - c(0, cumsum(df)[1:(length(df) - interval)]))/interval
}

df <- as.vector(AirPassengers)
interval <- 10
moving_average(df, interval)
