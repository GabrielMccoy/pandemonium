## code to prepare `Bikes` dataset goes here
Bikes <- list()
df <- read.csv("data-raw/bikes_w_activations.csv")

Bikes$space1 <- df[c("A1","A2","A3","A4","A5","A6","A7","A8")]
Bikes$space2 <- df[c("yr","temp","weathersit","atemp","hum","windspeed")]
Bikes$other  <- df[c("cnt","pred","cnt_scaled","res")]

usethis::use_data(Bikes, overwrite = TRUE)
