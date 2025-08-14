# adds up number of birds and dogs
#defined function
bird_dog_sum <- function(bird, dog){
  pets <- bird + dog
  return(pets) #allows us to use the value pets
}

# use function
total_pets <- bird_dog_sum(bird = 2, dog=5 ) #order is defined in the function arguments place
# storing total pets as a variable

# create a function to double values

times2 <- function(x){
print(2*x)
}

times2(x = 5)

quarter_splits <- c(1, 1.1, 1.2, 1.3, 1.4)
for(i in 1:length(quarter_splits)){
  total <- quarter_splits[i] + quarter_splits[i+1]
  print(total)
}

# write a function with conditionals
animal_age <- function(animal, age){
if(animal == "dog"){
  print(age*7)
}else if(animal == "goat"){
  print(age*4.7)
}
}

animal_age(animal = "goat", age = 15)

# try using for a cow 
animal_age("cow", 8) #does nothing

#write an updated version of animal_age function with warning messages
animal_age_stop<- function(animal, age){
  if(!animal %in% c("dog", "goat")){
    stop("haha that's not a real animal")
  }
  if(is.numeric(age) == FALSE){
    stop("age must be a numeric")
  }
  if(age <= 0 | age >50){
    warning("are you sure about this age?")
  }
  if(animal == "dog"){
    print(age*7)
  }else if(animal == "goat"){
    print(age*4.7)
  }
}

animal_age_stop("dog", -1)


#all data frames in the function are called df _> argument df
df_means <- function(df){
for(i in 1:ncol(df)){
if(is.numeric(df[[i]])){
column_name <- colnames(df[i]) # filter by column name
col_mean <- mean(df[[i]]) # pull out the mean of that column
print(paste("The mean value of", column_name, "is", col_mean))
}
}
}

df_means(df = palmerpenguins::penguins)


# logistic growth model

logistic_growth <- function(N0, K, r, t){
  Nt <- K /(1+((K-N0)/N0)*exp(-r*t))
  print(Nt)
}

logistic_growth(N0= 100, K= 6000, r = 0.27, t = 40)


#working on example just dealing with time
time_vec <- seq(from= 0 , to = 35, by = 0.1)

# apply logisitic growth function to vector

pop_35 <- logistic_growth(N0=100, K= 600, r = 0.27, t = time_vec)

pop_time_35 <- data.frame(time_vec, pop_35)

library(tidyverse)
ggplot(pop_time_35, aes(x = time_vec, y = pop_35)) +
  geom_line(size=0.5)

# alternatively with an internal foreloop
#pre-allocate storage for output vector

pop_35_vec <- vector(mode = "numeric", length = length(time_vec))
 #for loop for stepping through time steps

for(i in seq_along(time_vec)){
population <- logistic_growth(N0=100, K= 600, r = 0.27, t = time_vec[i])
pop_35_vec[i] <- population
}

# now, building to estimating across growth rates
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

# creating a matrix to store output r by time vecs
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for(j in seq_along(r_seq)){
for(i in seq_along(time_vec)){
  population <- logistic_growth(N0=100, K= 600, r = r_seq[j], t = time_vec[i])
  out_matrix[i, j] <- population
out_matrix[i, j] <- population
}
}

# adding time as a variable
out_df <- data.frame(out_matrix, time = time_vec)

# update column names for growth rates
colnames(out_df) <- c(paste0("gr_", r_seq), "time")

# that was ugly! lets clean that up by converting it to long form data
out_df_long <- out_df |> pivot_longer(cols = -time,
                                      names_to = "growth_rate", 
                                      values_to = "population")

# plot it!
ggplot(data = out_df_long, aes(x = time, y = population))+
  geom_line(aes(color = growth_rate))+
  theme_minimal()

