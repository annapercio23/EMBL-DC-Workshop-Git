weight_kg <- 55

Weight_Kg <- 45

my_weight_kg <- 60

(my_weight_kg <- 50)
(my_weight_kg <- 60)
my_weight_kg

2.2 * my_weight_kg
weight_kg <- 62
2.2 + my_weight_kg
2.2 * my_weight_kg
my_weight_kg <- 65
2.2 * my_weight_kg

weight_lb <-  2.2 * my_weight_kg
weight_kg <-  100

weight_lb <-  2.2 * weight_kg
# I have to update value of the weight_kg object, be verbose!!!

mass <- 47.5
age <-  122
mass <- mass * 2
age <- age - 20
mass_index <-  mass/age
sqrt(49)
myweight_kg <- sqrt(9) 
round(4675.98675, 2)
my_number <- round(4675.98675, 2)
round(digits = 3, x = 19.9999999)
round(digits = 3, x = 19.56789)

my_function <- function(x){
  x = x + 2
}

my_result <- my_function(3)

weight_kg <- c(40, 50, 60, 70)
mean(weight_kg [1:4])
weigh_g <- c(50, 60, 65, 82)

animals <-  c("duck", "dog", "dolphin")

length(animals)
class(animals)
class(weigh_g)
str(animals)

# add element to vector
animals <- c("cincilla", animals)
animals <-c
animals <- c(animals, "cats")
typeof(animals)

num_char <- c(1, 2 , 3, "a")
class(num_char)
num_logical <- c(1, 2 , 3, TRUE)
class(num_logical)
tricky <- c(1, 2 , 3, "4")
class(tricky)

char_logical <- c("a", "b", "c", TRUE)
class(char_logical)

#subsetting vectors per accedere a value specifici
animals[2]
animals[2: 3]
animals[2:4]
animals[c(2,4)]

more_animals <- animals[c(1, 2, 3, 2, 1, 4)]
#subset con logical values
weigh_g[c(FALSE, FALSE, TRUE, TRUE)]
#usando operazioni logiche
weigh_g > 63
#FALSE FALSE  TRUE  TRUE#
weigh_g[c(FALSE, FALSE,  TRUE, TRUE)]
weigh_g > 50
#FALSE TRUE TRUE  TRUE#
weigh_g[c(FALSE, TRUE,  TRUE, TRUE)]

#combine logiacal op

weigh_g[weigh_g > 63 & weigh_g < 75]
weigh_g[weigh_g < 58 | weigh_g > 75]


#operators < < == equal to sta con 2
# <= e >= 
# != diverso da

animals[1:4]
animals == "cincilla"
animals == "cincilla" | animals == "duck"
animals [animals == "cincilla" | animals == "duck" | animals == "wolf"]


#percentage in percentage per trovare gli 
#elementi corrispondenti al vettore di nostra scelta
animals %in% c("duck", "dog", "cats")



#missing data
heights <- c(2, 4, 4, NA, 6)
mean(heights, na.rm = TRUE)

is.na(heights)
#( ! = non )is.na ci toglie i missing
heights[!is.na(heights)]
na.omit(heights)
na.omit(heights)
complete.cases(heights)
heights[complete.cases(heights)]



heights_no_na <- heights[complete.cases(heights)]

#challenge 2

heights_1 <- c(63, 69,60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63 )
na.omit(heights_1)
complete.cases(heights_1)
heights_1[complete.cases(heights_1)]
heights_1_no_na <-  heights_1[complete.cases(heights_1)]
median(heights_1_no_na)
heights_1_no_na > 67
sum(heights_1_no_na > 67)

#correzione esercizio
heights_2 <- c(63, 69,60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63 )
heights_2_no_na <- heights_2[!is.na(heights_2)]
median(heights_2_no_na)
#oppure
median(heights_2, na.rm = T)
heights_2_no_na[heights_2_no_na > 67]
sum(heights_2_no_na > 67)
#con sum ogni falso è 0 ogni vero è 1
length(heights_2_no_na[heights_2_no_na > 67])
