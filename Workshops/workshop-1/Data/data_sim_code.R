# Workshop 1 example datasets

# Aligator bite strength
set.seed(2024)
n <- 78
ali <- data.frame(
  length = rnorm(n, mean = 550, sd = 110)
)
b0 <- 0
b1 <- 3.4
mu <- b0 + b1 * ali$length
ali$bite = rnorm(n, mean = mu, sd = 60)
# Creating a data entry mistake caused by a "missing" decimal point
# Observation 20 is problem sample
mistake <- 20
ali$bite[mistake] <- ali$bite[mistake] * 100
ali$length[mistake] <- ali$length[mistake] * 10
# ggplot(ali) +
#   geom_histogram(aes(x = bite), colour = "white")
ali$length <- round(ali$length, digits = 1)
ali$bite <- round(ali$bite, digits = 2)

# Swift migration
set.seed(2024)
n <- 92
swi <- data.frame(
  d_country = sample(size = n, c("South Africa", "Namibia", "Botswana"), replace = T)
)
nam <- ifelse(swi$d_country == "Namibia", 1, 0)
rsa <- ifelse(swi$d_country == "South Africa", 1, 0)

b0 <- 22000
b1 <- 100
b2 <- -5000

mu <- b0 + b1 * nam + b2 * rsa
swi$migration <- rnorm(n = n, mean = mu, sd = 1250)

swi$migration <- round(swi$migration, digits = 0)
mistake <- sample(x = 1:n, size = 4)
swi$migration[mistake] <- swi$migration[mistake] * 1000

hist(swi$migration)

write.table(swi, "H:/003 - Teaching/006-BI3010/Workshops/Workshop 1 - Hidden assumptions/Data/swift_migration.txt", row.names = FALSE)


setwd("H:/003 - Teaching/006-BI3010/Workshops/Workshop 1 - Hidden assumptions/Data")

read.table("aligator_bite.txt", header = TRUE)
