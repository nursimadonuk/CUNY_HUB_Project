ratings <- sample(seq(1.1, 5, 0.1), 4600, replace = T)
ratings
mean(ratings)
max(ratings)

email <- c()

i <- 1

while(i <= length(profs$facultyId)) {
  email <- c(email, 
             paste(tolower(substring(profs$facultyFirstName[i], 1, 2)),
                   tolower(profs$facultyLastName[i]), "@email.cuny.edu", sep = ""))
  i <- i + 1
}
email

startyear <- sample(1990:2020, 4600, replace = T)

faculty <- data.frame(factultyId = profs$facultyId,
                      facultyLast = profs$facultyLastName,
                      facultyFirst = profs$facultyFirstName,
                      facultyRating = ratings,
                      facultyEmail = email,
                      facultyStart = startyear)
write.csv(faculty, "faculty.csv")

facId <- c()
depId <- c()
yrs <- c()

for(j in faculty$factultyId) {
  x = sample(1:6, 1)
  y = sample(1:838, x)
  m = faculty$facultyStart[j]
  z = sample(m:2020, x, replace = T)
  yrs <- c(yrs, z)
  depId <- c(depId, y)
  for(k in 1:x) {
    facId <- c(facId, j)
  }
}
facId
max(facId)
max(depId)
min(yrs)
yrs
mean(yrs)

i <- 1
while(i <= length(yrs)) {
  if(yrs[i] < 1990) {
    yrs[i] = 2020
  }
  i <- i + 1
}


for(f in yrs) {
  if(f < 1990) {
    f = 2020
  }
}

m = faculty$facultyStart[9]
z = sample(m:2020, x, replace = T)
z


depsTaught <- data.frame(facultyId = facId,
                        departmentId = depId,
                        year = yrs)


depsTaught

write.csv(depsTaught, "depsTaught.csv")

