# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

find_course <- function(course){
  file.path(find.package("swirl"), "Courses", gsub(" ", "_", course))
}

# attempt to display a swirl file
display_swirl_file <- function(filename, course, lesson=""){
  fname <- filename
  if(lesson != "")fname <- file.path(lesson, filename)
  loc <- gsub(" ", "_", file.path( find_course(course),  fname))
  toloc <- file.path("swirl_temp", filename)
  if(!file.exists("swirl_temp"))dir.create("swirl_temp")
  file.copy(loc, "swirl_temp", overwrite=TRUE)
  if(isTRUE(1 == grep("*[.]R$", filename))){
    file.edit(toloc, title=filename)
  } else {
    file.show(toloc, title=filename)
  }
  message(paste("(Copied file", filename, "to", file.path(getwd(), toloc), ")."))
}

data(ChickWeight)
ChickWeight$time <- ChickWeight$Time
ChickWeight$Time <- as.factor(ChickWeight$time)
fixed <- lm(weight ~ Time * Diet, data = ChickWeight)
suppressWarnings(random <- aov(weight ~ Diet + Time + Error(Diet:Time), data = ChickWeight))
