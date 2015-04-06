# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

data(ChickWeight)
ChickWeight$time <- ChickWeight$Time
ChickWeight$Time <- as.factor(ChickWeight$time)
fixed <- lm(weight ~ Time * Diet, data = ChickWeight)
random <- aov(weight ~ Diet + Time + Error(Diet:Time), data = ChickWeight)