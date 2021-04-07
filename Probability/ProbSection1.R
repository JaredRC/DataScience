# Section 1.1 - Monte Carlo Simulations
beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 6, replace = TRUE)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)
x <- sample(beads,5)
x[2:5]
# Random Seed
set.seed(1)

# mean Function
mean(beads == "blue")
#The multiplication rule for independent events is:
# Pr(𝐴 and 𝐵 and 𝐶)=Pr(𝐴)×Pr(𝐵)×Pr(𝐶) 
#The multiplication rule for dependent events considers the conditional probability of both events occurring:
#  Pr(𝐴 and 𝐵)=Pr(𝐴)×Pr(𝐵∣𝐴) 
#We can expand the multiplication rule for dependent events to more than 2 events:
#  Pr(𝐴 and 𝐵 and 𝐶)=Pr(𝐴)×Pr(𝐵∣𝐴)×Pr(𝐶∣𝐴 and 𝐵)

# Q1
balls <- rep(c("cyan","magenta","yellow"), times = c(3,5,7))
# create an balls
balls
B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(balls, 1))    # draw 1 ball, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)
