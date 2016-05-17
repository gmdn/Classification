library(Matrix)

acq <- read.table(file = "./data/reuters21578.10.training.acq.labels")
acq <- as.numeric(acq$V1)
acq <- acq - 1

corn <- read.table(file = "./data/reuters21578.10.training.corn.labels")
corn <- as.numeric(corn$V1)
corn <- corn - 1

crude <- read.table(file = "./data/reuters21578.10.training.crude.labels")
crude <- as.numeric(crude$V1)
crude <- crude - 1

earn <- read.table(file = "./data/reuters21578.10.training.earn.labels")
earn <- as.numeric(earn$V1)
earn <- earn - 1

grain <- read.table(file = "./data/reuters21578.10.training.grain.labels")
grain <- as.numeric(grain$V1)
grain <- grain - 1

interest <- read.table(file = "./data/reuters21578.10.training.interest.labels")
interest <- as.numeric(interest$V1)
interest <- interest - 1

money <- read.table(file = "./data/reuters21578.10.training.money-fx.labels")
money <- as.numeric(money$V1)
money <- money - 1

ship <- read.table(file = "./data/reuters21578.10.training.ship.labels")
ship <- as.numeric(ship$V1)
ship <- ship - 1

trade <- read.table(file = "./data/reuters21578.10.training.trade.labels")
trade <- as.numeric(trade$V1)
trade <- trade - 1

wheat <- read.table(file = "./data/reuters21578.10.training.wheat.labels")
wheat <- as.numeric(wheat$V1)
wheat <- wheat - 1

labelsTraining <- data.frame(acq, corn, crude, earn, grain,
                             interest, money, ship, trade, wheat)

t <- read.table(file = "./data/reuters21578.10.training.matrix")

datasetTraining <- sparseMatrix(t[,1], t[,2], x=t[, 3] <- 1)

acq <- read.table(file = "./data/reuters21578.10.test.acq.labels")
acq <- as.numeric(acq$V1)
acq <- acq - 1

corn <- read.table(file = "./data/reuters21578.10.test.corn.labels")
corn <- as.numeric(corn$V1)
corn <- corn - 1

crude <- read.table(file = "./data/reuters21578.10.test.crude.labels")
crude <- as.numeric(crude$V1)
crude <- crude - 1

earn <- read.table(file = "./data/reuters21578.10.test.earn.labels")
earn <- as.numeric(earn$V1)
earn <- earn - 1

grain <- read.table(file = "./data/reuters21578.10.test.grain.labels")
grain <- as.numeric(grain$V1)
grain <- grain - 1

interest <- read.table(file = "./data/reuters21578.10.test.interest.labels")
interest <- as.numeric(interest$V1)
interest <- interest - 1

money <- read.table(file = "./data/reuters21578.10.test.money-fx.labels")
money <- as.numeric(money$V1)
money <- money - 1

ship <- read.table(file = "./data/reuters21578.10.test.ship.labels")
ship <- as.numeric(ship$V1)
ship <- ship - 1

trade <- read.table(file = "./data/reuters21578.10.test.trade.labels")
trade <- as.numeric(trade$V1)
trade <- trade - 1

wheat <- read.table(file = "./data/reuters21578.10.test.wheat.labels")
wheat <- as.numeric(wheat$V1)
wheat <- wheat - 1

labelsTest <- data.frame(acq, corn, crude, earn, grain,
                         interest, money, ship, trade, wheat)

t <- read.table(file = "./data/reuters21578.10.test.matrix")

datasetTest <- sparseMatrix(t[,1], t[,2], x=t[, 3] <- 1)

rm(list = c("t", "acq", "corn", "crude", "earn", "grain",
            "interest", "money", "ship", "trade", "wheat"))

## set number of (training) objects and features
numberOfObjects <- dim(datasetTraining)[1]
numberOfFeatures <- dim(datasetTraining)[2]