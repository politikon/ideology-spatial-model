## Description: Spatial model in Stan
## Author: Gonzalo Rivero
## Date: 09-Mar-2015 12:55

library(ggplot2); theme_set(theme_bw())
library(rstan)
library(reshape2)

## Read data in
setwd("~/Documents/datablog/spatial/")
spat <- read.csv("dta/DA3050.csv")

## Unfactor the main variable
ideonum <- function(x) {
    levels(x) <- gsub("[^0-9]", "", levels(x))
    x <- as.numeric(as.character(x))
    return(x)
}

#################### Data cleaning ####################
## Ideology - individual 
ideo <- spat$P20
ideo <- ideonum(ideo)

## Ideology - party
pideo <- spat[, c("P2106", "P2103", "P2105", "P2102", "P2101")]
pideo <- sapply(pideo, ideonum)

## Probability of voting
pvoto <- spat[, c("P1106", "P1103", "P1105", "P1102", "P1101")]
pvoto <- sapply(pvoto, ideonum)
preds <- pvoto
for (i in 1:nrow(pvoto)) preds[i, ] <- pvoto[i, ]/sum(pvoto[i, ])

## ## Distance
distance <- (ideo - pideo)

#################### Model ####################

## Remove incomplete cases
keep <- complete.cases(preds)
preds <- preds[keep, ]
distance <- distance[keep, ]
ideo <- ideo[keep]

keep <- complete.cases(distance)
preds <- preds[keep, ]
distance <- distance[keep, ]
ideo <- ideo[keep]

jdata <- list("N"=nrow(distance),
              "K"=ncol(distance),
              "pvoto"=preds,
              "ideo"=abs(distance))

mymodel <- stan(file="src/conjoint.stan", data=jdata, iter=1E4, chains=1)

coefs <- extract(mymodel)

## Location of parties (mean of reported ideology of everyone)
ploc <- colMeans(pideo, na.rm=TRUE)
simul <- Map(function(x) abs(x - ploc), seq(1, 10, by=0.2))

## Recover predicted values from model for a given location
replicate <- function(alpha0, alpha) {
    p <- matrix(NA, ncol=5, nrow=length(simul))
    for (i in 1:length(simul)) {
        for (j in 1:length(coefs$alpha)) {
            xb <- c(0, alpha0) + alpha * simul[[i]] 
            p[i, ] <- exp(xb)/sum(exp(xb))
        }
    }
    return(p)
}

## Summary of the posteriors
alpha <- quantile(coefs$alpha, c(0.025, 0.5, 0.975))
alpha0 <- apply(coefs$free_alpha0, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))


## Apply posteriors to sim data
mypreds <- vector("list", 3)
for (i in 1:3) {
    mypreds[[i]] <- replicate(alpha0[i, ], alpha[i])
}

colc <- rgb(218, 119, 55, maxColorValue = 255)
coliu <- rgb(54, 118, 25, maxColorValue = 255)
colpodemos <- rgb(78, 30, 75, maxColorValue = 255)
colpsoe <- rgb(225, 39, 42, maxColorValue = 255)
colpp <- rgb(57, 142, 207, maxColorValue = 255)

## Reshape
mypreds <- do.call(rbind, mypreds)
p <- data.frame(mypreds)
p$y <- rep(c("ymin", "ymed", "ymax"), each=46)
names(p) <- c("Ciudadanos", "IU", "Podemos", "PSOE", "PP", "y")
probparty <- melt(p)
probparty$ideo <- seq(1, 10, by=0.2)

## Create variable per party
probparty$ygroup <- apply(probparty, 1, function(x) paste(x[["y"]], x[["variable"]]))
probparty$ltype <- as.factor(as.numeric((probparty$y != "ymed")*3))

## Plot predictions from model
p <- ggplot(probparty, aes(x=ideo, y=value, group=factor(ygroup)))
pq <- p + geom_density(stat='identity', aes(colour=factor(variable), linetype=ltype)) +
    labs(title="Partido elegido para cada ideología (Predicciones)",
         x="Ideología",
         y="Probabilidad") +
             scale_y_continuous(limits=c(0, .75)) +
             scale_color_manual(name="Partido",
                                values=c(colc,
                                    coliu,
                                    colpodemos,
                                    colpsoe,
                                    colpp)) +
                                    guides(linetype=FALSE)
ggsave('./img/fract_pvote.png', pq, height=6, width=11)


## Descriptive figures
pdescript <- matrix(NA, nrow=10, ncol=5)
for (i in 1:10) {
    pdescript[i, ] <- colMeans(jdata$pvoto[ideo == i, ], na.rm=TRUE)
}

pdescript <- data.frame(pdescript)
names(pdescript) <- c("Ciudadanos", "IU", "Podemos", "PSOE", "PP")
sumparty <- melt(pdescript)
sumparty$ideo <- seq(1, 10)

## Figure
p <- ggplot(sumparty, aes(x=ideo, y=value, group=variable))
pq <- p + geom_density(stat='identity', aes(colour=factor(variable))) +
    labs(title="Partido elegido para cada ideología (Descriptivos)",
         x="Ideología",
         y="Probabilidad") +
             scale_y_continuous(limits=c(0, .75)) +
             scale_color_manual(name="Partido",
                                values=c(colc,
                                    coliu,
                                    colpodemos,
                                    colpsoe,
                                    colpp)) +
                                    guides(linetype=FALSE)
ggsave('./img/fract_pvote_desc.png', pq, height=6, width=11)


## Put everything together and take the difference
mypreds <- merge(sumparty, probparty, by=c("variable", "ideo"))
mypreds$diff <- mypreds$value.y - mypreds$value.x
mypreds <- mypreds[mypreds$y == "ymed", ]

## Figure
p <- ggplot(mypreds, aes(x=ideo, y=diff, group=variable))
pq <- p + geom_density(stat='identity', aes(colour=factor(variable))) +
    labs(title="Partido elegido para cada ideología (Diferencias)",
         x="Ideología",
         y="Diferencia en probabilidad") +
             scale_color_manual(name="Partido",
                                values=c(colc,
                                    coliu,
                                    colpodemos,
                                    colpsoe,
                                    colpp)) +
                                    guides(linetype=FALSE)
ggsave('./img/fract_pvote_diff.png', pq, height=6, width=11)
