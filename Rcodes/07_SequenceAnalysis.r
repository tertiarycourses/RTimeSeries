library(TraMineR)
data(mvad)            # study of school to work transition in Northern Ireland.

#Check the alphabet
#(from Sept 93 to June 99; i.e., positions 17 to 86: We skip July-August 93)

mvad.alph <- seqstatl(mvad[, 17:86])
mvad.lab <- c("employment", "further education", "higher education",
"joblessness", "school", "training")

mvad.shortlab <- c("EM", "FE", "HE", "JL", "SC", "TR")

mvad.seq <- seqdef(mvad[, 17:86], alphabet = mvad.alph, labels = mvad.lab,
states = mvad.shortlab, weights = mvad$weight, xtstep = 6)

### Rendering sequences
par(mfrow = c(2, 2))
seqfplot(mvad.seq, withlegend = FALSE, title = "f-plot", border = NA)
seqdplot(mvad.seq, withlegend = FALSE, title = "d-plot", border = NA)
seqIplot(mvad.seq, withlegend = FALSE, title = "I-plot", sortv = "from.end")
seqlegend(mvad.seq, position = "bottomright", fontsize = 1.2)

## Rendering sequence by groups
seqIplot(mvad.seq, group = mvad$male, sortv = "from.start", title = "Sex")


## Transition rates
round(trate <- seqtrate(mvad.seq), 3)

## mean time in each state
# by qualification gained at end of compulsory school
seqmtplot(mvad.seq, group = mvad$gcse5eq, title = "End CS qualification")

## Sequence of cross-sectional distributions
#For bad qualification at end of compulsory school, 9 months
seqstatd(mvad.seq[mvad$gcse5eq == "bad", 6:15])

## Sequence of transversal distributions (chronogram)
# by qualification gained at end of compulsory school
seqdplot(mvad.seq, group = mvad$gcse5eq, title = "End CS qualification",
border = NA)

##Sequence of modal states
# by qualification gained at end of compulsory school
seqmsplot(mvad.seq, group = mvad$gcse5eq, title = "End CS qualification",
border = NA)

## Cross-sectional entropies
# Time evolution of the cross-sectional (transversal) state diversity
seqplot.tentrop(mvad.seq, title = "End CS qualification", group = mvad$gcse5eq)


## Distinct successive states and their durations
# Distinct successive states(DSS)
seqdss(mvad.seq)[1:3, ]

# Duration in successive states
seqdur(mvad.seq)[1:3, 1:5]



######################## sequence complexity measures #########################

mvad.ient <- seqient(mvad.seq)
mvad.cplx <- seqici(mvad.seq)
mvad.turb <- seqST(mvad.seq)
ctab <- data.frame(mvad.ient, mvad.cplx, mvad.turb)

plot(ctab)

boxplot(mvad.cplx ~ mvad$male, col = "lightsteelblue")

lm.ici <- lm(mvad.cplx ~ male + funemp + gcse5eq, data = mvad)

## Dissimilarity matrix

subm.custom <- matrix(
c(0,1,1,2,1,1,
1,0,1,2,1,2,
1,1,0,3,1,2,
2,2,3,0,3,1,
1,1,1,3,0,2,
1,2,2,1,2,0),
nrow = 6, ncol = 6, byrow = TRUE,
dimnames = list(mvad.shortlab, mvad.shortlab))

mvad.dist <- seqdist(mvad.seq, method="OM", indel=4, sm=subm.custom)
dim(mvad.dist)

print(mvad.seq[1:4, ], format = "SPS")


############################## cluster analysis

library(WeightedCluster)
set.seed(4)
pam.mvad <- wcKMedoids(mvad.dist, k = 4, weight = mvad$weight)

mvad.cl4 <- pam.mvad$clustering
xtabs(~mvad.cl4)

seqdplot(mvad.seq, group = group.p(mvad.cl4), border = NA)

cl4.labels <- c("FE-Employment", "Training-Employment", "Education",
"Joblessness")
mvad.cl4.factor <- factor(mvad.cl4, levels = c(467, 66, 607,
641), labels = cl4.labels)

# mean time in each state
seqmtplot(mvad.seq, group = mvad.cl4.factor)

# Most frequent sequences
seqfplot(mvad.seq, group = mvad.cl4.factor, border = NA)

# Individual sequences (sorted by states from start)
seqIplot(mvad.seq, group = mvad.cl4.factor, sortv = "from.start")

#Sorted by states from the end
seqIplot(mvad.seq, group = mvad.cl4.factor, sortv = "from.end")



dt <- seqtree(mvad.seq ~ male + Grammar + funemp + gcse5eq + fmpr + livboth,
weighted = FALSE, data = mvad, diss = mvad.dist, R = 5000)
print(dt, gap = 3)

seqtreedisplay(dt, filename = "fg_mvadseqtree.png", type = "d", border = NA)


##################### Forecasting pattern sequence ########################


library(PSF)
nottem_model <- psf(nottem)

nottem_model

nottem_preds <- predict(nottem_model, n.ahead = 12)
nottem_preds

plot(nottem_model, nottem_preds)