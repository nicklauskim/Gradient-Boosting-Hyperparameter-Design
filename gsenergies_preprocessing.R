library(data.table)
library(plot3D)



# Make sure the R session's working directory is set to the right place
setwd('~/Documents/STATS 201A/Final Project')


outcomes <- fread("./Data/gsenergies/out.dat.0_16274.txt", skip = 2, header = FALSE, colClasses = c("integer", "numeric"))
colnames(outcomes) <- c("Id", "E")


AeSub <- fread("./Data/gsenergies/AEsub.out.txt", header = FALSE, colClasses = c("integer","numeric"))
colnames(AeSub) <- c("Id", "Esub")

# Merge AeSub and outcomes; compute atomization energies
outcomesAe <- merge(outcomes, AeSub, by = "Id")
outcomesAe[,Eat := E-Esub]
outcomesAe[,E:=NULL]; outcomesAe[,Esub:=NULL] # Remove unnescessary columns
rm(outcomes,AeSub)

# Scale factor: The largest value of atomization energy in the data
scl <- -max(abs(outcomesAe$Eat), na.rm = TRUE)


CoulombLambda <- fread("./Data/gsenergies/coulombL.csv", header = FALSE)


# Assign column names
nam <- paste0('px', 1:(ncol(CoulombLambda)-1))
nam <- c("Id", nam)
colnames(CoulombLambda) <- nam
CoulombLambda[,Id:=as.integer(Id)] # Make Id variable integer

# Match with Id's so that there is no mistmatch in order
combined <- merge(CoulombLambda, outcomesAe, by="Id")
rm(outcomesAe,CoulombLambda)

# Remove NA's (calculations that failed to converge)
l.complete <- complete.cases(combined$Eat)
combined <- combined[l.complete,]

# Store atomization energies in a vector Y and remove unnecessary columns from combined
Y <- combined$Eat
#combined[,Eat:=NULL] # No need for Eat - keep Id for writing csv file
combined[,Id:=NULL] # No need for Id


pca <- prcomp(combined, center = TRUE)

plot3D::points2D(pca$x[,1], pca$x[,2], type = "p", colvar = -Y,
                 xlab = expression("Z"[1]), ylab = expression("Z"[2]), 
                 pch=19, cex.axis=1.0, cex.lab=1.0, clab = "|E| (Ry)")


# Output combined to a file
write.csv(combined, file = './Data/gsenergies/lambda.csv')





