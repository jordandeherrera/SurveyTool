for (i in 1:3)
{
presults.1 <- unique(presults[,i])

presults.1.df <- data.frame(sapply(presults.1, str_count, presults[,i]))

presults.1.sum <- data.frame(Answer=presults.1,Total=sapply(presults.1.df,sum))

colnames(presults.1.sum)[1] <- colnames(presults)[i]

assign(paste("Demographic.",i,sep=""), presults.1.sum)
}