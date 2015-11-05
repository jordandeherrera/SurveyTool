library(random)
x <- 10
Validation.Code <- data.frame(Code = randomStrings(n=x, len=5), Count = rep(0,x), Name = rep("",x), Title = rep("",x), Organization = rep("",x), ResponseNo = rep("Replace",x))
colnames(Validation.Code) <- c("Code","Count","Name","Title","Organization", "ResponseNo")
save(Validation.Code, file="Validation.Rdata")