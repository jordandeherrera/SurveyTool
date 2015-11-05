Validation.Code <- data.frame(Code = randomStrings(n=10, len=5), Count = rep(0,10), Name = rep("",10), Title = rep("",10), Organization = rep("",10))
colnames(Validation.Code) <- c("Code","Count","Name","Title","Organization")
save(Validation.Code, file="Validation.Rdata")