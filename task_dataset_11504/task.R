df <- read.table('dataset_11504_15.txt')
df$V2 <- factor(df$V2)

bartlett.test(V1 ~ V2, df)
t.test(V1 ~ V2, df, var.equal = T)

wilcox.test(V1 ~ V2, df)

# --------------------------------
df <- read.table('dataset_11504_16.txt')
t.test(df$V1, df$V2, var.equal = F)
