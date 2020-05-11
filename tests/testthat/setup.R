mFile <- system.file("extdata/pwm1", package = "seqLogo")
dfDNA <- read.table(mFile)

## DNA
mDNA <- as.matrix(dfDNA)
rownames(mDNA) <- c("A", "C", "G", "T")
colnames(mDNA) <- seq_len(ncol(mDNA))

## RNA
mRNA <- mDNA
rownames(mRNA) <- c("A", "C", "G", "U")

## Amino Acid
mAA <- rbind(mDNA, matrix(0, nrow=16, ncol=ncol(mDNA)))
rownames(mAA) <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", 
                   "M", "N", "P", "Q", "R", "S", "T", "V", "W", "Y")

# default fills
fills = c(A = "#61D04F", C = "#2297E6", G = "#F5C710", T = "#DF536B")
