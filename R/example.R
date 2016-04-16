require(data.table)

system("../.cabal-sandbox/bin/color-counter --analyze=analysis.tsv --tally=tallies.tsv --quantize=quantized.png ../data/sample.jpg")

analysis <- fread("../analysis.tsv")

analysis[, RGB:=rgb(Red/255,Green/255,Blue/255)]

png(file="observations.png", width=600, height=400, units="px")
pairs(analysis[, .(L, A, B)], col=analysis$RGB, pch=".", main="Observations")
dev.off()

png(file="classifications.png", width=600, height=400, units="px")
pairs(analysis[, .(L, A, B)], col=analysis$Color, pch=".", main="Classifications")
dev.off()