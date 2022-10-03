
library(ggplot2)
library(cowplot)


######################################################################
# Extract data from previous simulations,
# stored in files "figure1_results_bn_F{I}_dif{J}.txt"
######################################################################


comb <- NULL
for(i in 1:6){
for(j in 1:3){

file.in <- paste0("figure1_results_bn_F", as.character(i), "_dif", as.character(j),".txt")
temp <- read.table(file.in, header= TRUE)
temp$dif <- 0
temp$dif <- j
comb <- rbind(comb, temp)
}
}


figure <- comb[comb$dif == 1,]

figure$log10.N <-  0
figure$log10.N <-  log10(figure$N)

figure$p <- as.factor(figure$p)
figure$F_ST <- factor(figure$F_ST, labels = c("italic(F)[ST]==0.02", "italic(F)[ST]==0.04", "italic(F)[ST]==0.06",
				 "italic(F)[ST]==0.08","italic(F)[ST]==0.10", "italic(F)[ST]==0.12"))


x.lab <- c("10^3","10^4","10^5","10^6","10^7","10^8","10^9")



figure1 <- ggplot(data=figure, aes(x=log10.N, y = power, colour = p))+
	geom_line()+
	theme_classic()+
	scale_y_continuous(name = expression(paste("Power ","(d = ",frac(1, 4), " ",sigma,")")), breaks = c(0.0 ,0.2,0.4, 0.6, 0.8, 1.0), labels = c("0.0","0.2","0.4", "0.6", "0.8", "1.0"), limits = c(0,1))+
	scale_x_continuous(name = "Sample size", breaks = c(3:9), labels = parse(text = x.lab), limits = c(3,9))+
	scale_colour_discrete(name = expression(paste("Polygenicity", ", ", italic(p))))
figure1_wrap <- figure1+facet_wrap(~F_ST, labeller = label_parsed, ncol = 3)

pdf(file = "power_curve_figure.pdf", width = 10, height = 6)
figure1_wrap
dev.off()

