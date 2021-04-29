######################################################################
############### NOTES ################################################

# https://github.com/Wookai/paper-tips-and-tricks

# https://github.com/devd/Academic-Writing-Check #checks + useful links

#arxiv submission guidelines
#https://arxiv.org/help/submit_tex


#examples
# https://www3.nd.edu/~pkamat/pdf/graphs.pdf
# http://web.mit.edu/me-ugoffice/communication/plotting.pdf
# https://media.nips.cc/Conferences/NIPS2018/Styles/nips_2018.pdf #nips guidelines
# https://towardsdatascience.com/10-tips-to-improve-your-plotting-f346fa468d18
# https://rafalab.github.io/dsbook/data-visualization-principles.html #colorbling friendly
# http://openaccess.thecvf.com/content_cvpr_2016/papers/He_Deep_Residual_Learning_CVPR_2016_paper.pdf #example of plot 
# https://www.springer.com/gp/authors-editors/authorandreviewertutorials/writing-a-journal-manuscript/figures-and-tables/10285530
# https://abacus.bates.edu/~ganderso/biology/resources/writing/HTWtablefigs.html
# https://writingcenter.unc.edu/tips-and-tools/figures-and-charts/
# http://elpub.bib.uni-wuppertal.de/edocs/dokumente/fbb/wirtschaftswissenschaft/sdp/sdp15/sdp15006.pdf

#######################################################################



################################################################
########### formal nice looking plot ###########################
################################################################


color_blind_friendly_cols <- 
  c("#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

color_blind_friendly_cols <-  c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")#RColorBrewer::brewer.pal(9, "Set1")#ggsci::pal_jama("default",1)(9) #ggsci::pal_npg("nrc",1)(9)

#arima, ets, theta, tbats, stlm, deep




#######################################################################

load("results/total_experiment.RData")
pdf("M1YearlyNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
err_results = list_results[[1]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(3.33,4.6))
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[1]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[1]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[1]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[1]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=1.3, y=err_results[1,1]+0.04, "ARIMA", col=color_blind_friendly_cols[1])

text(x=7, y=err_results[1,2]+0.04, "ETS", col=color_blind_friendly_cols[2])
text(x=7, y=err_results[1,3]+0.04, "theta", col=color_blind_friendly_cols[3])
text(x=7, y=deep_results[1,3+7]+0.04, "Deep", col=color_blind_friendly_cols[6])
text(x=2, y=poly2_results[1,3+2]+0.04, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=2.4, y=poly3_results[1,3+2]-0.06, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=7.4, y=tree_results[1,3+8]-0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()


################################################

pdf("M1QuartNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=2
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(1.61, 1.95)
     )
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=1.3, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=3, y=err_results[1,2]+0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]-0.01, "theta", col=color_blind_friendly_cols[3])
text(x=8, y=deep_results[1,3+8]-0.01, "Deep", col=color_blind_friendly_cols[6])
text(x=2.2, y=poly2_results[1,3+3]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=2, y=poly3_results[1,3+2]-0.02, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=8.2, y=tree_results[1,3+8]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()

#################################################################################


pdf("M1MonthNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=3
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2

)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=1.3, y=err_results[1,1]+0.013, "ARIMA", col=color_blind_friendly_cols[1])
text(x=7, y=err_results[1,2]+0.011, "ETS", col=color_blind_friendly_cols[2])
text(x=12, y=err_results[1,3]-0.015, "theta", col=color_blind_friendly_cols[3])

text(x=21, y=deep_results[1,3+20]-0.02, "Deep", col=color_blind_friendly_cols[6])
text(x=14, y=1.22, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=12.4, y=1.45, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=20.2, y=tree_results[1,3+20]+0.02, "Trees", col=color_blind_friendly_cols[5])
dev.off()

##############################################################################################

pdf("M3YearlyNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=4
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(2.56,3)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=6, y=err_results[1,1]+0.02, "ARIMA", col=color_blind_friendly_cols[1])

text(x=7, y=err_results[1,2]+0.02, "ETS", col=color_blind_friendly_cols[2])
text(x=10, y=err_results[1,3]+0.02, "theta", col=color_blind_friendly_cols[3])
text(x=11, y=deep_results[1,3+11]-0.02, "Deep", col=color_blind_friendly_cols[6])
text(x=5, y=poly2_results[1,3+5]-0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=7, y=poly3_results[1,3+7]-0.02, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=11, y=tree_results[1,3+11]+0.02, "Trees", col=color_blind_friendly_cols[5])
dev.off()

#############################################################################################

pdf("M3QuartNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=5
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(1.08,1.40)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=2, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=2, y=err_results[1,2]+0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.01, "theta", col=color_blind_friendly_cols[3])
text(x=13.3, y=deep_results[1,3+13]-0.01, "Deep", col=color_blind_friendly_cols[6])
text(x=10, y=poly2_results[1,3+10]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=7, y=poly3_results[1,3+7]+0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=14, y=tree_results[1,3+14]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()

#######################################################

pdf("M3OtherNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=7
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=2, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=2, y=err_results[1,2]+0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.01, "theta", col=color_blind_friendly_cols[3])
text(x=13.3, y=deep_results[1,3+13]-0.01, "Deep", col=color_blind_friendly_cols[6])
text(x=10, y=poly2_results[1,3+10]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=7, y=poly3_results[1,3+7]+0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=14, y=tree_results[1,3+14]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()

######################################################################################################

pdf("TourismMonthNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=8
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(1.4, 3.5)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=2, y=err_results[1,1]-0.03, "ARIMA", col=color_blind_friendly_cols[1])

text(x=5, y=err_results[1,2]+0.05, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.06, "theta", col=color_blind_friendly_cols[3])
text(x=13.3, y=deep_results[1,3+13]+0.51, "Deep", col=color_blind_friendly_cols[6])
text(x=13, y=poly2_results[1,3+10]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=20, y=poly3_results[1,3+20]+0.1, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=14, y=tree_results[1,3+14]+0.61, "Trees", col=color_blind_friendly_cols[5])
dev.off()

#######################################################

pdf("TourismQuartNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=9
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(1.5, 2.5)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=2, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=2, y=err_results[1,2]+0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.01, "theta", col=color_blind_friendly_cols[3])
text(x=13.3, y=deep_results[1,3+13]-0.01, "Deep", col=color_blind_friendly_cols[6])
text(x=10, y=poly2_results[1,3+10]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=7, y=poly3_results[1,3+7]+0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=14, y=tree_results[1,3+14]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()




pdf("TourismYearNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=10
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(2.56, 3.2)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=2, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=2, y=err_results[1,2]+0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.01, "theta", col=color_blind_friendly_cols[3])
text(x=4, y=deep_results[1,3+4]-0.01, "Deep", col=color_blind_friendly_cols[6])
text(x=4, y=poly2_results[1,3+4]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=4, y=poly3_results[1,3+4]+0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=4, y=tree_results[1,3+4]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()


#######################################################
load("results/total_experiment.RData")
pdf("M4YearNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=11

#remove fforma and smyl
list_results[[nlist]]$lin_err_res =  list_results[[nlist]]$lin_err_res[, -(4:5)]
list_results[[nlist]]$p2_err_res =  list_results[[nlist]]$p2_err_res[, -(4:5)]
list_results[[nlist]]$p3_err_res =  list_results[[nlist]]$p3_err_res[, -(4:5)]
list_results[[nlist]]$ensdeep_err_res =  list_results[[nlist]]$ensdeep_err_res[, -(4:5)]
list_results[[nlist]]$tree_err_res =  list_results[[nlist]]$tree_err_res[, -(4:5)]

err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(3.02, 3.7)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=8, y=err_results[1,1]+0.02, "ARIMA", col=color_blind_friendly_cols[1])

text(x=6, y=err_results[1,2]+0.02, "ETS", col=color_blind_friendly_cols[2])
text(x=8, y=err_results[1,3]-0.02, "theta", col=color_blind_friendly_cols[3])
text(x=6, y=deep_results[1,3+6]-0.02, "Deep", col=color_blind_friendly_cols[6])
text(x=4, y=poly2_results[1,3+4]+0.03, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=3, y=poly3_results[1,3+3]-0.02, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=10, y=tree_results[1,3+10]+0.02, "Trees", col=color_blind_friendly_cols[5])
dev.off()

##############################################################

load("results/total_experiment.RData")
pdf("M4QuartNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=12

#remove fforma and smyl
list_results[[nlist]]$lin_err_res =  list_results[[nlist]]$lin_err_res[, -(4:5)]
list_results[[nlist]]$p2_err_res =  list_results[[nlist]]$p2_err_res[, -(4:5)]
list_results[[nlist]]$p3_err_res =  list_results[[nlist]]$p3_err_res[, -(4:5)]
list_results[[nlist]]$ensdeep_err_res =  list_results[[nlist]]$ensdeep_err_res[, -(4:5)]
list_results[[nlist]]$tree_err_res =  list_results[[nlist]]$tree_err_res[, -(4:5)]

err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(1.12, 1.39)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=3, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=2, y=err_results[1,2]-0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.01, "theta", col=color_blind_friendly_cols[3])
text(x=8, y=deep_results[1,3+8]-0.02, "Deep", col=color_blind_friendly_cols[6])
text(x=13.5, y=poly2_results[1,3+13]-0.015, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=5, y=poly3_results[1,3+4]-0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=10, y=tree_results[1,3+10]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()

#################################################### Monthly


load("results/total_experiment.RData")
pdf("M4MonthNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=13

#remove fforma and smyl
list_results[[nlist]]$lin_err_res =  list_results[[nlist]]$lin_err_res[, -(4:5)]
list_results[[nlist]]$p2_err_res =  list_results[[nlist]]$p2_err_res[, -(4:5)]
list_results[[nlist]]$p3_err_res =  list_results[[nlist]]$p3_err_res[, -(4:5)]
list_results[[nlist]]$ensdeep_err_res =  list_results[[nlist]]$ensdeep_err_res[, -(4:5)]
list_results[[nlist]]$tree_err_res =  list_results[[nlist]]$tree_err_res[, -(4:5)]

err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2, x=c(2,12,24,41),
     ylim=c(0.891, 1.5)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(x=c(2,12,24,41), y=as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(x=c(2,12,24,41),y=as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(x=c(2,12,24,41), y=as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(x=c(2,12,24,41), y=as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=3, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])

text(x=6, y=err_results[1,2]+0.01, "ETS", col=color_blind_friendly_cols[2])
text(x=2, y=err_results[1,3]+0.015, "theta", col=color_blind_friendly_cols[3])
text(x=40, y=deep_results[1,3+4]-0.02, "Deep", col=color_blind_friendly_cols[6])
text(x=11, y=poly2_results[1,3+2]-0.015, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=22, y=poly3_results[1,3+3]-0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=35, y=tree_results[1,3+4]+0.03, "Trees", col=color_blind_friendly_cols[5])
dev.off()



############################# M4Weekly

load("results/total_experiment.RData")
pdf("M4WeekNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist=14

#remove fforma and smyl
list_results[[nlist]]$lin_err_res =  list_results[[nlist]]$lin_err_res[, -(4:5)]
list_results[[nlist]]$p2_err_res =  list_results[[nlist]]$p2_err_res[, -(4:5)]
list_results[[nlist]]$p3_err_res =  list_results[[nlist]]$p3_err_res[, -(4:5)]
list_results[[nlist]]$ensdeep_err_res =  list_results[[nlist]]$ensdeep_err_res[, -(4:5)]
list_results[[nlist]]$tree_err_res =  list_results[[nlist]]$tree_err_res[, -(4:5)]

err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2, x=c(2,12,24,55),
     ylim=c(2.25, 3.26)
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
poly2_results = list_results[[nlist]]$p2_err_res
lines(x=c(2,12,24,55), y=as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
poly3_results = list_results[[nlist]]$p3_err_res
lines(x=c(2,12,24,55),y=as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
deep_results = list_results[[nlist]]$ensdeep_err_res
lines(x=c(2,12,24,55), y=as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
tree_results = list_results[[nlist]]$tree_err_res
lines(x=c(2,12,24,55), y=as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)


text(x=20, y=err_results[1,1]+0.03, "ARIMA", col=color_blind_friendly_cols[1])

text(x=2, y=err_results[1,2]-0.02, "ETS", col=color_blind_friendly_cols[2])
text(x=6, y=err_results[1,3]+0.03, "theta", col=color_blind_friendly_cols[3])
text(x=50, y=deep_results[1,3+4]+0.03, "Deep", col=color_blind_friendly_cols[6])
text(x=14, y=poly2_results[1,3+2]-0.035, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=31, y=poly3_results[1,3+3]-0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=35, y=tree_results[1,3+4]+0.03, "Trees", col=color_blind_friendly_cols[5])
dev.off()



#####################################################################
########### MEMORY EXPERIMENTS FIGURE ###############################

load("results/total_experiment.RData")
pdf("M1MonthlyMem.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist = 3
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
# poly2_results = list_results[[nlist]]$p2_err_res
# lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
# poly3_results = list_results[[nlist]]$p3_err_res
# lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
# deep_results = list_results[[nlist]]$ensdeep_err_res
# lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)


text(x=1.3, y=err_results[1,1]+0.013, "ARIMA", col=color_blind_friendly_cols[1])

text(x=7, y=err_results[1,2]+0.011, "ETS", col=color_blind_friendly_cols[2])
text(x=12, y=err_results[1,3]-0.015, "theta", col=color_blind_friendly_cols[3])
# text(x=7, y=deep_results[1,3+7]+0.04, "Deep", col=color_blind_friendly_cols[6])
# text(x=2, y=poly2_results[1,3+2]+0.04, "Poly 2nd", col=color_blind_friendly_cols[7])
# text(x=2.4, y=poly3_results[1,3+2]-0.06, "Poly 3rd", col=color_blind_friendly_cols[8])
dev.off()

#################################################################################################
####################### NONLINEARITIES FIGURE ###################################################
#################################################################################################

load("results/total_experiment.RData")
pdf("M3MonthNonlin.pdf", width=1.5*1080*0.005, height=1080*0.005)
nlist = 6
err_results = list_results[[nlist]]$lin_err_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=c(0.854, 1.2))
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
 poly2_results = list_results[[nlist]]$p2_err_res
 lines(as.numeric(poly2_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[7], lwd=2)
 poly3_results = list_results[[nlist]]$p3_err_res
 lines(as.numeric(poly3_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[8], lwd=2)
 deep_results = list_results[[nlist]]$ensdeep_err_res
 lines(as.numeric(deep_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[6], lwd=2)
 tree_results = list_results[[nlist]]$tree_err_res
 lines(as.numeric(tree_results[1,-(1:benchmark_ind)]), col=color_blind_friendly_cols[5], lwd=2)

 
text(x=3, y=err_results[1,1]+0.01, "ARIMA", col=color_blind_friendly_cols[1])
text(x=7, y=err_results[1,2]+0.011, "ETS", col=color_blind_friendly_cols[2])
text(x=5, y=err_results[1,3]-0.007, "theta", col=color_blind_friendly_cols[3])

text(x=13+2, y=deep_results[1,3+13]+0.01, "Deep", col=color_blind_friendly_cols[6])
text(x=31, y=poly2_results[1,3+31]+0.01, "Poly 2nd", col=color_blind_friendly_cols[7])
text(x=31, y=poly3_results[1,3+31]-0.01, "Poly 3rd", col=color_blind_friendly_cols[8])
text(x=42, y=tree_results[1,3+42]+0.01, "Trees", col=color_blind_friendly_cols[5])
dev.off()


#############################################################
########### in-sample vs out analysis #######################
#############################################################

load("results/in_vs_out.RData")

lighter_color_blind_friendly_cols <-  c("#B9B9B9", "#FFBF20", "#76D4FF", "#20BE93", "#FFFF62", "#2092D2", "#F57E20", "#EC99C7")#RColorBrewer::brewer.pal(9, "Set1")#ggsci::pal_jama("default",1)(9) #ggsci::pal_npg("nrc",1)(9)


a = lapply(list_results, function (ll) {
  res = as.matrix(c(ll$outsamp))
  res = res[-6, , drop=FALSE]
  res = res / max(ll$outsamp[-6])
  #res = res / res[6,]
  row.names(res) <- c("inets", "intheta", "inarima", "inglob_lin", "inglob_deep")
  colnames(res) <- paste(ll$dataset, ll$period, sep="")
  res
})

b = lapply(list_results, function (ll) {
  res = as.matrix(c(ll$insamp))
  res = res[-6, , drop=FALSE]
  res = res / max(ll$outsamp[-6])
  row.names(res) <- c("inets", "intheta", "inarima", "inglob_lin", "inglob_deep")
  colnames(res) <- paste(ll$dataset, ll$period, sep="")
  res
})

ds_of_interest = 1:3
instab = data.table::melt(b[ds_of_interest])

tab = data.table::melt(a[ds_of_interest])



col_tab = c(color_blind_friendly_cols[c(2,3,1, 4)], color_blind_friendly_cols[6] )
light_col_tab = c(lighter_color_blind_friendly_cols[c(2,3,1, 4)], lighter_color_blind_friendly_cols[6] )

pdf("invsout.pdf", width=1.5*1080*0.005, height=1080*0.005*2.0)

par(mfrow = c(4,1))
par(mar = c(2.1, 4.1, 2.1, 2.1))
barplot(value ~ Var1 + Var2, data = tab,
        beside=TRUE,
        col=col_tab[c(1:5)],
        ylab="Training MASE (Solid) vs Test MASE (Shaded)",
        xlab="",
        ylim=c(0,1.1))

oldpar <- par("lwd")
par(lwd=2)
barplot(value ~ Var1 + Var2, data = instab,
        beside=TRUE,
        col=light_col_tab[c(1:5)],
        add = TRUE,
        ylim=c(0,1.1))
par(lwd=oldpar)

legend(x=1.8, y=1.15,
       legend=c("ETS", "theta", "ARIMA",  "Global Linear", "Global DeepNet"),
       bty = "n",
       fill=col_tab, ncol=6, x.intersp = 0.2,
       text.width = c(1,1,1,1.2,1.6),
       cex=1.5
      )



ds_of_interest = 4:6
instab = data.table::melt(b[ds_of_interest])
tab = data.table::melt(a[ds_of_interest])

barplot(value ~ Var1 + Var2, data = tab,
        beside=TRUE,
        col=col_tab[c(1:5)],
        ylab="Training MASE (Solid) vs Test MASE (Shaded)",
        xlab="Dataset",
        ylim=c(0,1.1))

oldpar <- par("lwd")
par(lwd=2)
barplot(value ~ Var1 + Var2, data = instab,
        beside=TRUE,
        col=light_col_tab[c(1:5)],
        add = TRUE,
        ylim=c(0,1.1))
par(lwd=opar)

ds_of_interest = 10:8
instab = data.table::melt(b[ds_of_interest])
tab = data.table::melt(a[ds_of_interest])

barplot(value ~ Var1 + Var2, data = tab,
        beside=TRUE,
        col=col_tab[c(1:5)],
        ylab="Training MASE (Solid) vs Test MASE (Shaded)",
        xlab="Dataset",
        ylim=c(0,1.1))

oldpar <- par("lwd")
par(lwd=2)
barplot(value ~ Var1 + Var2, data = instab,
        beside=TRUE,
        col=light_col_tab[c(1:5)],
        add = TRUE,
        ylim=c(0,1.1))
par(lwd=oldpar)

ds_of_interest = 11:13
instab = data.table::melt(b[ds_of_interest])
tab = data.table::melt(a[ds_of_interest])

barplot(value ~ Var1 + Var2, data = tab,
        beside=TRUE,
        col=col_tab[c(1:5)],
        ylab="Training MASE (Solid) vs Test MASE (Shaded)",
        xlab="Dataset",
        ylim=c(0,1.1))

oldpar <- par("lwd")
par(lwd=2)
barplot(value ~ Var1 + Var2, data = instab,
        beside=TRUE,
        col=light_col_tab[c(1:5)],
        add = TRUE,
        ylim=c(0,1.1))
par(lwd=oldpar)


dev.off()


####################################################################
############ plot coefficients of linear AR ########################
####################################################################


load("results/coefs_lin.RData")


sapply(list_results, function (ll) {as.character(ll$period)} )

color_blind_friendly_cols

pdf("ARcoef.pdf", width = 16*0.43, height = 18*0.43)
par(mfrow=c(3,1))
par(mar = c(4.4, 4.1, 1.3, 2.1))
sublist = list_results[c(1,4,10,11)]

plot(sublist[[2]]$linc_coef, type="b", ylim = c(-0.25, 1.25), pch=2, col=color_blind_friendly_cols[2], lwd=2,
     xlab="AR Coefficient of the Pooled Model",
     ylab="Coefficient Value (Yearly Data)")
lines(sublist[[1]]$linc_coef, type="b", pch=1, col=color_blind_friendly_cols[1], lwd=2)
lines(sublist[[3]]$linc_coef, type="b", pch=3, col=color_blind_friendly_cols[3], lwd=2)
lines(sublist[[4]]$linc_coef, type="b", pch=4, col=color_blind_friendly_cols[4], lwd=2)
legend("topright", sapply(sublist, function(ll) ll$ds_name), col=color_blind_friendly_cols[1:4],
       pch=c(1,2,3,4), lwd=2)

sublist = list_results[c(2,5,9,12)]
plot(sublist[[3]]$linc_coef, type="b", ylim = c(-0.41, 0.85), pch=3, col=color_blind_friendly_cols[3], lwd=2,
     xlab="AR Coefficient of the Pooled Model",
     ylab="Coefficient Value (Quarterly Data)")
lines(sublist[[2]]$linc_coef, type="b", pch=2, col=color_blind_friendly_cols[2], lwd=2)
lines(sublist[[1]]$linc_coef, type="b", pch=1, col=color_blind_friendly_cols[1], lwd=2)
lines(sublist[[4]]$linc_coef, type="b", pch=4, col=color_blind_friendly_cols[4], lwd=2)
legend("topright", sapply(sublist, function(ll) ll$ds_name), col=color_blind_friendly_cols[1:4],
       pch=c(1,2,3,4), lwd=2)

sublist = list_results[c(3,6,8,13)]
plot(sublist[[3]]$linc_coef, type="b", ylim = c(-0.22, 0.61), pch=3, col=color_blind_friendly_cols[3], lwd=2,
     xlab="AR Coefficient of the Pooled Model",
     ylab="Coefficient Value (Monthly Data)")
lines(sublist[[2]]$linc_coef, type="b", pch=2, lwd=2, col=color_blind_friendly_cols[2])
lines(sublist[[1]]$linc_coef, type="b", pch=1, lwd=2, col=color_blind_friendly_cols[1])
lines(sublist[[4]]$linc_coef, type="b", pch=4, lwd=2, col=color_blind_friendly_cols[4])
legend("topright", sapply(sublist, function(ll) ll$ds_name), col=color_blind_friendly_cols[1:4],
       pch=c(1,2,3,4), lwd=2)
dev.off()


###############################################################
############# HETEROGENEITY ###################################
###############################################################

load("results/heterog_res.RData")
res_lin
res_dl

feat_res_dl

a = cbind( res_lin[,c(1:3, 6)], period_res_lin[,6],
           res_dl[,6], period_res_dl[,6], feat_res_dl[,6])
colnames(a) = c("arima", "ets", "theta", "Global Linear", "Cluster Linear",
                "Glob. DeepNet", "Cluster DeepNet", "Glob. Deep+Feat.")
a = a[1,]
# 
# a = data.table::melt(a)
# a[,2] = a[,2] / a[4,2]
# barplot(value~variable, data = a)

xtable::xtable(t(a), digits=4)



###########################################################################
#################### long memory linear ###################################
###########################################################################

load("results/longmemlin_res.RData")

pdf("longmem.pdf", 16*5.5*0.1, 9*12*0.1)
par(mfrow=c(8,2))
par(mar=c(0,0,0,0))
for (nlist in 1:length(list_results)) {
  err_results = list_results[[nlist]]$lin_err_res
  cnames = colnames(err_results)
  benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "custom")[1]) - 1
  plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
       ylab="MASE", lwd=2,
       ylim=range(err_results[1,]),
                  xaxt='n', yaxt='n',
  )
  plotcoords = par("usr")
  text( plotcoords[1] + diff(plotcoords[1:2])/2, plotcoords[3] + diff(plotcoords[3:4])/1.3, paste(list_results[[nlist]]$ds_name, list_results[[nlist]]$period))
  for (nc in 1:benchmark_ind) {
    abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
           lty=1+nc, lwd=2)
  }
  
  if (nlist==1) {
    text(x=2, y=err_results[1,1]+0.04, "ARIMA", col=color_blind_friendly_cols[1])
    
    text(x=2, y=err_results[1,2]+0.04, "ETS", col=color_blind_friendly_cols[2])
    text(x=2, y=err_results[1,3]+0.04, "theta", col=color_blind_friendly_cols[3])
  }
}
dev.off()


########################################################################
###################### partitioning results ############################
########################################################################

load("results/partitioning_res.RData")

pdf("partitioning.pdf", 16*5.5*0.1, 9*12*0.1)
par(mfrow=c(7,2))
par(mar=c(0,0,0,0))

for (nlist in 1:length(list_results)) {
  err_results = list_results[[nlist]]$tot_experi_res
  part_res = list_results[[nlist]]$red_experi_res
  cnames = colnames(err_results)
  benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "custom")[1]) - 1
  plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
       ylab="MASE", lwd=2,
       ylim=range(c(err_results[1,],part_res[1,])),
       xaxt='n', yaxt='n'
  )
  plotcoords = par("usr")
  text( plotcoords[1] + diff(plotcoords[1:2])/2, plotcoords[3] + diff(plotcoords[3:4])/1.3, paste(list_results[[nlist]]$ds_name, list_results[[nlist]]$period))
  
  for (nc in 1:benchmark_ind) {
    abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
           lty=1+nc, lwd=2)
  }
  part_res = list_results[[nlist]]$red_experi_res
  lines(as.numeric(part_res[1, -(1:benchmark_ind)]),  lty=2, lwd=2)
  
  if (nlist==1) {
    text(x=2, y=err_results[1,1]+0.04, "ARIMA", col=color_blind_friendly_cols[1])
    
    text(x=2, y=err_results[1,2]+0.04, "ETS", col=color_blind_friendly_cols[2])
    text(x=2, y=err_results[1,3]+0.04, "theta", col=color_blind_friendly_cols[3])
  }
}

dev.off()

pdf("partM4.pdf", width=1.5*1080*0.005, height=1080*0.005) # 14*0.7, 9*0.7)
load("results/partitioning_res.RData")
nlist = 13
err_results = list_results[[nlist]]$tot_experi_res
part_res = list_results[[nlist]]$red_experi_res
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "custom")[1]) - 1
plot(as.numeric(err_results[1, -(1:benchmark_ind)]), type="l", xlab="AR order of the Pooled Model",
     ylab="MASE", lwd=2,
     ylim=range(c(err_results[1,],part_res[1,]))
)
for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}
part_res = list_results[[nlist]]$red_experi_res
lines(as.numeric(part_res[1, -(1:benchmark_ind)]),  lty=2, lwd=2)


  text(x=8, y=err_results[1,1]+0.009, "ARIMA", col=color_blind_friendly_cols[1])
  
  text(x=4, y=err_results[1,2]+0.009, "ETS", col=color_blind_friendly_cols[2])
  text(x=2, y=err_results[1,3]+0.009, "theta", col=color_blind_friendly_cols[3])
  text(x=7, y=err_results[1,10]-0.05, "Partitioned Linear", col="black")
  text(x=30, y=err_results[1,34]+0.02, "Full Global Linear", col="black")
dev.off()

########################################################################
###################### scale normalizations ############################
########################################################################


load("results/scalenorm_experiment.RData")

list_results[[1]]$res_dl_norm[1,4] / 
list_results[[1]]$res_dl_notnorm[1,4]

list_results[[5]]$res_dl_norm[1,ncol(ll$res_dl_norm)] / 
  list_results[[5]]$res_dl_notnorm[1,4]

MASE_relat = (sapply(list_results, function (ll) {
  
 ( as.numeric(ll$res_dl_norm[1,ncol(ll$res_dl_norm)] / 
    ll$res_dl_notnorm[1,ncol(ll$res_dl_norm)]))
}))

ABS_relat = (sapply(list_results, function (ll) {
  
  as.numeric(ll$res_dl_norm[3,ncol(ll$res_dl_norm)] / 
               ll$res_dl_notnorm[3,ncol(ll$res_dl_norm)])
}))


mean(MASE_relat)
mean(ABS_relat)

MASE_relat_scalfeat = (sapply(list_results, function (ll) {
  
  ( as.numeric(ll$res_dl_scal_norm[1,ncol(ll$res_dl_norm)] / 
                 ll$res_dl_notnorm[1,ncol(ll$res_dl_norm)]))
}))

ABS_relat_scalfeat = (sapply(list_results, function (ll) {
  
  as.numeric(ll$res_dl_scal_norm[3,ncol(ll$res_dl_norm)] / 
               ll$res_dl_notnorm[3,ncol(ll$res_dl_norm)])
}))


mean(MASE_relat_scalfeat)
mean(ABS_relat_scalfeat)

c(mean(MASE_relat), mean(MASE_relat_scalfeat), mean(ABS_relat), mean(ABS_relat_scalfeat))


############# EXTRA DATASETS ################################################

#######################################################################################

load("results/pedestrian_res.RData")
descr = " "
pdf("pedestrian.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, ]), type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2)
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[4:3][nc],
         lty=1+nc, lwd=2)
}

text(x=50, y=err_results[1,1]+0.15, "TBATS", col=color_blind_friendly_cols[4:3][1])

text(x=100, y=err_results[1,2]+0.15, "theta", col=color_blind_friendly_cols[4:3][2])

dev.off()

################################################################################################


load("results/FREDMD_MONTH_results.RData")
descr = " "
pdf("FREDMD_MONTH.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[1:3][nc],
         lty=1+nc, lwd=2)
}

text(x=50, y=err_results[1,1]+0.004, "ARIMA", col=color_blind_friendly_cols[1:3][1])

text(x=100, y=err_results[1,2]+0.004, "ETS", col=color_blind_friendly_cols[1:3][2])

text(x=150, y=err_results[1,3]-0.0045, "theta", col=color_blind_friendly_cols[1:3][3])


dev.off()

##########################################################################################



load("results/dominick100k_results.RData")
descr = " "
pdf("dominick.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(5,3)][nc],
         lty=1+nc, lwd=2)
}

text(x=50, y=err_results[1,1]-0.005, "STLM-AR", col=color_blind_friendly_cols[c(5,3)][1])

text(x=100, y=err_results[1,2]+0.005, "theta", col=color_blind_friendly_cols[c(5,3)][2])


dev.off()

#################################################################################################


load("results/results_traffic_25days.RData")
descr = " "
pdf("traffic.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(5,3)][nc],
         lty=1+nc, lwd=2)
}

text(x=50, y=err_results[1,1]+0.12, "STLM-AR", col=color_blind_friendly_cols[c(5,3)][1])

text(x=60, y=err_results[1,2]+0.12, "theta", col=color_blind_friendly_cols[c(5,3)][2])


dev.off()


##################################################################################################

load("results/electricity.RData")
descr = " "
pdf("electricity.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(5,3)][nc],
         lty=1+nc, lwd=2)
}

text(x=50, y=err_results[1,1]+0.12, "STLM-AR", col=color_blind_friendly_cols[c(5,3)][1])

text(x=60, y=err_results[1,2]+0.12, "theta", col=color_blind_friendly_cols[c(5,3)][2])

dev.off()




##################################################################################################

load("results/weather_733_5478.RData")
descr = " "
pdf("weather.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(4,5,3)][nc],
         lty=1+nc, lwd=2)
}

text(x=15, y=err_results[1,1]+0.03, "TBATS", col=color_blind_friendly_cols[4])

text(x=300, y=err_results[1,2]-0.03, "STLM-AR", col=color_blind_friendly_cols[5])

text(x=100, y=err_results[1,3]+0.03, "theta", col=color_blind_friendly_cols[3])

dev.off()

######################################################################################

load("results/CIF2016_res.RData")
descr = " "
pdf("CIF2016.pdf", width=1.5*1080*0.005, height=1080*0.005)

err_results = err_results[,-(1)] #remove the winner of the competition
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(1:3)][nc],
         lty=1+nc, lwd=2)
}


text(x=7, y=err_results[1,1]+0.015, "ARIMA", col=color_blind_friendly_cols[1:3][1])

text(x=5, y=err_results[1,2]+0.015, "ETS", col=color_blind_friendly_cols[1:3][2])

text(x=10, y=err_results[1,3]+0.015, "theta", col=color_blind_friendly_cols[1:3][3])

dev.off()



######################################################################################

load("results/hospital_res.RData")
descr = " "
pdf("hospital.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,] #remove the winner of the competition
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(c(0.72,0.9)))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(1:3)][nc],
         lty=1+nc, lwd=2)
}


text(x=30, y=err_results[1,1]+0.006, "ARIMA", col=color_blind_friendly_cols[1:3][1])

text(x=40, y=err_results[1,2]+0.006, "ETS", col=color_blind_friendly_cols[1:3][2])

text(x=10, y=err_results[1,3]-0.006, "theta", col=color_blind_friendly_cols[1:3][3])

dev.off()


######################################################################################

load("results/wiki_last35days.RData")
descr = " "
pdf("wiki.pdf", width=1.5*1080*0.005, height=1080*0.005)

err_results = err_results[,-(1:2)] #remove the winner of the competition
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(3)][nc],
         lty=1+nc, lwd=2)
}


text(x=7, y=err_results[1,1]+0.05, "theta", col=color_blind_friendly_cols[1:3][3])


dev.off()



######################################################################################

load("results/parts.RData")
descr = " "
pdf("parts.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,] #remove the winner of the competition
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(1:3)][nc],
         lty=1+nc, lwd=2)
}


text(x=10, y=err_results[1,1]-0.012, "ARIMA", col=color_blind_friendly_cols[1:3][1])

text(x=30, y=err_results[1,2]-0.012, "ETS", col=color_blind_friendly_cols[1:3][2])

text(x=20, y=err_results[1,3]+0.012, "theta", col=color_blind_friendly_cols[1:3][3])

dev.off()

######################################################################################

load("results/pendul_results.RData")
descr = " "
png("pendul.pdf", width=1920, height=1080, pointsize = 30)

#err_results = err_results[,-(1:2)]
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1

glob_lags = seq(1, length(err_results[1,]),  2,)

plot(as.numeric(err_results[1, glob_lags]), type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2)
lines(as.numeric(err_results[1, -glob_lags]), lwd=2, col="red")
title(descr)

text(x=140, y=19, "Local Linear", col="red")
dev.off()

##############################################################################

load("results/results_nn5.RData")
descr = " "
pdf("nn5.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,] #remove the winner of the competition
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[c(4,5,3)][nc],
         lty=1+nc, lwd=2)
}


text(x=20, y=err_results[1,1]-0.04, "TBATS", col=color_blind_friendly_cols[c(4,5,3)][1])

text(x=100, y=err_results[1,2]+0.06, "STLM-AR", col=color_blind_friendly_cols[c(4,5,3)][2])

text(x=200, y=err_results[1,3]+0.06, "theta", col=color_blind_friendly_cols[c(4,5,3)][3])

dev.off()



##############################################################################

load("results/covid_res.RData")
descr = " "
pdf("covid.pdf", width=1.5*1080*0.005, height=1080*0.005)

#err_results = err_results[,] #remove the winner of the competition
cnames = colnames(err_results)
benchmark_ind = (which(sapply(cnames, function (x) unlist(strsplit(x, "_"))[1]) == "linglob")[1]) - 1


plot(as.numeric(err_results[1, -(1:benchmark_ind)]),
     type="l", xlab="AR order of the Pooled Model", ylab="MASE", lwd=2,
     ylim=range(err_results[1,]))
title(descr)

for (nc in 1:benchmark_ind) {
  abline(h=err_results[1,nc], col=color_blind_friendly_cols[nc],
         lty=1+nc, lwd=2)
}


text(x=8, y=err_results[1,1]+0.09, "ARIMA", col=color_blind_friendly_cols[1])

text(x=6, y=err_results[1,2]+0.09, "ETS", col=color_blind_friendly_cols[2])

text(x=6, y=err_results[1,3]-0.05, "theta", col=color_blind_friendly_cols[3])

dev.off()
