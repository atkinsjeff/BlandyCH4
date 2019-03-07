#jorge species analysis


rm(list=ls())

plots<-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\abundance_plot.csv",head=TRUE)
plots <-data.frame(plots)

plots$plot <- paste(plots$transect, plots$Distance)
plots$plot <-as.factor(plots$plot)

jim <-read.csv(file="c:\\Users\\Jeff\\Documents\\R\\DATA\\jorge2.csv",head=TRUE)

#Lumbricus    Eisenia Allolobophora Aporrectodea Dendrobaena   Amynthas

