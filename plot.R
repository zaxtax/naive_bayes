library(ggplot2)
library(grid)
library(scales)

library(tidyr)
library(dplyr)
library(extrafont)

suppressMessages(loadfonts())
dataFile <- "nbtimes.csv"
data <- read.csv(dataFile, header=T)

pd <- position_dodge(0.1)

ci_calc <- function(sd, n, conf.lev = .95) {
    sd/sqrt(n) * qt(conf.lev/2 + .5, n - 1)
}

data$Acc <- data$Acc * 100

dataT <- data %>%
    gather(Time.var, Time, Init.time, Update.time) %>%
    group_by(System,DocSize,Time.var) %>%
    summarise_each(funs(mean,
                        sd,
                        se=sd(.)/sqrt(n()),
                        ci=ci_calc(sd(.),n())),
                   Time) %>%
    rename(Time=mean)

dataT$System <- paste(dataT$System, dataT$Time.var, sep=".")
dataT$Time.var <- NULL

data <- data %>%
    group_by(System,DocSize) %>%
    summarise_each(funs(mean,
                        sd,
                        se=sd(.)/sqrt(n()),
                        ci=ci_calc(sd(.),n())),
                   Acc) %>%
    rename(Acc=mean)

theming <- theme_bw() +
        #expand_limits(x=0,y=0) +                        # Expand y range
        #scale_y_continuous(expand = c(0, 0), limits= c(0,4)) +
        #scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
    
        theme(panel.grid.major = element_line(colour = "black", size=0.15)) +
        theme(panel.grid.minor = element_blank()) +
        theme(panel.grid.major.x = element_blank()) +
        theme(panel.border = element_blank()) + #rect(colour = "black", fill=NA, size=0.1)) +
        theme(axis.line = element_line(colour = "black")) +
        theme(text = element_text(family="Times")) +
        theme(plot.title = element_text(size = rel(2))) +
        #theme(plot.margin = margin(0.5, 1, 0.5, 1, "cm")) +
        theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +
        theme(axis.title.x = element_text(size = rel(1.5))) +
        theme(axis.text.y = element_text(size = rel(1.5), angle = 90)) +
        theme(axis.text.x = element_text(size = rel(1.5), hjust = 1)) +
        theme(legend.title = element_text(size = rel(1.5))) +
        theme(legend.text = element_text(size = rel(1.3))) +
        theme(legend.background = element_rect(fill = "transparent")) +
    
        theme(legend.justification=c(0.02,1.0),
              legend.position=c(0.02,1.0))               # Position legend in bottom right

pAcc <- ggplot(data, aes(x=DocSize,
                         y=Acc,
                         colour=System,
                         shape=System,
                         group=System)) +
        geom_errorbar(aes(ymin=Acc-se, ymax=Acc+se),
                      colour="black", width=2000) +
        geom_line(size=1.5, position=pd) +
        #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
        xlab("Data size (documents)") +
        ylab("Accuracy (%)") +
        geom_point(size=5) +
        scale_shape(name="") +
        #scale_color_hue(name="", l=40) +
        scale_color_manual(name="",
                           values=c("cornflowerblue", "firebrick2")) +
        scale_x_continuous(expand=c(0, 0),
                           limits=c(0, 20000),
                           oob=rescale_none) +
        scale_y_continuous(expand=c(0, 0),
                           breaks=1:3*25,
                           limits=c(0, 90)) +
        theming   

gAcc <- ggplot_gtable(ggplot_build(pAcc))
gAcc$layout$clip[gAcc$layout$name=="panel"] <- "off"

ggsave("nb_plot.pdf", gAcc) # width=4, height=3.5)
embed_fonts("nb_plot.pdf", outfile="plots/nbplotacc.pdf")

timing.fields = c("JAGS.Init.time", "JAGS.Update.time",
                  "Hakaru.Init.time", "Hakaru.Update.time")
timing.labels = c("JAGS + initialization",
                  "JAGS",
                  "Hakaru + initialization",
                  "Hakaru")

pT <-   ggplot(dataT, aes(x=DocSize,
                          y=Time,
                          colour=System,
                          linetype=System,
                          shape=System,
                          group=System)) +
        geom_errorbar(aes(ymin=Time-se, ymax=Time+se),
                    colour="black", position="dodge") +
        geom_line(size=1.5, position=pd) +
        #geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
        xlab("Data size (documents)") +
        ylab("Run time (secs)") +
        geom_point(size=5) +
        scale_linetype_manual(name="",    # Legend label, use darker colors
                              breaks=timing.fields,
                              labels=timing.labels,
                              values=c("solid", "longdash", "solid", "dotted")) +
        scale_shape_manual(name="",    # Legend label, use darker colors
                           breaks=timing.fields,
                           labels=timing.labels,
                           values=c(0,4,1,3)) +
        scale_color_manual(name="",
                           breaks=timing.fields,
                           labels=timing.labels,
                           values=c("cornflowerblue", "navyblue", "firebrick3", "red3")) +
        ## scale_color_hue(name="",    # Legend label, use darker colors
        ##                 breaks=timing.fields,
        ##                 labels=timing.labels,
        ##                 l=40) +
        scale_x_continuous(expand=c(0, 0)) +
        scale_y_continuous(expand=c(0, 0),
                           breaks=1:5*2500,
                           limits=c(0, 10000)) +
        theming

gT <- ggplot_gtable(ggplot_build(pT))
gT$layout$clip[gT$layout$name=="panel"] <- "off"

ggsave("nb_plot.pdf", gT) # width=4, height=3.5)
embed_fonts("nb_plot.pdf", outfile="plots/nbplottimes.pdf")
