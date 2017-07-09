library('ggplot2')
library('data.table')
library('zoo')

data <- fread("nbsweeps3.csv")
data$Accuracy <- data$Accuracy * 100

window.size <- 20

dt <- data.table(data)
dt[, Accuracy:=rollmean(Accuracy, window.size, fill=list(NA, NULL, NA)), by = "Chains,System"]
data2 <- data.frame(dt)

p <- ggplot(data2,
            aes(x=Sweeps, y=Accuracy, group=interaction(Chains, System), colour=System)) +
     geom_line(alpha=0.4) +
     ylab("Accuracy (%)") +
     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
     scale_color_manual(name="",
                        values=c("cornflowerblue", "firebrick2")) +
     scale_x_continuous(expand=c(0, 0)) +
     scale_y_continuous(expand=c(0, 0)) +
     theme_bw() + 
     theme(panel.grid.major = element_line(colour = "black", size=0.15)) +
     theme(panel.grid.minor = element_blank()) +
     theme(panel.grid.major.x = element_blank()) +
     theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
     theme(text = element_text(family="Times")) +
     theme(plot.title = element_text(size = rel(2))) +
     theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +
     theme(axis.title.x = element_text(size = rel(1.5))) +
     theme(axis.text.y = element_text(size = rel(1.5), angle = 90)) +
     theme(axis.text.x = element_text(size = rel(1.5), hjust = 1)) +
     theme(legend.title = element_text(size = rel(1.5))) +
     theme(legend.text = element_text(size = rel(1.3))) +
     theme(legend.background = element_rect(fill = "transparent")) +
     theme(legend.justification=c(0.02,1.0),
           legend.position=c(0.85,0.15))               # Position legend in bottom right


ggsave("plots/nbsweeps4.pdf", p)
