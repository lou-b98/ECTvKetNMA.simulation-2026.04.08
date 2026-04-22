library(esc)
library(gemtc)
library(multinma)
library(ggplot2)
library(stringr)
library(rjags)

# set labels
NEG_LABEL = "Favours intervention"
POS_LABEL = "Favours placebo"

# set file names
SMD.DATA <- 'C:\\Users\\Admin\\OneDrive - UNSW\\Studies\\Ketamine\\TBD - Ketamine TREK\\data\\df_smd.csv'

# load the data from a csv file
df_smd <- read.csv(SMD.DATA, header=TRUE)

# plot a network based on a contrast-based effect size data frame
plot_network_with_multinma <- function(df, title){
  
  # create a multinma network object
  network = set_agd_contrast(df,
                             study = study,
                             trt = treatment,
                             y = diff,
                             se = std.err,
                             sample_size = n)
  
  # plot network; for style options see:
  # https://dmphillippo.github.io/multinma/reference/plot.nma_data.html
  plot(network, weight_edges=TRUE, weight_nodes=TRUE) +
    ggplot2::theme(legend.position = "bottom",
                   legend.box = "vertical",
                   legend.margin = ggplot2::margin(0,0,0,0),
                   legend.spacing = ggplot2::unit(0.5, "lines")) +
    ggplot2::ggtitle(title)
}

# plot the network diagram
plot_network_with_multinma(df_smd, "Antidepressant efficacy")

# create a gemtc network object from the .csv data
smd.network = mtc.network(data.re = df_smd,
                          description = "Bayesian NMA data - SMD")

# create a random effects, normal likelihood, identity link model
smd.model = mtc.model(smd.network,
                      likelihood = "normal",
                      link = "identity",
                      linearModel = 'random')

# run MCMC - burn-in 50,000, iterations 100,000, thin 20
smd.mcmc1 = mtc.run(smd.model,
                    n.adapt=50000,
                    n.iter=100000,
                    thin=20)

# forest plots relative to high-dose racemic ketamine
forest(relative.effect(smd.mcmc1, t1="Ket_High"), 
       digits=3,
       draw.no.effect=TRUE,
       right.label=POS_LABEL,
       left.label=NEG_LABEL,
       use.description=TRUE)

plot.output <- relative.effect(smd.mcmc1, t1 = "Placebo")
plot.output$model$network$treatments$description <- c("Esketamine 84mg*", 
                                                      "Esketamine <84mg", 
                                                      "Ketamine \u22650.5mg/kg*'",
                                                      "Ketamine <0.5mg/kg'",
                                                      "Placebo")




##### ORIGINAL #####
blob.data <- data.frame(id = c("Placebo",
                               "Esketamine 84mg*", 
                               "Esketamine <84mg", 
                               "Ketamine \u22650.5mg/kg*'",
                               "Ketamine <0.5mg/kg'"),
                        group = c(1,2,2,3,3),
                        pe = c(0.00, -0.31, -0.38, -0.93, -0.44),
                        ci.l = c(0.00, -0.59, -0.71, -1.56, -1.07),
                        ci.u = c(0.00, -0.03,  0.00, -0.31,  0.19),
                        trials = c(15,7,2,5,3),
                        participants = c(814,599,195,104,96),
                        style = c("indirect",rep("normal",4)))

my.styles <- data.frame(style=c('normal', 'pooled','indirect','group'),
                        font.weight=c('plain', 'plain','plain','bold'),
                        row.height=c(1, 1, 1, 1.5),
                        pe.style=c('circle','square','square',NA),
                        pe.scale=c(FALSE, FALSE, FALSE, NA),
                        lty=c(1,1,1,NA))
rownames(my.styles) <- my.styles[['style']]

# 700 x 300
blobbogram(blob.data, 
           group.labels=c("Reference category", "S-ketamine","Generic ketamine"),
           columns=c("trials", "participants"), 
           column.labels=c('Trial arms', 'Participants'),
           column.groups=c(1, 2), 
           grouped=TRUE,
           column.group.labels=c('Intervention', 'Control'),
           id.label=" ", 
           ci.label="SMD (95% CrI)", 
           log.scale=FALSE,
           xlim=c(-1.8, 0.4),
           digits=2,
           right.label="Favours placebo",
           left.label="Favours intervention",
           styles=my.styles)

###### MADRS REDUCTION #####
blob.data <- data.frame(id = c("Esketamine 84mg*", 
                               "Esketamine <84mg", 
                               "Ketamine \u22650.5mg/kg*'",
                               "Ketamine <0.5mg/kg'"),
                        group = c(1,1,2,2),
                        pe = c(-2.92, -2.93, -8.56, -4.98),
                        ci.l = c(-6.63, -9.09, -13.4, -11.9),
                        ci.u = c(0.75,  3.28, -2.58,  1.90),
                        trials = c(7,2,4,2),
                        participants = c(599,195,91,55),
                        style = rep("normal",4))

my.styles <- data.frame(style=c('normal', 'pooled','indirect','group'),
                        font.weight=c('plain', 'plain','plain','bold'),
                        row.height=c(1, 1, 1, 1.5),
                        pe.style=c('circle','square','square',NA),
                        pe.scale=c(FALSE, FALSE, FALSE, NA),
                        lty=c(1,1,1,NA))
rownames(my.styles) <- my.styles[['style']]

# 700 x 300
blobbogram(blob.data, 
           group.labels=c("S-ketamine","Generic ketamine"),
           columns=c("trials", "participants"), 
           column.labels=c('Trial arms', 'Participants'),
           column.groups=c(1, 2), 
           grouped=TRUE,
           column.group.labels=c('Intervention', 'Control'),
           id.label=" ", 
           ci.label="MADRS Change (95% CrI)", 
           log.scale=FALSE,
           xlim=c(-14, 4),
           digits=3,
           right.label="Favours placebo",
           left.label="Favours intervention",
           styles=my.styles)

###### SMD REDUCTION #####
blob.data <- data.frame(id = c("Spravato 84mg*", 
                               "Spravato <84mg", 
                               "Ketamine \u22650.5mg/kg*'",
                               "Ketamine <0.5mg/kg'"),
                        group = c(1,1,2,2),
                        pe = c(-0.31, -0.38, -0.93, -0.44),
                        ci.l = c(-0.59, -0.71, -1.56, -1.07),
                        ci.u = c(-0.03,  0.00, -0.31,  0.19),
                        trials = c(7,2,5,3),
                        participants = c(599,195,104,96),
                        style = rep("normal",4))

my.styles <- data.frame(style=c('normal', 'pooled','indirect','group'),
                        font.weight=c('plain', 'plain','plain','bold'),
                        row.height=c(1, 1, 1, 1.5),
                        pe.style=c('circle','square','square',NA),
                        pe.scale=c(FALSE, FALSE, FALSE, NA),
                        lty=c(1,1,1,NA))
rownames(my.styles) <- my.styles[['style']]

# 700 x 300
blobbogram(blob.data, 
           group.labels=c("S-ketamine","Racemic ketamine"),
           columns=c("trials", "participants"), 
           column.labels=c('Trial arms', 'Participants'),
           column.groups=c(1, 2), 
           grouped=TRUE,
           column.group.labels=c('Intervention', 'Control'),
           id.label=" ", 
           ci.label="SMD (95% CrI)", 
           log.scale=FALSE,
           xlim=c(-1.8, 0.4),
           digits=2,
           right.label="Favours placebo",
           left.label="Favours intervention",
           styles=my.styles)


blobbogram(blob.data, 
           group.labels=c("S-ketamine","Racemic ketamine"),
           columns=c("trials"), 
           column.labels=c('Trial arms'),
           column.groups=c(1), 
           grouped=TRUE,
           column.group.labels=c('Intervention', 'Control'),
           id.label=" ", 
           ci.label="SMD (95% CrI)", 
           log.scale=FALSE,
           xlim=c(-1.8, 0.4),
           digits=2,
           right.label="Favours placebo",
           left.label="Favours intervention",
           styles=my.styles)