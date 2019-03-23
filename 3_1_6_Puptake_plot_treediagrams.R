# Restart R session with: cmd + shift + F10
# Initialize Session. Set working directory, Import functions, libraries, Update Data, import data
library(MyInit)
rm(list = ls())
my_init(mywd = "C:/Users/Florian/Documents/Uni/P-Mobilization-Goethite",
        update_data = FALSE)

# Source Balancing:
 
# Select Input for the evaluation!
##################################################################
ELEMENT <- "P"
EXTRACT <- "DI"
BALANCE_MODE <- "mean" # choose "mean" or "weighted"
source(paste0("./stats/data-analysis-masterthesis/scripts/Balancing/",
              "balance_",BALANCE_MODE,".R"))
#source("./stats/data-analysis-masterthesis/scripts/Analyses/balance_weighted.R")
##################################################################


print(paste("Balance calculated for Soil Extract:",EXTRACT))

soilEx <- quo(!! sym(paste0("Bulk.",EXTRACT,".2018")))
soilExd <- quo(!! sym(paste0("Bulk.",EXTRACT,".delta")))
soilEx2017 <- quo(!! sym(paste0("Bulk.",EXTRACT,".2017")))
soilExp <- quo(!! sym(paste0("Bulk.",EXTRACT,".deltap")))
soilExc <- quo(!! sym(paste0("Bulk.",EXTRACT,".2017")))


# Prepare data for plotting

balance_ind <- balanceEND %>% 
  transmute(Series = Series,
            Pot.Number = Pot.Number,
            bulk.2018 = !! soilEx,
            root.2018 = Root.DA.2018,
            stem.2018 = StemTotal.DA.2018,
            leaf.growth = LeafTotal.DA.delta) %>% 
  mutate(total.P   = root.2018 + stem.2018 + leaf.growth,
         #rootstem.flow = tree.uptake - root.delta,
         stemleaf.flow = leaf.growth)

balance_ind

sumba <- balance_ind %>% 
  select(-Pot.Number) %>% 
  group_by(Series) %>% 
  summarise_if(is.numeric,   std.err)

ggplot(balance_ind, aes(x = Series, y = total.P))+
  stat_boxplot(geom = "errorbar", width = .5, position=position_dodge(0))+
  geom_violin()+
  geom_boxplot(alpha = 0.5)+
  theme_few()+myleg+mycol

# some calculation
# 


pm2017 <- balance_summary %>% 
  transmute(bulk.2017 = !! soilEx2017,
            root.2017 = Root.DA.2017,
            stem.2017 = Stem.DA.2017,
            leaf.growth = 0) %>% 
  mutate(total.P   = root.2017 + stem.2017 + leaf.growth,
         rootstem.flow = 0,
         stemleaf.flow = leaf.growth) %>% 
  t() %>% 
  round(1)



init <- apply(pm2017,1,mean)

pm2018 <- balance_summary %>% 
  transmute(bulk.2018 = !! soilEx,
            root.2018 = Root.DA.2018,
            stem.2018 = StemTotal.DA.2018,
            leaf.growth = LeafTotal.DA.delta) %>% 
  mutate(total.P   = root.2018 + stem.2018 + leaf.growth,
         rootstem.flow = 0,
         stemleaf.flow = leaf.growth)

pm2018 %>% 
  gather() %>% 
  group_by(key) %>% 
  summarise(mean = mean(value),
            se = std.err(value),
            sd = sd(value))

pm <- pm2018 %>% 
  rbind(init) %>% 
  t() %>% 
  round(1)

soil_init <- BAL %>% 
  filter(Probenart == "Bulk",
         Balance == "start",
         Analysis == EXTRACT) %>% 
  group_by(Jahr) %>% 
  summarise(mean = mean(bal_mean),
            sd = sd(bal_mean),
            var = var(bal_mean),
            min = min(bal_mean),
            max = max(bal_mean)) 




pm <- rbind(matrix(as.character(format(signif(pm[1,],3),nsmall = 0)),ncol = 7),
            matrix(as.character(format(signif(pm[2:7,],3),nsmall = 1)),ncol = 7))

pm[1,7] <- paste0(round(soil_init$min,0),"-",round(soil_init$max,0))
pm

pmp2017 <- conc_summary %>% 
  transmute(bulk.deltap = 0,
            root.deltap = Root.DA.2017,
            stem.deltap = Stem.DA.2017,
            leaf.deltap = 0)

pmpinit <- apply(pmp2017,2,mean)

pmp <- conc_summary %>% 
  transmute(bulk.deltap = !! soilExp,
            root.deltap = Root.DA.2018,
            stem.deltap = StemTotal.DA.2018,
            leaf.deltap = LeafTotal.DA.2018) %>% 
  rbind(pmpinit)


pmp

loc <- data.frame(x = c(50,80,0,-160, 85, 110, -90),
                  y = c(60 ,250,450,620, 120, 355, 560),
                  width = c(80,70,70,70,70,70,70),
                  width.init = c(160,70,70,70,70,70,70),
                  color = c("black",rep("black",6)))


flows <- cbind(pm,loc)
colnames(flows)[1:7]<-c(as.character(unique(balance_summary$Series)),"Initial")

t(round(sumba[,-1],1))[3,]

flows <- cbind(flows,
               textcolor = c("black","black","black",rep("black",4)),
               textsize = c(3,3,3,3,3,3,3))


flow <- flows[7,]
delta <- flows[1:4,]

pmpcol <- pmp %>% 
  transmute(
    #Series = trtmts,
    Bulk = bulk.deltap +1,
    Root = root.deltap / max(root.deltap),
    Stem = stem.deltap / max(stem.deltap),
    Leaf = leaf.deltap /max(leaf.deltap))
pmpcol

trtmts <- c("GPorg","GPinorg","G","Porg","Pinorg","Control","Initial")
trt <- as.data.frame(t(trtmts))
colnames(trt) <- trtmts
null <- c(0,0,0,0)

pmpcol <-rbind(pmpcol,null)
seriescol <- c(trtmts,"null")
pmpcol <- cbind(Series = seriescol,pmpcol) 
pmpcol

pmp2 <- pmpcol %>% 
  transmute(
    Series = Series,
    Bulk = Bulk  * 100,
    Root = Root  * max(pmp$root.deltap),
    Stem = Stem  * max(pmp$stem.deltap),
    Leaf = Leaf * max(pmp$leaf.deltap) )
pmp2  
############ DRAWIN* 100
############
############
pot <- data.frame(x = c(-170,170,230,-230), 
                  y = c(0,0,380,380 ))

tree <- read.csv("./Fotos/baum/tree_coord.txt")[,2:4]
tree$x <- tree$x - mean(tree$x)+10

leaves <- tree %>% 
  filter(group %nin% c("stem","root","arrowroot","arrowstem","arrowleaf","arrowrootdown","arrowstemdown"))

stem <- tree %>% 
  filter(group == "stem")

root <- tree %>% 
  filter(group == "root")

arrowl <- tree %>% 
  filter(group %in% c("arrowleaf")) %>% 
  select(-group)

arrowr <-tree %>% 
  filter(group %in% c("arrowroot")) %>% 
  select(-group)

arrowrd <- tree %>% 
  filter(group %in% c("arrowrootdown")) 

arrows <-tree %>% 
  filter(group %in% c("arrowstem")) %>% 
  select(-group)
arrowsd <-tree %>% 
  filter(group %in% c("arrowstemdown")) %>% 
  select(-group)


i <- 1
TREAT <- list()
trtmts <- c("GPorg","GPinorg","G","Porg","Pinorg","Control","Initial")
order <- c(7,1,4,2,5,3,6)

for(Treatment in order){
  print(trtmts[Treatment])
  TREAT[[i]] <- ggplot()+
    ylim(0,900)+
    xlim(-280,340)+
    geom_polygon(data = pot,aes(x,y ),fill = "#a85100", color = "black", 
                 alpha= pmpcol$Bulk[Treatment])+
    #geom_line(data = roots, aes(x-2,y,group = group, color = y), lwd = .1)+
    #geom_polygon(data= stem,aes(x,y ))+
    geom_polygon(data=leaves,aes(x,y,fill = group),
                 alpha = pmpcol$Leaf[Treatment])+
    scale_fill_manual(values = rep("#72bf00",19))+ 
                  # c = c(50+pmp$Stem.deltap[Treatment]*50,
                  #       50+pmp$Stem.deltap[Treatment]*50))+
    geom_polygon(data=stem,aes(x,y),fill = "#422000",
                 alpha = pmpcol$Stem[Treatment])+
    geom_polygon(data=root,aes(x,y),fill = "#422000",
                 alpha = pmpcol$Root[Treatment])
  #  geom_polygon(data=arrowl,aes(x,y),color = "black",fill = "white")
  # 
  # if(flow[1,Treatment] >= 0) {
  #   TREAT[[i]] <- TREAT[[i]] + geom_polygon(data=arrowr,aes(x,y),color = "black",fill = "white")
  # } else{
  #   TREAT[[i]] <- TREAT[[i]] + geom_polygon(data=arrowrd,aes(x,y),color = "black",fill = "white")
  # }
  # 
  # if(flow[2,Treatment] >= 0) {
  #   TREAT[[i]] <- TREAT[[i]] + geom_polygon(data=arrows,aes(x,y),color = "black",fill = "white")
  # } else{
  #   TREAT[[i]] <- TREAT[[i]] + geom_polygon(data=arrowsd,aes(x,y),color = "black",fill = "white")
  # }
  #   
  
    #geom_point(data = delta,aes(x,y),color = "black", fill = "white", size = 9, pch = 21)+
    if(Treatment == 7){
      TREAT[[i]] <- TREAT[[i]] + 
        geom_tile(data = delta,aes(x,y,height = 60,width = width.init),
                  color = "black", fill = "white")
    } else{
      TREAT[[i]] <- TREAT[[i]] + 
        geom_tile(data = delta,aes(x,y,height = 60,width = width),
                  color = delta$color, fill = "white")
    }
    # geom_text(data=flow,
    #           aes_string(x="x",y="y",label=trtmts[Treatment]),
    #           colour = flow$textcolor,
    #           fontface = "bold", 
    #           size = flow$textsize, 
    #           family = "serif")+
  TREAT[[i]] <- TREAT[[i]] + 
    geom_text(data=delta,
              aes_string(x="x",y="y",label=trtmts[Treatment]),
              colour = delta$textcolor,
              fontface = "bold", 
              size = delta$textsize, 
              family = "serif")+
    geom_text(data = trt,
              aes_string(x = 340, y = 30, label = trtmts[Treatment]), 
              hjust = 1,
              size = 3.5,
              family = "serif") +
    theme_few()+myleg+mygui+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")
  
  i <- i+1
}

l1 <- ggplot(pmpcol, aes(x = Series, y = Stem, alpha = Stem))+
  geom_col(fill = "#422000")+
  scale_alpha_continuous(
    name = "Stem, Root\n[mg g-1]", 
    range = c(0,1),
    breaks = range2seq(pmpcol$Stem,5), 
    labels = as.character(round(range2seq(pmp2$Stem,5),2)))+
  theme_few()+myleg+theme(legend.title = element_text(size = 10))
  
l2 <- ggplot(pmpcol, aes(x = Series, y = Leaf, alpha = Leaf))+
  geom_col(fill = "#72bf00")+
  scale_alpha_continuous(
    name = "Leaves \n[mg g-1]", 
    range = c(0,1),
    breaks = range2seq(pmpcol$Leaf,5), 
    labels = as.character(round(range2seq(pmp2$Leaf,5),2)))+ 
  theme_few()+myleg+theme(legend.title = element_text(size = 10))

l3 <- ggplot(pmpcol, aes(x = Series, y = Bulk, alpha = Bulk))+
  geom_col(fill = "#a85100")+
  scale_alpha_continuous(
    name = "Bulk \n[%]", 
    range = c(.0,1),
    breaks = range2seq(pmpcol$Bulk,5), 
    labels = as.character(round(range2seq(pmp2$Bulk,5),0)))+ 
  theme_few()+myleg+theme(legend.title = element_text(size = 10))


library(gridExtra)

l1 <- get_legend(l1)
l2 <- get_legend(l2)
l3 <- get_legend(l3)


pl <- list()
pl[[1]] <- TREAT[[1]]
pl[[2]] <- l1
pl[[3]] <- l2
pl[[4]] <- l3
pl[5:10] <- TREAT[2:7]



# construct a scale

lay <- rbind(c(1,1,1,2,3,4),
             c(5,5,5,6,6,6),
             c(7,7,7,8,8,8),
             c(9,9,9,10,10,10))


lay_presentation <- rbind(
  c(1,1,1,5,5,5,7,7,7,9,9,9),
  c(2,3,4,6,6,6,8,8,8,10,10,10))

pl <- as_ggplot(arrangeGrob(grobs = pl, layout_matrix = lay))

pl <- pl + draw_plot_label(label =letters[1:7], size = 15,
                x = c(0, 0, 0.5, 0,0.5,0,0.5), y = c(1, 0.75, 0.75, 0.5,0.5,0.25,0.25))

pl

ggsave(filename = paste0("./plots/Balance_conc_",
                         BALANCE_MODE,
                         "/flowdiags_",
                         EXTRACT,"_",ELEMENT,
                         "2.pdf"),
       plot = pl,
       width = 14, height = 20, units = "cm", dpi = 300)

ggsave(filename = paste0("./plots/Balance_conc_",
                         BALANCE_MODE,
                         "/flowdiags_",
                         EXTRACT,"_",ELEMENT,
                         "2.png"),
       plot = pl,
       width = 14, height = 20, units = "cm", dpi = 600)

