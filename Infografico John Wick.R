library(readxl)
library(ggplot2)

#Base de dados para estudo
Base_de_dados <- read_excel("John Wick Base de dados.xlsx")

#Vetores de armazenamentos
Scene <- unique(Base_de_dados$Scene)
Weapon <- unique(Base_de_dados$Weapon)
Body <- unique(Base_de_dados$Body)
Body <- Body[!is.na(Body)]
Accuracy <- unique(Base_de_dados$Accuracy)
Accuracy <- Accuracy[!is.na(Accuracy)]

#Variáveis de armazenamentos das extrações de informações
GlobalMortes <- data.frame(Weapon)
GlobalAcuracidade <- data.frame(Accuracy)
GlobalCorpo <- data.frame(Body)

#For responsável pelo filtro da Cenas
for (q in Scene) {
  Cena <- Base_de_dados[Base_de_dados$Scene == q,]

##Contagem de armas utilizadas
  Vector <- Cena$Weapon
  Mortes <- data.frame(Weapon, Count = rep(0,length(Weapon))) 
  Disparos <- data.frame(Weapon, Count = rep(0,length(Weapon)))
    for (j in Weapon) {
      p<-0
      for (i in Vector) {
        p<-p+1
        if (j == i && Cena[p,6] == 1) {
            y <- Mortes$Weapon == j
            Mortes[y,2] <- Mortes[y,2]+1
        }
      }  
    }
    

##Acuracidade dos disparos
  Vector <- Cena$Accuracy
  Vector <- Vector[!is.na(Vector)]  #Comando que remove os NA da lista
  Acuracidade <- data.frame(Accuracy, Count = rep(0,length(Accuracy)))
    for (j in Accuracy) {
      for (i in Vector) {
        if (j == i) {
          x <- Acuracidade$Accuracy == j
          Acuracidade[x,2] <- Acuracidade[x,2]+1
        }
      }
    }
  
##Contagem de Precisão
  Vector<-Cena$Body
  Vector<-Vector[!is.na(Vector)]
  Corpo <- data.frame(Body, Count = rep(0,length(Body)))
    for (j in Body) {
      for (i in Vector) {
        if (j == i) {
          x <- Corpo$Body == j
          Corpo[x,2] <- Corpo[x,2]+1
        }
      }
    }

#Armazenamento das informações
  GlobalMortes<-cbind(GlobalMortes, Mortes$Count)
  GlobalAcuracidade<-cbind(GlobalAcuracidade, Acuracidade$Count)
  GlobalCorpo<-cbind(GlobalCorpo, Corpo$Count)
}

#####################################################################
#Somatório das Base de Dados
j <- length(Scene)
for (i in GlobalMortes$Weapon) {
  x <- match(i,GlobalMortes$Weapon)
  GlobalMortes[x,j+2] <- sum(GlobalMortes[x,1:j+1])
}

for (i in GlobalAcuracidade$Accuracy) {
  x <- match(i,GlobalAcuracidade$Accuracy)
  GlobalAcuracidade[x,j+2] <- sum(GlobalAcuracidade[x,1:j+1])
}

for (i in GlobalCorpo$Body) {
  x <- match(i,GlobalCorpo$Body)
  GlobalCorpo[x,j+2] <- sum(GlobalCorpo[x,1:j+1])
}
#####################################################################
#Ajuste de Cabeçalhos
x <- c()
for (i in Scene) {
  x <- c(x, i)
}
colnames(GlobalAcuracidade)<- c("Accuracy",x, "Total")
colnames(GlobalCorpo)<- c("Body",x, "Total")
colnames(GlobalMortes)<- c("Weapon",x, "Total")

#####################################################################
#Criação dos gráficos
##John's House
###Gráfico Armas
p <- 1
q <- GlobalMortes$`John's House` != 0
Weapons = GlobalDisparos[q,1] 
Count = GlobalMortes[q,p+1]

data <- data.frame(Weapons, Count)
ggplot(data, aes(x = Count, y= Weapons))+
  geom_bar(stat = "identity", 
           color="White", 
           width = 0.9) +
  ggtitle(paste("Kill Count = ", sum(Count))) +
  ylab("")+
  xlab("")+
  geom_text(label=Count, vjust=-0.05, color="white", size=7.0)+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=14, 
                               colour = "White"),
        plot.title=element_text(size=19,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.y=element_text(colour =NA),
        legend.position = "none")

###Gráfico Acuracidade
q <- GlobalAcuracidade$`John's House` != 0
Category = GlobalAcuracidade[q ,1] 
Count = GlobalAcuracidade[q,p+1]

data <- data.frame(Category,Count)
data$fraction <- data$Count / sum(data$Count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$Category, "\n ", data$Count)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette = 6, direction = -1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  xlab("")+
  ylab("")+
  ggtitle(paste0("Shot Accuracy = ", trunc(data$fraction*100),"%")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text=element_text(colour = NA),
        plot.title=element_text(size=20,face="bold",hjust = 0.5, colour = "White"),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position = "none")

###Gráfico Precisão
q<- GlobalCorpo$`John's House` != 0
Body = GlobalCorpo[q,1] 
Count = GlobalCorpo[q,p+1]

data <- data.frame(Count, Body)
ggplot(data, aes(x = Body, y= Count))+
  geom_bar(stat = "identity", 
           color="black", 
           width = 0.9)+
  geom_text(label=Count, hjust=1.2, color="white", size=7.0)+
  ggtitle("Shots") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=15, 
                               colour = "White"),
        plot.title=element_text(size=20,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")

##Red Circle
###Gráfico Armas
p <- 2
q <- GlobalMortes$`Red Circle` != 0
Weapons = GlobalDisparos[q,1] 
Count = GlobalMortes[q,p+1]

data <- data.frame(Weapons, Count)
ggplot(data, aes(x = Count, y= Weapons))+
  geom_bar(stat = "identity", 
           color="White", 
           width = 0.9) +
  ggtitle(paste("Kill Count = ", sum(Count))) +
  ylab("")+
  xlab("")+
  geom_text(label=Count, vjust=-0.05, color="white", size=7.0)+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=14, 
                               colour = "White"),
        plot.title=element_text(size=19,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.y=element_text(colour =NA),
        legend.position = "none")

###Gráfico Acuracidade
q <- GlobalAcuracidade$`Red Circle` != 0
Category = GlobalAcuracidade[q ,1] 
Count = GlobalAcuracidade[q,p+1]

data <- data.frame(Category,Count)
data$fraction <- data$Count / sum(data$Count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$Category, "\n ", data$Count)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette = 6, direction = -1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  xlab("")+
  ylab("")+
  ggtitle(paste0("Shot Accuracy = ", trunc(data$fraction*100),"%")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text=element_text(colour = NA),
        plot.title=element_text(size=20,face="bold",hjust = 0.5, colour = "White"),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position = "none")

###Gráfico Precisão
q<- GlobalCorpo$`Red Circle` != 0
Body = GlobalCorpo[q,1] 
Count = GlobalCorpo[q,p+1]

data <- data.frame(Count, Body)
ggplot(data, aes(x = Body, y= Count))+
  geom_bar(stat = "identity", 
           color="black", 
           width = 0.9)+
  geom_text(label=Count, hjust=1.2, color="white", size=7.0)+
  ggtitle("Shots") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=15, 
                               colour = "White"),
        plot.title=element_text(size=20,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")

##The Church
###Gráfico Armas
p <- 3
q <- GlobalMortes$`The Church` != 0
Weapons = GlobalDisparos[q,1] 
Count = GlobalMortes[q,p+1]

data <- data.frame(Weapons, Count)
ggplot(data, aes(x = Count, y= Weapons))+
  geom_bar(stat = "identity", 
           color="White", 
           width = 0.9) +
  ggtitle(paste("Kill Count = ", sum(Count))) +
  ylab("")+
  xlab("")+
  geom_text(label=Count, vjust=-0.05, color="white", size=7.0)+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=14, 
                               colour = "White"),
        plot.title=element_text(size=19,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.y=element_text(colour =NA),
        legend.position = "none")

###Gráfico Acuracidade
q <- GlobalAcuracidade$`The Church` != 0
Category = GlobalAcuracidade[q ,1] 
Count = GlobalAcuracidade[q,p+1]

data <- data.frame(Category,Count)
data$fraction <- data$Count / sum(data$Count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$Category, "\n ", data$Count)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette = 6, direction = -1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  xlab("")+
  ylab("")+
  ggtitle(paste0("Shot Accuracy = ", trunc(data$fraction*100),"%")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text=element_text(colour = NA),
        plot.title=element_text(size=20,face="bold",hjust = 0.5, colour = "White"),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position = "none")

###Gráfico Precisão
q<- GlobalCorpo$`The Church` != 0
Body = GlobalCorpo[q,1] 
Count = GlobalCorpo[q,p+1]

data <- data.frame(Count, Body)
ggplot(data, aes(x = Body, y= Count))+
  geom_bar(stat = "identity", 
           color="black", 
           width = 0.9)+
  geom_text(label=Count, hjust=1.2, color="white", size=7.0)+
  ggtitle("Shots") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=15, 
                               colour = "White"),
        plot.title=element_text(size=20,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")

##The Complex
###Gráfico Armas
p <- 4
q <- GlobalMortes$`The Complex` != 0
Weapons = GlobalDisparos[q,1] 
Count = GlobalMortes[q,p+1]

data <- data.frame(Weapons, Count)
ggplot(data, aes(x = Count, y= Weapons))+
  geom_bar(stat = "identity", 
           color="White", 
           width = 0.9) +
  ggtitle(paste("Kill Count = ", sum(Count))) +
  ylab("")+
  xlab("")+
  geom_text(label=Count, vjust=-0.05, color="white", size=7.0)+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=14, 
                               colour = "White"),
        plot.title=element_text(size=19,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.y=element_text(colour =NA),
        legend.position = "none")

###Gráfico Acuracidade
q <- GlobalAcuracidade$`The Complex` != 0
Category = GlobalAcuracidade[q ,1] 
Count = GlobalAcuracidade[q,p+1]

data <- data.frame(Category,Count)
data$fraction <- data$Count / sum(data$Count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$Category, "\n ", data$Count)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette = 6, direction = -1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  xlab("")+
  ylab("")+
  ggtitle(paste0("Shot Accuracy = ", trunc(data$fraction*100),"%")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text=element_text(colour = NA),
        plot.title=element_text(size=20,face="bold",hjust = 0.5, colour = "White"),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position = "none")

###Gráfico Precisão
q<- GlobalCorpo$`The Complex` != 0
Body = GlobalCorpo[q,1] 
Count = GlobalCorpo[q,p+1]

data <- data.frame(Count, Body)
ggplot(data, aes(x = Body, y= Count))+
  geom_bar(stat = "identity", 
           color="black", 
           width = 0.9)+
  geom_text(label=Count, hjust=1.2, color="white", size=7.0)+
  ggtitle("Shots") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=15, 
                               colour = "White"),
        plot.title=element_text(size=20,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")

##The Dock
###Gráfico Armas
p <- 5
q <- GlobalMortes$`The Dock` != 0
Weapons = GlobalDisparos[q,1] 
Count = GlobalMortes[q,p+1]

data <- data.frame(Weapons, Count)
ggplot(data, aes(x = Count, y= Weapons))+
  geom_bar(stat = "identity", 
           color="White", 
           width = 0.9) +
  ggtitle(paste("Kill Count = ", sum(Count))) +
  ylab("")+
  xlab("")+
  geom_text(label=Count, vjust=-0.05, color="white", size=7.0)+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=14, 
                               colour = "White"),
        plot.title=element_text(size=19,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.y=element_text(colour =NA),
        legend.position = "none")

###Gráfico Acuracidade
q <- GlobalAcuracidade$`The Dock` != 0
Category = GlobalAcuracidade[q ,1] 
Count = GlobalAcuracidade[q,p+1]

data <- data.frame(Category,Count)
data$fraction <- data$Count / sum(data$Count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$Category, "\n ", data$Count)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette = 6, direction = -1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  xlab("")+
  ylab("")+
  ggtitle(paste0("Shot Accuracy = ", trunc(data$fraction*100),"%")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text=element_text(colour = NA),
        plot.title=element_text(size=20,face="bold",hjust = 0.5, colour = "White"),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position = "none")

###Gráfico Precisão
q<- GlobalCorpo$`The Dock` != 0
Body = GlobalCorpo[q,1] 
Count = GlobalCorpo[q,p+1]

data <- data.frame(Count, Body)
ggplot(data, aes(x = Body, y= Count))+
  geom_bar(stat = "identity", 
           color="black", 
           width = 0.9)+
  geom_text(label=Count, hjust=1.2, color="white", size=7.0)+
  ggtitle("Shots") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=15, 
                               colour = "White"),
        plot.title=element_text(size=20,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")

##John Wick
###Gráfico Armas
p <- 6
Weapons = GlobalMortes[,1] 
Count = GlobalMortes[,p+1]

data <- data.frame(Weapons, Count)
ggplot(data, aes(y = Count, x= Weapons))+
  geom_bar(stat = "identity", 
           color="White", 
           width = 0.9) +
  ylab("")+
  xlab("")+
  geom_text(label=Count, hjust=-0.3, color="white", size=7.0)+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=14, 
                               colour = "White"),
        plot.title=element_text(size=19,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")

###Gráfico Acuracidade
Category = GlobalAcuracidade[ ,1] 
Count = GlobalAcuracidade[,p+1]

data <- data.frame(Category,Count)
data$fraction <- data$Count / sum(data$Count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$Category, "\n ", data$Count)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette = 6, direction = -1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  xlab("")+
  ylab("")+
  ggtitle(paste0("Shot Accuracy = ", trunc(data$fraction*100),"%")) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), 
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text=element_text(colour = NA),
        plot.title=element_text(size=20,face="bold",hjust = 0.5, colour = "White"),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position = "none")

###Gráfico Precisão
Body = GlobalCorpo[,1] 
Count = GlobalCorpo[,p+1]

length(GlobalCorpo[,p+1])

data <- data.frame(Count, Body)
ggplot(data, aes(x = Body, y= Count))+
  geom_bar(stat = "identity", 
           color="white", 
           width = 0.9)+
  geom_text(label=Count, hjust=1, color="white", size=7.0)+
  ggtitle("Shots") +
  ylab("")+
  xlab("")+
  coord_flip()+
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = NA), 
        plot.background = element_rect(fill = "transparent", 
                                       colour = NA),
        axis.text=element_text(size=15, 
                               colour = "White"),
        plot.title=element_text(size=20,face="bold",
                                hjust = 0.5, 
                                colour = "White"),
        panel.grid = element_line(colour = NA),
        axis.text.x=element_text(colour =NA),
        legend.position = "none")
