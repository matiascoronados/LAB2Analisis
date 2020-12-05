
library(tidyverse)
library(ggplot2)
library(GGally)
library(psych)
library(corrplot)
#PDF
#https://www.researchgate.net/publication/311950799_Analysis_of_the_Wisconsin_Breast_Cancer_Dataset_and_Machine_Learning_for_Breast_Cancer_Detection/link/5864757e08ae329d6203aa82/download


url_datos <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'

datos.duros <- read.csv(url_datos)
data <- data.frame(datos.duros)

colnames(data) <- c("id",
                    "clump.thickness",
                    "unif.cell.size",
                    "unif.cell.shape",
                    "marg.adhesion",
                    "epith.cell.size",
                    "bare.nuclei",
                    "bland.chroma",
                    "norm.nucleoli",
                    "mitoses",
                    "class")


bool.values <- data$bare.nuclei=='?'
data <- data[!bool.values,]

#Para confirmar que se eliminaron los ?
sum(data$bare.nuclei=='?')


dd <- data

#En el caso de que se quiera cambiar los 4 y 2, por M y B
dd$class <- replace(dd$class,dd$class==2,'Benigno')
dd$class <- replace(dd$class,dd$class==4,'Maligno')

#Se aplica factor
dd[["class"]] <- factor(dd[["class"]])
data[["bare.nuclei"]] <- factor(data[["bare.nuclei"]])


#Se pasa a numerico los valores de bare nuclei.
data$bare.nuclei <- as.numeric(data$bare.nuclei)
data$class <- as.numeric(data$class)

#Se elimina la columna ID
#dd$id <- NULL
data$id <- NULL


#VISTA GENERAL
ggpairs(data, aes(colour=class, alpha=0.4))


#GRAFICO DE CORRELACION
corrplot.mixed(cor(data),
               lower = "number", 
               upper = "circle",
               tl.col = "black")


#GRAFICO DE CAJAS

library(ggpubr)
library(cowplot)

boxplot.width =  ggboxplot(data = data, x = "class", y = "width", color = "class") + border()

#Grafico de cajas para clump.thickness

boxplot.clump.thickness =  ggboxplot(data = dd, x = "class", y = "clump.thickness", color = "class") + border()
ydens = axis_canvas(boxplot.clump.thickness, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.clump.thickness = insert_yaxis_grob(boxplot.clump.thickness, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.clump.thickness)


#Grafico de cajas para unif.cell.size

boxplot.unif.cell.size =  ggboxplot(data = dd, x = "class", y = "unif.cell.size", color = "class") + border()
ydens = axis_canvas(boxplot.unif.cell.size, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.unif.cell.size = insert_yaxis_grob(boxplot.unif.cell.size, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.unif.cell.size)


#Grafico de cajas para unif.cell.shape

boxplot.unif.cell.shape =  ggboxplot(data = dd, x = "class", y = "unif.cell.shape", color = "class") + border()
ydens = axis_canvas(boxplot.unif.cell.shape, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.unif.cell.shape = insert_yaxis_grob(boxplot.unif.cell.shape, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.unif.cell.shape)


#Grafico de cajas para marginal.adhesion

boxplot.marginal.adhesion =  ggboxplot(data = dd, x = "class", y = "marginal.adhesion", color = "class") + border()
ydens = axis_canvas(boxplot.marginal.adhesion, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.marginal.adhesion = insert_yaxis_grob(boxplot.marginal.adhesion, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.marginal.adhesion)

#Grafico de cajas para epith.cell.size

boxplot.epith.cell.size =  ggboxplot(data = dd, x = "class", y = "epith.cell.size", color = "class") + border()
ydens = axis_canvas(boxplot.epith.cell.size, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.epith.cell.size = insert_yaxis_grob(boxplot.epith.cell.size, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.epith.cell.size)


#Grafico de cajas para bare.nuclei

boxplot.bare.nuclei =  ggboxplot(data = dd, x = "class", y = "bare.nuclei", color = "class") + border()
ydens = axis_canvas(boxplot.bare.nuclei, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.bare.nuclei = insert_yaxis_grob(boxplot.bare.nuclei, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.bare.nuclei)


#Grafico de cajas para bland.chromatin

boxplot.bland.chromatin =  ggboxplot(data = dd, x = "class", y = "bland.chromatin", color = "class") + border()
ydens = axis_canvas(boxplot.bland.chromatin, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.bland.chromatin = insert_yaxis_grob(boxplot.bland.chromatin, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.bland.chromatin)


#Grafico de cajas para normal.nucleoli

boxplot.normal.nucleoli =  ggboxplot(data = dd, x = "class", y = "normal.nucleoli", color = "class") + border()
ydens = axis_canvas(boxplot.normal.nucleoli, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.normal.nucleoli = insert_yaxis_grob(boxplot.normal.nucleoli, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.normal.nucleoli)

#Grafico de cajas para mitoses

boxplot.mitoses =  ggboxplot(data = dd, x = "class", y = "mitoses", color = "class") + border()
ydens = axis_canvas(boxplot.mitoses, axis = "y", coord_flip = TRUE) + geom_density(data = dd, aes(x = clump.thickness, fill = class), alpha = 0.7, size = 0.2) + coord_flip()
boxplot.mitoses = insert_yaxis_grob(boxplot.mitoses, ydens, grid::unit(.2, "null"), position = "right")

ggdraw(boxplot.mitoses)


data.aux <- data

data$class <- replace(data$class,data$class==2,0)
data$class <- replace(data$class,data$class==4,1)

data$class <- factor(data$class)



total <- glm(class ~ ., family = binomial(link = "logit"), data = data)


nulo <- glm(
   class ~ 1,
   family = binomial(link = "logit"),
   data = data
)

modelo <- step(nulo, scope = list(lower = nulo, upper = total),
               direction = "forward", test = "LRT", trace = 1)


#Para analizar 

#El modelo creado con step, entrego el singuiente RLM

#class ~ unif.cell.size + clump.thickness + bland.chroma + 
#marg.adhesion + unif.cell.shape + norm.nucleoli + bare.nuclei + 
#  mitoses

#Este es similar al modelo completo, con la diferencia de que este ultimo contempla la variable epith.cell.size

#Para este tipo de modelo, es importante reconocer si las variables guaran una correlacion entre ellas.

#Debido al conocimiento previo que se tiene del tema, se sabe que la variable single epith cell size guarnda una 
#estrecha relacion con marginal adhesion.
#Ademas de lo anterior, se sabe que existe una relacion entre la forma, y tamaño de la celula.

#Por loa p






















