library(xlsx)
library(gWidgets)
library(gWidgetsRGtk2)
source('D:/Program Files/R/stepen.R')
source('D:/Program Files/R/balans.R')
source('D:/Program Files/R/Regr.R')
source('D:/Program Files/R/fullmodel.R')
source('D:/Program Files/R/logitplot.R')
source('D:/Program Files/R/logitquod.R')
source('D:/Program Files/R/logit_roc.R')
source('D:/Program Files/R/logit_area.R')
source('D:/Program Files/R/logitrocplot.R')
source('D:/Program Files/R/logitss.R') 
		
File <- function(filename) {
f <- filename
f1 <- unlist(strsplit(f, ".", fixed = TRUE))[2]		
ifelse (f1 == "xls", mydata <- read.xls(filename, sheetIndex = 1),
  ifelse (f1 == "xlsx", mydata <- read.xlsx(filename, sheetIndex = 1), 
    ifelse (f1 == "csv", mydata <- read.csv(filename, header=TRUE,sep="\t"),
      mydata <- read.delim(filename, header = TRUE, sep = "\t", dec = ".", fill = TRUE))))
return(mydata)
}
	
chooseFile <- function() {
x <- gfile("Select a file", type="open", filter = list("All files" = list(patterns = c("*")),
	"Excel files" = list(patterns = c("*.xls", "*.xlsx")),
        "CSV files" = list(patterns = c("*.csv"))),
	handler = function(h,...)
	mydata <- File(h$file))
x <- File(x)
return(x)
}
	
mbl <- list()
mbl$File$Open$handler = function(h,...) {
  mydata <- chooseFile()
  attach(mydata)
  obj2 <- gtable(mydata,container = w,expand=TRUE)
  a <- colnames(mydata)
  obj1 <- glabel("", container = group)
  svalue(obj1) <- paste("Количество наблюдений","=",nrow(mydata))
			
  obj4 <- gframe("Построение модели", container=group)
  obj <- gframe("Зависимые переменные", container=obj4)
  variables <- gradio(a, container=obj)
  obj <- gframe("Независимые переменные", container=obj4)
  invar <- gcheckboxgroup(a, container=obj)
		
  obj4 <- gframe("Альфа/Гамма внешнего критерия", container=group)
  A <- gedit(0.5, container = obj4)
  Y <- gedit(0.5, container = obj4)

  obj4 <- gframe("Соотношение по выборкам,%", container=group)
  nA <- gedit(60, container = obj4)
  nB <- gedit(30, container = obj4)
  nC <- gedit(10, container = obj4)

  obj4 <- gframe("Соотношение балансов в классах,%", container=group)
  N <- gedit(60, container = obj4)
  M <- gedit(40, container = obj4)
			
  obj4 <- gframe("Дополнительно", horizontal = FALSE, container=group)
  obj5 <- gframe("Свойства моделей", horizontal = TRUE, container=obj4)
  obj <- gframe("Количество моделей для вывода", container=obj5)
  nMod <- gedit(5, container = obj)
  obj <- gframe("Значение факторной переменной", container = obj5)
  zY<-gedit( , initial.msg="Введите значения через запятую", container = obj)
		
  objY <- gcheckboxgroup("Уравнять наблюдения в классах", checked = TRUE, container=obj4)
		
  objG <- gcheckboxgroup("Ввести расширеную матрицу переменных", checked = TRUE, container=obj4)
		
  obj <- gbutton("Старт", container = group, handler = function(h,...) {
    m <- svalue(invar)
    k <- m[1]
    for (i in 1:(length(m) - 1)) {
      k <- paste0(k, "+", m[i+1])
    }
    full.model <- as.formula(paste(svalue(variables), "~", k))
			
    y <- NULL
    for (i in 1:length(mydata)) { #заполняем значениями наши найденные переменные
      ifelse (colnames(mydata)[i] == svalue(variables), factor.var <- mydata[[i]], next)
      }
    zY <- svalue(zY)
    zY <- as.numeric(unlist(strsplit(zY, ",", fixed = TRUE)))
    x <- NULL
    y <- sample(0:0, nrow(mydata), replace = TRUE)
    for (j in 1:length(zY)) {
      for (i in 1:nrow(mydata)) {				#переводим факторную переменную в 0 и 1
        ifelse (factor.var[i] == zY[j], x[i]=1, x[i]=0)
      } y <- x + y 
    }
    
    for(i in 1:length(mydata)) {
      ifelse(mydata[[i]] == factor.var, mydata <- subset(mydata, select= -i),next)
    } 
    mydata <- cbind(mydata, y)
    mydata.exam <- mydata

    if (length(svalue(objY)) != 0) {
      mydata <- balans(mydata, y ,0, 1, as.numeric(svalue(N)), as.numeric(svalue(M)))
    }
    if (length(svalue(objG)) != 0) {
      mydata.exam <- stepen(mydata.exam, 2, 3, 1/2)
      mydata.exam <- cbind(mydata.exam, y) 
       if (length(svalue(objY)) != 0) {					
        mydata1 <- subset(mydata, select = -y)
       }
      mydata2 <- stepen(mydata1, 2, 3, 1/2)
      full.model <- full.m(full.model, mydata1, mydata2, mydata$y)
      mydata<-cbind(mydata2, mydata$y)
    }

    f <- regression(mydata, as.numeric(svalue(nA)), as.numeric(svalue(nB)),
      as.numeric(svalue(A)), as.numeric(svalue(Y)), full.model, as.numeric(svalue(nMod)))
    model   <- NULL
    in.krit <- NULL
    p.enter <- NULL
    p.leave <- NULL
    logit   <- NULL
    print(f)
    f1 <- unlist(strsplit(f, "|", fixed = TRUE))
    x <- c(1:4)
    for (i in 1:length(f1)) {
      model[i] <- f1[x[2]]
      in.krit[i] <- f1[x[1]]
      p.enter[i] <- f1[x[3]]
      p.leave[i] <- f1[x[4]]
      x <- x+4
    }

    tbl <- list(quit = list(icon = "print", handler = function(...) {
      winresult <- gwindow("Лучшие модели", visible = TRUE)
      group <- ggroup(horizontal = FALSE, visible = TRUE, container = winresult)
      gcombobox(model[1:as.numeric(svalue(nMod))], container = group, handler = function(h,...) {
        xnam <- paste0(svalue(h$obj))
	fmla <- as.formula(paste("y ", "~", paste(xnam, collapse= "+")))
	logit <- glm(fmla, data = mydata.exam, family ="binomial")
	oldVal <- svalue(widget2)
	nb1 <- gnotebook( width = 800, height = 600, container = nb, horizontal = TRUE, visible = FALSE)
	names(nb)[as.numeric(oldVal)] <- paste("Model", "", svalue(widget2))	
	svalue(widget2) <- as.numeric(oldVal) + 1
	for (i in 1:length(model)) {
	  ifelse (model[i] == svalue(h$obj), k <- i, next)
	}		
	frame <- gframe("Результаты модели", use.scrollwindow = TRUE, visible = FALSE, container = nb1, label = "Результаты модели")
        g2 <- ggroup(horizontal = FALSE, visible = TRUE, container = frame)	
	res1 <- glabel(paste("Модель", "-", logit[23]), container = g2, visible = TRUE, label = "Результаты модели")	
	res2 <- glabel(logit[1], container = g2, visible = TRUE)
	res3 <- glabel(paste("Внешний критерий", "-", in.krit[k]), container = g2, visible = TRUE)	
	res4 <- glabel(paste("Ошибка первого рода", "-", p.enter[k]), container = g2, visible = TRUE)	
	res5 <- glabel(paste("Ошибка второго рода", "-", p.leave[k]), container = g2, visible = TRUE)
	sf <- sort(fitted(logit), index=T)
	field.name <- attr(attr(terms(formula(logit)), "factors"), "dimnames")[[1]][1]
	eval(parse(text = paste("tmp <- ", ifelse(class(logit$data) == "data.frame", "logit$data$", ""), field.name, sep = "")))
	tn <- sum((!tmp[sf$ix]) & (sf$x < 0.5))
   	fn <- sum((!tmp[sf$ix]) & (sf$x >= 0.5))
  	tp <- sum(tmp[sf$ix] & (sf$x >= 0.5))
  	fp <- sum(tmp[sf$ix] & (sf$x < 0.5))
	res6 <- glabel(paste("Чувствительность",":",round(tp / (tp + fp), 4), "|||", 
	  "Специфичность", ":", round(tn / (tn + fn), 4)), container = g2)				
	res7 <- gbutton("Save", container = g2, visible = TRUE, handler = function (h,...) {
          gfile("Save file", type = "save", handler = function(h,...) {
	    write.csv(logit[3], file = paste0(h$file, ".", "csv"))
          })
	})
	devs <- lapply("Success of logistic model", function(i)
	  ggraphics(width = 800, height = 600, container = nb1, label = as.character(i)))
	par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
	pl1 <- logitplot(logit)

	devs <- lapply("Model success", function(i) 
	  ggraphics(width = 800, height = 600, container = nb1, label = as.character(i)))
	par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
	pl2 <- logitquad(logit)
	
	r <- logit.roc(logit)
	area <- logit.roc.area(r)
	devs <- lapply("ROC Curve", function(i) 
	  ggraphics(width = 800, height = 600, container = nb1, label = as.character(i)))
	par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), omi=c(0, 0, 0, 0))
	pl3 <- logit.roc.plot(r,area)

	devs <- lapply("Sensitivity and specificity vs. threshold", function(i) 
          ggraphics(width = 800, height = 600, container=nb1, label=as.character(i)))
	par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), omi = c(0, 0, 0, 0))
	pl4 <- logit.plot.ss(r)
	visible(nb1) <- TRUE
	})

	g <- ggroup(horizontal = TRUE, visible = FALSE, container = group)
	widget2 <- glabel(1, container = g, visible = FALSE)			
	nb <- gnotebook(width = 800, height = 600, container = group, horizontal = TRUE, visible = FALSE)
        })
      )
    gtoolbar(tbl, container=w)	
    })
  }
mbl$File$Open$icon = "open"	
mbl$File$Quit$handler = function(h,...) dispose(w)
mbl$File$Quit$icon = "quit"
mbl$Edit$Find$handler = function(h,...) print("Find")
mbl$Edit$Replace$handler = function(h,...) print("Replace")
 
w <- gwindow("Шаговая регрессия")
mb <- gmenu(mbl, container = w)
Big <- ggroup(use.scrollwindow = TRUE, cont = w)
group <- ggroup(horizontal = FALSE, container = Big)
		
	
