# Nuevo comentario 1234
# Leer datos de IdG
setwd("/data/WTAC/R/TORO_2/")
# librería para plot
#require(ggplot2)

# para los manejadores de los gráficos, se usa la función manipulate()
library(manipulate)

# librería para los ejes del ggplot
require(scales)

# librería para masajear los datos
library(gdata)

# librería para conexión con BBDD
library(RODBC)

# Para colores
library(RColorBrewer)

# Para manipulación de fechas, como la función floor_date()
library(lubridate)

# Para hacer el forecast de series temporales
library(forecast)

# Para usar la función monthDays() que da el número de días del mes de una fecha
library(Hmisc)

# Para hacer dcast y melt
library(reshape2)

# manipulación de data frames de rstudio
library(tidyr)
library(dplyr)

options(java.parameters = "-Xmx64000m")
library(xlsx)
detach("package:xlsx", unload = TRUE)
library(openxlsx)

# Para usar la función na.locf que rellena los valores faltantes
library(zoo)

# Para usar la función word que separa palabras
#library(stringr)

# para usar la función shift
library(binhf)
library(data.table)

library("plotly", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")
library(shiny)

f_ini <- as.Date("2014-01-01")
# Lectura de las tablas necesarias
odbcDataSources("all")
WTACv2 <- odbcConnect(dsn = "myodbcWTACv2", uid = "comun", pwd = "wtacwtac")

db.table.name <- "TORO_REAL_LIGHT"
query <- paste("select * from ",db.table.name,"where YEAR > ",year(f_ini)," or (YEAR = ",year(f_ini),"and MONTH > ",
               month(f_ini),")")

# Se pone la tabla en el data frame DPI_traf_prot, para el tráfico por protocolo
toro_real <- sqlQuery(WTACv2,query,errors = TRUE)


db.table.name <- "TORO_BGT_LIGHT"
query <- paste("select * from ",db.table.name,"where YEAR > ",year(f_ini)," or (YEAR = ",year(f_ini),"and MONTH > ",
               month(f_ini),")")

# Se pone la tabla en el data frame DPI_traf_prot, para el tráfico por protocolo
toro_bgt <- sqlQuery(WTACv2,query,errors = TRUE)

db.table.name <- "TORO_PDC_LIGHT"
query <- paste("select * from ",db.table.name,"where YEAR > ",year(f_ini)," or (YEAR = ",year(f_ini),"and MONTH > ",
               month(f_ini),")")

# Se pone la tabla en el data frame DPI_traf_prot, para el tráfico por protocolo
toro_qfc <- sqlQuery(WTACv2,query,errors = TRUE)


toro_bgt$EURO_FXBGT <- toro_bgt$EURO
toro_bgt$EURO_FXQFC <- 0

toro_real$VERSION <- "REAL"
toro_qfc$VERSION <- "QFC"
toro_bgt$VERSION <- "BGT"

toro <- bind_rows(toro_bgt,toro_qfc,toro_real)

# Resumen para cada capa/concepto
toro_idg <- toro %>% group_by(COUNTRY,DATETIME,YEAR,MONTH,RED,TIPO_COSTE,CONCEPT,VERSION) %>%
  summarise(
    EU=sum(EURO),
    EU_FXPRY=sum(EURO_FXPREVYEAR),
    EU_FXBGT=sum(EURO_FXBGT),
    EU_FXQFC=sum(EURO_FXQFC),
    LC=sum(LOCAL_CURRENCY)
  )

# Cálculo del YTD
toro_idg <- toro_idg %>% group_by(COUNTRY,YEAR,RED,TIPO_COSTE,CONCEPT,VERSION) %>% arrange(MONTH) %>%
  mutate(
    EU_YTD=cumsum(EU),
    EU_FXPRY_YTD=cumsum(EU_FXPRY),
    EU_FXBGT_YTD=cumsum(EU_FXBGT),
    EU_FXQFC_YTD=cumsum(EU_FXQFC),
    LC_YTD=cumsum(LC)
  )

toro_idg_real <- filter(toro_idg,VERSION=="REAL")
toro_idg_real_1Y <- toro_idg_real
toro_idg_real_1Y$YEAR <- toro_idg_real_1Y$YEAR+1
toro_idg_real_1Y$DATETIME <- as.Date(paste(toro_idg_real_1Y$YEAR,"-",toro_idg_real_1Y$MONTH,"-01",sep=""))
toro_idg_real_1Y$EU_1Y <- toro_idg_real_1Y$EU
toro_idg_real_1Y$EU_YTD_1Y <- toro_idg_real_1Y$EU_YTD
toro_idg_real_1Y$LC_1Y <- toro_idg_real_1Y$LC
toro_idg_real_1Y$LC_YTD_1Y <- toro_idg_real_1Y$LC_YTD
toro_idg_real <- full_join(toro_idg_real,
                           toro_idg_real_1Y[,c("COUNTRY","DATETIME","YEAR","MONTH",
                                               "RED","TIPO_COSTE","CONCEPT",
                                               "EU_1Y","EU_YTD_1Y",
                                               "LC_1Y","LC_YTD_1Y")])

toro_idg_qfc <- filter(toro_idg,VERSION=="QFC")
toro_idg_qfc <- full_join(toro_idg_qfc,
                          toro_idg_real_1Y[,c("COUNTRY","DATETIME","YEAR","MONTH",
                                              "RED","TIPO_COSTE","CONCEPT",
                                              "EU_1Y","EU_YTD_1Y",
                                              "LC_1Y","LC_YTD_1Y")])

toro_idg_bgt <- filter(toro_idg,VERSION=="BGT")
toro_idg_bgt <- full_join(toro_idg_bgt,
                          toro_idg_real_1Y[,c("COUNTRY","DATETIME","YEAR","MONTH",
                                              "RED","TIPO_COSTE","CONCEPT",
                                              "EU_1Y","EU_YTD_1Y",
                                              "LC_1Y","LC_YTD_1Y")])



toro_idg <- full_join(toro_idg_qfc,toro_idg_bgt,
                 by=c("COUNTRY","DATETIME","YEAR","MONTH","RED","TIPO_COSTE","CONCEPT"),
                 suffix=c(".QFC",".BGT"))

toro_idg <- full_join(toro_idg,toro_idg_real,
                 by=c("COUNTRY","DATETIME","YEAR","MONTH","RED","TIPO_COSTE","CONCEPT"),
                 suffix=c("",".REAL"))

toro_idg <- dplyr::filter(toro_idg,!COUNTRY%in%c("TEF","HISPAM","CAM"))

toro_idg$COUNTRY=as.factor(as.character(toro_idg$COUNTRY))

toro_idg[is.na(toro_idg)] <- 0

sel <- dplyr::filter(toro_idg,YEAR==2016,MONTH==12)

filtro=c("4G","3G")
df <- toro_idg
fecha=as.Date("2016-12-01")
columnY="COUNTRY"
columnF="CONCEPT"
columnX1="sum(EU_FXBGT_YTD-EU_YTD.BGT)"
columnX2="sum(EU_YTD)"
columnX3="sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y)"
columnX4="sum(LC_YTD)/sum(LC_YTD.BGT)"
so="plot1"

formatoM <- function(num,dig){
  return(paste(format(num/1e6,trim=TRUE,digits=dig,format="f",big.mark=",",
                      scientific=FALSE),"M",sep=""))
}

formatoP <- function(num,dig){
  return(paste(format(num*100,trim=TRUE,digits=dig),"%"))
}

negr <- function(cad){
  return(paste("<b>",cad,"</b>",sep=""))
}


grafica_comparativa<-function(df,columnY,columnF,filtro=NULL,so)
{
  
  df1 <- df[df[[columnF]] %in% filtro,] %>% group_by_(columnY) %>% summarise(
    DATETIME=first(DATETIME),
    TOTAL_YTD=sum(EU_YTD),
    YoY_YTD=sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y),
    Diff_YTD_REAL=sum(EU_FXPRY_YTD-EU_YTD_1Y),
    TOTAL_QFC=sum(EU_YTD.QFC),
    YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
    Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
    Diff_YTD_QFC=sum(EU_FXQFC_YTD-EU_YTD.QFC),
    Cov_YTD_QFC=sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC),
    TOTAL_BGT=sum(EU_YTD.BGT),
    YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
    Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
    Diff_YTD_BGT=sum(EU_FXBGT_YTD-EU_YTD.BGT),
    Cov_YTD_BGT=sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT)
  )
  
  tooltip <- paste(negr(paste("YTD",df1$DATETIME)),"</br></br>",
                   negr("Actual:"),formatoM(df1$TOTAL_YTD,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_YTD,1),
                   paste("(",formatoM(df1$Diff_YTD_REAL,1),")",sep=""),"</br></br>",
                   negr("QFC:"),formatoM(df1$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_QFC,1),
                   paste("(",formatoM(df1$Diff_QFC_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_YTD_QFC,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_YTD_QFC,1),"</br></br>",
                   negr("BGT:"),formatoM(df1$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_BGT,1),
                   paste("(",formatoM(df1$Diff_BGT_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_YTD_BGT,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_YTD_BGT,1),"</br>",
                   sep=""
  )
  
  
  formY=reformulate(columnY)
  
  
  # Graph 1
  formX1=~df1$Diff_YTD_BGT
  VAR1=df1$Diff_YTD_BGT
  p1 <- df1 %>% plot_ly(y = formY, x = formX1,hoverinfo="text",text=tooltip,source=so) %>%
    add_bars(showlegend=FALSE,orientation="h") %>%
    layout(barmode = "group") %>%
    add_annotations(xref="x3",yref="y",
                    y = formY, x = ((4*VAR1+1/0.25/4*abs(max(VAR1)-min(VAR1,0))*VAR1/abs(VAR1))),
                    text=paste(format(VAR1/1e6,
                                      trim=TRUE,digits=1,format="f",big.mark=","),"M",sep=""),
                    font=list(size=9),
                    showarrow=FALSE) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  
  # Graph 2
  formX2=~df1$TOTAL_YTD
  VAR2=df1$TOTAL_YTD
  p2 <- df1 %>% plot_ly(y = formY, x = formX2,hoverinfo="text",text=~tooltip,source=so) %>%
    add_trace(type="bar",showlegend=FALSE,orientation="h") %>%
    layout(xaxis=list(
      showline=TRUE),
      yaxis=list(zeroline=FALSE,showline=FALSE)) %>%
    add_annotations(xref="x4",yref="y",
                    y = formY, x = ((4*VAR2+abs(max(VAR2)-min(VAR2,0))*VAR2/abs(VAR2))),
                    text=paste(format(VAR2/1e6,trim=TRUE,digits=1,format="f",big.mark=","),"M",sep=""),
                    font=list(size=9),
                    showarrow=FALSE) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  # Graph 3
  VAR3=df1$Cov_YTD_BGT
  p3 <- df1 %>% plot_ly(x = 0,y = formY,source=so) %>%
    add_text(text=~paste(format(VAR3*100,trim=TRUE,digits=1),"%"),textposition="middle center",
             textfont=list(size=10),showlegend=FALSE) %>%
    add_markers(showlegend=FALSE,marker=list(opacity=0),hoverinfo="text",text=~tooltip) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  # Graph 4
  VAR4=df1$YoY_YTD
  p4 <- df1 %>% plot_ly(x = 0,y = formY,source=so) %>%
    add_text(text=~paste(format(VAR4*100,trim=TRUE,digits=1),"%"),textposition="middle center",
             textfont=list(size=10),showlegend=FALSE) %>%
    add_markers(showlegend=FALSE,marker=list(opacity=0),hoverinfo="text",text=~tooltip) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  
  p <- subplot(p3,p4,p1,p2,shareY=TRUE,nrows=1,widths=c(0.15,0.15,0.3,0.3))  %>%
    layout(title=negr("Budget analysis"),
           xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE),
           yaxis=list(title=negr(columnY),
                      showline=FALSE,showgrid=FALSE,zeroline=FALSE,
                      tickfont=list(size=9)),
           hovermode="closest") %>%
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x1",yref="y1",text=negr("%Cov"),showarrow=FALSE)  %>%
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x2",yref="y2",text=negr("YoY"),showarrow=FALSE) %>% 
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x4",yref="y4",text=negr("Executed"),showarrow=FALSE) %>%
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x3",yref="y3",text=negr("Deviation"),showarrow=FALSE)
  
  return(p)
}

grafica_comparativa1<-function(df,columnY,columnF,filtro=NULL,so)
{
  
  df1 <- df[df[[columnF]] %in% filtro,] %>% group_by_(columnY) %>% summarise(
    DATETIME=first(DATETIME),
    TOTAL_YTD=sum(EU_YTD),
    YoY_YTD=sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y),
    Diff_YTD_REAL=sum(EU_FXPRY_YTD-EU_YTD_1Y),
    TOTAL_QFC=sum(EU_YTD.QFC),
    YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
    Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
    Diff_YTD_QFC=sum(EU_FXQFC_YTD-EU_YTD.QFC),
    Cov_YTD_QFC=sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC),
    TOTAL_BGT=sum(EU_YTD.BGT),
    YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
    Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
    Diff_YTD_BGT=sum(EU_FXBGT_YTD-EU_YTD.BGT),
    Cov_YTD_BGT=sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT)
  )
  
  tooltip <- paste(negr(paste("YTD",df1$DATETIME)),"</br></br>",
                   negr("Actual:"),formatoM(df1$TOTAL_YTD,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_YTD,1),
                   paste("(",formatoM(df1$Diff_YTD_REAL,1),")",sep=""),"</br></br>",
                   negr("QFC:"),formatoM(df1$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_QFC,1),
                   paste("(",formatoM(df1$Diff_QFC_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_YTD_QFC,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_YTD_QFC,1),"</br></br>",
                   negr("BGT:"),formatoM(df1$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_BGT,1),
                   paste("(",formatoM(df1$Diff_BGT_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_YTD_BGT,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_YTD_BGT,1),"</br>",
                   sep=""
  )
  
#   sd <- SharedData$new(df1)
# 
#   p1 <- ggplot(sd,aes(x=COUNTRY,y=Diff_YTD_BGT)) + geom_bar(stat="identity",position="stack")
# #  + coord_flip()
#   
#   p2 <- ggplotly(p1) %>% highlight(on = "plotly_click",persistent=TRUE,color="red")
    
  formY=reformulate(columnY)
  
  
  # Graph 1
  formX1=~df1$Diff_YTD_BGT
  VAR1=df1$Diff_YTD_BGT
  p1 <- df1 %>% plot_ly(y = formY, x = formX1,hoverinfo="text",text=tooltip,source=so) %>%
    add_bars(showlegend=FALSE,orientation="h") %>%
    layout(barmode = "group") %>%
    add_annotations(xref="x3",yref="y",
                    y = formY, x = ((4*VAR1+1/0.25/4*abs(max(VAR1)-min(VAR1,0))*VAR1/abs(VAR1))),
                    text=paste(format(VAR1/1e6,
                                      trim=TRUE,digits=1,format="f",big.mark=","),"M",sep=""),
                    font=list(size=9),
                    showarrow=FALSE) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  
  # Graph 2
  formX2=~df1$TOTAL_YTD
  VAR2=df1$TOTAL_YTD
  p2 <- df1 %>% plot_ly(y = formY, x = formX2,hoverinfo="text",text=~tooltip,source=so) %>%
    add_trace(type="bar",showlegend=FALSE,orientation="h") %>%
    layout(xaxis=list(
      showline=TRUE),
      yaxis=list(zeroline=FALSE,showline=FALSE)) %>%
    add_annotations(xref="x4",yref="y",
                    y = formY, x = ((4*VAR2+abs(max(VAR2)-min(VAR2,0))*VAR2/abs(VAR2))),
                    text=paste(format(VAR2/1e6,trim=TRUE,digits=1,format="f",big.mark=","),"M",sep=""),
                    font=list(size=9),
                    showarrow=FALSE) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  # Graph 3
  VAR3=df1$Cov_YTD_BGT
  p3 <- df1 %>% plot_ly(x = 0,y = formY,source=so) %>%
    add_text(text=~paste(format(VAR3*100,trim=TRUE,digits=1),"%"),textposition="middle center",
             textfont=list(size=10),showlegend=FALSE) %>%
    add_markers(showlegend=FALSE,marker=list(opacity=0),hoverinfo="text",text=~tooltip) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  # Graph 4
  VAR4=df1$YoY_YTD
  p4 <- df1 %>% plot_ly(x = 0,y = formY,source=so) %>%
    add_text(text=~paste(format(VAR4*100,trim=TRUE,digits=1),"%"),textposition="middle center",
             textfont=list(size=10),showlegend=FALSE) %>%
    add_markers(showlegend=FALSE,marker=list(opacity=0),hoverinfo="text",text=~tooltip) %>%
    layout(xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
           yaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE))
  
  
  p <- subplot(p3,p4,p1,p2,shareY=TRUE,nrows=1,widths=c(0.15,0.15,0.3,0.3))  %>%
    layout(title=negr("Budget analysis"),
           xaxis=list(showline=FALSE,showgrid=FALSE,zeroline=FALSE),
           yaxis=list(title=negr(columnY),
                      showline=FALSE,showgrid=FALSE,zeroline=FALSE,
                      tickfont=list(size=9)),
           hovermode="closest") %>%
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x1",yref="y1",text=negr("%Cov"),showarrow=FALSE)  %>%
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x2",yref="y2",text=negr("YoY"),showarrow=FALSE) %>% 
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x4",yref="y4",text=negr("Executed"),showarrow=FALSE) %>%
    add_annotations(x=0,y=length(unique(df[[columnY]]))+1,xref="x3",yref="y3",text=negr("Deviation"),showarrow=FALSE)
  
  return(p)
}


grafica_mensual<-function(df,columnF,filtro=NULL,fecha)
{
  
  df1 <- df[df[[columnF]] %in% filtro & df$DATETIME > fecha-365 & 
              year(df$DATETIME)==year(fecha),] %>% 
    group_by(DATETIME) %>%
    summarise(
      TOTAL=sum(EU_FXBGT),
      YoY=sum(EU_FXPRY-EU_1Y)/sum(EU_1Y),
      Diff_REAL=sum(EU_FXPRY-EU_1Y),
      TOTAL_QFC=sum(EU.QFC),
      YoY_QFC=sum(EU_FXPRY.QFC-EU_1Y)/sum(EU_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY.QFC-EU_1Y),
      Diff_QFC=sum(EU_FXQFC-EU.QFC),
      Cov_QFC=sum(EU_FXQFC)/sum(EU.QFC),
      TOTAL_BGT=sum(EU.BGT),
      YoY_BGT=sum(EU_FXPRY.BGT-EU_1Y)/sum(EU_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY.BGT-EU_1Y),
      Diff_BGT=sum(EU_FXBGT-EU.BGT),
      Cov_BGT=sum(EU_FXBGT)/sum(EU.BGT)
    )
  
  tooltip <- paste(negr(paste("YTD",df1$DATETIME)),"</br></br>",
                   negr("Actual:"),formatoM(df1$TOTAL,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY,1),
                   paste("(",formatoM(df1$Diff_REAL,1),")",sep=""),"</br>",
                   negr("QFC:"),formatoM(df1$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_QFC,1),
                   paste("(",formatoM(df1$Diff_QFC_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_QFC,1),"</br><",
                   "%Cov:",formatoP(df1$Cov_QFC,1),"</br></br>",
                   negr("BGT:"),formatoM(df1$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_BGT,1),
                   paste("(",formatoM(df1$Diff_BGT_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_BGT,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_BGT,1),"</br>",
                   sep=""
  )
  
  
  
  # Graph 
  p <- df1 %>% plot_ly(y = ~TOTAL_BGT, x = ~DATETIME,hoverinfo="text",text=tooltip) %>%
    add_lines(showlegend=FALSE,
              line=list(color="grey")) %>%
    add_bars(x=~DATETIME,y=~TOTAL,hoverinfo="text",text=tooltip,marker=list(color="green"),
             showlegend=FALSE) %>%
    add_annotations(xref="x",yref="y",
                    y = ~TOTAL/2, x = ~DATETIME,
                    text=~paste(format(TOTAL/1e6,
                                       trim=TRUE,digits=1,format="f",big.mark=","),"M",sep=""),
                    font=list(size=9,color="white"),
                    showarrow=FALSE) %>%
    layout(xaxis=list(showline=TRUE,showgrid=FALSE,
                      range=list(as.Date(paste(year(fecha),"01","01",sep="-")),
                                 as.Date(paste(year(fecha),"12","01",sep="-")))),
           yaxis=list(showline=TRUE,showgrid=TRUE),
           hovermode="closest")
  
  
  return(p)
}

grafica_mensual_acum<-function(df,columnF,filtro=NULL,fecha)
{
  
  df1 <- df[df[[columnF]] %in% filtro & df$DATETIME > fecha-365 & 
              year(df$DATETIME)==year(fecha),] %>% 
    group_by(DATETIME) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  if (sum(as.numeric(df1$TOTAL_YTD==0))>0) df1[df1$TOTAL_YTD==0,]$TOTAL_YTD=NA
  
  tooltip <- paste(negr(paste("YTD",df1$DATETIME)),"</br></br>",
                   negr("Actual:"),formatoM(df1$TOTAL_YTD,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_YTD,1),
                   paste("(",formatoM(df1$Diff_YTD_REAL,1),")",sep=""),"</br></br>",
                   negr("QFC:"),formatoM(df1$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_QFC,1),
                   paste("(",formatoM(df1$Diff_QFC_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_YTD_QFC,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_YTD_QFC,1),"</br></br>",
                   negr("BGT:"),formatoM(df1$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                   negr("YoY:"),formatoP(df1$YoY_BGT,1),
                   paste("(",formatoM(df1$Diff_BGT_REAL,1),")",sep=""),"</br>",
                   "Diff:",formatoM(df1$Diff_YTD_BGT,1),"</br>",
                   "%Cov:",formatoP(df1$Cov_YTD_BGT,1),"</br>",
                   sep=""
  )
  
  
  
  # Graph 1
  p1 <- df1 %>% plot_ly(x = ~DATETIME,y = ~Diff_YTD_BGT, hoverinfo="text",text=tooltip) %>%
    add_lines(showlegend=FALSE,
              line=list(color="green")) %>%
    layout(title=negr("Year to Date Deviation"),
           xaxis=list(showline=TRUE,showgrid=FALSE,
                      range=c(as.numeric(as.POSIXct(paste(year(fecha),"01","01",sep="-"), format="%Y-%m-%d"))*1000,
                              as.numeric(as.POSIXct(paste(year(fecha),"12","31",sep="-"), format="%Y-%m-%d"))*1000),
                      domain=c(0,1)),
           yaxis=list(showline=TRUE,showgrid=TRUE),
           hovermode="closest")
  
  # Graph 2
  p2 <- df1 %>% plot_ly(x = ~DATETIME,y = ~TOTAL_BGT, hoverinfo="text",text=tooltip) %>%
    add_lines(showlegend=FALSE,
              line=list(color="grey")) %>%
    add_trace(x=~DATETIME,y=~TOTAL_YTD,hoverinfo="text",text=tooltip,
              type="scatter",mode="lines+markers",marker=list(color="blue"),
              line=list(color="blue"),
              showlegend=FALSE) %>%
    layout(title=negr("Year to Date"),
           xaxis=list(showline=TRUE,showgrid=FALSE,
                      range=c(as.numeric(as.POSIXct(paste(year(fecha),"01","01",sep="-"), format="%Y-%m-%d"))*1000,
                              as.numeric(as.POSIXct(paste(year(fecha),"12","31",sep="-"), format="%Y-%m-%d"))*1000),
                      domain=c(0,1)),
           yaxis=list(showline=TRUE,showgrid=TRUE),
           hovermode="closest")
  
  # Subplots
  p <- subplot(p1,p2,shareY=FALSE,nrows=1,widths=c(0.5,0.5))  %>%
    layout(title=negr("Cost Evolution"),
           hovermode="closest") %>%
    add_annotations(x=0.15,y=1.02,xref="paper",yref="paper",text=negr("Year to Date Deviation"),showarrow=FALSE)  %>%
    add_annotations(x=0.8,y=1.02,xref="paper",yref="paper",text=negr("Year to Date"),showarrow=FALSE) 
  
  return(p)
}

fecha=as.Date("2016-12-01")

grafica_nw_cost_donut<-function(df,fecha)
{
  
  df1 <- df[df$DATETIME > fecha-365 & 
              year(df$DATETIME)==year(fecha),] %>%
    group_by(TIPO_COSTE) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  
  df_c <- df[df$DATETIME > fecha-365 & 
               year(df$DATETIME)==year(fecha) & df$TIPO_COSTE=="CAPEX",] %>% 
    group_by(TIPO_COSTE) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  df_o <- df[df$DATETIME > fecha-365 & 
               year(df$DATETIME)==year(fecha) & df$TIPO_COSTE=="OPEX",] %>% 
    group_by(TIPO_COSTE) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  
  df_t <- df[df$DATETIME > fecha-365 & 
               year(df$DATETIME)==year(fecha),] %>% 
    group_by() %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  
  tooltip_c <- paste(negr(paste("YTD",fecha)),"</br></br>",
                     negr("Actual:"),formatoM(df_c$TOTAL_YTD,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_c$YoY_YTD,1),
                     paste("(",formatoM(df_c$Diff_YTD_REAL,1),")",sep=""),"</br></br>",
                     negr("QFC:"),formatoM(df_c$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_c$YoY_QFC,1),
                     paste("(",formatoM(df_c$Diff_QFC_REAL,1),")",sep=""),"</br>",
                     "Diff:",formatoM(df_c$Diff_YTD_QFC,1),"</br>",
                     "%Cov:",formatoP(df_c$Cov_YTD_QFC,1),"</br></br>",
                     negr("BGT:"),formatoM(df_c$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_c$YoY_BGT,1),
                     paste("(",formatoM(df_c$Diff_BGT_REAL,1),")",sep=""),"</br>",
                     "Diff:",formatoM(df_c$Diff_YTD_BGT,1),"</br>",
                     "%Cov:",formatoP(df_c$Cov_YTD_BGT,1),"</br>",
                     sep=""
  )
  
  tooltip_o <- paste(negr(paste("YTD",fecha)),"</br></br>",
                     negr("Actual:"),formatoM(df_o$TOTAL_YTD,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_o$YoY_YTD,1),
                     paste("(",formatoM(df_o$Diff_YTD_REAL,1),")",sep=""),"</br></br>",
                     negr("QFC:"),formatoM(df_o$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_o$YoY_QFC,1),
                     paste("(",formatoM(df_o$Diff_QFC_REAL,1),")",sep=""),"</br>",
                     "Diff:",formatoM(df_o$Diff_YTD_QFC,1),"</br>",
                     "%Cov:",formatoP(df_o$Cov_YTD_QFC,1),"</br></br>",
                     negr("BGT:"),formatoM(df_o$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_o$YoY_BGT,1),
                     paste("(",formatoM(df_o$Diff_BGT_REAL,1),")",sep=""),"</br>",
                     "Diff:",formatoM(df_o$Diff_YTD_BGT,1),"</br>",
                     "%Cov:",formatoP(df_o$Cov_YTD_BGT,1),"</br>",
                     sep=""
  )
  
  tooltip <-c(tooltip_c,tooltip_o)
  
  tooltip_t <- paste(negr(paste("YTD",fecha)),"</br></br>",
                     negr("Actual:"),formatoM(df_t$TOTAL_YTD,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_t$YoY_YTD,1),
                     paste("(",formatoM(df_t$Diff_YTD_REAL,1),")",sep=""),"</br></br>",
                     negr("QFC:"),formatoM(df_t$TOTAL_QFC,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_t$YoY_QFC,1),
                     paste("(",formatoM(df_t$Diff_QFC_REAL,1),")",sep=""),"</br>",
                     "Diff:",formatoM(df_t$Diff_YTD_QFC,1),"</br>",
                     "%Cov:",formatoP(df_t$Cov_YTD_QFC,1),"</br></br>",
                     negr("BGT:"),formatoM(df_t$TOTAL_BGT,0),"&nbsp;&nbsp;&nbsp;&nbsp;",
                     negr("YoY:"),formatoP(df_t$YoY_BGT,1),
                     paste("(",formatoM(df_t$Diff_BGT_REAL,1),")",sep=""),"</br>",
                     "Diff:",formatoM(df_t$Diff_YTD_BGT,1),"</br>",
                     "%Cov:",formatoP(df_t$Cov_YTD_BGT,1),"</br>",
                     sep=""
  )
  
  
  # Graph: donut
  p <- df1 %>% plot_ly(labels = ~TIPO_COSTE, values = ~TOTAL_YTD, type = 'pie',hole=0.5,sort=FALSE,
                       textinfo="none",hoverinfo="text",text=tooltip) %>%
    add_markers(x=0,y=0,xref="paper",yref="paper",opacity=0,showlegend=FALSE,marker=list(symbol="200",opacity=0,size=80,color="white"),
                hoverinfo="text",text=tooltip_t) %>%
    layout(title=negr("Network cost (FX YTD)"),
           hovermode="closest",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}

grafica_nw_cost_cov<-function(df,fecha)
{
  
  df1 <- df[df$DATETIME > fecha-365 & 
              year(df$DATETIME)==year(fecha),] %>%
    group_by(TIPO_COSTE) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  
  df_c <- df[df$DATETIME > fecha-365 & 
               year(df$DATETIME)==year(fecha) & df$TIPO_COSTE=="CAPEX",] %>% 
    group_by(TIPO_COSTE) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  df_o <- df[df$DATETIME > fecha-365 & 
               year(df$DATETIME)==year(fecha) & df$TIPO_COSTE=="OPEX",] %>% 
    group_by(TIPO_COSTE) %>%
    summarise(
      TOTAL_YTD=sum(EU_YTD),
      YoY_YTD=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y)/sum(EU_YTD_1Y) else NA,
      Diff_YTD_REAL=if (TOTAL_YTD>0) sum(EU_FXPRY_YTD-EU_YTD_1Y) else NA,
      TOTAL_QFC=sum(EU_YTD.QFC),
      TOTAL_YTD_FXQFC=sum(EU_FXQFC_YTD),
      YoY_QFC=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_QFC_REAL=sum(EU_FXPRY_YTD.QFC-EU_YTD_1Y),
      Diff_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD-EU_YTD.QFC) else NA,
      Cov_YTD_QFC=if (TOTAL_YTD>0) sum(EU_FXQFC_YTD)/sum(EU_YTD.QFC) else NA,
      TOTAL_BGT=sum(EU_YTD.BGT),
      TOTAL_YTD_FXBGT=sum(EU_FXBGT_YTD),
      YoY_BGT=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y)/sum(EU_YTD_1Y),
      Diff_BGT_REAL=sum(EU_FXPRY_YTD.BGT-EU_YTD_1Y),
      Diff_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD-EU_YTD.BGT) else NA,
      Cov_YTD_BGT=if (TOTAL_YTD>0) sum(EU_FXBGT_YTD)/sum(EU_YTD.BGT) else NA
    )
  
  
  
  
  tooltip_cov_bgt_c <- paste(negr(paste("YTD",fecha)),"</br>",
                             negr("CAPEX"),"</br></br>",
                             negr("BGT:"),formatoM(df_c$TOTAL_BGT,0),"</br></br>",
                             negr("Actual:"),formatoM(df_c$TOTAL_YTD_FXBGT,0),"</br>",
                             "Diff:",formatoM(df_c$Diff_YTD_BGT,1),"</br>",
                             "%Cov:",formatoP(df_c$Cov_YTD_BGT,1),"</br>",
                             sep=""
  )
  
  tooltip_cov_bgt_o <- paste(negr(paste("YTD",fecha)),"</br>",
                             negr("OPEX"),"</br></br>",
                             negr("BGT:"),formatoM(df_o$TOTAL_BGT,0),"</br></br>",
                             negr("Actual:"),formatoM(df_o$TOTAL_YTD_FXBGT,0),"</br>",
                             "Diff:",formatoM(df_o$Diff_YTD_BGT,1),"</br>",
                             "%Cov:",formatoP(df_o$Cov_YTD_BGT,1),"</br>",
                             sep=""
  )
  
  tooltip_cov_qfc_c <- paste(negr(paste("YTD",fecha)),"</br>",
                             negr("CAPEX"),"</br></br>",
                             negr("QFC:"),formatoM(df_c$TOTAL_QFC,0),"</br></br>",
                             negr("Actual:"),formatoM(df_c$TOTAL_YTD_FXQFC,0),"</br>",
                             "Diff:",formatoM(df_c$Diff_YTD_QFC,1),"</br>",
                             "%Cov:",formatoP(df_c$Cov_YTD_QFC,1),"</br>",
                             sep=""
  )
  
  tooltip_cov_qfc_o <- paste(negr(paste("YTD",fecha)),"</br>",
                             negr("OPEX"),"</br></br>",
                             negr("QFC:"),formatoM(df_o$TOTAL_QFC,0),"</br></br>",
                             negr("Actual:"),formatoM(df_o$TOTAL_YTD_FXQFC,0),"</br>",
                             "Diff:",formatoM(df_o$Diff_YTD_QFC,1),"</br>",
                             "%Cov:",formatoP(df_o$Cov_YTD_QFC,1),"</br>",
                             sep=""
  )
  
  # Graph 2 y 3: Coverages
  p1 <- df1 %>% plot_ly(type='bar',x = ~TIPO_COSTE,y = ~100*Cov_YTD_BGT,
                        hoverinfo="text",text=c(tooltip_cov_bgt_c,tooltip_cov_bgt_o),
                        marker=list(color=c("blue","green"))) %>%
    layout(title=negr("BGT"),
           xaxis=list(showline=FALSE,showgrid=FALSE,showticklabels = FALSE,title=""),
           yaxis=list(showline=FALSE,showgrid=TRUE,showticklabels = TRUE,title="",tickvals=c(100),
                      ticktext=c("100%"),gridwidth=3),
           hovermode="closest")
  
  p2 <- df1 %>% plot_ly(type='bar',x = ~TIPO_COSTE,y = ~100*Cov_YTD_QFC,
                        hoverinfo="text",text=c(tooltip_cov_qfc_c,tooltip_cov_qfc_o),
                        marker=list(color=c("blue","green"))) %>%
    layout(title=negr("QFC"),
           xaxis=list(showline=FALSE,showgrid=FALSE,showticklabels = FALSE,title=""),
           yaxis=list(showline=FALSE,showgrid=TRUE,showticklabels = TRUE,title="",tickvals=c(100),
                      ticktext=c("100%"),gridwidth=3),
           hovermode="closest")
  
  
  # Subplots
  p <- subplot(p1,p2,nrows=1,widths=c(0.5,0.5),heights=c(0.8))  %>%
    layout(title="",hovermode="closest",showlegend=FALSE) %>%
    add_annotations(x=0.5,y=1.08,xref="paper",yref="paper",text=negr("Coverage"),showarrow=FALSE) %>%
    add_annotations(x=0.2,y=1,xref="paper",yref="paper",text=negr("BGT"),showarrow=FALSE) %>%
    add_annotations(x=0.8,y=1,xref="paper",yref="paper",text=negr("QFC"),showarrow=FALSE) 
  
  return(p)
}



# plot1 <- grafica_comparativa(filter(toro_idg,DATETIME==as.Date("2016-12-01")),"COUNTRY","CONCEPT",c("4G"))
# 
# plot2 <- grafica_comparativa(filter(toro_idg,DATETIME==as.Date("2016-12-01")),"CONCEPT","COUNTRY",c("ES"))
# 
# plot3 <- grafica_mensual(toro_idg,"CONCEPT",filtro=c("3G","4G"),as.Date("2016-12-01"))
# 
# plot4 <- grafica_mensual(toro_idg,"CONCEPT",filtro=c("3G","4G"),as.Date("2017-12-01"))
# 
# plot5 <- grafica_mensual_acum(toro_idg,"CONCEPT",filtro=c("3G","4G"),as.Date("2016-12-01"))
# 
# plot6 <- grafica_mensual_acum(toro_idg,"CONCEPT",filtro=c("3G","4G"),as.Date("2017-12-01"))
# 
# plot7 <- grafica_nw_cost_donut(toro_idg,as.Date("2017-12-01"))
# 
# plot8 <- grafica_nw_cost_cov(toro_idg,as.Date("2017-12-01"))
