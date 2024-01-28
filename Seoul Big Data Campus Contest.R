library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(rgdal)
library(maptools)
library(dplyr)
library(DT)
library(cluster)
library(raster)
library(plotly)

if (!require(gpclib)) install.packages("gpclib", type = "source")
gpclibPermit()

category <- read.csv('C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/category.csv', header = T, sep = ',', Encoding('euc-kr'))
facility <- read.csv('C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/facility.csv', header = T, sep = ',', Encoding('euc-kr'))
area <- read.csv('C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/area.csv', header = T, sep = ',', Encoding('euc-kr'))

register_google(key = 'AIzaSyAXwMAM2aP2gEUY93P3DS-yoxMs7h_UsQA')
seoul <- get_map("Seoul, South Korea", zoom = 11, maptype = "roadmap")


map <- shapefile("C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
rate <- read.csv("C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/rate.csv", header = TRUE)
fac <- read.csv("C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/fac.csv", header = TRUE)
seoul_id <- read.csv("C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/seoul_id.csv", header = TRUE) 
xy <- read.csv("C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/xy.csv", header = TRUE) 
person <- read.csv('C:/Users/LG/Desktop/EC/CONTEST/2021 서울시 빅데이터 캠퍼스 공모전/2021 서울시 빅데이터 캠퍼스 공모전 Data/person.csv', header = T, sep = ',', Encoding('euc-kr'))
person <- left_join(person, seoul_id, by = "자치구") 
category <- left_join(category, seoul_id, by = "자치구") 

rate <- left_join(rate, seoul_id, by = "자치구") 

ui <- fluidPage(
  fluidRow(
    box(plotlyOutput("map"), width = 600, height = 500)),
  fluidRow(
    box(selectInput("mn","Selet Type", choices = c("합계","지체","시각","청각","지적+자폐+정신"), selected = "합계"))
  ),
  fluidRow(    
    box(plotlyOutput("people"), width = 600, height = 500)),
  fluidRow(    
    box(plotlyOutput("people2"), width = 600, height = 500)),
  fluidRow( 
    box(plotlyOutput("fac"), width = 600, height = 500)),
  fluidRow( 
    box(plotlyOutput("peoplot"), width = 600, height = 500)),
  fluidRow( 
    box(plotlyOutput("peoplot2"), width = 600, height = 500))
)


server <- function(input, output) {
  output$map <-  renderPlotly({
    seoulplot <- 
      ggmap(seoul) +
  
      geom_point(facility, 
                 mapping = aes(x = 경도, y = 위도,color = 시설종류상세명), shape = 2) +
    
      geom_point(lib, 
                 mapping = aes(x = 경도, y = 위도,color = "장애인도서관"))
  })
  
  output$fac <-  renderPlotly({
    fac <- left_join(fac, seoul_id, by = "자치구") 
    
    seoul_sum <- fac %>%
      group_by(id) %>%
      summarise(sum_n = n())
    
    facdata <<- seoul_sum
    
    
    M <- merge(seoul_map, seoul_sum, by = "id")
    P <- merge(xy, seoul_sum, by = "id")
    ggplot() +
      geom_polygon(data = M,
                   aes(x = long,
                       y = lat,
                       group = group,
                       fill = sum_n),
                   color = "white") +
      scale_fill_gradient(low = "Light Goldenrod",
                          high = "Dark Goldenrod",
                          space = "Lab",
                          guide = "colourbar") +
      geom_text(data = P,
                aes(x = lat,
                    y = long,
                    label = paste(piece, sum_n, sep = "\n")))
  })
  
  
  output$people <-  renderPlotly({
    if (input$mn == "합계") {
      seoul_sum <- category %>% 
        group_by(id, 자치구) %>%
        summarise(sum_n = sum(합계))
    }
    if (input$mn == "지체") {
      seoul_sum <- category %>% 
        group_by(id, 자치구) %>%
        summarise(sum_n = sum(지체))
    }
    if (input$mn == "뇌병변") {
      seoul_sum <- category %>% 
        group_by(id, 자치구) %>%
        summarise(sum_n = sum(뇌병변))
    }
    if (input$mn == "시각") {
      seoul_sum <- category %>% 
        group_by(id, 자치구) %>%
        summarise(sum_n = sum(시각))
    }
    if (input$mn == "청각") {
      seoul_sum <- category %>% 
        group_by(id, 자치구) %>%
        summarise(sum_n = sum(청각))
    }
    if (input$mn == "지적+자폐+정신") {
      seoul_sum <- category %>% 
        group_by(id, 자치구) %>%
        summarise(sum_n = sum(지적.자폐.정신))
    }
    
    
    
    M <- merge(seoul_map, seoul_sum, by = "id")
    P <- merge(xy, seoul_sum, by = "id")
    ggplot() + 
      geom_polygon(data = M, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = sum_n), 
                   color = "white") +
      scale_fill_gradient(low = "Light Goldenrod", 
                          high = "Dark Goldenrod",
                          space = "Lab", 
                          guide = "colourbar") + 
      geom_text(data = P, 
                aes(x = lat, 
                    y = long, 
                    label = paste(piece, sum_n, sep = "\n")))
    
  })
  output$people2 <-  renderPlotly({
    seoul_sum <- rate %>%
      group_by(id, 자치구) %>%
      summarise(sum_n = sum(계))
    
    peodata <<- seoul_sum
    
    M <- merge(seoul_map, seoul_sum, by = "id")
    P <- merge(xy, seoul_sum, by = "id")
    ggplot() +
      geom_polygon(data = M,
                   aes(x = long,
                       y = lat,
                       group = group,
                       fill = sum_n),
                   color = "white") +
      scale_fill_gradient(low = "Light Goldenrod",
                          high = "Dark Goldenrod",
                          space = "Lab",
                          guide = "colourbar") +
      geom_text(data = P,
                aes(x = lat,
                    y = long,
                    label = paste(piece, sum_n, sep = "\n")))
  })
  output$peoplot <-  renderPlotly({ ################장애인 거주시설 제외
    
    fac1 <- left_join(fac, seoul_id, by = "자치구") 
    fac1 <- fac1[!(fac1$시설종류상세명 == '장애인거주시설' ), ]
    
    seoul_sum <- fac1 %>%
      group_by(id) %>%
      summarise(sum_n = n())
    
    facdata <<- seoul_sum
    facdata <- facdata[-26,]
    
    dat = peodata$sum_n / facdata$sum_n
    a = data.frame(dat)
    df <- cbind(peodata, a)
    df$dat <- round(df$dat, 1)
    M <- merge(seoul_map, df, by = "id")
    P <- merge(xy, df, by = "id")
    ggplot() +
      geom_polygon(data = M,
                   aes(x = long,
                       y = lat,
                       group = group,
                       fill = dat),
                   color = "white") +
      scale_fill_gradient(low = "Light Goldenrod",
                          high = "Dark Goldenrod",
                          space = "Lab",
                          guide = "colourbar") +
      geom_text(data = P,
                aes(x = lat,
                    y = long,
                    label = paste(piece, dat, sep = "\n")))
  })
  output$peoplot2 <-  renderPlotly({ ################장애인 거주시설
    
    fac2 <- left_join(fac, seoul_id, by = "자치구") 
    fac2 <- fac2[fac2$시설종류상세명 == '장애인거주시설', ]
    
    
    seoul_sum <- fac2 %>%
      group_by(id) %>%
      summarise(sum_n = n())
    
    facdata <<- seoul_sum
    facdata <- facdata[-26,]
    
    dat = peodata$sum_n / facdata$sum_n
    a = data.frame(dat)
    df <- cbind(peodata, a)
    df$dat <- round(df$dat, 1)
    M <- merge(seoul_map, df, by = "id")
    P <- merge(xy, df, by = "id")
    ggplot() +
      geom_polygon(data = M,
                   aes(x = long,
                       y = lat,
                       group = group,
                       fill = dat),
                   color = "white") +
      scale_fill_gradient(low = "Light Goldenrod",
                          high = "Dark Goldenrod",
                          space = "Lab",
                          guide = "colourbar") +
      geom_text(data = P,
                aes(x = lat,
                    y = long,
                    label = paste(piece, dat, sep = "\n")))
  })
}

shinyApp(ui = ui, server = server)
