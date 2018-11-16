#connect to Shinny App
library(rsconnect)
rsconnect::setAccountInfo(name='zhiyongyici',
                          token='4D8A55365A7547C9F03C3AD815C2E850',
                          secret='7eCcVp+Yevd6fINkmNq+ZboRm8sIMDvmXK9tuvXq')


#data preparation
library(tidyverse)
library(readxl)
library(magrittr)
library(maps)
library(leaflet)
dono <- read_csv("11-5 MASSCONTRIBUTIONS-csv.csv")
donor_Q08aa <- dono %>% group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))

dmap <- spread(donor_Q08aa, party, party)

#dim(dmap)

dmap_stR <- dmap %>% filter(R==R)

dmap_stD <- dmap %>% filter(D==D)

dmap_stI <- dmap %>% filter(I==I)

###################################### Republican donor map

dmapR <- dmap_stR %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

### usa <- map_data("usa")

states <- map_data("state")
states %<>% select(long,lat,group,order,region) %>% rename(state=region)

st_name <- unique(states$state)

st_abrev <- dmapR$state

st <- read_csv("states.csv")
st %<>% rename(state=st_name)

states <- left_join(states, st, by="state")

dmapR %<>% rename(st_abrev=state)

states <- left_join(states, dmapR, by="st_abrev")
states$Donors <- as.character(states$Donors)

states %<>% select(state,Donations,Donors) %>%
  unique()





ui <- fluidPage("Political Party Donations", id="nav",
                theme = "bootstrap.css",
                tabPanel("mapping",
                         div(class="outer",
                             leafletOutput("map", width="100%", height = 585),
                             absolutePanel(top = 70,right = 50, width = 200,
                                           h2("Search"), 
                                           selectInput("var","Select Political Party",
                                                       choices=c("Democrats","Republican","Independent")),
                                           checkboxGroupInput("group", "Group Members", c("Qixuan Zhang", "Tingrui Huang", "Si Chen","Chaoqun Yin"),
                                                              selected = c("Jianhao Yan", "Xiang Xu", "Jing(Mira) Tang","Ningze(Summer) Zu"))
                             )))
                
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    library(tigris)
    states <- states(cb=T)
    
    if (input$var == "Democrats") {
      
      d_join <- geo_join(states, dmap_stD, "STUSPS", "state")
      pal <- colorNumeric("Greens", domain = states$total)
      popup_sb <- paste0("<strong>", d_join$state, 
                         "</strong><br />Donations: ", d_join$total,
                         "<br />Donors: ", 
                         as.character(d_join$num))
      leaflet() %>%
        addTiles() %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = d_join , 
                    fillColor = ~pal(d_join$total), 
                    fillOpacity = 0.7, 
                    weight = 0.4, 
                    smoothFactor = 0.2,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = d_join$total, 
                  position = "bottomright", 
                  title = "Donations")
      
      
      
      
    } else if (input$var == "Republican") {
      
      r_join <- geo_join(states, dmap_stR, "STUSPS", "state")
      pal <- colorNumeric("Oranges", domain=r_join$total)
      popup_sb <- paste0("<strong>", r_join$state, 
                         "</strong><br />Donations: ", r_join$total,
                         "<br />Donors: ", 
                         as.character(r_join$num))
      leaflet() %>%
        addTiles() %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = r_join , 
                    fillColor = ~pal(r_join$total), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = r_join$total, 
                  position = "bottomright", 
                  title = "Donations")
      
    } else if (input$var == "Independent") {
      
      i_join <- geo_join(states, dmap_stI, "STUSPS", "state")
      pal <- colorNumeric("Blues", domain=i_join$total)
      popup_sb <- paste0("<strong>", i_join$state, 
                         "</strong><br />Donations: ", i_join$total,
                         "<br />Donors: ", 
                         as.character(i_join$num))
      leaflet() %>%
        addTiles() %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = i_join , 
                    fillColor = ~pal(i_join$total), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = i_join$total, 
                  position = "bottomright", 
                  title = "Donations")
      
    }
    
    
    
  })
  
}
shinyApp(ui=ui,server=server)



