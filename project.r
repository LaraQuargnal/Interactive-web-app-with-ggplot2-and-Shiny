#install.packages("shiny")
library(shiny)

# User Interface / Korisničko sučelje
ui <- fluidPage(
  # Header
  titlePanel("Analiza učinkovitosti rada djelatnika"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Odabir između tri inputID-a. 
      selectInput(inputId="evalKriterij", 
                  label=h3("Kriterij"),
                  choices=c("Evaluacija rada prema strukturiranom obrascu ocjenjivanja"="ObrazacOcjenivanja",
                               "Financijski uspjeh prodajnih aktivnosti"="ProdajniUspjeh"), 
                  selected = 1),
      
      selectInput(inputId="preduvjet", 
                  label=h3("Odabrati varijablu"),
                  choices=c("Predznanje i Iskustvo"="PredznanjeIskustvo",
                               "Učinkovitost u radu"="RadniUcinak"), 
                  selected=1),
      
      selectInput(inputId="korelacija", 
                  label=h3("Klasifikacija djelatnika"),
                  choices=c("Godine"="Godine",
                               "Spol"="Spol",
                               "Strucna Sprema"="StrucnaSprema"), 
                  selected=1)      
    ),
    
    
    # MainPanel s tab-ovima
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Histogram", 
                           fluidRow(
                             column(4, plotOutput("histogram1")),
                             column(4, plotOutput("barchart")),
                             column(4, plotOutput("histogram2")),
                             column(6, plotOutput("histogram3")),
                             column(6, plotOutput("histogram4"))
                           )),
                  tabPanel("Raspršeni grafikon", 
                           fluidRow(plotOutput("rasprseni"))),
                  tabPanel("Model linearne regresije", verbatimTextOutput("summary")),
                  tabPanel("Interakcijski grafikon", 
                           fluidRow(plotOutput("interakcija"))),
                  tabPanel("Koeficijent regresije", verbatimTextOutput("koef.regresije"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(interactions)
  library(dplyr)
  
  # učitavanje dataframe-a iz .csv datoteke
  # df <- read_csv("kompanija.csv")
  
  # Data pre-processing
  df$PredznanjeIskustvo <- df$PredznanjeIskustvo - mean(df$PredznanjeIskustvo, na.rm=TRUE)
  df$RadniUcinak <- df$RadniUcinak - mean(df$RadniUcinak, na.rm=TRUE)
  
  # izračun godina, manje od 40 ili više od 40 godina
  df$Godine <- ifelse(df$Godine < 40, "ispod_40", "iznad_40")
  
  # izostavi NA
  drop_na(df)
  df <- as.data.frame(df)
  
  # Histogram 
  output$histogram1 <- renderPlot({
    ggplot() +
      # AES funkcija za definiranje varijable. Stupac odabran u "input$preduvjet" = varijabla za hist.
      aes(df[,input$preduvjet]) +
      geom_histogram(fill = "#2C5282", color = "white") +
      labs(x="Prema izabranoj varijabli", y="Učestalost") +
      ggtitle("Distribucija") +
      # Izgled: veličina i boja naslova, osi, izgled pozadine, okvira, širina i visina histograma
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank(),
            plot.title = element_text(size = 20, hjust = 0.5))
  }, height=300, width=300)
  
  # Stupičasti dijagram  
  output$barchart <- renderPlot({
    ggplot() +
      # Definiranje stupičastog dijagrama na temelju odabira korisnika godine, spol ili strucna sprema
      aes(df[,input$korelacija]) +
      geom_bar(position = "stack", color = "black") +
      scale_fill_manual(values = c("#9B59B6", "#3498DB"), name = "Godine") +
      labs(x="Prema klasifikaciji djelatnika", y="Učestalost") +
      ggtitle("Raspodjela") +
      theme_bw() +
      # Podešavanje izgleda dijagrama (veličine naslova i oznaka na osima, boje linija na osima, izgleda grid-a, okvira i pozadine)
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 20)
      )
  }, height=300, width=300)
  
  # Histogram 
  output$histogram2 <- renderPlot({
    ggplot() +
      aes(df[,input$evalKriterij]) +
      geom_histogram(fill = "#6ab04c") +
      labs(x="Prema izabranom kriteriju", y="Učestalost") +
      ggtitle("Distribucija") +
      theme(
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=300)
  
  # Histogram 4
  output$histogram3 <- renderPlot({
    ggplot() +
      aes(x=df[,input$preduvjet], fill=df[,input$korelacija]) +
      geom_histogram(position="identity", alpha=.7, color="white") +
      labs(x="Prema varijabli i klasif.djelatnika", y="Učestalost", fill="Klasifikacija djelatnika") +
      ggtitle("Distribucija") +
      theme(plot.title = element_text(hjust = 0.5, size=20),
            axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=500)
  
  # Histogram 5
  output$histogram4 <- renderPlot({
    ggplot() +
      aes(x=df[,input$evalKriterij], fill=df[,input$korelacija]) +
      geom_histogram(position="identity", alpha=.7, color="black") +
      labs(x="Prema kriteriju i klas.djelatnika", y="Učestalost", fill="Klasifikacija djelatnika") +
      ggtitle("Distribucija") +
      theme(plot.title = element_text(size=20, hjust = 0.5),
            axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=500)
  
  # Rasprseni grafikon
  output$rasprseni <- renderPlot({
    ggplot() +
      aes(x=df[,input$preduvjet], y=df[,input$evalKriterij], color=df[,input$korelacija]) +
      geom_point() +
      labs(x="Prema izabranoj varijabli", y="Prema izabranom kriteriju", color="Klasifikacija djelatnika",
      title = "Raspršeni dijagram", subtitle = "Odnos između izabrane varijable i kriterija prema klasifikaciji djelatnika") +
      scale_color_manual(values = c("#9B59B6", "#3498DB")) +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=600, width=800)
  
  # Model linearne regresije
  output$summary <- renderPrint({
    mod1 <- lm(df[,input$evalKriterij] ~ df[,input$preduvjet]*df[,input$korelacija])
    names(mod1$coefficients) <- c("Intercept", input$preduvjet, input$korelacija, "Interakcija")
    summary(mod1)
  })
  
  # Interaction plot
  output$interakcija <- renderPlot({
    ggplot() +
      aes(x=df[,input$preduvjet], y=df[,input$evalKriterij], color=df[,input$korelacija]) +
      geom_point() +
      # dodavanje linearne regresije
      geom_smooth(method="lm") + 
      labs(x="Prema izabranoj varijabli", y="Prema izabranom kriteriju", color="Klasifikacija djelatnika") +
      ggtitle("Interakcija između izabrane varijable i kriterija") +
      labs(subtitle="Ovisnost kriterija o varijabli, s obzirom na klasifikaciju djelatnika") +
      theme(plot.title = element_text(hjust = 0.5, size = 20),
            axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  }, height=600, width=800)
  
  # Koeficijent regresije
  output$koef.regresije <- renderText({
    # model linearne regresije  
    mod1 <- lm(df[,input$evalKriterij] ~ df[,input$preduvjet]*df[,input$korelacija])
    # koeficijenti regresije
    b0 = summary(mod1)$coefficients[1,1]
    b1 = summary(mod1)$coefficients[2,1]
    b2 = summary(mod1)$coefficients[3,1]
    b3 = summary(mod1)$coefficients[4,1]
    # Z-scores - usporedi vrijednosti jednog pokazatelja s drugim u skupu podataka. = statističke vrijednosti koje pokazuju koliko se jedna vrijednost razlikuje od prosjeka u standardnim devijacijama
    
    zlo = 0  
    zhi = 1
    # varijance kovarijacije matrice i vrijednosti
    covmat = vcov(mod1)
    var_b1 <- covmat[2,2]
    var_b3 <- covmat[4,4]
    var_b13 <- covmat[2,4]
    # SKR=standardizirani koef.regresije, SP=standardizirana pogreška, t- i p-, za prvi nivo kategoričkog predictora
    # vrijednosti se koriste za procjenu važnosti utjecaja tog prvog nivoa na zavisnu varijablu u regresijskoj analizi.
    skr_hi = b1 + b3*zhi
    sg_hi = sqrt(var_b1 + (zhi^2)*var_b3 + 2*zhi*var_b13)
    t_hi = skr_hi/sg_hi
    df = nrow(model.frame(mod1)) - 3 - 1
    p_hi = 2*pt(-abs(t_hi), df=df)
    
    skr_lo = b1 + b3*zlo
    sg_lo = sqrt(var_b1 + (zlo^2)*var_b3 + 2*zlo*var_b13)
    t_lo = skr_lo/sg_lo
    p_lo = 2*pt(-abs(t_lo), df=df)
    
    # ukoliko je izabrana koleracija GODINE
    if(input$korelacija=="Godine"){
      line1 <- paste0("SKR od ", input$preduvjet, " kada ", input$korelacija, " ima preko 40 ", 
                      "= ", round(skr_lo, 3),
                      " (SG=", round(sg_lo, 3), ", t=", round(t_lo, 3), ", p=", round(p_lo, 3), ")")
      line2 <- paste0("SKR od ", input$preduvjet, " kada ", input$korelacija, " ima manje od 40  ",  
                      "= ", round(skr_hi, 3),
                      " (sg=", round(sg_hi, 3), ", t=", round(t_hi, 3), ", p=", round(p_hi, 3), ")")
      full <- paste0(line1, "\n", line2, "\n", "\n", "\n", "SKR: Standardizirani koeficijent regresije", 
                     "\nSG: Standardna greška koeficijenta regresije", "\nt: t vrijednost", "\np: p vrijednost")
      print(full)}
    
    else if(input$korelacija=="Spol"){
      line1 <- paste0("SKR od ", input$preduvjet, " kada ", input$korelacija, " je ženski ", 
                      "= ", round(skr_hi, 3),
                      " (SG=", round(sg_hi, 3), ", t=", round(t_hi, 3), ", p=", round(p_hi, 3), ")")
      line2 <- paste0("SKR od ", input$preduvjet, " kada ", input$korelacija, " je muški ", 
                      "= ", round(skr_lo, 3),
                      " (SG=", round(sg_lo, 3), ", t=", round(t_lo, 3), ", p=", round(p_lo, 3), ")")
      full <- paste0(line1, "\n", line2, "\n", "\n", "\n", "SKR: Standardizirani koeficijent regresije", 
                     "\nSG: Standardna greška koeficijenta regresije", "\nt: t vrijednost", "\np: p vrijednost")
      print(full)}
    
    else if(input$korelacija=="StrucnaSprema"){
      line1 <- paste0("SKR od ", input$preduvjet, " kada ", input$korelacija, " je trogodišnja. ", 
                      "= ", round(skr_hi, 3),
                      " (SG=", round(sg_hi, 3), ", t=", round(t_hi, 3), ", p=", round(p_hi, 3), ")")
      line2 <- paste0("SKR od ", input$preduvjet, " kada ", input$korelacija, " je četverogodišnja. ", 
                      "= ", round(skr_lo, 3),
                      " (SG=", round(sg_lo, 3), ", t=", round(t_lo, 3), ", p=", round(p_lo, 3), ")")
      full <- paste0(line1, "\n", line2, "\n", "\n", "\n", "SKR: Standardizirani koeficijent regresije", 
                     "\nSG: Standardna greška koeficijenta regresije", "\nt: t vrijednost", "\np: p vrijednost")
      print(full)}
  })
  
}

# shiny App
shinyApp(ui=ui, server=server)
