library(tidyverse)
library("readxl")
library(factoextra)
library(ggrepel)
library(shiny)
library(reactable)


SERVER <- function(input, output, session) {
  playerTable <- eventReactive(input$show,{

    GMMData <- read_xlsx("newTry.xlsx") %>%
      #filter(Pos %in% c("MF","MFFW","MFDF")) %>%
      filter(Min >= 1000) %>%
      na.omit()
    
    GMMData <- GMMData %>%
      mutate("TotalTouches"=TouchesAttPen*(1/90)*Min)
    
    GMMData <- GMMData %>% mutate("Shot_Eff" = (`SoT/90`)/TouchesAttPen) %>%
      mutate("NPG/Shot" = `G-PK`/(`Sh/90`)) %>%
      filter(npxG >= 0)
    
    GMMData <- GMMData %>%
      mutate("xG_OP" = `NPG/Shot`- `npxG/Sh`)
    
    strike = GMMData %>% filter(Pos == c("FW","FWMF","FWDF"))
    mean_xG <- mean(strike$xG_OP)
    std_xG <- sd(strike$xG_OP)
    
    mean_shot <- mean(strike$Shot_Eff)
    sd_shot <- sd(strike$Shot_Eff)
    
    mean_quality <- mean(strike$`npxG/Sh`)
    sd_quality <- sd(strike$`npxG/Sh`)
    
    GMMData <- GMMData %>% mutate("ClinicalFinishing" = (0.6*(`xG_OP`- mean_xG)*(1/std_xG)) +
                                            (0.25*(`Shot_Eff`- mean_shot)*(1/sd_shot)) +
                                            (0.15*(`npxG/Sh`- mean_quality)*(1/sd_quality)) )
    
    PCAData <- GMMData %>% select(-Rk) %>%
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s", "G-PK")) %>%
      na.omit() %>%
      scale()
    
    print("PCA")
    print(nrow(PCAData))
    
    Passing = c("Pressing","PressDefThird",
                "PressMidThird","PressAttThird",
                "PassAtt","PassLive",	"PassDead",	
                "PassPress",	"PassSwitch",	"PassCross", "PassCK",
                "PassGround",	"PassLow",	"PassHigh",	"PassLeft",
                "PassRight",	"PassHead",	"PassTI",	"PassOther", "PassOff",
                "PassOut",	"PassTotDist",	"PassPrgDist",
                "ShrtAtt",	"MedAtt",	"LongAtt", 
                "PenaltyAreaDirectness", "PassDirectness",
                "AttackCarries", "CarryDirectness",
                "ProgDistPerCarry", "DribblesPerTouch")
    
    Defending = c("Tkl",	"TklDefThird",	"TklMidThird",	"TklAttThird",
                  "Blocks",	"PassBlock",	"Int",	"Clr", "Err")
    
    Attacking = c("TouchesAttThird",	"TouchesAttPen", "AttDribble",
                  "Carries",	"TotDistCarries",	"PrgDistCarries",	
                  "ProgCarries",	"FinalThirdCarries"	,"CPA",
                  "SCADrib",	"SCAShots", "GCADrib", "GCAShots",
                  "CrnIn",	"CrnOut",	"CrnStr",
                  "Sh/90",	"SoT/90",	"DistanceShot",
                  "PTF3",	"PPA",	"CrsPA", "PassTB","KP",	
                  "SCAPassLive",	"GCAPassLive",
                  "npxG", "npxG/Sh")
    
    
    PassData <- GMMData %>% select(-Rk) %>%
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s")) %>%
      na.omit() %>%
      select(Passing) %>%
      scale()
    
    DefData <- GMMData %>% select(-Rk) %>%
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s")) %>%
      na.omit() %>%
      select(Defending) %>%
      scale()
    
    AttData <- GMMData %>% select(-Rk) %>%
      select(-c("Player","Nation","Pos","Squad","Comp","Age","Born","90s")) %>%
      na.omit() %>%
      select(Attacking) %>%
      scale()
    
    print("VARIUS")
    print(nrow(PassData))
    print(nrow(DefData))
    print(nrow(AttData))
    #applying KMC
    kpasses <- kmeans(PCAData, centers = 7, nstart = 25)
    
    pca <- prcomp(PCAData)
    pcaPass <- prcomp(PassData)
    pcaDef <- prcomp(DefData)
    pcaAtt <- prcomp(AttData)
    
    pcGeneral <- as.data.frame(pca$x[,1:8]) # extract first two PCs
    
    #Now adding in the Cluster factor to the passes
    pcGeneral <- pcGeneral %>% mutate(Cluster=kpasses$cluster)
    pcGeneral$Cluster <- as.factor(pcGeneral$Cluster) # add player clusters 
    
    pcPass <- as.data.frame(pcaPass$x[,1:8]) # extract first two PCs
    pcDef <- as.data.frame(pcaDef$x[,1:8])
    pcAtt <- as.data.frame(pcaAtt$x[,1:8])
    
    pcGeneral <- pcGeneral %>% mutate(Player = GMMData$Player) %>%
      mutate(Squad = GMMData$Squad) %>%
      mutate(Position = GMMData$Pos) %>%
      mutate(Age = GMMData$Age)%>%
      mutate(Comp = GMMData$Comp) %>%
      mutate(Nation = GMMData$Nation) %>%
      mutate(CFM = GMMData$ClinicalFinishing)%>%
      mutate(npxG = GMMData$npxG)
    
    pcPass <- pcPass %>% mutate(Player = GMMData$Player) %>%
      mutate(Squad = GMMData$Squad) %>%
      mutate(Position = GMMData$Pos) %>%
      mutate(Age = GMMData$Age)%>%
      mutate(Comp = GMMData$Comp)%>%
      mutate(Nation = GMMData$Nation) %>%
      mutate(CFM = GMMData$ClinicalFinishing)
    
    pcDef <- pcDef %>% mutate(Player = GMMData$Player) %>%
      mutate(Squad = GMMData$Squad) %>%
      mutate(Position = GMMData$Pos) %>%
      mutate(Age = GMMData$Age)%>%
      mutate(Comp = GMMData$Comp)%>%
      mutate(Nation = GMMData$Nation)%>%
      mutate(CFM = GMMData$ClinicalFinishing)
    
    pcAtt <- pcAtt %>% mutate(Player = GMMData$Player) %>%
      mutate(Squad = GMMData$Squad) %>%
      mutate(Position = GMMData$Pos) %>%
      mutate(Age = GMMData$Age) %>%
      mutate(Comp = GMMData$Comp)%>%
      mutate(Nation = GMMData$Nation)%>%
      mutate(CFM = GMMData$ClinicalFinishing)
    
    print("General")
    print(nrow(pcGeneral))
    
    print("Player")
    print(input$player)
    findGeneral <- pcGeneral %>% filter(Player == input$player)
     findPass <- pcPass %>% filter(Player == input$player)
    findDef <- pcDef %>% filter(Player == input$player)
    findAtt <- pcAtt %>% filter(Player == input$player)
    
    print("Find General")
    print(nrow(findGeneral))

    if(input$league == "All"){
      leagueFinder = c("fr Ligue 1",
                       "de Bundesliga",
                       "eng Premier League",
                       "es La Liga",
                       "it Serie A")
    }
    else{
      leagueFinder = input$league
    }
    
    if(input$nation == "All"){
      nationFinder = GMMData$Nation
    }
    else{
      nationFinder = input$nation
    }
    
    similar <- pcGeneral %>%
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      filter(CFM >= input$cfm[1]) %>%
      filter(CFM <= input$cfm[2]) %>%
      mutate(euc = sqrt((PC1 -  findGeneral$PC1)^2+(PC2 - findGeneral$PC2)^2+
                          (PC3 -  findGeneral$PC3)^2+(PC4 - findGeneral$PC4)^2+
                          (PC5 -  findGeneral$PC5)^2+(PC6 -  findGeneral$PC6)^2+
                          (PC7 -  findGeneral$PC7)^2+(PC8 -  findGeneral$PC8)^2)) %>%
      mutate(cos = (PC1*findGeneral$PC1+PC2*findGeneral$PC2+PC3*findGeneral$PC3+
                      PC4*findGeneral$PC4+PC5*findGeneral$PC5+PC6*findGeneral$PC6)/(
                        sqrt(PC1^2+PC2^2+PC3^2+PC4^2+PC5^2+PC6^2)*
                          sqrt(findGeneral$PC1^2+findGeneral$PC2^2+findGeneral$PC3^2+findGeneral$PC4^2+findGeneral$PC5^2+findGeneral$PC6^2)
                      )) %>%
      mutate(percent = (1-cos)*100)
    
    pcPass = pcPass %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      filter(CFM >= input$cfm[1]) %>%
      filter(CFM <= input$cfm[2]) %>%
      mutate(eucPass = sqrt((PC1 -  findPass$PC1)^2+(PC2 - findPass$PC2)^2+
                                                (PC3 -  findPass$PC3)^2+(PC4 - findPass$PC4)^2+
                                                (PC5 -  findPass$PC5)^2+(PC6 -  findPass$PC6)^2+
                                                (PC7 -  findPass$PC7)^2+(PC8 -  findPass$PC8)^2))
    pcPass <- pcPass[ !(pcPass$Player %in% c(input$player)), ]
    
    pcDef = pcDef %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      filter(CFM >= input$cfm[1]) %>%
      filter(CFM <= input$cfm[2]) %>%
      mutate(eucDef = sqrt((PC1 -  findDef$PC1)^2+(PC2 - findDef$PC2)^2+
                                             (PC3 -  findDef$PC3)^2+(PC4 - findDef$PC4)^2+
                                             (PC5 -  findDef$PC5)^2+(PC6 -  findDef$PC6)^2+
                                             (PC7 -  findDef$PC7)^2+(PC8 -  findDef$PC8)^2))
    pcDef <- pcDef[ !(pcDef$Player %in% c(input$player)), ]
    
    pcAtt = pcAtt %>% 
      filter(Age >= input$ager[1]) %>%
      filter(Age <= input$ager[2]) %>%
      filter(Comp %in% leagueFinder) %>%
      filter(Nation %in% nationFinder) %>%
      filter(CFM >= input$cfm[1]) %>%
      filter(CFM <= input$cfm[2]) %>%
      mutate(eucAtt = sqrt((PC1 -  findAtt$PC1)^2+(PC2 - findAtt$PC2)^2+
                                             (PC3 -  findAtt$PC3)^2+(PC4 - findAtt$PC4)^2+
                                             (PC5 -  findAtt$PC5)^2+(PC6 -  findAtt$PC6)^2+
                                             (PC7 -  findAtt$PC7)^2+(PC8 -  findAtt$PC8)^2))
    pcAtt <- pcAtt[ !(pcAtt$Player %in% c(input$player)), ]
    
    
    view <- similar %>% select(c("Player", "Squad", "Position", "Age", "Cluster", "euc", "CFM", "npxG"))
    view <- view[ !(view$Player %in% c(input$player)), ]
    view <- view %>%
      mutate(Rank = rank(euc)) %>%
      mutate(PassScore = pcPass$eucPass) %>%
      mutate(PassScore = rank(PassScore)) %>%
      mutate(DefScore = pcDef$eucDef) %>%
      mutate(DefScore = rank(DefScore)) %>%
      mutate(AttScore = pcAtt$eucAtt) %>%
      mutate(AttScore = rank(AttScore)) %>%
      select(-euc) %>%
      na.omit() %>%
      arrange(Rank)
    
    
    
    
    red_pal <- function(x) rgb(colorRamp(c("#30a2da", "#e5ae38", "#fc4f30"))(x), maxColorValue = 255)
    reactable(view, filterable = TRUE,
              defaultPageSize = 20,
              columns = list(
                Rank = colDef(
                  style = function(value) {
                    normalized <- (value - min(view$Rank)) / (max(view$Rank) - min(view$Rank))
                    color <- red_pal(normalized)
                    list(background = color)
                  }
                ),
                PassScore = colDef(
                  style = function(value) {
                    normalized <- (value - min(view$PassScore)) / (max(view$PassScore) - min(view$PassScore))
                    color <- red_pal(normalized)
                    list(background = color)
                  }
                ),
                DefScore = colDef(
                  style = function(value) {
                    normalized <- (value - min(view$DefScore)) / (max(view$DefScore) - min(view$DefScore))
                    color <- red_pal(normalized)
                    list(background = color)
                  }
                ),
                AttScore = colDef(
                  style = function(value) {
                    normalized <- (value - min(view$AttScore)) / (max(view$AttScore) - min(view$AttScore))
                    color <- red_pal(normalized)
                    list(background = color)
                  }
                )
              ),
              theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                searchInputStyle = list(width = "100%")
              )
    )
  })
  
  output$playerData <- renderReactable({
    playerTable()
  })
  print("NEW : ")
}

