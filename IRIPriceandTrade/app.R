# Price and Trade Calc
library(readr)
library(stringr)
library(scales)
library(openxlsx)
library(shiny)
library(shinybusy)
library(rsconnect)

#Shiny UI

ui <- fluidPage(
  textInput("client_name", "Client Name", "As you would like it to appear in the output."),
  fileInput("PTATargetCoef","PTATargetCoef.csv",accept=".csv"),
  fileInput("PTACompCoef","PTACompCoef.csv",accept=".csv"),
  fileInput("PTABaseThresholdCoef","PTABaseThresholdCoef.csv",accept=".csv"),
  fileInput("PTAPromoThresholdCoef","PTAPromoThresholdCoef.csv",accept=".csv"),
  fileInput("PTABaseGapCoef","PTABaseGapCoef.csv",accept=".csv"),
  numericInput("perc", "Percent Price Change (as decimal)",min = 0.01,max = 1,value = 0.10),
  actionButton("goButton", "Go!", class = "btn-success"),
  downloadButton("to_download", "Download"),
  verbatimTextOutput("error")
)


server <- function(input, output, session) {
  
  #Shiny Inputs
  # PTATargetCoef <- read_csv("C:/Users/mddmd/Downloads/AMP_RGM_PICTSWEET_PS_2021_1533_28122021152231/5_FROZEN VEGETABLES/PTATargetCoef.csv")
  # PTACompCoef <- read_csv("C:/Users/mddmd/Downloads/AMP_RGM_PICTSWEET_PS_2021_1533_28122021152231/5_FROZEN VEGETABLES/PTACompCoef.csv")
  # PTABaseThresholdCoef <- read_csv("C:/Users/mddmd/Downloads/AMP_RGM_PICTSWEET_PS_2021_1533_28122021152231/5_FROZEN VEGETABLES/PTABaseThresholdCoef.csv")
  # PTAPromoThresholdCoef <- read_csv("C:/Users/mddmd/Downloads/AMP_RGM_PICTSWEET_PS_2021_1533_28122021152231/5_FROZEN VEGETABLES/PTAPromoThresholdCoef.csv")
  # PTABaseGapCoef <- read_csv("C:/Users/mddmd/Downloads/AMP_RGM_PICTSWEET_PS_2021_1533_28122021152231/5_FROZEN VEGETABLES/PTABaseGapCoef.csv")
  # perc <- 0.10
  # client_name <- "Pictsweet Farms"
  
  observeEvent(input$goButton, {
    
    PTATargetCoef <- read.csv(input$PTATargetCoef$datapath)
    PTACompCoef <- read.csv(input$PTACompCoef$datapath)
    PTABaseThresholdCoef <- read.csv(input$PTABaseThresholdCoef$datapath)
    PTAPromoThresholdCoef <- read.csv(input$PTAPromoThresholdCoef$datapath)
    PTABaseGapCoef <- read.csv(input$PTABaseGapCoef$datapath)
    perc <- input$perc
    client_name <- input$client_name
    
    if (exists("PTATargetCoef") == FALSE | exists("PTACompCoef") == FALSE | exists("PTABaseThresholdCoef") == FALSE | exists("PTAPromoThresholdCoef") == FALSE | exists("PTABaseGapCoef") == FALSE | exists("client_name") == FALSE ) {
      error <- renderText("Missing Inputs")
      }
    
    if (exists("PTATargetCoef") == TRUE & exists("PTACompCoef") == TRUE & exists("PTABaseThresholdCoef") == TRUE & exists("PTAPromoThresholdCoef") == TRUE & exists("PTABaseGapCoef") == TRUE & exists("client_name") == TRUE ) {
      show_modal_progress_line()
      primary_color <- "#16843B"
      text_color <-  "#FFFFFF"
      
      PPG_List <- sort(unique(PTATargetCoef$PPGName))
      geog_list <- unique(PTATargetCoef$geogName)
      
      #Cleaning and Object Prep
      
      TUS_geogs <- str_detect(geog_list,"Total US")
      TUS_geogs <- geog_list[TUS_geogs == TRUE]
      
      walmart_geogs <- str_detect(geog_list,"Walmart")
      walmart_geogs <- geog_list[walmart_geogs == TRUE]
      
      IRI_geogs <- str_detect(geog_list,"IRI Standard")
      IRI_geogs <- geog_list[IRI_geogs == TRUE]
      
      temp <- c(TUS_geogs,walmart_geogs,IRI_geogs)
      geog_list <- c(temp,setdiff(geog_list,temp))
      
      char_check <- nchar(geog_list)
      
      num_geog <- length(geog_list)
      num_PPG <- length(PPG_List)
      progress <- 0
      increment <- 1/num_geog
      
      joint_dist <- subset(PTACompCoef, select = c("PPGName","CompPPGName","geogName","JointDist"))
      joint_dist <- unique(joint_dist)
      PTABaseGapCoef <- merge(PTABaseGapCoef, joint_dist, by = c("PPGName","CompPPGName","geogName"), all.x = TRUE)
      
      perc_text = perc*100
      
      row_names <- c( paste("Sales Loss on ",perc_text,"% price increase", sep = ""),
                      "If Product changes prices alone",
                      "If all competitors change price too",
                      "Base Price Thresholds",
                      "Threshold to manage against",
                      "Additional impact above elasticity",
                      "Competitive Pricing Interactions",
                      " Strongest Competitive Price Interaction (JD Wtd Coef >= 0.05)",
                      " Second Strongest Competitive Price Interaction (JD Wtd Coef >= 0.05)",
                      "Base Price Gaps",
                      "Key Competitive Gap Significant Base Price Gap Sales Risk",
                      "",
                      "Key Competitive Gap Significant Base Price Gap Sales Risk",
                      "",
                      "Quality Merchandising",
                      paste("Quality Merch. TPR Lift w ",perc_text,"%", sep = ""),
                      paste("Feature Lift w ",perc_text,"% Disc.", sep = ""),
                      paste("Display Lift w ",perc_text,"% Disc.", sep = ""),
                      paste("Feature & Display Lift w ",perc_text,"%Disc.", sep = ""),
                      "Promoted Price Thresholds",
                      "Threshold to target with events",
                      "Additional impact above elasticity")
      
      
      PPG_Headers <- matrix(, nrow = 0, ncol = length(PPG_List))
      colnames(PPG_Headers) <- PPG_List
      
      to_download <- createWorkbook()
      
      for (geog in geog_list) {
        progress <- progress + increment
        update_modal_progress(progress)
        outputs <-matrix(,nrow = 22,ncol = 0)
        for (ppg in PPG_List) {
          
          #Sales Loss on X% Price Increase
          BPE <-PTATargetCoef$BasePrice[PTATargetCoef$PPGName == ppg & PTATargetCoef$geogName == geog]
          product_changes_price_alone <- paste(sprintf(round(100*((1+perc)^BPE-1),2), fmt = '%#.2f'),"%",sep = "")
          PTACompCoef$JDWeightedImpact <- PTACompCoef$Comprice* PTACompCoef$JointDist/100
          net <- BPE + sum(PTACompCoef$JDWeightedImpact[PTACompCoef$PPGName == ppg & PTACompCoef$geogName == geog])
          all_comps_change_price <- paste(sprintf(round(100*((1+perc)^net-1),2), fmt = '%#.2f'),"%",sep = "")
          if (length(BPE) == 0) {
            product_changes_price_alone <- "-"
          } 
          if (length(net) == 0) {
            all_comps_change_price <- "-"
          } 
          
          #Base Price Thresholds
          threshold_coeffs <- PTABaseThresholdCoef$Coefficient[PTABaseThresholdCoef$PPGName == ppg & PTABaseThresholdCoef$geogName == geog]
          threshold_order <- order(threshold_coeffs, decreasing = TRUE)
          thresholds_baseprice <- PTABaseThresholdCoef$BasePrice[PTABaseThresholdCoef$PPGName == ppg & PTABaseThresholdCoef$geogName == geog]
          threshold_impacts <- exp(threshold_coeffs)-1
          
          thresholds <- c()
          impacts <- c()
          len <- length(threshold_order)
          if (len > 1) {
            for (g in threshold_order) {
              if (g != threshold_order[len]) {
                thresh <- paste("$",thresholds_baseprice[g]," / ",sep = "")
                imp <- paste(sprintf(round(threshold_impacts[g],2), fmt = '%#.2f'),"% / ",sep = "")
              }
              if (g == threshold_order[len]) {
                thresh <- paste("$",thresholds_baseprice[g],sep = "")
                imp <- paste(sprintf(round(threshold_impacts[g],2), fmt = '%#.2f'),"%",sep = "")
              }
              thresholds <-trimws(paste(thresholds,thresh))
              impacts <-trimws(paste(impacts,imp))
            }
          }
          if (len == 1) {
            thresholds <- trimws(paste("$",thresholds_baseprice,sep = ""))
            impacts<- trimws(paste(sprintf(round(threshold_impacts,2), fmt = '%#.2f'),"%",sep = ""))
          }
          if (len == 0) {
            thresholds <- "-"
            impacts <- "-"
          }
          
          #Competitive Pricing Interaction
          comp_impacts <- sort(PTACompCoef$JDWeightedImpact[PTACompCoef$PPGName == ppg & PTACompCoef$geogName == geog],decreasing = TRUE)
          
          if (length(comp_impacts) != 0) {
            first_impact <- comp_impacts[1]
            second_impact <- comp_impacts[2]
            
            if (first_impact >= 0.05) {
              first_comp <- PTACompCoef$CompPPGName[PTACompCoef$PPGName == ppg & PTACompCoef$geogName == geog & PTACompCoef$JDWeightedImpact== first_impact]
            }
            if (first_impact < 0.05 | is.na(first_impact) == TRUE) {
              first_comp <- "-"
            }
            
            if (is.na(second_impact) != TRUE) {
              if (second_impact >= 0.05) {
                second_comp <- PTACompCoef$CompPPGName[PTACompCoef$PPGName == ppg & PTACompCoef$geogName == geog & PTACompCoef$JDWeightedImpact== second_impact]
              }
              if (second_impact < 0.05) {
                second_comp <- "-"
              }
            }
          }
          if (length(comp_impacts) == 0) {
            first_comp <- "-"
            second_comp <- "-"
          }
          
          #Base Price Gaps
          PTABaseGapCoef$JointDist[is.na(PTABaseGapCoef$JointDist) == TRUE] <- 0
          PTABaseGapCoef$JDWeightedImpact <- (exp(PTABaseGapCoef$Coefficient)-1)* PTABaseGapCoef$JointDist/100
          
          gap_impacts <- sort(PTABaseGapCoef$JDWeightedImpact[PTABaseGapCoef$PPGName == ppg & PTABaseGapCoef$geogName == geog],decreasing = FALSE)
          
          if (length(gap_impacts) != 0) {
            first_gap_impact <- gap_impacts[1]
            first_gap <- round(PTABaseGapCoef$Gap[PTABaseGapCoef$PPGName == ppg & PTABaseGapCoef$geogName == geog & PTABaseGapCoef$JDWeightedImpact == first_gap_impact],2)
            first_gap_ppg<- PTABaseGapCoef$CompPPGName[PTABaseGapCoef$PPGName == ppg & PTABaseGapCoef$geogName == geog & PTABaseGapCoef$JDWeightedImpact == first_gap_impact]
            first_gap_ppg <- first_gap_ppg[1]
            first_sales_risk <- paste(sprintf('$%.2f',first_gap), " / ", sprintf(round(first_gap_impact,2), fmt = '%#.2f'), sep = "")
            first_sales_risk <-first_sales_risk[1]
          }
          if (length(gap_impacts) == 1) {
            second_sales_risk <- "-"
          }
          if (length(gap_impacts) >= 2) {
            second_gap_impact <- gap_impacts[2]
            second_gap <- PTABaseGapCoef$Gap[PTABaseGapCoef$PPGName == ppg & PTABaseGapCoef$geogName == geog & PTABaseGapCoef$JDWeightedImpact == second_gap_impact]
            second_gap_ppg<- PTABaseGapCoef$CompPPGName[PTABaseGapCoef$PPGName == ppg & PTABaseGapCoef$geogName == geog & PTABaseGapCoef$JDWeightedImpact == second_gap_impact]
            second_gap_ppg <- second_gap_ppg[1]
            second_sales_risk <- paste(sprintf('$%.2f',second_gap), " / ", sprintf(round(second_gap_impact,2), fmt = '%#.2f'), sep = "")
            second_sales_risk <-second_sales_risk[1]
          }
          if (length(gap_impacts) == 0) {
            first_sales_risk <- "-"
            second_sales_risk <- "-"
          }
          
          
          #Quality Merchandising
          PPE <- PTATargetCoef$Discount[PTATargetCoef$PPGName == ppg & PTATargetCoef$geogName == geog]
          FO <- PTATargetCoef$FeatOnly[PTATargetCoef$PPGName == ppg & PTATargetCoef$geogName == geog]
          DO <- PTATargetCoef$DispOnly[PTATargetCoef$PPGName == ppg & PTATargetCoef$geogName == geog]
          FD <- PTATargetCoef$FeatDisp[PTATargetCoef$PPGName == ppg & PTATargetCoef$geogName == geog]
          
          QM_TPR <- paste(round(100*((1-perc)^PPE-1),0),"%",sep = "")
          QM_Feat <- paste(round(100*((1-perc)^PPE*exp(FO)-1),0),"%",sep = "")
          QM_Disp <- paste(round(100*((1-perc)^PPE*exp(DO)-1),0),"%",sep = "")
          QM_FeatDisp <- paste(round(100*((1-perc)^PPE*exp(FD)-1),0),"%",sep = "")
          
          if (length(PPE) == 0) {
            QM_TPR <- "-"
          }
          if (length(FO) == 0) {
            QM_Feat <- "-"
          }
          if (length(DO) == 0) {
            QM_Disp <- "-"
          }
          if (length(FD) == 0) {
            QM_FeatDisp <- "-"
          }
          
          
          
          #Promoted Price Thresholds
          promo_threshold_coeffs <- PTAPromoThresholdCoef$Coefficient[PTAPromoThresholdCoef$PPGName == ppg & PTAPromoThresholdCoef$geogName == geog]
          promo_threshold_order <- order(promo_threshold_coeffs, decreasing = TRUE)
          thresholds_promoprice <- PTAPromoThresholdCoef$PromoPrice[PTAPromoThresholdCoef$PPGName == ppg & PTAPromoThresholdCoef$geogName == geog]
          promo_threshold_impacts <- exp(promo_threshold_coeffs)-1
          
          promo_thresholds <- c()
          promo_impacts <- c()
          len <- length(promo_threshold_order)
          if (len > 1) {
            for (g in promo_threshold_order) {
              if (g != promo_threshold_order[len]) {
                thresh <- paste(sprintf(thresholds_promoprice[g], fmt = '$%.2f')," / ",sep = "")
                imp <- paste(sprintf(round(promo_threshold_impacts[g],2), fmt = '%#.2f'),"% / ",sep = "")
              }
              if (g == promo_threshold_order[len]) {
                thresh <- paste(sprintf(thresholds_promoprice[g], fmt = '$%.2f'),sep = "")
                imp <- paste(sprintf(round(promo_threshold_impacts[g],2), fmt = '%#.2f'),"%", sep = "")
              }
              promo_thresholds <-trimws(paste(promo_thresholds,thresh))
              promo_impacts <-trimws(paste(promo_impacts,imp))
            }
          }
          if (len == 1) {
            promo_thresholds <- trimws(paste(sprintf(thresholds_promoprice, fmt = '$%.2f'),sep = ""))
            promo_impacts<- trimws(paste(sprintf(round(promo_threshold_impacts,2), fmt = '%#.2f'),"%",sep = ""))
          }
          if (len == 0) {
            promo_thresholds <- "-"
            promo_impacts <- "-"
          }
          
          temp <- c("",
                    as.character(product_changes_price_alone),
                    as.character(all_comps_change_price),
                    "",
                    as.character(thresholds),
                    as.character(impacts),
                    "",
                    as.character(first_comp),
                    as.character(second_comp),
                    "",
                    as.character(first_gap_ppg),
                    as.character(first_sales_risk),
                    as.character(second_gap_ppg),
                    as.character(second_sales_risk),
                    "",
                    as.character(QM_TPR),
                    as.character(QM_Feat),
                    as.character(QM_Disp),
                    as.character(QM_FeatDisp),
                    "",
                    as.character(promo_thresholds),
                    as.character(promo_impacts))
          
          dim(temp) <- c(22,1)
          colnames(temp)<- ppg
          outputs <- cbind(outputs, temp)
          
        }
        
        geog_tab <-gsub(geog,pattern = "/",replacement = "")
        geog_tab <- substr(geog_tab,1,31)
        
        #Create and Format Worksheet
        addWorksheet(wb = to_download, sheetName = geog_tab)
        
        setColWidths(
          to_download,
          sheet = geog_tab,
          cols = 1:(num_PPG+1),
          widths <- "auto"
        )
        setRowHeights(
          to_download,
          sheet = geog_tab,
          rows = c(1:26),
          heights <- c(64,32,16,64,16,16,16,16,16,16,16,64,64,16,64,16,64,16,16,16,16,16,16,16,16,16)
        )
        writeData(
          to_download,
          outputs,
          sheet = geog_tab,
          startRow = 4,
          startCol = 2
        )
        writeData(
          to_download,
          row_names,
          sheet = geog_tab,
          startRow = 5,
          startCol = 1
        )
        writeData(
          to_download,
          PPG_Headers,
          sheet = geog_tab,
          startRow = 3,
          startCol = 2
        )
        writeData(
          to_download,
          geog,
          sheet = geog_tab,
          startRow = 2,
          startCol = 1
        )
        writeData(
          to_download,
          paste(client_name, " - Pricing and Trading Cheat Sheet",sep = ""),
          sheet = geog_tab,
          startRow = 2,
          startCol = 2
        )
        writeData(
          to_download,
          paste(client_name," PPGs",sep = ""),
          sheet = geog_tab,
          startRow = 3,
          startCol = 1
        )
        writeData(
          to_download,
          "PRICE / PROMO ARCHITECTURE",
          sheet = geog_tab,
          startRow = 4,
          startCol = 1
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 2:(num_PPG+1),
          rows = 1:26,
          style = createStyle(halign = "center"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1:(num_PPG+1),
          rows = 1:26,
          style = createStyle(valign = "center"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 2:(num_PPG+1),
          rows = c(2,3,5,6,7,14,19,24),
          style = createStyle(textDecoration = "Bold"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1,
          rows = c(2,3,4,5,8,11,14,19,24),
          style = createStyle(textDecoration = "Bold"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 2:(num_PPG+1),
          rows = c(5,16),
          style = createStyle(fontColour = "#FF0000"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 2:(num_PPG+1),
          rows = c(7,26),
          style = createStyle(fontColour = "#00B600"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1:(num_PPG+1),
          rows = 1:26,
          style = createStyle(wrapText = TRUE),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1:(num_PPG+1),
          rows = c(2,5,8,11,14,19,24),
          style = createStyle(fgFill = primary_color),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1:(num_PPG+1),
          rows = c(2,5,8,11,14,19,24),
          style = createStyle(fontColour = text_color),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1,
          rows = c(6,7,9,10,12,13,15,17,20,21,22,23,25,26),
          style = createStyle(indent = 2),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1,
          rows = c(21,22,23),
          style = createStyle(indent = 3),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = c(1,2),
          rows = 2,
          style = createStyle(fontSize = 20),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 2,
          rows = 2,
          style = createStyle(textDecoration = "italic"),
          gridExpand = TRUE,
          stack = TRUE
        )
        addStyle(
          to_download,
          sheet = geog_tab,
          cols = 1,
          rows = c(2,3,4),
          style = createStyle(halign = "center"),
          gridExpand = TRUE,
          stack = TRUE
        )
        mergeCells(
          to_download,
          sheet = geog_tab,
          cols = 2:(num_PPG+1),
          rows = 2
        )
        mergeCells(
          to_download,
          sheet = geog_tab,
          cols = 1,
          rows = c(15,16)
        )
        mergeCells(
          to_download,
          sheet = geog_tab,
          cols = 1,
          rows = c(17,18)
        )
        for (i in 2:(num_PPG+1)) {
          mergeCells(
            to_download,
            sheet = geog_tab,
            cols = i,
            rows = c(3,4)
          )
        }
        insertImage(
          to_download,
          sheet = geog_tab,
          file = "www/IRI_Logo_trasp.png",
          width = 1.75,
          height = 0.75,
          startRow = 1,
          startCol = 1,
          units = "in",
          dpi = 300
        )
       
      }
      
      output$to_download <- downloadHandler(
        filename = function() {paste(client_name,"Pricing and Trade Cheat Sheet.xlsx", sep="")},
        content = function(file) {saveWorkbook(to_download,file)}
      )
      remove_modal_progress()
    }
  })
}

shinyApp(ui, server)