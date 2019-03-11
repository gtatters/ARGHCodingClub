

# ========================= LOAD REQUIRED PACKAGES, DATA AND FUNCTIONS ####
#  load necessary packages
library('shiny')
library('ggplot2')
library('reshape2')
library("data.table")
library("dplyr")
library("vegan")
library("gdata")
#
#setwd("/Users/alisonwaller/Documents/Professional/Brock/Bidochka_Microbiome/shiny/")

# Load experimental data (abundance tables)
# first row is the header, and first column is rownames (ie. doesn't need corresponding column name)
f_genus<-read.table("b_Genus.csv",header=T,sep=",",row.names=1)
f_family<-read.table("b_Family.csv",header=T,sep=",",row.names=1)
f_phylum<-read.table("b_Phylum.csv",header=T,sep=",",row.names=1)
# read in bacterial data
bact_genus<-read.table("ss_Genus.csv",header=T,sep=",",row.names=1)
bact_family<-read.table("ss_Family.csv",header=T,sep=",",row.names=1)
bact_phylum<-read.table("ss_Phylum.csv",header=T,sep=",",row.names=1)
# sample metadata
sample_metadata<-read.csv("sample_metadata.csv",row.names=1,header=T)
##
abundance_tables<-list(fungi_genus=f_genus,fungi_family=f_family,fungi_phylum=f_phylum,
                       bact_genus=bact_genus,bact_family=bact_family,bact_phylum=bact_phylum)
## add diversity and species count columns to each abundance table
abundance_tables<-lapply(abundance_tables,function(tab) {tab$diversity<-diversity(tab) ; tab})
abundance_tables<-lapply(abundance_tables,function(tab){                           
  tab$species_count<-apply(tab,1, function(x) {length(x[x>0])});tab})
##
FactorsOfInt<-c("Metarhizium","Insect","Sample_Type","Metarhizium*Insect","Metarhizium*Sample_Type")

ui <- fluidPage(
  # Make a title to display in the app
  titlePanel(" Exploring the Effect of Metarhizium on the Soil and Root Microbiome "),
  # Make the Sidebar layout
  sidebarLayout(
    # Put in the sidebar all the input functions
    sidebarPanel(
      tabsetPanel(id="tabs",
                  tabPanel("otu",br(), 
                           p("On this tab you can choose to look at results from anova analysis and pair-wise tests for otus' of choice\n
                             first choose you dataset (either bacterial or fungal, and the phylogenetic resolution),then the specific otu"),
                           selectInput('datasetO', 'dataset', names(abundance_tables),selected=names(abundance_tables)[1]),
                           uiOutput("otu"), br(),
                           # Add comment
                           p("For details on OTU identification please refer to the original publications")),
                  tabPanel("anova", br(),
                           p("On this tab you can view anova results based on all the treatments, or just those of the same sample type (root or soil), choose a p-value cutoff and factors of interest"),br(),
                           sliderInput('pval','p-value for significance',
                                                value=0.1,min=0,max=0.5,step=0.00001),
                           selectInput('datasetA', 'dataset', names(abundance_tables)),
                           selectInput('FactorsOfInt','factor of interest',FactorsOfInt,selected="Metarhizium")),
                  tabPanel("raw_data",selectInput('datasetD', 'dataset', names(abundance_tables),selected=names(abundance_tables)[1]),
                           downloadButton("downloadData", "Download"))
      ) 
    ),
    # Put in the main panel of the layout the output functions 
    mainPanel(
      conditionalPanel(condition="input.tabs == 'otu'",
                       plotOutput('plot'),
                       h2("anova by Sample Type"),
                       fluidRow(
                         column(strong("Root"),width=6,tableOutput("tableR")),
                         column(strong("Soil"),width=6,tableOutput("tableS"))
                       ),
                       h2("pair-wise t-tests by Sample Type"),
                       fluidRow(
                         column(strong("Root"),width=6,tableOutput("pwTabR")),
                         column(strong("Soil"),width=6,tableOutput("pwTabS"))
                       ),
                       h2("anova (all treatments)"),
                       tableOutput("table1")
                       
      ),
      conditionalPanel(condition="input.tabs == 'anova'",
                       #plotOutput('plot2')
                       br(),
                       h2("anova (all treatments)"),
                       br(),
                       tableOutput("anova_tab2"),
                       h2("anova by Sample Type"),
                       fluidRow(
                         column(strong("Root"),width=6,tableOutput("anova_tab2R")),
                         column(strong("Soil"),width=6,tableOutput("anova_tab2S"))
                       )
                       
      )
                       
    )
  )
)

# ========================= SERVER ####
server <- function(input, output){
  # Return the requested dataset ----
  datasetNameO<-reactive({input$datasetO})
 # datasetInput <- reactive({
#    abundance_tables[[input$dataset]]
#  })
  pvalInput<-reactive({
    input$pval
  })
  comparisonInput<-reactive({
    input$FactorsOfInt
  })
  datasetNameA<-reactive({input$datasetA})
  # dataset chosen in download tab
  datasetNameD<-reactive({input$datasetD})
  # output otus to choose basaed on dataset selection
  output$otu <- renderUI({
    selectInput(inputId = "otu", label = "otu",
                choices = colnames(abundance_tables[[datasetNameO()]]),selected="Metarhizium")
  })
  otuInput<-reactive({
    input$otu
  })
  ##
  output$plot <- renderPlot({
     req(is.null(input$otu)==F)
    df<-abundance_tables[[datasetNameO()]]
    otu<-otuInput()
    ## melt and add sample metadata
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    #
   # df_annot<-subset(df_annot,df_annot$Bean =="Bean")
    #
    dfM<-melt(df_annot,id.vars = c("Location","Bean","Fungi","Insect"),value.name="abund")
    # renaming Fungi level to metarhizium
    levels(dfM$Fungi)<-c("M+","M-")
    # subset based on otu of interest
    dfM.sub<-subset(dfM,dfM$variable==otu)
    dfM.sub$Insect[which(dfM.sub$Bean =="No bean")]<-"Soil_alone"
    # 
    ggplot(dfM.sub,aes(x=Insect,y=abund,fill=Fungi))+geom_boxplot()+
      facet_wrap(~Location,scales="free" )+
      guides(fill=guide_legend("Metarhizium")) +
      ggtitle(otu)+
      scale_x_discrete(labels= c("I+","I-","soil alone"))+
      theme(plot.title = element_text(size = 18, face = "bold"))+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14)) + 
      theme(legend.text=element_text(size=14),
            legend.title=element_text(size=14)) +
      theme(strip.text.x = element_text(size = 14))
  })
  ## now make anova table
  ## all factors
  output$table1 <- renderTable({
    req(is.null(input$otu)==F)
    df<-abundance_tables[[datasetNameO()]]
    #df<-datasetInput()
    otu<-otuInput()
    print(paste("otu selected:",otu))
    ## melt and add sample metadata
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    #
    df_annot<-subset(df_annot,df_annot$Bean =="Bean")
    df_annot<-drop.levels(df_annot)
    #
    dfM<-melt(df_annot,id.vars = c("Location","Bean","Fungi","Insect"),value.name="abund")
    # renaming Fungi level to metarhizium
    # levels(dfM$Fungi)<-c("Metarhizium","No Meta")
    # subset based on otu of interest
    dfM.sub<-subset(dfM,dfM$variable==otu)
    aov.ex <- aov(dfM.sub$abund~dfM.sub$Location+dfM.sub$Fungi+dfM.sub$Insect+
                    dfM.sub$Fungi:dfM.sub$Insect+dfM.sub$Fungi:dfM.sub$Location+dfM.sub$Insect:dfM.sub$Location)
    anova_table<-as.data.frame(summary(aov.ex)[[1]])
    anova_table<-transform(anova_table,Factor=c("Sample","Metarhizium","Insect","Meta*Insect","Meta*Sample","Insect*Sample","residuals"))
    anova_table<-anova_table[-7,c(6,4,5)] 
    colnames(anova_table)<-c("Factor(s)","F-value","P-value")
    anova_table$`P-value`<-format(anova_table$`P-value`,digits=3,scientific=5)
    return(anova_table)
  })
  ## now just for soil
  output$tableS <- renderTable({
    req(is.null(input$otu)==F)
    df<-abundance_tables[[datasetNameO()]]
   # df<-datasetInput()
    otu<-otuInput()
    ## melt and add sample metadata
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    #
    df_annot<-subset(df_annot,df_annot$Bean =="Bean" & df_annot$Location=="Soil")
    df_annot<-drop.levels(df_annot)
    #
    dfM<-melt(df_annot,id.vars = c("Location","Bean","Fungi","Insect"),value.name="abund")
    # renaming Fungi level to metarhizium
    # levels(dfM$Fungi)<-c("Metarhizium","No Meta")
    # subset based on otu of interest
    dfM.sub<-subset(dfM,dfM$variable==otu)
    aov.ex <- aov(dfM.sub$abund~dfM.sub$Fungi+dfM.sub$Insect+dfM.sub$Fungi:dfM.sub$Insect)
    anova_tableS<-as.data.frame(summary(aov.ex)[[1]])
    anova_tableS<-transform(anova_tableS,Factor=c("Metarhizium","Insect","Meta*Insect","residuals"))
    anova_tableS<-anova_tableS[-4,c(6,4,5)]
    colnames(anova_tableS)<-c("Factor(s)","F-value","P-value")
    anova_tableS$`P-value`<-format(anova_tableS$`P-value`,digits=3,scientific=5)
    return(anova_tableS)
  })
  ## now just for root
  output$tableR <- renderTable({
    req(is.null(input$otu)==F)
    df<-abundance_tables[[datasetNameO()]]
    #df<-datasetInput()
    otu<-otuInput()
    ## melt and add sample metadata
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    #
    df_annot<-subset(df_annot,df_annot$Bean =="Bean" & df_annot$Location=="Root")
    df_annot<-drop.levels(df_annot)
    #
    dfM<-melt(df_annot,id.vars = c("Location","Bean","Fungi","Insect"),value.name="abund")
    # renaming Fungi level to metarhizium
    # levels(dfM$Fungi)<-c("Metarhizium","No Meta")
    # subset based on otu of interest
    dfM.sub<-subset(dfM,dfM$variable==otu)
    aov.ex <- aov(dfM.sub$abund~dfM.sub$Fungi+dfM.sub$Insect+dfM.sub$Fungi:dfM.sub$Insect)
    anova_tableR<-as.data.frame(summary(aov.ex)[[1]])
    anova_tableR<-transform(anova_tableR,Factor=c("Metarhizium","Insect","Meta*Insect","residuals"))
    anova_tableR<-anova_tableR[-4,c(6,4,5)]
    colnames(anova_tableR)<-c("Factor(s)","F-value","P-value")
    anova_tableR$`P-value`<-format(anova_tableR$`P-value`,digits=3,scientific=5)
    return(anova_tableR)
  })
  ## all pairwise comparisons
  output$pwTabR<-renderTable({
    req(is.null(input$otu)==F)
    df<-abundance_tables[[datasetNameO()]]
    #df<-datasetInput()
    otu<-otuInput()
    ##  add sample metadata
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    #
    df_loc<-subset(df_annot,df_annot$Bean =="Bean" & df_annot$Location=="Root")
    df_loc<-drop.levels(df_loc)
    #
    pw.FvsNFtab<-data.frame(comparison=as.character(),
                            FoldChange=as.numeric(),pval=as.numeric(),
                            stringsAsFactors = FALSE)
    comparison=c("M+/I+ vs M-/I+","M+/I- vs M-/I-","I+/M+ vs I-/M+","I+/M- vs I-/M-")
      # with insect comparison
      df_loc_FI<-subset(df_loc,Fungi=="Fungi" & Insect=="Insect")
      df_loc_NFI<-subset(df_loc,Fungi=="NF" & Insect=="Insect")
      ttI<-t.test(df_loc_FI[,otu],df_loc_NFI[,otu])
      FCI<-median(df_loc_FI[,otu],na.rm=T)/median(df_loc_NFI[,otu],na.rm=T)
      pvalI<-ttI$p.value
      # without insect
      df_loc_FNI<-subset(df_loc,Fungi=="Fungi" & Insect=="NI")
      df_loc_NFNI<-subset(df_loc,Fungi=="NF" & Insect=="NI")
      ttNI<-t.test(df_loc_FNI[,otu],df_loc_NFNI[,otu])
      FCNI<-median(df_loc_FNI[,otu],na.rm=T)/median(df_loc_NFNI[,otu],na.rm=T)
      pvalNI<-ttNI$p.value
      ## now hold meta constant
      tt3<-t.test(df_loc_FI[,otu],df_loc_FNI[,otu])
      FC3<-median(df_loc_FI[,otu],na.rm=T)/median(df_loc_FNI[,otu],na.rm=T)
      pval3<-tt3$p.value
      # 
      tt4<-t.test(df_loc_NFI[,otu],df_loc_NFNI[,otu])
      FC4<-median(df_loc_NFI[,otu],na.rm=T)/median(df_loc_NFNI[,otu],na.rm=T)
      pval4<-tt4$p.value
      #
      pw.FvsNFtab[1,]<-c(comparison[1],format(FCI,digits=3),format(pvalI,digits=3))
      pw.FvsNFtab[2,]<-c(comparison[2],format(FCNI,digits=3),format(pvalNI,digits=3))
      pw.FvsNFtab[3,]<-c(comparison[3],format(FC3,digits=3),format(pval3,digits=3))
      pw.FvsNFtab[4,]<-c(comparison[4],format(FC4,digits=3),format(pval4,digits=3))
      return(pw.FvsNFtab)
      
    })
  output$pwTabS<-renderTable({
    req(is.null(input$otu)==F)
    df<-abundance_tables[[datasetNameO()]]
    #df<-datasetInput()
    otu<-otuInput()
    ##  add sample metadata
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    #
    df_loc<-subset(df_annot,df_annot$Bean =="Bean" & df_annot$Location=="Soil")
    df_loc<-drop.levels(df_loc)
    #
    pw.FvsNFtab<-data.frame(comparison=as.character(),
                            FoldChange=as.numeric(),pval=as.numeric(),
                            stringsAsFactors = FALSE)
    comparison=c("M+/I+ vs M-/I+","M+/I- vs M-/I-","I+/M+ vs I-/M+","I+/M- vs I-/M-")
    # with insect comparison
    df_loc_FI<-subset(df_loc,Fungi=="Fungi" & Insect=="Insect")
    df_loc_NFI<-subset(df_loc,Fungi=="NF" & Insect=="Insect")
    ttI<-t.test(df_loc_FI[,otu],df_loc_NFI[,otu])
    FCI<-median(df_loc_FI[,otu],na.rm=T)/median(df_loc_NFI[,otu],na.rm=T)
    pvalI<-ttI$p.value
    #
    df_loc_FNI<-subset(df_loc,Fungi=="Fungi" & Insect=="NI")
    df_loc_NFNI<-subset(df_loc,Fungi=="NF" & Insect=="NI")
    ttNI<-t.test(df_loc_FNI[,otu],df_loc_NFNI[,otu])
    FCNI<-median(df_loc_FNI[,otu],na.rm=T)/median(df_loc_NFNI[,otu],na.rm=T)
    pvalNI<-ttNI$p.value
    ##
    ## now hold meta constant
    tt3<-t.test(df_loc_FI[,otu],df_loc_FNI[,otu])
    FC3<-median(df_loc_FI[,otu],na.rm=T)/median(df_loc_FNI[,otu],na.rm=T)
    pval3<-tt3$p.value
    # 
    tt4<-t.test(df_loc_NFI[,otu],df_loc_NFNI[,otu])
    FC4<-median(df_loc_NFI[,otu],na.rm=T)/median(df_loc_NFNI[,otu],na.rm=T)
    pval4<-tt4$p.value
    #
    pw.FvsNFtab[1,]<-c(comparison[1],format(FCI,digits=3),format(pvalI,digits=3))
    pw.FvsNFtab[2,]<-c(comparison[2],format(FCNI,digits=3),format(pvalNI,digits=3))
    pw.FvsNFtab[3,]<-c(comparison[3],format(FC3,digits=3),format(pval3,digits=3))
    pw.FvsNFtab[4,]<-c(comparison[4],format(FC4,digits=3),format(pval4,digits=3))
    return(pw.FvsNFtab)
  })

 #### now output for anova tab 
  output$anova_tab2 <- renderTable({
#    req(is.null(comparisonInput())==F)
   # df<-datasetInput()
    df<-abundance_tables[[datasetNameA()]]
    pval<-pvalInput()
    compFacts<-comparisonInput()
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    df_annot<-subset(df_annot,df_annot$Bean =="Bean")
    sum_tab.sig<-data.frame(Df=as.numeric(),Sum.Sq=as.numeric(),Mean.Sq=as.numeric(),Fval=as.numeric(),Pval=as.numeric(),OTU=as.character())
    for (i in 1:ncol(df)) {
      if (compFacts=="Metarhizium"){
      aov.ex <- aov(df_annot[,i]~df_annot$Fungi)
      sum_tab<-summary(aov.ex)[[1]]
      } 
      else if (compFacts=="Insect"){
        aov.ex <- aov(df_annot[,i]~df_annot$Insect)
        sum_tab<-summary(aov.ex)[[1]]
      } else if (compFacts=="Sample_Type"){
        aov.ex <- aov(df_annot[,i]~df_annot$Location)
        sum_tab<-summary(aov.ex)[[1]]
      }
      else if (compFacts=="Metarhizium*Insect"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi*df_annot$Insect)
        sum_tab<-summary(aov.ex)[[1]][3,]
      }
      else if (compFacts=="Metarhizium*Sample_Type"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi*df_annot$Location)
        sum_tab<-summary(aov.ex)[[1]][3,]
        }
      ## summary of the anova given as a list
      sum_tab<-transform(sum_tab,OTU=colnames(df)[i])
      temp.sig<-sum_tab[sum_tab[,5]<pval,]  
      sum_tab.sig<-rbind(sum_tab.sig,temp.sig)
    }
    sum_tab.sig<-sum_tab.sig[!is.na(sum_tab.sig[,1]),]
    sum_tab.sig<-sum_tab.sig[-1,c(6,4,5)]
    sum_tab.sig<-sum_tab.sig[order(sum_tab.sig[,3]),]
    colnames(sum_tab.sig)<-c("OTU","F-value","P-value")
    sum_tab.sig$'P-value'<-format(sum_tab.sig$'P-value',digits=3)
    sum_tab.sig$'F-value'<-format(sum_tab.sig$'F-value',digits=3)
    if (dim(sum_tab.sig)[1]==0){
      sum_tab.sig<-c(rep(0,3),"no significant trends at this pvalue")
    }
    return(sum_tab.sig)
  },striped = TRUE, bordered = TRUE)
  ## now just for soil
  output$anova_tab2S <- renderTable({
    req(is.null(comparisonInput())==F)
    pval<-pvalInput()
    df<-abundance_tables[[datasetNameA()]]
   # df<-datasetInput()
    compFacts<-comparisonInput()
    print(paste("compFacts:",compFacts))
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    df_annot<-subset(df_annot,df_annot$Bean =="Bean" & df_annot$Location =="Soil")
    sum_tab.sig<-data.frame(Df=as.numeric(),Sum.Sq=as.numeric(),Mean.Sq=as.numeric(),Fval=as.numeric(),Pval=as.numeric(),OTU=as.character())
    for (i in 1:ncol(df)) {
      if (input$FactorsOfInt=="Metarhizium"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi)
        sum_tab<-summary(aov.ex)[[1]]
      } 
      else if (compFacts=="Insect"){
        aov.ex <- aov(df_annot[,i]~df_annot$Insect)
        sum_tab<-summary(aov.ex)[[1]]
      } else if (compFacts=="Sample_Type"){
        aov.ex <- aov(df_annot[,i]~df_annot$Location)
        sum_tab<-summary(aov.ex)[[1]]
      }
      else if (compFacts=="Metarhizium*Sample_Type"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi*df_annot$Location)
        sum_tab<-summary(aov.ex)[[1]][3,]
      }
      else if (compFacts=="Metarhizium*Insect"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi*df_annot$Insect)
        sum_tab<-summary(aov.ex)[[1]][3,]
      }
      ## summary of the anova given as a list
      sum_tab<-transform(sum_tab,OTU=colnames(df)[i])
      temp.sig<-sum_tab[sum_tab[,5]<pval,]  
      sum_tab.sig<-rbind(sum_tab.sig,temp.sig)
      sum_tab.sig<-sum_tab.sig[!is.na(sum_tab.sig[,1]),]
    }
    sum_tab.sig<-sum_tab.sig[-1,c(6,4,5)]
    sum_tab.sig<-sum_tab.sig[order(sum_tab.sig[,3]),]
    colnames(sum_tab.sig)<-c("OTU","F-value","P-value")
    sum_tab.sig$'P-value'<-format(sum_tab.sig$'P-value',digits=3)
    sum_tab.sig$'F-value'<-format(sum_tab.sig$'F-value',digits=3)
    if (dim(sum_tab.sig)[1]==0){
      sum_tab.sig<-c(rep(0,3),"no significant trends at this pvalue")
    }
    return(sum_tab.sig)
  },striped = TRUE, bordered = TRUE)
  ## now just for root
  output$anova_tab2R <- renderTable({
    req(is.null(comparisonInput())==F)
    pval<-pvalInput()
    df<-abundance_tables[[datasetNameA()]]
    #df<-datasetInput()
    compFacts<-comparisonInput()
    df_annot<-merge(df,sample_metadata,by="row.names",all.x=T)
    rownames(df_annot)<-df_annot[,1]
    df_annot<-df_annot[,-1]
    df_annot<-subset(df_annot,df_annot$Bean =="Bean" & df_annot$Location=="Root")
    sum_tab.sig<-data.frame(Df=as.numeric(),Sum.Sq=as.numeric(),Mean.Sq=as.numeric(),Fval=as.numeric(),Pval=as.numeric(),OTU=as.character())
    for (i in 1:ncol(df)) {
      if (input$FactorsOfInt=="Metarhizium"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi)
        sum_tab<-summary(aov.ex)[[1]]
      } 
      else if (compFacts=="Insect"){
        aov.ex <- aov(df_annot[,i]~df_annot$Insect)
        sum_tab<-summary(aov.ex)[[1]]
      } else if (compFacts=="Sample_Type"){
        aov.ex <- aov(df_annot[,i]~df_annot$Location)
        sum_tab<-summary(aov.ex)[[1]]
      }
      else if (compFacts=="Metarhizium*Sample_Type"){
      aov.ex <- aov(df_annot[,i]~df_annot$Fungi*df_annot$Location)
      sum_tab<-summary(aov.ex)[[1]][3,]
      }
      else if (compFacts=="Metarhizium*Insect"){
        aov.ex <- aov(df_annot[,i]~df_annot$Fungi*df_annot$Insect)
        sum_tab<-summary(aov.ex)[[1]][3,]
      }
      ## summary of the anova given as a list
      sum_tab<-transform(sum_tab,OTU=colnames(df)[i])
      temp.sig<-sum_tab[sum_tab[,5]<pval,]  
      sum_tab.sig<-rbind(sum_tab.sig,temp.sig)
      sum_tab.sig<-sum_tab.sig[!is.na(sum_tab.sig[,1]),]
    }
    sum_tab.sig<-sum_tab.sig[-1,c(6,4,5)]
    sum_tab.sig<-sum_tab.sig[order(sum_tab.sig[,3]),]
    colnames(sum_tab.sig)<-c("OTU","F-value","P-value")
    sum_tab.sig$'P-value'<-format(sum_tab.sig$'P-value',digits=3)
    sum_tab.sig$'F-value'<-format(sum_tab.sig$'F-value',digits=3)
    if (dim(sum_tab.sig)[1]==0){
      sum_tab.sig<-c(rep(0,3),"no significant trends at this pvalue")
    }
    return(sum_tab.sig)
  }, striped = TRUE, bordered = TRUE)
  ##
  
  #### downloadData ##
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("metarhiz-microbiome",datasetNameD(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(abundance_tables[[datasetNameD()]], file, row.names = TRUE)
    }
  )
}
  ### end of server
shinyApp(ui=ui,server=server)
  