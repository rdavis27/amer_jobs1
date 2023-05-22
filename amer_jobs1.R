library(shiny)
library(ggplot2)
library("foreign")
#library(AER)  # contains ivreg (old)
library(ivreg) # contains ivreg
library(plotly)
library(lmtest) # contains coeftest
library(sandwich) # contains vcovCL
library(DT)

# Replace 'ui <- fluidPage('...')' with
#       'shinyUI(fluidPage('...'))' if in separate ui.R file
ui <- fluidPage(
    titlePanel("Effect of the Immigrant Share on the Native Employment Rate"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput("repro", "Reproduce",
                        choices = c("Table 2 OLS","Table 3 OLS","Use years below"),
                        selected = "Use years below",
                        multiple = FALSE),
            splitLayout(
                numericInput("minyear","Min Year",2000,min = 2000,max = 2010),
                numericInput("maxyear","Max Year",2007,min = 2000,max = 2010)
            ),
            textInput("incstates","Include states",value = "#CA,NY,NJ,MA,MI,TX,MD,FL,IL,PA"), #most employed foreign STEM with Adv US Degrees
            #textInput("incstates","Include states",value = "#CA,TX,NY,FL,PA,IL,MI,NJ,MA,MD"), #most employed foreign STEM with Adv US Degrees
            #textInput("incstates","Include states",value = "#CA,TX,NY,FL,PA,OH,IL,MI,GA,NJ"), #most employed native Workers
            #checkboxInput("useols","Use OLS (else 2SLS)",value = TRUE),
            checkboxInput("sround","Round like study",value = TRUE),
            checkboxInput("useyears" ,"Add years indicator" ,value = TRUE),
            checkboxInput("usestates","Add states indicator",value = TRUE),
            checkboxInput("useweights","Add weights",value = TRUE),
            checkboxInput("clusterstates","Cluster(states)",value = TRUE),
            #checkboxInput("uselogs","Use logs",value = FALSE)
        ),
        mainPanel(
            width = 10,
            tabsetPanel(
                type = "tabs",
                tabPanel("Plotly",
                    sidebarPanel(
                        width = 2,
                        selectInput("yvar", "Y variable",
                                    choices = c("Slope","Tvalue","Pvalue","d_Slope","d_Stderr"),
                                    selected = "d_Slope",
                                    multiple = FALSE),
                        textInput("vcolor","Color",value = "black,blue2,orange2,red2"),
                        textInput("vshape","Shape",value = "1,10,16,15"),
                        checkboxInput("mark0","Mark x-axis",value = TRUE)
                    ),
                    mainPanel(
                        width = 10,
                        plotlyOutput(outputId = "myPlotly")
                    )
                ),
                tabPanel("Text", verbatimTextOutput("myText")),
                tabPanel("DT",
                         sidebarPanel(
                             width = 2,
                             selectInput("degree2", "Degree",
                                         choices = c("Bachelor's or higher","Advanced","Advanced in STEM Occupation"),
                                         selected = "Advanced in STEM Occupation",
                                         multiple = FALSE),
                             selectInput("location2", "Degree location",
                                         choices = c("U.S.","Abroad"),
                                         selected = "U.S.",
                                         multiple = FALSE),
                             textInput("vcolor2","Color",value = "white,lightblue,orange,red")
                         ),
                         mainPanel(
                             width = 10,
                             DT::dataTableOutput("myDT")
                         )
                ),
                tabPanel("Usage", htmlOutput(outputId = "myUsage")
                )
            )
        )
    )
)

# Replace 'server <- function(input, output) {'...'}' with
#       'shinyServer(function(input, output) {'...'})' if in separate server.R file
server <- function(input, output) {
    stabbr <- c("AL","AK","AZ","AR","CA",
                "CO","CT","DE","DC","FL",
                "GA","HI","ID","IL","IN",
                "IA","KS","KY","LA","ME",
                "MD","MA","MI","MN","MS",
                "MO","MT","NE","NV","NH",
                "NJ","NM","NY","NC","ND",
                "OH","OK","OR","PA","RI",
                "SC","SD","TN","TX","UT",
                "VT","VA","WA","WV","WI","WY")
    statid <- c(  1 ,  2 ,  4 ,  5 ,  6 ,
                  8 ,  9 , 10 , 11 , 12 ,
                  13 , 15 , 16 , 17 , 18 ,
                  19 , 20 , 21 , 22 , 23 ,
                  24 , 25 , 26 , 27 , 28 ,
                  29 , 30 , 31 , 32 , 33 ,
                  34 , 35 , 36 , 37 , 38 ,
                  39 , 40 , 41 , 42 , 44 ,
                  45 , 46 , 47 , 48 , 49 ,
                  50 , 51 , 53 , 54 , 55 , 56)
    
    slope  <- c(0.003,0.004,0.006,0.006,0.004,-0.0002)
    stderr <- c(0.003,0.003,0.003,0.003,0.003, 0.0025)
    zslope <- c(3,    3,    3,    3,    3,     4)
    zstderr<- c(3,    3,    3,    3,    3,     4)
    sig    <- c(0,    0,    2,    2,    1,     0)
    label <- c("Bachelor+ US","Bachelor+ AB",
               "Advanced US","Advanced AB",
               "Adv STEM US","Adv STEM AB")
    desc <- c("Bachelor's Degree or higher, US","Bachelor's Degree or higher, Abroad",
              "Advanced Degree, US"            ,"Advanced Degree, Abroad",
              "Advanced Degree in STEM Occ, US","Advanced Degree in STEM Occ, Abroad")
    tab2_ols <- data.frame(slope, stderr, zslope, zstderr, sig, label, desc)
    slope  <- c(0.001,0.002,0.001,0.005,0.002,0.001)
    stderr <- c(0.004,0.003,0.003,0.003,0.003,0.002)
    zslope <- c(3,    3,    3,    3,    3,     3)
    zstderr<- c(3,    3,    3,    3,    3,     3)
    sig    <- c(0,    0,    0,    0,    0,     0)
    tab3_ols <- data.frame(slope, stderr, zslope, zstderr, sig, label, desc)
    
    output$myUsage <- renderUI({
        includeHTML("http://econdataus.com/amer_jobs1.htm")
    })
    output$myPlotly <- renderPlotly({
        dd <- myTable()
        labelLevels <- c("Bachelor+ US","Bachelor+ AB",
                         "Advanced US","Advanced AB",
                         "Adv STEM US","Adv STEM AB")
        dd$Label <- factor(labelLevels, labelLevels)
        dd$Sig <- 0
        dd$Sig[dd$Pvalue < 0.1] <- 1
        dd$Sig[dd$Pvalue < 0.05] <- 2
        dd$Sig[dd$Pvalue < 0.01] <- 3
        dd$Significance <- factor(dd$Sig, levels = c(0,1,2,3))
        gg <- ggplot(data=dd, aes_string(x="Label",y=input$yvar))
        gg <- gg + geom_point(data=dd ,aes_string(color="Significance",shape="Significance"), size=5, alpha=1.0)
        vcolor <- unlist(strsplit(input$vcolor, ","))
        #gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
        #gg <- gg + scale_color_manual(values = vcolor) # Line Graph
        gg <- gg + scale_discrete_manual(aesthetics = "color", values = vcolor, drop = FALSE)
        vshape <- as.numeric(unlist(strsplit(input$vshape, ",")))
        gg <- gg + scale_shape_manual(values = vshape, drop = FALSE)
        if (input$mark0){
            gg <- gg + geom_hline(yintercept=0, color="black")
        }
        gg <- gg + ggtitle(paste0("<b>",get_title(),"</b><br>",
                           get_subtitle()))
        gg <- gg + xlab("Factor (AB = Abroad)")
        if (input$yvar == "d_Slope"){
            gg <- gg + ylab("% difference of Slope from Study")
        }
        else if (input$yvar == "d_Stderr"){
            gg <- gg + ylab("% difference of Stderr from Study")
        }
        else{
            gg <- gg + ylab(input$yvar)
        }
        gg
    })
    output$myText <- renderPrint({
        dd <- myTable()
        ivreg_print(dd)
        return('')
    })
    output$myDT <- DT::renderDataTable({
        if (input$degree2 == "Bachelor's or higher"){
            var1 <- "immshare_emp_coll_e"
            var2 <- "immshare_emp_coll_n"
        }
        else if (input$degree2 == "Advanced"){
            var1 <- "immshare_emp_grad_e"
            var2 <- "immshare_emp_grad_n"
        }
        else{
            var1 <- "immshare_emp_stem_e_grad"
            var2 <- "immshare_emp_stem_n_grad"
        }
        if (input$location2 == "Abroad"){
            iloc <- 3
        }
        else{
            iloc <- 2
        }
        dd <- myData()
        year <- seq(2000,2007)
        xx <- data.frame(year)
        varnames <- "Year"
        for (j in 2003:2010){
            var1name <- paste0("y",j)
            var2name <- paste0("z",j)
            varnames <- c(varnames, var1name,var2name)
            #assign(varname, rep(0,8))
            for (i in 2000:2007){
                #print(paste0("BEFORE do_reg(",i,",",j,")")) #DEBUG-RM
                if ((j-i) > 2){
                    tt <- do_reg(1, dd, var1, var2, i, j)
                    xx[[var1name]][i-1999] <- round(tt[iloc,1],4)
                    pvalue <- tt[iloc,4]
                    if (pvalue < 0.01) sig <- 3
                    else if (pvalue < 0.05) sig <- 2
                    else if (pvalue < 0.1)  sig <- 1
                    else sig <- 0
                    xx[[var2name]][i-1999] <- sig
                }
                else{
                    xx[[var1name]] [i-1999] <- 0
                    xx[[var2name]] [i-1999] <- 0
                }
            }
        }
        colnames(xx) <- c('Year','2003','z2003','2004','z2004','2005','z2005','2006','z2006',
                          '2007','z2007','2008','z2008','2009','z2009','2010','z2010')
        vcolor2 <- unlist(strsplit(input$vcolor2, ","))
        dt <- datatable(xx, caption = "This is the caption",
                        options = list(columnDefs = list(list(targets = c(3,5,7,9,11,13,15,17), visible = FALSE)))) %>%
            formatStyle(
            c('2003','2004','2005','2006','2007','2008','2009','2010'),
            c('z2003','z2004','z2005','z2006','z2007','z2008','z2009','z2010'),
            backgroundColor = styleEqual(c(0,1,2,3), vcolor2))
        zdt <<- dt
        return(dt)
    })
    get_title <- function(){
        if (input$repro != "Use years below") title <- paste0(input$repro," - ")
        else title <- ""
        title <- paste0(title,"Effect of the Immigrant Share on the Native Employment Rate")
        if (input$repro == "Table 2 OLS"){
            title <- paste0(title,", 2000-2007")
        }
        else if (input$repro == "Table 3 OLS"){
            title <- paste0(title,", 2000-2010")
        }
        else{
            title <- paste0(title,", ",input$minyear,"-",input$maxyear)
        }
    }
    get_subtitle <- function(){
        subtitle <- "by degree type and place"
        if (input$sround) subtitle <- paste0(subtitle,", rounded")
        if (input$useyears & input$usestates) subtitle <- paste0(subtitle,", includes years and states indicators")
        else if (input$useyears) subtitle <- paste0(subtitle,", includes years indicators")
        else if (input$usestates) subtitle <- paste0(subtitle,", includes states indicators")
        if (input$clusterstates) subtitle <- paste0(subtitle,", cluster(states)")
        return(subtitle)
    }
    ivreg_print <- function(xx){
        cat(paste0(get_title(),"\n"))
        cat(paste0(get_subtitle(),"\n\n"))
        cat("    CALCULATED VALUES                                             STUDY VALUES            % DIFFERENCES   DIFF\n")
        cat("    ------------------------------------------------------------  ---------------------  ---------------- ----\n")
        cat(" N  INTERCEPT     SLOPE   STDERR   TVALUE   PVALUE  SIG     JOBS    SLOPE   STDERR  SIG    SLOPE   STDERR  SIG  DESCRIPTION\n")
        cat("--  ---------  --------  -------  -------  -------  ---  -------  -------  -------  ---  -------  -------  ---  -----------\n")
        for (i in 1:NROW(xx)){
            vv <- xx[i,]
            cat(sprintf("%2d  %9.4f %9.4f %8.4f %8.4f %8.4f %4d %8.4f %8.4f %8.4f %4d %8.4f %8.4f %4d  %s\n",
                        i, vv$Intercept, vv$Slope, vv$Stderr, vv$Tvalue, vv$Pvalue, vv$Sig, vv$Jobs,
                        vv$s_Slope, vv$s_Stderr, vv$s_Sig,
                        vv$d_Slope, vv$d_Stderr, vv$d_Sig, vv$Description))
        }
    }
    save_mm <- function(ii, ivr, Intercept, Slope, Stderr, Tvalue, Pvalue, study, dd, var){
        Sig <- 0
        if (Pvalue < 0.10) Sig <- 1
        if (Pvalue < 0.05) Sig <- 2
        if (Pvalue < 0.01) Sig <- 3
        s_Slope  <- study$slope[ii]
        s_Stderr <- study$stderr[ii]
        s_Sig    <- study$sig[ii]
        if (input$sround){
            Slope  <- round(Slope,  study$zslope[ii])
            Stderr <- round(Stderr, study$zstderr[ii])
        }
        Jobs <- sum(dd$emp_native)/sum(dd[[var]])*Slope
        d_Slope  <- 100* (Slope  - s_Slope)  / s_Slope
        d_Stderr <- 100* (Stderr - s_Stderr) / s_Stderr
        d_Sig    <- Sig - s_Sig
        newrow <- data.frame(ii, Intercept, Slope, Stderr, Tvalue, Pvalue, Sig, Jobs,
                             s_Slope, s_Stderr, s_Sig, d_Slope, d_Stderr, d_Sig)
        ivr <- rbind(ivr, newrow)
    }
    do_reg <- function(ii, dd, var1, var2, year1, year2){
        # remove non-positive values so that their logs are valid values (not NaN or infinite, plus or minus)
        dd <- dd[dd$year >= year1 & dd$year <= year2,]
        kk <- dd[dd[[var1]] > 0 & dd[[var2]] > 0,]
        kk$log1 <- log(kk[[var1]])
        kk$log2 <- log(kk[[var2]])
        sform <- "lnemprate_native ~ log1 + log2"
        if (input$useyears){
            sform <- paste(sform,"+ fyear")
        }
        if (input$usestates){
            sform <- paste(sform,"+ fstate")
        }
        form <- as.formula(sform)
        if (input$useweights){
            mm <- lm(form, weights=weight_native, data=kk)
        }
        else{
            mm <- lm(form, data=kk)
        }
        if (input$clusterstates){
            tt <- coeftest(mm, vcov = vcovCL, cluster = ~fstate)
        }
        else{
            tt <- summary(mm)$coefficients
        }
        zmm <<- mm #DEBUG-RM
        ztt <<- tt #DEBUG-RM
        return(tt)
    }
    myTable <- function(){
        dd <- myData()
        # vi <- numeric(0)
        # coef1 <- numeric(0)
        # coef2 <- numeric(0)
        # study <- numeric(0)
        # pdiff <- numeric(0)
        # sum22 <- numeric(0)
        # sum23 <- numeric(0)
        # sum24 <- numeric(0)
        # ivr <- data.frame(vi, coef1, coef2, study, pdiff, sum22, sum23, sum24)
        ivr <- NULL
        if (input$repro == "Table 2 OLS"){
            minyear <- 2000
            maxyear <- 2007
            study <- tab2_ols
        }
        else if (input$repro == "Table 3 OLS"){
            minyear <- 2000
            maxyear <- 2010
            study <- tab3_ols
        }
        else{
            minyear <- input$minyear
            maxyear <- input$maxyear
            study <- tab2_ols
        }
        dd <- dd[dd$year >= minyear & dd$year <= maxyear,]
        tt <- do_reg(1, dd, "immshare_emp_coll_e", "immshare_emp_coll_n", minyear, maxyear)
        ivr <- save_mm(1, ivr, tt[1,1], tt[2,1], tt[2,2], tt[2,3], tt[2,4], study, dd, "emp_edus_coll")
        ivr <- save_mm(2, ivr, tt[1,1], tt[3,1], tt[3,2], tt[3,3], tt[3,4], study, dd, "emp_nedus_coll")
        tt <- do_reg(1, dd, "immshare_emp_grad_e", "immshare_emp_grad_n", minyear, maxyear)
        ivr <- save_mm(3, ivr, tt[1,1], tt[2,1], tt[2,2], tt[2,3], tt[2,4], study, dd, "emp_edus_grad")
        ivr <- save_mm(4, ivr, tt[1,1], tt[3,1], tt[3,2], tt[3,3], tt[3,4], study, dd, "emp_nedus_grad")
        tt <- do_reg(1, dd, "immshare_emp_stem_e_grad", "immshare_emp_stem_n_grad", minyear, maxyear)
        ivr <- save_mm(5, ivr, tt[1,1], tt[2,1], tt[2,2], tt[2,3], tt[2,4], study, dd, "emp_edus_stem_grad")
        ivr <- save_mm(6, ivr, tt[1,1], tt[3,1], tt[3,2], tt[3,3], tt[3,4], study, dd, "emp_nedus_stem_grad")
        
        zivr <<- ivr #DEBUG-RM
        xx <- ivr[,c(-1)]
        rownames(xx) <- seq(1,NROW(xx))
        colnames(xx) <- c("Intercept","Slope","Stderr","Tvalue","Pvalue","Sig","Jobs",
                          "s_Slope","s_Stderr","s_Sig","d_Slope","d_Stderr","d_Sig")
        xx$Description <- study$desc
        #cols_align(dd, align = "left", columns = "Description")
        zxx <<- xx #DEBUG-RM
        return(xx)
    }
    myData <- reactive({
        dd <- myFile()
        #dd <- dd[dd$year >= input$minyear & dd$year <= input$maxyear,]
        if (input$incstates != ""){
            incstates <- input$incstates
            loc <- unlist(gregexpr('#',incstates))[1]
            if (loc > 0){
                incstates <- substr(incstates,1,(loc-1))
            }
            if (incstates != ""){
                states <- unlist(strsplit(incstates, ","))
                istates <- rep(0, length(states))
                for (i in 1:length(states)){
                    istates[i] <- statid[which(stabbr == states[i])]
                }
                dd <- dd[dd$statefip %in% istates,]
                zdds <<- dd #DEBUG-RM
            }
        }
        # * Table 2
        # * by whether likely educated in the US or not
        # * row 1, columns 1 & 2
        # xi: reg lnemprate_native lnimmshare_emp_coll_e lnimmshare_emp_coll_n  i.statefip i.year [aw=weight_native] if year<2008, robust cluster(statefip)
        # * row 1, columns 3 & 4
        # xi: ivregress 2sls lnemprate_native (lnimmshare_emp_coll_e lnimmshare_emp_coll_n = lnimmshare_pop_coll_e lnimmshare_pop_coll_n)  i.statefip i.year [aw=weight_native] if year<2008, robust cluster(statefip)
        # estat firststage, all
        # 
        # * row 2, columns 1 & 2
        # xi: reg lnemprate_native lnimmshare_emp_grad_e lnimmshare_emp_grad_n  i.statefip i.year [aw=weight_native] if year<2008, robust cluster(statefip)
        # * row 2, columns 3 & 4
        # xi: ivregress 2sls lnemprate_native (lnimmshare_emp_grad_e lnimmshare_emp_grad_n = lnimmshare_pop_grad_e lnimmshare_pop_grad_n)  i.statefip i.year [aw=weight_native] if year<2008, robust cluster(statefip)
        # estat firststage, all
        # 
        # * row 3, columns 1 & 2
        # xi: reg lnemprate_native lnimmshare_emp_stem_e_grad lnimmshare_emp_stem_n_grad i.statefip i.year [aw=weight_native] if year<2008, robust cluster(statefip)
        # * row 3, columns 3 & 4
        # xi: ivregress 2sls lnemprate_native (lnimmshare_emp_stem_e_grad lnimmshare_emp_stem_n_grad = lnimmshare_pop_stem_e_grad lnimmshare_pop_stem_n_grad) i.statefip i.year [aw=weight_native] if year<2008, robust cluster(statefip)
        # estat firststage, all
        
        dd$emprate_native           <- dd$emp_native          / dd$pop_native * 100

        # from Stata code in lines 844-863 of Tables1to3.do
        dd$immshare_emp_coll_e <- dd$emp_edus_coll /dd$emp_total *100
        dd$immshare_pop_coll_e <- dd$pop_edus_coll /dd$emp_total *100
        dd$immshare_emp_coll_n <- dd$emp_nedus_coll/dd$emp_total *100
        dd$immshare_pop_coll_n <- dd$pop_nedus_coll/dd$emp_total *100
        dd$immshare_emp_grad_e <- dd$emp_edus_grad /dd$emp_total *100
        dd$immshare_pop_grad_e <- dd$pop_edus_grad /dd$emp_total *100
        dd$immshare_emp_grad_n <- dd$emp_nedus_grad/dd$emp_total *100
        dd$immshare_pop_grad_n <- dd$pop_nedus_grad/dd$emp_total *100
        dd$immshare_emp_stem      <- (dd$emp_edus_stem_coll + dd$emp_nedus_stem_coll)/dd$emp_total *100
        dd$immshare_pop_stem      <- (dd$pop_edus_stem_coll + dd$pop_nedus_stem_coll)/dd$emp_total *100
        dd$immshare_emp_stem_grad <- (dd$emp_edus_stem_grad + dd$emp_nedus_stem_grad)/dd$emp_total *100
        dd$immshare_pop_stem_grad <- (dd$pop_edus_stem_grad + dd$pop_nedus_stem_grad)/dd$emp_total *100
        dd$immshare_emp_e_stem <- dd$emp_edus_stem_coll/dd$emp_total *100
        dd$immshare_pop_e_stem <- dd$pop_edus_stem_coll/dd$emp_total *100
        dd$immshare_emp_stem_e_grad <- dd$emp_edus_stem_grad/dd$emp_total *100
        dd$immshare_pop_stem_e_grad <- dd$pop_edus_stem_grad/dd$emp_total *100
        dd$immshare_emp_n_stem <- dd$emp_nedus_stem_coll/dd$emp_total *100
        dd$immshare_pop_n_stem <- dd$pop_nedus_stem_coll/dd$emp_total *100
        dd$immshare_emp_stem_n_grad <- dd$emp_nedus_stem_grad/dd$emp_total *100
        dd$immshare_pop_stem_n_grad <- dd$pop_nedus_stem_grad/dd$emp_total *100
        
        # logs of prior values
        dd$lnemprate_native           <- log(dd$emprate_native)

        # year and state converted to factors as indicator variables
        dd$fyear  <- as.factor(dd$year)
        dd$fstate <- as.factor(dd$statefip)

        # need to normalize population weights so that each year has same weight;
        # otherwise, too much weight assigned to later years
        # do for total native population (age 16-64)
        dd$sum_pop_native <- with(dd, ave(pop_native, year, FUN=sum))
        dd$weight_native <- dd$pop_native / dd$sum_pop_native
        zdd <<- dd #DEBUG-RM
        return(dd)
    })
    myFile <- reactive({
        dd <- read.dta("public.dta")
        return(dd)
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)