
#pkgs <- c("reticulate","keras", "lime", "tidyquant", "rsample", "recipes", "corrr","stringr","rworldmap")
#install.packages(pkgs)

library(rworldmap)
library(stringr)
library(ggplot2)
library(tidyverse)
library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(corrr)
library(ggforce)
library(scales)


gdp <- read.csv("Data/GDP.csv")
cost <- read.csv("Data/cost.csv")
nb <- read.csv("Data/NB.csv")
LowerS <- read.csv("Data/LowerS.csv")
percent_UP <- read.csv("Data/percent_urban_population.csv")
procedures <- read.csv("Data/procedures.csv")
sexratio <- read.csv("Data/sexratio.csv")
time <- read.csv("Data/time.csv")
countries <- read.delim("Data/countries.txt", header = FALSE, sep = "\n")
EODB_index <- read.csv("Data/EODB-index.csv")





# Preprocess Data
cost1 <- cost %>%
    mutate(Cost = cost$Cost...Men....of.income.per.capita. , Country = cost$Economy , Year = cost$DB.Year) %>%
    select( Country, Year, Cost ) %>%
    drop_na() %>%
    filter(!Year %in% c(2019,2020,2005)) %>%
    mutate(Cost, Cost = ifelse(Cost>75,"High",ifelse(Cost>25,"Medium High",ifelse(Cost>10,"Medium Low","Low"))))


gdp1 <- gdp %>%
    select (Country.Code , Country.Name, X2000:X2018) %>%
    gather("Year","GDP",3:21) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    mutate ( Log_GDP = log(GDP)) %>%
    select(Country = Country.Name, Code = Country.Code, Year,Log_GDP) %>%
    filter(!Year %in% c(2000,2001,2002,2003,2004,2005))


nb1 <- nb %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    rowwise() %>%
    mutate(X2017,X2017 = mean(c(X2015,X2016),na.rm = TRUE)) %>%
    mutate(X2018, X2018 = mean(c(X2016,X2017),na.rm = TRUE)) %>%
    gather("Year","NB",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year,NB) #%>%
#mutate(NB, NB = ifelse(NB>20000, "High",ifelse(NB>1000,"Medium","Low"))) 

LowerS1 <- LowerS %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","LowerSecondary",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, LowerSecondary)


percent_UP1 <- percent_UP %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","Percent_Urban_Population",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, Percent_Urban_Population)

procedures1 <- procedures %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","procedures",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, procedures)

sexratio1 <- sexratio %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    rowwise() %>%
    mutate(X2018,X2018 = mean(c(X2015,X2016),na.rm = TRUE)) %>%
    mutate(X2006,X2006 = mean(c(X2008,X2009),na.rm = TRUE)) %>%
    gather("Year","sexratio",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, sexratio)

time1 <- time %>%
    select (Country.Code , Country.Name, X2006:X2018) %>%
    gather("Year","time_days",3:15) %>%
    arrange(desc(Year)) %>%
    arrange(Country.Name)  %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Country.Name, Year, time_days)

EODB_index1 <- EODB_index %>%
    select (Jurisdiction, X2018:X2006) %>%
    gather("Year","EODB_index",2:14) %>%
    mutate(Year, Year = str_replace(Year, "X", "")) %>%
    select(Country = Jurisdiction, Year, EODB_index) %>%
    mutate(Country, Country = str_trim(Country)) %>%
    arrange(desc(Year)) %>%
    arrange(Country) %>%
    rowwise() %>%
    mutate(EODB_index, EODB_index = ifelse(EODB_index!= "N/A", parse_number(EODB_index),NA ))




Xian <- cost1
Xian <- merge(Xian,gdp1,by = c("Country","Year"), all.y = TRUE)
Xian <- merge(Xian,procedures1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,time1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,sexratio1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,nb1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,LowerS1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,percent_UP1,by = c("Country","Year"),all.y = TRUE)
Xian <- merge(Xian,EODB_index1,by = c("Country","Year"),all.x = TRUE)




#REPLACE NA VALUES AND REMOVE OTHER JUNK

Xian1 <- Xian %>%
    filter(Country %in% countries$V1 )  %>%
    arrange(desc(Year)) %>%
    arrange(Country) 

Xian <- Xian1 %>%
    mutate(index = seq(1,length(Xian1$Year),1)) %>%
    filter(index <= 2080 ) %>%
    select(index,Country,Code,Year,everything()) 


### Cost
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(Cost)), bestval = "") %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
#for few costs missing
for (i in 1:length(Xian$Cost)) {
    if (is.na(Xian$Cost[i]) == TRUE) {
        if (Xian$Country[i] == Xian$Country[i-1]){
            Xian$Cost[i] = Xian$Cost[i-1]}
    }
}
#For all costs missing
Xian <- Xian %>%
    mutate(Country, Cost = ifelse(Country == "Liechtenstein","Medium High",Cost ))

### GDP
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(Log_GDP)), bestval = max(Log_GDP, na.rm = TRUE))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(Log_GDP) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( Log_GDP, Log_GDP = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , Log_GDP , Log_GDP) ) ) 



### Procedures
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(procedures)), bestval = max(procedures, na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(procedures) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( procedures, procedures = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , procedures, procedures) ) ) 



### Time_days
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(time_days)), bestval = mean(time_days, na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(time_days) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( time_days, time_days = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , time_days, time_days) ) ) 


### sexratio
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(sexratio)), bestval = mean(Xian$sexratio, na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(sexratio) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( sexratio, sexratio = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , natest$bestval[which(natest$Country==Country)], sexratio) ) ) 


### NB and convert to category
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(NB)), bestval = mean(NB,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(NB) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( NB, NB = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$NB,na.rm = TRUE), NB) ) )  %>%
    mutate(NB, NB = ifelse(NB>20000, "High",ifelse(NB>1000,"Medium","Low"))) 


### Lower Secondary
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(LowerSecondary)), bestval = max(LowerSecondary,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(LowerSecondary) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( LowerSecondary, LowerSecondary = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$LowerSecondary,na.rm = TRUE), LowerSecondary) ) ) 


### Percent Urban Population
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(Percent_Urban_Population)), bestval = max(Percent_Urban_Population,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(Percent_Urban_Population) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( Percent_Urban_Population, Percent_Urban_Population = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$Percent_Urban_Population,na.rm = TRUE), Percent_Urban_Population) ) ) 


### EODB_Index
natest <- Xian %>%
    group_by(Country) %>%
    summarise(count = sum(is.na(EODB_index)), bestval = max(EODB_index,na.rm = TRUE)) %>%
    mutate(Method = ifelse(count>0 , ifelse(count==13,"All","Few") , "NAN"))
Xian <- Xian %>%
    mutate(HasNA = ifelse(is.na(EODB_index) == TRUE,"Yes","No")) %>%
    rowwise() %>%
    mutate(Method = ifelse(HasNA == "Yes", ifelse(natest$count[which(natest$Country==Country)]<13, "Few" , "All"), "NAN")) %>%
    mutate( EODB_index, EODB_index = ifelse( Method=="Few" , natest$bestval[which(natest$Country==Country)], ifelse(Method == "All" , mean(Xian$EODB_index,na.rm = TRUE), EODB_index) ) )

### Generate outcome variables
Xian$Y <- as.numeric(cut(Xian$EODB_index, 2))
Xian <- Xian  %>%
    select(-HasNA,-Method) 




train_data <- Xian %>%
    filter(Year != "2018") %>%
    mutate(Country, Country = as.character(Country)) %>%
    select(-index,-Code,-Year,-EODB_index)


test_data <- Xian %>%
    filter(Year == "2018") %>%
    mutate(Country, Country = as.character(Country)) %>%
    select(-index,-Code,-Year,-EODB_index)



rec_obj <- recipe(Y ~ ., data = train_data) %>%
    step_dummy(Country, one_hot = TRUE) %>%
    step_dummy(Cost, one_hot = TRUE) %>%
    step_dummy(NB, one_hot = TRUE) %>%
    step_center(all_predictors(), -all_outcomes()) %>%
    step_scale(all_predictors(), -all_outcomes()) %>%
    prep(data = train_data)

Xtrain <- bake(rec_obj, new_data = train_data) %>% select(-Y) 
Xtest  <- bake(rec_obj, new_data = test_data) %>% select(-Y) 

#str(Xtrain)
#glimpse(Xtrain)
# Response variables both sets
Ytrain <- to_categorical(train_data$Y)
Ytest  <- to_categorical(test_data$Y)



# Building our Artificial Neural Network
model <- keras_model_sequential()

model %>% 
    # First hidden layer
    layer_dense(units= 80, activation= "relu", kernel_initializer = "uniform",input_shape = ncol(Xtrain)) %>% 
    #layer_dense(units= 16, activation= "relu", kernel_initializer = "uniform") %>% 
    # Output layer
    layer_dense(units= 3, kernel_initializer = "uniform",activation = "softmax") 


# Compile ANN
model %>%  compile(optimizer = 'adam', loss  = 'binary_crossentropy', metrics   = 'accuracy' )


history <- model %>% fit(
    as.matrix(Xtrain), 
    as.matrix(Ytrain), 
    epochs = 20,
    batch_size = 32 , 
    validation_split = 0.2
)


pred <- model %>% predict_classes(as.matrix(Xtest))

# Confusion matrix
#table(actual=test_data$Y, pred)


#Predict probabilities
YProb  <- predict_proba(object = model, x = as.matrix(Xtest))

colnames(YProb) <- c("none","easy","noteasy")

YProb <- as.data.frame(YProb)
YProb <- YProb %>%
    rowwise() %>%
    mutate(asdad =  max(none,easy,noteasy)) 

YProb$Predicted = as.vector(pred)
YProb$Country = test_data$Country


Top_1_cat <- YProb %>%
    filter(Predicted == 1) %>%
    arrange(desc(asdad)) %>%
    select(Country)

Top_2_cat <- YProb %>%
    filter(Predicted == 2) %>%
    arrange(asdad) %>%
    select(Country)

OutputRank <- as.data.frame(bind_rows(Top_1_cat, Top_2_cat))

OutputRank <- OutputRank %>%
    mutate(Rank = seq(1,length(OutputRank$Country),1))
















#install.packages('rsconnect')
#install.packages("shinydashboard")
#install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(shinydashboard)
#library(devtools)
#install_github("nik01010/dashboardthemes")
#library(dashboardthemes)

header <- dashboardHeader(
    title = "Start-Up Global",
    titleWidth = 300
)

sidebar <- dashboardSidebar(sidebarMenuOutput('menu'))

body <- dashboardBody(
    
    #shinyDashboardThemes(theme = "blue_gradient"),
    
    tabItems(
        # homepage tab content
        tabItem(tabName="hp",
                fluidRow(
                    box(title= "Project Inrtoduction",height = 150,width = 4)),
                fluidRow(
                    box(title = "Global Overview",plotOutput("plot1"),height = 500,width = 5),
                    box(title="Best and worst performing countries historically",plotOutput("plot2"),height = 500,width = 5)),
                fluidRow(
                    box(title = "Other Information", height = 500,width = 5),
                    box(title = "Feature Correltaion",plotOutput("plot3"),height = 500,width = 5))),
        
        ## second tab content
        tabItem(tabName="countryselect",
                fluidRow(
                    box(title = "Select Country" , selectInput("CountrySelected",label = "",choices = Xian$Country), height = 150,width = 2 ),
                    box(title = "EODB Rank",textOutput("txtOutput1"),height = 150,width = 2,   tags$head(tags$style("#txtOutput1{color: blue;font-size: 40px;font-style: bold;}")))),
                fluidRow( 
                    box(title = "Index Table",tableOutput("table1"), height = 600,width = 2),
                    box(title = "Urban Population",plotOutput("plot4"), height= 480,width = 4),
                    box(title = "GDP",plotOutput("plot5"), height= 480,width = 4))
                #box(title = "Extra",plotOutput("plot6"), height= 350,width = 4))
                
                
                
        )
    )
)





server <- function(input, output){
    
    output$menu<-renderMenu({sidebarMenu(menuItem(text='Home',tabName = 'hp'),menuItem(text= "Country Measure",tabName = "countryselect")) })  
    
    output$plot1 <- renderPlot({
        ExtractCode <- Xian %>%
            group_by(Country,Code) %>%
            summarise(size = n()) %>%
            select(-size)
        OutputRank <- OutputRank %>%
            rowwise() %>%
            mutate(Country, Code = ExtractCode$Code[which(ExtractCode$Country==Country)])
        mapped_data <- joinCountryData2Map(OutputRank, joinCode = "ISO3", nameJoinColumn = "Code") 
        mapCountryData(mapped_data, nameColumnToPlot = "Rank", colourPalette=c('#1E6C03', '#E5FEDC'), addLegend='TRUE', borderCol = "grey", mapTitle = "Rank - Ease Of Doing Business")
    })
    
    output$plot2 <- renderPlot({
        plot_countries = c('New Zealand', 'Finland', 'Norway', 'Canada', 'Eritrea', 'Central African Republic', 'Iraq', 'Chad')
        bottom_countries = c('Eritrea', 'Equatorial Guinea', 'Central African Republic', 'Iraq', 'Chad')
        index1 <- Xian %>%
            filter(Country %in% plot_countries) %>%
            select(Country, Year, EODB_index)
        Plot2 <- ggplot(data=index1, aes(x=Year, y=EODB_index, color = Country, group = Country)) +geom_line() + geom_point() + labs(x="Year", y = "rank of ease of doing business")
        Plot2
    })
    
    output$plot3 <- renderPlot({
        Y2 <- as.data.frame(Ytrain) %>%
            mutate(easy = ifelse(as.data.frame(Ytrain)$V2 == 1, 1, 0))
        corrr_analysis <- Xtrain %>%
            mutate(EODB_Measure = as.vector(Y2$easy)) %>%
            correlate() %>%
            focus(EODB_Measure) %>%
            rename(feature = rowname) %>%
            arrange(abs(EODB_Measure)) %>%
            mutate(feature = as_factor(feature)) %>%
            arrange(desc(abs(EODB_Measure)))
        corrr_analysis2 <- corrr_analysis %>%
            filter(!startsWith(as.character(feature),"Country"))
        
        corrr_analysis2 %>%ggplot(aes(x = EODB_Measure, y = fct_reorder(feature, desc(EODB_Measure)))) + geom_point() +
            # Positive Correlations - Contribute 
            geom_segment(aes(xend = 0, yend = feature), 
                         color = palette_light()[[2]], 
                         data = corrr_analysis2 %>% filter(EODB_Measure > 0)) +
            geom_point(color = palette_light()[[2]], 
                       data = corrr_analysis2 %>% filter(EODB_Measure > 0)) +
            # Negative Correlations - Prevent 
            geom_segment(aes(xend = 0, yend = feature), 
                         color = palette_light()[[1]], 
                         data = corrr_analysis2 %>% filter(EODB_Measure < 0)) +
            geom_point(color = palette_light()[[1]], 
                       data = corrr_analysis2 %>% filter(EODB_Measure < 0)) +
            # Vertical lines
            geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
            geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
            geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
            # Aesthetics
            theme_tq() +
            labs(title = "Feature correlation in training data",y = "Feature",x = "Correlation Measure")
    })
    
    output$plot4 <- renderPlot({
        Countryset <- input$CountrySelected 
        UP <- Xian %>%
            group_by(Country) %>%
            summarise(Urban_Portion = mean(Percent_Urban_Population)) %>%
            mutate(Rural_Portion = 100 - Urban_Portion)
        UP1 <- UP %>%
            gather("Urban_Portion","Rural_Portion",2:3)  %>%
            arrange(Country) %>%
            filter(Country == Countryset)
        colnames(UP1) <- c("Country","Class","Perc")
        Plot4<- ggplot(UP1, aes(x = 2, y = Perc, fill = Class)) + geom_bar(stat = "identity", color = "white") + coord_polar(theta = "y", start = -pi/2)+ #geom_text(aes(y = round(Perc), label = round(Perc))) +
            theme_void()+xlim(0.5, 2.5)
        Plot4
    })
    
    output$plot5 <- renderPlot({
        Countryset <- input$CountrySelected
        gdpline <- Xian %>%
            select (Country, Year, Log_GDP) %>%
            mutate (GDP = exp(Log_GDP)) %>%
            filter(Country == Countryset)
        Plot5 <- ggplot(data = gdpline, aes(x = Year, y = GDP, group = 1)) + geom_line() + geom_point()
        Plot5
    })
    
    output$table1 <- renderTable ({
        Countryset <- input$CountrySelected 
        Table1<- Xian %>%
            filter(Year == '2018') %>%
            filter(Country == Countryset) %>%
            mutate(Log_GDP = format(round(Log_GDP, 2), nsmall = 2)) %>%
            mutate(Lower_Secondary = format(round(LowerSecondary, 2), nsmall = 2)) %>%
            mutate(Urban_Population = format(round(Percent_Urban_Population, 2), nsmall = 2)) %>%
            select(-Country,-index,-Y,-EODB_index,-LowerSecondary,-Percent_Urban_Population) %>%
            gather(key= Feature , value = Value)
        Table1
    })
    
    output$txtOutput1 <- renderText({
        Countryset <- input$CountrySelected 
        Text1<- OutputRank %>% filter(Country == Countryset) 
        Text1$Rank
    })
    
    
}

ui <- dashboardPage(header,sidebar,body,title = "Start-Up Global")
shinyApp(ui,server)




library(rsconnect)
rsconnect::setAccountInfo(name='neonflux56',
                          token='D55E3671B8842273FAA12FD48364FC26',
                          secret='lCgh0c7L9NLvY6nOYd22HOPd9ttr7Oyd0zou5jzI')

rsconnect::deployApp()


