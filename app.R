library(reactable)
library(shiny)
library(tidyverse)
library(gt)
library(googlesheets4)
library(janitor)
library(writexl)
library(readxl)
library(bslib)
library(bs4Dash)
library(plotly)
library(rsconnect)
library(caret)
library(e1071)
library(class)


# Preliminary Data Loading
load("models.RData")
load("knn_data.RData")

soc_size = events_model$swmem[nrow(events_model)]
models = c("KNN", "SVR", "Regression Tree", "Random Forest")
# Load in the discord server data
discord = read.csv("data/server_stats.csv")
new_names = c("messages", "new_members", "timeouts", "warning_bans", "poggers", "user_raised_issues", "week", "start_date", "end_date", "message_ban", "warnings_member", "warnings_message", "warnings_new", "warnings_mem", "issues_message", "issues_new", "user_raised_issues_percent", "health_score", "rating")

colnames(discord) = new_names

# List of Stats for Event Analytics tool
soc_stats = c("attendance", 
              "fmratio", "returning", "demographic", "year", "international")

current_member_info <- read_sheet("https://docs.google.com/spreadsheets/d/1rZbDscF7A1N1Qivh3H0NTHDnw41BGZX6228cIn-ccaM/edit?gid=1763387998#gid=1763387998", sheet = "Data Wrangling") # Read in Data. 
# ================================================
# Preprocessing
new_names = c("member", "firstname", "preferredname", "lastname", "usu_number", "pronouns","card_no","email","notes","gender","media","nationality","year","subculture")
new_names2022 = c("member", "firstname","preferredname", "lastname","usu_number","card_no","notes", "card_no2","year")
stats = read_excel("data/2025_stats.xlsx")

member2022 = read_sheet("https://docs.google.com/spreadsheets/d/1RAMAuK7TWcQPi6cs-nphxJIy3aO8CwBKkdA3vSygKRs/edit?gid=316341411#gid=316341411", sheet = "Data Wrangling")
member2023 = read_sheet("https://docs.google.com/spreadsheets/d/1lDs-xJq3hIx46krHSRdEkIqcMThrBB5AvZUeLtXTcks/edit?gid=1526743482#gid=1526743482", sheet = "Data Wrangling")
member2024 = read_sheet("https://docs.google.com/spreadsheets/d/1W9VP5X0-ZclRysUeUlIBMw-_QVVQsQZbZs-65FemgSU/edit?gid=822533709#gid=822533709", sheet = "Data Wrangling")
member2025 = read_sheet("https://docs.google.com/spreadsheets/d/1rZbDscF7A1N1Qivh3H0NTHDnw41BGZX6228cIn-ccaM/edit?gid=1763387998#gid=1763387998", sheet = "Data Wrangling")

c(2072539, 2224556, 2189572, 2471004, 2436355, 2223030, 2384144, 2351235, 2386582, 2411427, 2229025, 2353751, 2402756, 2466680)

events = read_sheet("https://docs.google.com/spreadsheets/d/1wegv4TXT2jBnZRoYtrP3s7eMm7-S22e97qzzmjdw0Ww/edit?gid=1078826250#gid=1078826250")

# Rename columns
colnames(member2022) = new_names2022
colnames(member2023) = new_names
colnames(member2024) = new_names
colnames(member2025) = new_names
# ========================================================

# Everything doer that calculates all event metrics
every_doer = function(eventurl){
  event <- read_sheet(eventurl)[1:2]
  colnames(event) = c("Member", "usu_number")
  
  attendees = (event$Member)[!is.na(event$Member)]
  newcount = 0
  
  new_members = attendees[!(attendees %in% c(stats$name))]
  
  for (mem in new_members) {
    newcount = newcount + 1
    new_member <- data.frame(mem, 0, 0, 0, 0, 0, 0, 0, 0)
    names(new_member) <- c("name", "cass1", "cass2", "hangs1", "hangs2", "draws1", "draws2", "regs1", "regs2")
    stats <- rbind(stats, new_member)
  }
  
  # Calculate the number of returning members
  returning =  signif(mean(event$usu_number!= 0 & !event$usu_number %in% exec_usus & (event$usu_number %in% member2024$usu_number | event$usu_number %in% member2023$usu_number |
                                                                                        event$usu_number %in% member2022$usu_number)), 2)
  
  # We have the option of also checking NEW members that are also first year
  new = mean(event$usu_number!= 0 & !event$usu_number %in% exec_usus & !(event$usu_number %in% member2024$usu_number | event$usu_number %in% member2023$usu_number |
                                                                           event$usu_number %in% member2022$usu_number))
  
  colnames(event)[2] = "usu_number" 
  
  tflist = stats$name %in% attendees
  stats$regs1 = stats$regs1 + tflist
  #Swap for relevant statistic!
  write_xlsx(stats, path = "2025_stats.xlsx")
  
  calculable <- inner_join(event, current_member_info, by = c("Member" = "Member"))
  
  calculable %>%
    mutate(first = (year == "First Year")) %>%
    mutate(second = (year == "Second Year")) %>%
    mutate(old = !(first | second)) %>%
    mutate(male = (gender == "Male")) %>%
    mutate(female = (gender == "Female")) %>%
    mutate(nonbi = !(male | female)) %>%
    mutate(national = (nationality == "International")) %>%
    mutate(discord = grepl('Discord', media)) %>%
    mutate(wechat = grepl('WeChat', media)) %>%
    mutate(tiktok = grepl('Tiktok', media)) %>%
    mutate(face = grepl('Facebook', media)) %>%
    mutate(insta = grepl('Instagram', media)) %>%
    mutate(returning = returning) %>%
    {math_obj <<- .}
  
  total = sum(math_obj$male + math_obj$female + math_obj$nonbi, na.rm = TRUE)
  first = signif(sum(math_obj$first, na.rm = TRUE) / total, 2)
  second = signif(sum(math_obj$second, na.rm = TRUE) / total, 2)
  old = signif(sum(math_obj$old, na.rm = TRUE) / total, 2)
  fmratio = signif(sum(math_obj$female, na.rm = TRUE)/sum(math_obj$male, na.rm = TRUE),2)
  nonbi = signif(sum(math_obj$nonbi, na.rm = TRUE)/total, 2)
  international = signif(sum(math_obj$national, na.rm = TRUE)/total, 2)
  discord = signif(sum(math_obj$discord, na.rm = TRUE)/total, 2)
  wechat = signif(sum(math_obj$wechat, na.rm = TRUE)/total, 2)
  tiktok = signif(sum(math_obj$tiktok, na.rm = TRUE)/total, 2)
  face = signif(sum(math_obj$face, na.rm = TRUE)/total, 2)
  insta = signif(sum(math_obj$insta, na.rm = TRUE)/total, 2)
  
  result = data.frame(attendance = total, first = first, second = second, old = old, international = international, discord = discord, wechat = wechat, tiktok = tiktok, face = face, insta= insta, fmratio = fmratio, nonbi = nonbi, returning = returning) 
  
  return(result) 
}


# Preprocessing for Subculture demographic interests
# ====================================================
member2025 = member2025 %>% mutate(subculture = tolower(member2025$subculture))
member2025$interests= str_split(member2025$subculture, ",") 
interests_temp = c()
# Split up the subculture interests into individual entries, then join them into a large vector
for (interest in member2025$interests){
  interests_temp = append(interests_temp, trimws(interest))
}

# Look at all entries in the vector, do some counting, replacing and get a dataframe of all subcultures
interests2025 = case_when(str_starts(interests_temp, "vtuber") ~ "vtuber", str_equal(interests_temp, "") ~ NA, TRUE ~ interests_temp) %>% forcats::fct_lump_min(min = 4) %>% table() %>% data.frame() %>% mutate(percent = 100* Freq/dim(member2025)[1], year = as.factor(2025))
colnames(interests2025) = c("subculture", "interested", "interested_prop", "year")

member2024 = member2024 %>% mutate(subculture = tolower(member2024$subculture))
member2024$interests= str_split(member2024$subculture, ",") 
interests_temp = c()
# Split up the subculture interests into individual entries, then join them into a large vector
for (interest in member2024$interests){
  interests_temp = append(interests_temp, trimws(interest))
}

# Look at all entries in the vector, do some counting, replacing and get a dataframe of all subcultures
interests2024 = case_when(str_starts(interests_temp, "vtuber") ~ "vtuber", str_equal(interests_temp, "") ~ NA, TRUE ~ interests_temp) %>% forcats::fct_lump_min(min = 4) %>% table() %>% data.frame() %>% mutate(percent = 100* Freq/dim(member2024)[1], year = as.factor(2024))
colnames(interests2024) = c("subculture", "interested", "interested_prop", "year")

member2023 = member2023 %>% mutate(subculture = tolower(member2023$subculture))
member2023$interests= str_split(member2023$subculture, ",") 
interests_temp = c()

# Split up the subculture interests into individual entries, then join them into a large vector
for (interest in member2023$interests){
  interests_temp = append(interests_temp, trimws(interest))
}

# Look at all entries in the vector, do some counting, replacing and get a dataframe of all subcultures
interests2023 = case_when(str_starts(interests_temp, "vtuber") ~ "vtuber", str_equal(interests_temp, "") ~ NA, TRUE ~ interests_temp) %>% forcats::fct_lump_min(min = 4) %>% table() %>% data.frame() %>% mutate(percent = 100* Freq/dim(member2023)[1], year = as.factor(2023))

colnames(interests2023) = c("subculture", "interested", "interested_prop", "year")

interests= rbind(interests2023, interests2024, interests2025)


# Preprocessing for year groups
member2022 = member2022 %>%
  mutate(degree_year = case_when(
    year == "Old" ~ "Alumni",
    year == "Alumni" ~ "I'm not currently attending university",
    year == "None" ~ "I'm not currently attending university",
    year == "Exchange" ~ "Other",
    year == "Prospective student" ~ "I'm not currently attending university",
    year == "" ~ "NA",
    year == "UNSW rat" ~ "Other",
    TRUE ~ year
  ), year = "2022")


member2023 = member2023 %>%
  mutate(degree_year = case_when(
    year == "000" ~ "Other",
    year == "UNSW rat" ~ "Other",
    year == "6th" ~ "Other",
    year == "Alumni" ~ "I'm not currently attending university",
    year == "Alumni from WSU" ~ "I'm not currently attending university",
    year == "Next year" ~ "I'm not currently attending university",
    year == "not student" ~ "I'm not currently attending university",
    year == "no" ~ "I'm not currently attending university",
    year == "Phd" ~ "Postgraduate",
    year == "DEC 25" ~ "Other",
    year == "Exchange" ~ "Other",
    year == "exchange student" ~ "Other",
    year == "Gang" ~ "Other",
    year == "Tahun kedua puluh tiga" ~ "Other",
    year == "Too many " ~ "Other",
    year == "WH visa " ~ "Other",
    year == "Yes" ~ "Other",
    year == "na" ~ "NA",
    year == "" ~ "NA",
    year == " " ~ "NA",
    TRUE ~ year
  ), year = "2023")

member2025 = member2025 %>% mutate(degree_year= year, year = "2025")

member_years = rbind(member2025 %>% select("degree_year", "year"), member2024 %>% select("degree_year", "year"), member2023 %>% select("degree_year", "year"), member2022 %>% select("degree_year","year"))

# Shiny UI
ui <- dashboardPage(
  title = "Operation Black Knights",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Black Knights",
      color = "primary",
      href = "",
      image = ""
    ), 
    skin = "light",
    status = "white"
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    status = "warning",
    elevation = 3,
    sidebarUserPanel(
      image = NULL,
      name = "Welcome!"
    ),
    sidebarMenu(
      sidebarHeader("Select Tool"),
      menuItem("Attendance Predictor",
               tabName= "predict",
               icon = icon("gear")
      ),
      
      menuItem("Event Analytics", tabName = "events",
               icon = icon("chart-simple")
      ),
      
      menuItem("Event Stats Calculator", tabName = "calc",
               icon = icon("table")
      ),
      
      menuItem("Server Analytics",
               tabName = "server",
               icon = icon("discord")
      ))
  ),
  
  controlbar = dashboardControlbar(),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "predict",
        fluidPage(
          selectInput("model", "Select a Model", models, selected = "KNN"),
          uiOutput("value_inputs"),
          actionButton("btn", "Run"),
          reactableOutput("table1"),)
      ),
      tabItem(
        tabName = "events", 
        fluidPage(
          selectInput("analytic", "Choose a stat: ", soc_stats, selected = "reg_attendance"),
          uiOutput("anal_out"),
        )
      ),
      
      tabItem(
        tabName = "calc",
        fluidPage(
          textInput("eventurl", "Attendance URL"),
          actionButton("calcevent", "Calculate Stats"),
          reactableOutput("eventtable")
        )
      ),
      
      tabItem(
        tabName = "server",
        # The titles are like this so there's spacing between buttons
        navset_card_underline(
          title = "Discord Server Stats",
          # Server growth
          nav_panel("Growth ", plotlyOutput("server_growth")),
          
          # Percent of messages triggering AutoMod
          nav_panel(" % Timeout ", plotlyOutput("server_timeout")),
          
          # Server Health Score
          nav_panel(" Health Score ", plotlyOutput("server_health")),
          
          # Raw table
          nav_panel(" Raw Table ", tableOutput("server_table"), downloadButton("download","Download"))
        )
      )
    )
  )
)

server <- function(input, output, session){
  # Server Stats GUI
  # ===================================
  # Plot server growth
  output$server_growth = renderPlotly(ggplotly(ggplot(discord, aes(x = week, y = cumsum(new_members)))+ geom_line() + geom_point() + ylab("Cumulative Members") + xlab("Week") + scale_x_continuous(breaks = 1:length(discord$week)) + ggtitle("Cumulative Members of SUAnime since May 7th 2024") + geom_vline(xintercept = 5, color = "Red", linetype= "dotted") + geom_vline(xintercept = 12, color = "Blue", linetype= "dotted")))
  
  # Plot server % message
  output$server_timeout = renderPlotly(ggplotly(ggplot(discord, aes(x= week, y = issues_message)) + geom_point() + geom_line() + ggtitle("Percentage of Messages that have Caused Issues") + ylab("Issues / Message (%)") + xlab("Week") + geom_vline(xintercept = 5, color = "Red", linetype= "dotted") + geom_vline(xintercept = 12, color = "Blue", linetype= "dotted")))
  
  # Plot server health
  output$server_health = renderPlotly(ggplotly(ggplot(discord, aes(x = week, y = health_score)) + geom_point() + geom_line() + ggtitle("Server Health throughout the Weeks") + ylab("Health Score") + xlab("Week") + geom_vline(xintercept = 5, color = "Red", linetype= "dotted") + geom_vline(xintercept = 12, color = "Blue", linetype= "dotted")))
  
  # Download button for table
  output$download <- downloadHandler(
    filename = function(){"server_stats.csv"},
    content = function(fname){
      write.csv(discord,fname)
    }
  )
  
  # Table for raw data
  output$server_table = renderTable(discord)
  # ===================================
  
  # GUI for Event Calculator 
  # ===================================
  observeEvent(input$calcevent,{
    eventurl = input$eventurl
    result = every_doer(eventurl)
    output$eventtable = renderReactable(result %>% reactable())
  })
  
  # ===================================
  
  # GUI for Event Analytics
  # ===================================
  output$anal_out = renderUI({
    page_sidebar(sidebar =sidebar(
      radioButtons("sem", "Semester", c('1' = '1', '2' = '2')),
      selectInput("year", "Year", as.character(unique(events$year)), selected = "2024"),
      selectInput(inputId = "event_type", "Event Type", c("Regular Event", "CAS", "Draw"), 
                  selected = "Regular Event")),
      plotlyOutput("statplot")
    )})
  
  # For Debugging purposes
  #   observe({
  #   print(input$event_type)
  #   print(input$year)
  #   print(input$analytic)
  #   print(input$sem)
  # })
  
  # Default the statplot variables will be what is selected in the select bar at the start:
  
  
  observeEvent(input$analytic, {
    # Need specific graphs for 'Year' = Year groups and 'Demographic'
    # Demographic
    # ===================================================================
    if (input$analytic == "demographic"){
      print("Demographic Graph")
      output$statplot = renderPlotly(ggplotly(ggplot(interests %>% filter(year == as.integer(input$year)), aes(x = subculture, y = interested_prop)) + geom_bar(stat = "Identity") + theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ggtitle(paste(input$year, " Member Subculture")) + ylab("Percentage") + xlab("Subculture")))
    }
    
    # Year
    # ====================================================================
    else if(input$analytic == "year"){
      print("Year group spread")
      output$statplot = renderPlotly(ggplotly(ggplot(member_years %>% filter(year == input$year),aes(x=degree_year, fill = degree_year)) + geom_bar() + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), guide_legend(title = "Degree Year")) + ylab("Count")))
    }
    
    # Check if the event type is not null (When the app starts this is set to NuLL, breaking the app, so
    # this is here as a countermeasure)
    else if (!is.null(input$event_type)){
      print("Update Statplot")
      output$statplot = renderPlotly(
        ggplotly(ggplot(events %>% filter(type == input$event_type, year == as.integer(input$year), 
                                          semester == as.integer(input$sem)), aes(x = week, y = .data[[input$analytic]])) + geom_bar(stat = "identity")))}
  })
  
  # ===================================
  # GUI for Attendance Predictor
  # ===================================
  # Use reactive values to store user's previously entered options
  stored_vals = reactiveValues(
    week = 0,
    actual_cost = 0,
    class = NULL, 
    duration = 0
  )
  # For any inputs, update stored_vals
  observeEvent(input$model, {
    stored_vals$week
    stored_vals$week = input$week
    stored_vals$class = input$class
    stored_vals$duration = input$duration
    stored_vals$actual_cost = input$actual_cost
  })
  
  # Input choices for Attendance predictor
  output$value_inputs = renderUI({
    model_choice = input$model
    renderUI({
      tagList(
        numericInput(inputId ="week", "Week", value = stored_vals$week, min = 0, max = 13),
        selectInput(inputId = "class", "Event Type", events_model$class, selected = stored_vals$class),
        numericInput(inputId = "duration", "Event Duration", value = stored_vals$duration, min = 0, max = 10),
        if (model_choice != "KNN"){
          numericInput(inputId = "actual_cost", "Budget", value = stored_vals$actual_cost, min = 0, max = 5000)
        }
      )})
  })
  
  # Input choices for Model type
  observeEvent(input$btn, {
    model_choice = input$model
    if (model_choice == "KNN"){
      input_data = data.frame(week = as.factor(input$week), duration = input$duration, class = input$class)
      temp_new_data = events_model %>% select("week", "duration", "class") %>% rbind(input_data)
      # Rescale the data 
      temp_new_data$duration = as.vector(scale(temp_new_data$duration))
      # Get the new entry
      knn_input = temp_new_data[nrow(temp_new_data),]
      # Prediction
      result = round(predict(knnv2_pred, newdata = knn_input), 2)
    }
    
    else{
      input_data = data.frame(week = as.factor(input$week), actual_cost = input$actual_cost, duration = input$duration, class = input$class)
      # No need to scale for the other methods here
      # Match selected option with the associated Model Type
      selected_model = switch(model_choice,
                              "SVR" = svm_fit_final,
                              "Regression Tree" = tree,
                              "Random Forest" = forest
      )
      
      result = round(predict(selected_model, newdata = input_data), 2)
    }
    
    output$table1 <- renderReactable({
      if (model_choice == "KNN"){
        data.frame(`socpercent` = result, `Members` = round(soc_size * result/ 100, 2)) %>% reactable()
      }
      
      else{
        data.frame(`socpercent` = result, `MemberCount` = round(soc_size * result/ 100, 2), `CostPerMember` = round(input$actual_cost/(soc_size * result/ 100), 2)) %>% round(2) %>% reactable()
      }
    })
    # ===================================
  })
}

shinyApp(ui = ui, server= server)

  


