options(repos = c(CRAN = "https://cloud.r-project.org"))
# Інсталюємо та підключаємо пакети
install.packages("DBI")
install.packages("RSQLite")
install.packages("shiny")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("DT")
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(RSQLite)

# Шлях до файлу бази даних
db_path <- "land_income.db"

# Функція для створення бази даних, якщо вона не існує
create_database <- function(db_path) {
  conn <- dbConnect(SQLite(), dbname = db_path)
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS land_income (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      region TEXT NOT NULL,
      income_per_hectare REAL NOT NULL
    )
  ")
  
  existing_data <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM land_income")
  
  if (existing_data$count == 0) {
    dbExecute(conn, "
      INSERT INTO land_income (region, income_per_hectare) VALUES 
      ('Київська область', 13000),
      ('Тернопільська область', 11000),
      ('Вінницька область', 11000),
      ('Черкаська область', 9400),
      ('Чернівецька область', 8000),
      ('Хмельницька область', 7200),
      ('Полтавська область', 6200),
      ('Дніпропетровська область', 6000),
      ('Закарпатська область', 5000),
      ('Сумська область', 5000),
      ('Миколаївська область', 5000),
      ('Харківська область', 4000),
      ('Житомирська область', 3700),
      ('Одеська область', 3500),
      ('Чернігівська область', 2300),
      ('Запорізька область', 2000),
      ('Рівненська область', 1000)
    ")
  }
  
  dbDisconnect(conn)
}

# Викликаємо функцію для створення бази даних
create_database(db_path)

# Отримуємо список регіонів з бази даних
get_regions <- function() {
  conn <- dbConnect(SQLite(), dbname = db_path)
  regions <- dbGetQuery(conn, "SELECT region FROM land_income")
  dbDisconnect(conn)
  return(regions$region)
}

# UI частина Shiny
ui <- fluidPage(
  titlePanel("Лабораторна робота №4. Розробка ShinyApp застосунку"),
  h4("Valeriia Marynchenko, 09.11.2024"),
  h2("Доходи пайовиків по областях"),
  sidebarLayout(
    sidebarPanel(
      numericInput("hectares", "Введіть кількість гектарів:", value = 1, min = 1),
      checkboxGroupInput("selected_regions", "Виберіть регіони:", choices = get_regions(), selected = get_regions()),
      selectInput("display_type", "Виберіть тип відображення:", choices = c("Таблиця" = "table", "Діаграма" = "chart")),
      actionButton("calculate", "Розрахувати дохід")
    ),
    mainPanel(
      uiOutput("output_ui")  # Динамічний UI для відображення таблиці або діаграми
    )
  )
)

# Server частина Shiny
server <- function(input, output) {
  # Підключення до бази даних та обробка даних після натискання кнопки
  observeEvent(input$calculate, {
    conn <- dbConnect(SQLite(), dbname = db_path)
    
    # Запит для вибраних регіонів
    query <- "SELECT region, income_per_hectare * ? AS total_income FROM land_income WHERE region IN (%s)"
    selected_regions <- paste(shQuote(input$selected_regions), collapse = ", ")
    full_query <- sprintf(query, selected_regions)
    
    # Виконуємо запит з урахуванням введеної кількості гектарів
    data <- dbGetQuery(conn, full_query, params = list(input$hectares))
    
    dbDisconnect(conn)
    
    # Вибір між таблицею та діаграмою
    output$output_ui <- renderUI({
      if (input$select == "table") {
        tableOutput("incomeTable")
      } else {
        plotOutput("incomeChart")
      }
    })
    
    # Відображення даних як таблиці
    output$incomeTable <- renderTable({
      data
    })
    
    # Відображення даних як діаграми
    output$incomeChart <- renderPlot({
      ggplot(data, aes(x = reorder(region, -total_income), y = total_income, fill = region)) +
        geom_col() +
        coord_flip() +
        labs(title = "Загальний дохід за обрану кількість гектарів",
             x = "Регіон",
             y = "Загальний дохід (грн)") +
        theme_minimal() +
        theme(legend.position = "none")
    })
  })
}

# Запускаємо додаток
shinyApp(ui = ui, server = server)
