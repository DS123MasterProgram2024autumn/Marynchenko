
# інсталюємо пакет
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

#install.packages("DBI")
#install.packages("RSQLite")
# Підключаємо бібліотеки
library(RSQLite)

# Шлях до файлу бази даних
db_path <- "land_income.db"

# Функція для створення бази даних, якщо вона не існує
create_database <- function(db_path) {
  # Підключаємось до бази даних
  conn <- dbConnect(SQLite(), dbname = db_path)
  
  # Створюємо таблицю, якщо вона не існує
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS land_income (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      region TEXT NOT NULL,
      income_per_hectare REAL NOT NULL
    )
  ")
  
  # Вставляємо початкові дані, якщо таблиця порожня
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
  
  # Закриваємо з'єднання з базою даних
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
  titlePanel("Доходи пайовиків по областях"),
  sidebarLayout(
    sidebarPanel(
      numericInput("hectares", "Введіть кількість гектарів:", value = 1, min = 1),
      checkboxGroupInput("selected_regions", "Виберіть регіони:", choices = get_regions(), selected = get_regions()),
      actionButton("calculate", "Розрахувати дохід")
    ),
    mainPanel(
      tableOutput("incomeTable")
    )
  )
)

# Server частина Shiny
server <- function(input, output) {
  # Підключення до бази даних та отримання даних для таблиці
  observeEvent(input$calculate, {
    conn <- dbConnect(SQLite(), dbname = db_path)
    
    # Запит для вибраних регіонів
    query <- "SELECT region, income_per_hectare * ? AS total_income FROM land_income WHERE region IN (%s)"
    selected_regions <- paste(shQuote(input$selected_regions), collapse = ", ")
    full_query <- sprintf(query, selected_regions)
    
    # Виконуємо запит з урахуванням введеної кількості гектарів
    data <- dbGetQuery(conn, full_query, params = list(input$hectares))
    
    dbDisconnect(conn)
    
    # Відображення результату в таблиці
    output$incomeTable <- renderTable({
      data
    })
  })
}


# Запускаємо додаток
shinyApp(ui = ui, server = server)
