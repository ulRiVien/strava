ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      column(3, box("text", height = 90)),
      column(3, infoBox("Distance", "100 km", width = 12)),
      infoBox("Elevation", width = 3, fill = TRUE),
      infoBox("Slope", width = 3)
      
    ),
    fluidRow(
      column(3, infoBox("Time", width = 12)),
      infoBox("Speed", "100 km", width = 3),
      infoBox("Elevation speed", width = 3)
      
    ),
    fluidRow(
      infoBox("Number of rides", width = 3),
      infoBox("Average distance", "100 km", width = 3),
      infoBox("Average elevation", width = 3)
    ),
    fluidRow(
      infoBox("Average time", width = 3)    
    )
  )
)

