############## https://github.com/jeroen/shinymagick/blob/master/app.R
# Minimal example of Shiny widget using 'magick' images
ui <- fluidPage(
  titlePanel("Firearms Comparison Microscopy Application"),
  
  sidebarLayout(
    
    fluidRow(
      sidebarPanel(
        selectInput(
          inputId = "Ksample", 
          label = "Choose a Known Sample (Left):",
          choices = c("Known 1", "Known_2", "Known_3", "Known_4", "Known_5", "Known_6", "Known_7", "Known_8")
        ),
        selectInput("Qsample", "Choose a Questioned Sample (Right):",
                    list(`Questioned Samples` = c("Questioned 1", "Questioned 2", "Questioned 3", "Questioned 4", "Questioned 5", "Questioned 6", "Questioned 7", "Questioned 8"))
        ),
        #fileInput("upload", "Upload new image", accept = c('image/png', 'image/jpeg')),
        #textInput("size", "Size", value = "500x500!"),
        sliderInput("Krotation", "Known Rotation", -90, 90, 0),
        sliderInput("Qrotation", "Questioned Rotation", -90, 90, 0),
        sliderInput("blur", "Blur", 0, 20, 0),
        sliderInput("implode", "Implode", -1, 1, 0, step = 0.01),
      
        # checkboxGroupInput("effects", "Effects",
        #                    choices = list("edge", "charcoal", "negate", "flip", "flop"))
      ),
      mainPanel(
        p("This app is intended to illustrate how comparison microscopy can be utilized for the comparison of firearms evidence."),
        imageOutput("img")
      )),
    fluidRow(
      mainPanel(
        p(HTML('&nbsp;'),HTML('&nbsp;'),"Developed by Dr. Jamie Spaulding, Hamline University.")
    )  
    )
  )
)

server <- function(input, output, session) {
  
  library(magick)
  path <- normalizePath(file.path('./cc-images'))
  
  K1 <- magick::image_read(paste0(path,'\\ccid_S1.jpg'))
  K2 <- magick::image_read(paste0(path,'\\ccid_S2.jpg'))
  K3 <- magick::image_read(paste0(path,'\\ccid_S3.jpg'))
  K4 <- magick::image_read(paste0(path,'\\ccid_S4.jpg'))
  K5 <- magick::image_read(paste0(path,'\\ccid_S5.jpg'))
  K6 <- magick::image_read(paste0(path,'\\ccid_S6.jpg'))
  K7 <- magick::image_read(paste0(path,'\\ccid_S7.jpg'))
  K8 <- magick::image_read(paste0(path,'\\ccid_S8.jpg'))
  
  image <- observeEvent(input$Ksample, {
    if(input$Ksample == "Known 1"){image <- K1}
  })
  
  
  #if(reactive({req(input$Ksample)}) == 'Known_1'){image <- image_read(paste0(path, '\\Known_1.jpg'))}
  # image <- reactive({
  #   magick::image_read(paste0(path, '\\',req(input$Ksample),'.jpg'))
  # })
  # image <- observeEvent(input$Ksample, {
  #   image_convert(image_read(paste0('D:\\Workspace\\VCM\\R\\cc-images\\',req(input$Ksample),'.jpg')), 'jpeg')
  #   })
  #image <- magick::image_read(paste0('D:\\Workspace\\VCM\\R\\cc-images\\Known 1.jpg'))
  
  
  # L_image <- reactive({
  #   get(input$Ksamples)
  # })
  # 
  # image <- observeEvent(input$Ksample, {
  #   magick::image_read(paste0('D:\\Workspace\\VCM\\R\\cc-images\\',L_image(),'.jpg'))
  # })
  # updateSelectInput(session, "inSelect",
                    # image <- image_read())
  #input$Ksample#image_read(paste0(path, '\\Known_1.jpg'))

  # Start with img
  #image <- reactive({magick::image_read(paste0(path, '\\',req(Limage()),'.jpg'))})
  #image <- image_read(paste0(path, '\\Known_1.jpg'))
  image2 <- image_read(paste0(path, '\\ccid_S8.jpg'))
  
  # When uploading new image
  # observeEvent(input$Ksample, {
  #   image <- image_convert(magick::image_read(paste0(path, '\\',input$Ksample,'.jpg')), "jpg")
  #   info <- image_info(image)
  #   #updateCheckboxGroupInput(session, "effects", selected = "")
  #   #updateTextInput(session, "size", value = paste0(info$width, "x", info$height, "!"))
  # })
  
  # A plot of fixed size
  output$img <- renderImage({
    
    # Boolean operators
    if("edge" %in% input$effects)
      image <- image_edge(image)
    
    if("charcoal" %in% input$effects)
      image <- image_charcoal(image)
    
    if("negate" %in% input$effects)
      image <- image_negate(image)    
    
    if("flip" %in% input$effects)
      image <- image_flip(image)
    
    if("flop" %in% input$effects)
      image <- image_flop(image)
    
    # Numeric operators
    tmpfile <- image %>%
      #image_resize(input$size) %>%
      image_implode(input$implode) %>%
      image_blur(input$blur, input$blur) %>%
      image_rotate(input$Krotation) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
  })
  
  
}


shinyApp(ui, server)