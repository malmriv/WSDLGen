library(shiny)
library(xml2)
library(shinyAce)

# Función para generar el WSDL
generate_wsdl <- function(service_name, input_fields, output_fields) {
  wsdl <- xml_new_root("wsdl:definitions")
  xml_set_attr(wsdl, "targetNamespace", paste0("http://avvale.com/", service_name))
  xml_set_attr(wsdl, "xmlns:xsd", "http://www.w3.org/2001/XMLSchema")
  xml_set_attr(wsdl, "xmlns:wsdl", "http://schemas.xmlsoap.org/wsdl/")
  xml_set_attr(wsdl, "xmlns:soap", "http://schemas.xmlsoap.org/wsdl/soap/")
  xml_set_attr(wsdl, "xmlns:wsoap12", "http://schemas.xmlsoap.org/wsdl/soap12/")
  xml_set_attr(wsdl, "xmlns:tns", paste0("http://avvale.com/", service_name))
  
  types <- xml_add_child(wsdl, "wsdl:types")
  schema <- xml_add_child(types, "xsd:schema", targetNamespace = paste0("http://avvale.com/", service_name))
  
  request_element <- xml_add_child(schema, "xsd:element", name = "Z_Operacion_Request")
  request_complex <- xml_add_child(request_element, "xsd:complexType")
  request_sequence <- xml_add_child(request_complex, "xsd:sequence")
  
  for (field in input_fields) {
    xml_add_child(request_sequence, "xsd:element", name = field$name, type = field$type,
                  maxOccurs = "1", minOccurs = "0")
  }
  
  response_element <- xml_add_child(schema, "xsd:element", name = "Z_Operacion_Response")
  response_complex <- xml_add_child(response_element, "xsd:complexType")
  response_sequence <- xml_add_child(response_complex, "xsd:sequence")
  
  for (field in output_fields) {
    xml_add_child(response_sequence, "xsd:element", name = field$name, type = field$type,
                  maxOccurs = "1", minOccurs = "0")
  }
  
  messages <- xml_add_child(wsdl, "wsdl:message", name = "Z_MensajeRequest")
  xml_add_child(messages, "wsdl:part", name = "parameters", element = "tns:Z_Operacion_Request")
  
  response_message <- xml_add_child(wsdl, "wsdl:message", name = "Z_MensajeResponse")
  xml_add_child(response_message, "wsdl:part", name = "parameters", element = "tns:Z_Operacion_Response")
  
  port_type <- xml_add_child(wsdl, "wsdl:portType", name = "Port_RequestResponse")
  operation <- xml_add_child(port_type, "wsdl:operation", name = "Z_OperacionRequest")
  xml_add_child(operation, "wsdl:input", message = "tns:Z_MensajeRequest")
  xml_add_child(operation, "wsdl:output", message = "tns:Z_MensajeResponse")
  
  binding <- xml_add_child(wsdl, "wsdl:binding", name = "SOAPBinding_Webservice", type = "tns:Port_RequestResponse")
  xml_add_child(binding, "wsoap12:binding", style = "document", transport = "http://schemas.xmlsoap.org/soap/http")
  
  operation_bind <- xml_add_child(binding, "wsdl:operation", name = "Z_OperacionRequest")
  xml_add_child(operation_bind, "wsdl:input", message = "tns:Z_MensajeRequest")
  xml_add_child(operation_bind, "wsdl:output", message = "tns:Z_MensajeResponse")
  
  service <- xml_add_child(wsdl, "wsdl:service", name = paste0("Webservice_", service_name))
  port <- xml_add_child(service, "wsdl:port", name = "Port_RequestResponse", binding = "tns:SOAPBinding_Webservice")
  xml_add_child(port, "soap:address", location = paste0("http://avvale.com/", service_name))
  
  return(as.character(wsdl))
}

# Interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Generador de WSDL"),
  tags$head(
    tags$link(rel="shortcut icon", href="https://tp-utils.s3-eu-west-1.amazonaws.com/images/logo_loginpage.png"),
    tags$style(HTML("
      .well { background-color: #d5f5ed;
      border: 2px solid #7fa39a;}
      body {
        background-color: #f7fffd;
      }
      .logo {
        position: absolute;
        top: 10px;
        right: 10px;
        width: 10%;
      }
    "))),
  img(src = "https://www.sap.com/dam/application/shared/logos/customer/a-g/avvale-customer-logo.png", class = "logo"),
  
  
  tags$br(),
  
  sidebarLayout(
    sidebarPanel(
      h4("Nombre del webservice"),
      textInput("service_name", "", value = "MiServicioWeb"),
      
      h4("Campos de la request"),
      uiOutput("input_fields_ui"),
      h4("Campos de la response"),
      uiOutput("output_fields_ui"),
      
      h4("Acciones:"),
      actionButton("generate_wsdl", "Visualizar WSDL", icon=icon("eye")),
      downloadButton("download_wsdl", "Descargar WSDL")
    ),
    
    mainPanel(
      aceEditor("wsdl_preview", mode = "xml", theme = "textmate", height = "80vh",
                wordWrap = TRUE, showLineNumbers = TRUE)
    )
  ),
  tags$footer(
    tags$p(paste("©",format(Sys.Date(), "%Y"),"Manuel Almagro (manuel.almagro@avvale.com)","🌱 Avvale"), style = "font-size: 12px; text-align: center; margin: 10px 0;"),
    style = "position: fixed; bottom: 0; width: 100%; background-color: white; border-top: 1px solid #ccc;"
  )
)

# Lógica del servidor (Server)
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    input_fields = list(list(id = 1, name = "Campo1", type = "xsd:string")),
    output_fields = list(list(id = 1, name = "Campo1", type = "xsd:string")),
    wsdl_content = ""
  )
  
  update_input_fields <- function() {
    rv$input_fields <- lapply(rv$input_fields, function(field) {
      field$name <- input[[paste0("input_field_name_", field$id)]]
      field$type <- input[[paste0("input_field_type_", field$id)]]
      field
    })
  }
  
  update_output_fields <- function() {
    rv$output_fields <- lapply(rv$output_fields, function(field) {
      field$name <- input[[paste0("output_field_name_", field$id)]]
      field$type <- input[[paste0("output_field_type_", field$id)]]
      field
    })
  }
  
  observeEvent(input$add_input_field, {
    update_input_fields()
    new_id <- if (length(rv$input_fields) == 0) 1 else max(sapply(rv$input_fields, `[[`, "id")) + 1
    rv$input_fields <- append(rv$input_fields, list(list(id = new_id, name = "", type = "xsd:string")))
  })
  
  observeEvent(input$add_output_field, {
    update_output_fields()
    new_id <- if (length(rv$output_fields) == 0) 1 else max(sapply(rv$output_fields, `[[`, "id")) + 1
    rv$output_fields <- append(rv$output_fields, list(list(id = new_id, name = "", type = "xsd:string")))
  })
  
  observe({
    lapply(rv$input_fields, function(field) {
      observeEvent(input[[paste0("remove_input_", field$id)]], {
        update_input_fields()
        rv$input_fields <- rv$input_fields[sapply(rv$input_fields, `[[`, "id") != field$id]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  observe({
    lapply(rv$output_fields, function(field) {
      observeEvent(input[[paste0("remove_output_", field$id)]], {
        update_output_fields()
        rv$output_fields <- rv$output_fields[sapply(rv$output_fields, `[[`, "id") != field$id]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  output$input_fields_ui <- renderUI({
    # Encabezados
    header_row <- fluidRow(
      column(4, h5("Nombre")),   # Encabezado para Nombre
      column(4, h5("Tipo")),     # Encabezado para Tipo
      column(4, "")               # Espacio vacío para acciones
    )
    
    # Campos dinámicos
    input_ui <- lapply(rv$input_fields, function(field) {
      fluidRow(
        column(4, textInput(paste0("input_field_name_", field$id), "", value = field$name)),
        column(4, selectInput(paste0("input_field_type_", field$id), "", choices = c("xsd:string", "xsd:int", "xsd:boolean"), selected = field$type)),
        column(4, 
               fluidRow(
                 actionButton(paste0("remove_input_", field$id), "", icon = icon("trash"), style = "margin-top: 25px;"),
                 if (field$id == max(sapply(rv$input_fields, `[[`, "id"))) {
                   actionButton("add_input_field", "", icon = icon("plus"), style = "margin-top: 25px; margin-left: 5px;")
                 }
               )
        )
      )
    })
    
    # Combinar encabezados y campos
    tagList(header_row, input_ui)
  })
  
  output$output_fields_ui <- renderUI({
    # Encabezados
    header_row <- fluidRow(
      column(4, h5("Nombre")),   # Encabezado para Nombre
      column(4, h5("Tipo")),     # Encabezado para Tipo
      column(4, "")               # Espacio vacío para acciones
    )
    
    # Campos dinámicos
    output_ui <- lapply(rv$output_fields, function(field) {
      fluidRow(
        column(4, textInput(paste0("output_field_name_", field$id), "", value = field$name)),
        column(4, selectInput(paste0("output_field_type_", field$id), "", choices = c("xsd:string", "xsd:int", "xsd:boolean"), selected = field$type)),
        column(4, 
               fluidRow(
                 actionButton(paste0("remove_output_", field$id), "", icon = icon("trash"), style = "margin-top: 25px;"),
                 if (field$id == max(sapply(rv$output_fields, `[[`, "id"))) {
                   actionButton("add_output_field", "", icon = icon("plus"), style = "margin-top: 25px; margin-left: 5px;")
                 }
               )
        )
      )
    })
    
    # Combinar encabezados y campos
    tagList(header_row, output_ui)
  })
  
  observeEvent(input$generate_wsdl, {
    update_input_fields()
    update_output_fields()
    rv$wsdl_content <- generate_wsdl(input$service_name, rv$input_fields, rv$output_fields)
    updateAceEditor(session, "wsdl_preview", value = rv$wsdl_content)
  })
  
  output$download_wsdl <- downloadHandler(
    filename = function() {
      paste(input$service_name, "wsdl", sep = ".")
    },
    content = function(file) {
      writeLines(rv$wsdl_content, file)
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)