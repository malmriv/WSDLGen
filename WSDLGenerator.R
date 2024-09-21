library(shiny)
library(xml2)

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
  
  sidebarLayout(
    sidebarPanel(
      textInput("service_name", "Nombre del servicio web", value = "MiServicioWeb"),
      
      h4("Campos de la request"),
      uiOutput("input_fields_ui"),
      h4("Campos de la response"),
      uiOutput("output_fields_ui"),
      
      h4("Acciones:"),
      actionButton("generate_wsdl", "Actualizar WSDL"),
      downloadButton("download_wsdl", "Descargar WSDL")
    ),
    
    mainPanel(
      verbatimTextOutput("wsdl_preview")
    )
  )
)

# Lógica del servidor (Server)
server <- function(input, output, session) {
  
  # Usar reactiveValues para almacenar los campos dinámicos
  rv <- reactiveValues(
    input_fields = list(list(id = 1, name = "Campo1", type = "xsd:string")),
    output_fields = list(list(id = 1, name = "Campo1", type = "xsd:string")),
    wsdl_content = ""
  )
  
  # Función para actualizar los valores actuales de los campos de Input
  update_input_fields <- function() {
    rv$input_fields <- lapply(rv$input_fields, function(field) {
      field$name <- input[[paste0("input_field_name_", field$id)]]
      field$type <- input[[paste0("input_field_type_", field$id)]]
      field
    })
  }
  
  # Función para actualizar los valores actuales de los campos de Output
  update_output_fields <- function() {
    rv$output_fields <- lapply(rv$output_fields, function(field) {
      field$name <- input[[paste0("output_field_name_", field$id)]]
      field$type <- input[[paste0("output_field_type_", field$id)]]
      field
    })
  }
  
  # Añadir un nuevo campo de Input
  observeEvent(input$add_input_field, {
    update_input_fields()
    new_id <- if (length(rv$input_fields) == 0) 1 else max(sapply(rv$input_fields, `[[`, "id")) + 1
    rv$input_fields <- append(rv$input_fields, list(list(id = new_id, name = "", type = "xsd:string")))
  })
  
  # Añadir un nuevo campo de Output
  observeEvent(input$add_output_field, {
    update_output_fields()
    new_id <- if (length(rv$output_fields) == 0) 1 else max(sapply(rv$output_fields, `[[`, "id")) + 1
    rv$output_fields <- append(rv$output_fields, list(list(id = new_id, name = "", type = "xsd:string")))
  })
  
  # Eliminar un campo de Input
  observe({
    lapply(rv$input_fields, function(field) {
      observeEvent(input[[paste0("remove_input_", field$id)]], {
        update_input_fields()
        rv$input_fields <- rv$input_fields[sapply(rv$input_fields, `[[`, "id") != field$id]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  # Eliminar un campo de Output
  observe({
    lapply(rv$output_fields, function(field) {
      observeEvent(input[[paste0("remove_output_", field$id)]], {
        update_output_fields()
        rv$output_fields <- rv$output_fields[sapply(rv$output_fields, `[[`, "id") != field$id]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
  # Generar UI dinámica para los campos de Input con botón para eliminar
  output$input_fields_ui <- renderUI({
    input_ui <- lapply(rv$input_fields, function(field) {
      fluidRow(
        column(4, textInput(paste0("input_field_name_", field$id), "", value = field$name)),
        column(4, selectInput(paste0("input_field_type_", field$id), "", choices = c("xsd:string", "xsd:int", "xsd:boolean"), selected = field$type)),
        column(4, 
               fluidRow(
                 actionButton(paste0("remove_input_", field$id), "", icon = icon("trash"), style = "margin-top: 25px;"),
                 actionButton("add_input_field", "", icon = icon("plus"), style = "margin-top: 25px; margin-left: 10px;")
               )
        )
      )
    })
    
    do.call(tagList, input_ui)
  })
  
  # Generar UI dinámica para los campos de Output con botón para eliminar
  output$output_fields_ui <- renderUI({
    output_ui <- lapply(rv$output_fields, function(field) {
      fluidRow(
        column(4, textInput(paste0("output_field_name_", field$id), "", value = field$name)),
        column(4, selectInput(paste0("output_field_type_", field$id), "", choices = c("xsd:string", "xsd:int", "xsd:boolean"), selected = field$type)),
        column(4, 
               fluidRow(
                 actionButton(paste0("remove_output_", field$id), "", icon = icon("trash"), style = "margin-top: 25px;"),
                 actionButton("add_output_field", "", icon = icon("plus"), style = "margin-top: 25px; margin-left: 10px;")
               )
        )
      )
    })
    
    do.call(tagList, output_ui)
  })
  
  # Generar el WSDL y mostrarlo en la vista previa
  observeEvent(input$generate_wsdl, {
    update_input_fields()
    update_output_fields()
    rv$wsdl_content <- generate_wsdl(input$service_name, rv$input_fields, rv$output_fields)
  })
  
  output$wsdl_preview <- renderText({
    rv$wsdl_content
  })
  
  # Permitir la descarga del WSDL
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
