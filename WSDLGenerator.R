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
      actionButton("add_input_field", "Añadir campo a la request"),
      
      h4("Campos de la response"),
      uiOutput("output_fields_ui"),
      actionButton("add_output_field", "Añadir campo a la response"),
      
      actionButton("generate_wsdl", "Actualizar WSDL"),
      downloadButton("download_wsdl", "Descargar WSDL")
    ),
    
    mainPanel(
      h4("Vista previa del WSDL"),
      verbatimTextOutput("wsdl_preview")
    )
  )
)

# Lógica del servidor (Server)
server <- function(input, output, session) {
  
  # Usar reactiveValues para almacenar los campos dinámicos
  rv <- reactiveValues(
    input_fields = list(),
    output_fields = list(),
    wsdl_content = ""
  )
  
  # Función para actualizar los valores actuales de los campos de Input
  update_input_fields <- function() {
    rv$input_fields <- lapply(seq_along(rv$input_fields), function(i) {
      list(name = input[[paste0("input_field_name_", i)]], type = input[[paste0("input_field_type_", i)]])
    })
  }
  
  # Función para actualizar los valores actuales de los campos de Output
  update_output_fields <- function() {
    rv$output_fields <- lapply(seq_along(rv$output_fields), function(i) {
      list(name = input[[paste0("output_field_name_", i)]], type = input[[paste0("output_field_type_", i)]])
    })
  }
  
  # Añadir un nuevo campo de Input
  observeEvent(input$add_input_field, {
    update_input_fields()
    rv$input_fields <- append(rv$input_fields, list(list(name = "", type = "xsd:string")))
  })
  
  # Añadir un nuevo campo de Output
  observeEvent(input$add_output_field, {
    update_output_fields()
    rv$output_fields <- append(rv$output_fields, list(list(name = "", type = "xsd:string")))
  })
  
  # Generar UI dinámica para los campos de Input
  output$input_fields_ui <- renderUI({
    lapply(seq_along(rv$input_fields), function(i) {
      fluidRow(
        column(6, textInput(paste0("input_field_name_", i), "Nombre:", value = rv$input_fields[[i]]$name)),
        column(6, selectInput(paste0("input_field_type_", i), "Tipo", choices = c("xsd:string", "xsd:int", "xsd:boolean"), selected = rv$input_fields[[i]]$type))
      )
    })
  })
  
  # Generar UI dinámica para los campos de Output
  output$output_fields_ui <- renderUI({
    lapply(seq_along(rv$output_fields), function(i) {
      fluidRow(
        column(6, textInput(paste0("output_field_name_", i), "Nombre:", value = rv$output_fields[[i]]$name)),
        column(6, selectInput(paste0("output_field_type_", i), "Tipo", choices = c("xsd:string", "xsd:int", "xsd:boolean"), selected = rv$output_fields[[i]]$type))
      )
    })
  })
  
  # Actualizar el contenido del WSDL cuando se presione el botón "Generar WSDL"
  observeEvent(input$generate_wsdl, {
    update_input_fields()
    update_output_fields()
    
    input_fields_list <- lapply(seq_along(rv$input_fields), function(i) {
      list(name = input[[paste0("input_field_name_", i)]], type = input[[paste0("input_field_type_", i)]])
    })
    
    output_fields_list <- lapply(seq_along(rv$output_fields), function(i) {
      list(name = input[[paste0("output_field_name_", i)]], type = input[[paste0("output_field_type_", i)]])
    })
    
    rv$wsdl_content <- generate_wsdl(input$service_name, input_fields_list, output_fields_list)
  })
  
  # Mostrar la vista previa del WSDL en la interfaz
  output$wsdl_preview <- renderText({
    rv$wsdl_content
  })
  
  # Descargar el WSDL generado
  output$download_wsdl <- downloadHandler(
    filename = function() {
      paste0(input$service_name, ".wsdl")
    },
    content = function(file) {
      writeLines(rv$wsdl_content, file)
    }
  )
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
