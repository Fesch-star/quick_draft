library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("QuickDraft – Email Assistant"),

  fluidRow(
    column(
      width = 4,
      wellPanel(
        textInput("user_name", "Your Name:", value = "Femke"),

        textAreaInput("email_input", "Paste Email or Describe Message:", "", rows = 6),

        selectInput("language", "Select Language:", choices = c("English", "Dutch")),

        radioButtons("tone", "Select Tone:",
                     choices = c("Friendly", "Formal", "Neutral"),
                     selected = "Neutral"),

        radioButtons("length", "Select Length:",
                     choices = c("Short", "Medium", "Detailed"),
                     selected = "Medium"),

        radioButtons("purpose", "Select Purpose:",
                     choices = c("Reply", "Compose", "Follow-up", "Thank-you", "Scheduling"),
                     selected = "Reply"),

        checkboxInput("dutch_style", "Use Dutch communication style", value = TRUE),

        actionButton("generate", "Generate Prompt for Draft"),
        actionButton("standard_reply", "Insert Standard Reply"),
        actionButton("save_prefs", "💾 Save Preferences"),
        actionButton("load_prefs", "📂 Load Preferences")
      )
    ),

    column(
      width = 8,
      wellPanel(
        textInput("extra_prompt", "Additional Instructions (optional):",
                  placeholder = "e.g., mention ECPR or keep it brief"),

        textOutput("copy_instructions"),
        verbatimTextOutput("draft_input"),
        actionButton("copy_prompt", "📋 Copy Prompt to Clipboard")
      )
    )
  )
)

server <- function(input, output, session) {
  prefs <- reactiveValues()
  prompt_text <- reactiveVal("")

  get_signoff <- reactive({
    if (input$language == "Dutch") {
      paste0("Vriendelijke groet,\n", input$user_name)
    } else {
      paste0("Best wishes,\n", input$user_name)
    }
  })

  observeEvent(input$generate, {
    prompt <- paste0(
      "Please help me write an email in ", input$language, ".\n",
      "The purpose is: ", input$purpose, ".\n",
      "Tone: ", input$tone, ".\n",
      "Length: ", input$length, ".\n",
      if (input$dutch_style)
        "Please use a Dutch communication style: clear, polite, modest, and not overly enthusiastic.\n"
      else "",
      if (nzchar(input$extra_prompt))
        paste0("Additional instructions: ", input$extra_prompt, "\n")
      else "",
      "Here is the context:\n", input$email_input, "\n\n",
      "Please end the email with the following sign-off:\n", get_signoff()
    )
    prompt_text(prompt)
    output$copy_instructions <- renderText({
      "Prompt ready! You can copy it below and paste it into this chat:"
    })
    output$draft_input <- renderText({ prompt })
  })

  observeEvent(input$copy_prompt, {
    runjs(sprintf("navigator.clipboard.writeText(`%s`);", gsub("`", "\\`", prompt_text())))
    showNotification("Prompt copied to clipboard!", type = "message")
  })

  observeEvent(input$standard_reply, {
    reply <- paste0("Thank you for your message. I will get back to you as soon as possible.\n\n", get_signoff())
    prompt_text(reply)
    output$copy_instructions <- renderText({
      "Standard reply ready! You can copy it below:"
    })
    output$draft_input <- renderText({ reply })
  })

  observeEvent(input$save_prefs, {
    prefs$name <- input$user_name
    prefs$language <- input$language
    prefs$tone <- input$tone
    prefs$length <- input$length
    prefs$purpose <- input$purpose
    prefs$dutch_style <- input$dutch_style
    showNotification("Preferences saved!", type = "message")
  })

  observeEvent(input$load_prefs, {
    if (!is.null(prefs$name)) {
      updateTextInput(session, "user_name", value = prefs$name)
      updateSelectInput(session, "language", selected = prefs$language)
      updateRadioButtons(session, "tone", selected = prefs$tone)
      updateRadioButtons(session, "length", selected = prefs$length)
      updateRadioButtons(session, "purpose", selected = prefs$purpose)
      updateCheckboxInput(session, "dutch_style", value = prefs$dutch_style)
      showNotification("Preferences loaded!", type = "message")
    } else {
      showNotification("No preferences saved yet.", type = "warning")
    }
  })
}

shinyApp(ui = ui, server = server)
