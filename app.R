library(shiny)
library(tidyverse)
library(later)
library(sparkler)

taskmaster_pairs <- read_csv("taskmaster_pairs.csv") |>
  mutate(quote = str_wrap(quote, width = 36)) |>
  select(quote, name)
reward <- str_c(
  '<iframe src="',
  {
    read_lines("reward.txt") |>
      str_remove("\\?img_index=\\d") |>
      str_c("embed")
  },
  '"width="360" height="400" frameborder="0" scrolling="no"></iframe>'
)

description <- function() {
  str_c(
    "
It's Fleur's birthday and she wants to share amazing Taskmaster quotes with everyone.
Making a scheurkalender is a lot of work and Tina's handwriting is illegible, so she made a game of Shoe Who instead.<br><br>

<b>Task I</b><br>
Do not get distracted by this image:
",
    sample(reward, 1),
    "<br><b>Task II</b><br><br>

Match each Taskmaster quote with the name of the person who said it.
Click on two cards: if they match, they stay revealed. If not, they hide again.
Every game is different so keep coming back to play more.<br><br>

Fastest wins.<br>
Your time starts now or when you click on 'I want to start again!'.<br>
Bonus points for having the most fun while playing.
"
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Special+Elite&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML(
      "
      body {
        background-color: #fdf6e3;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        color: #982626;
      }
      .taskmaster-title {
        font-size: 2em;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        color: #982626;
        text-shadow: 2px 2px #fff3e0;
        margin-bottom: 10px;
        letter-spacing: 2px;
        text-align: center;
      }
      .taskmaster-subtitle {
        font-size: 1.75em;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        color: #982626;
        text-shadow: 2px 2px #fff3e0;
        margin-bottom: 20px;
        letter-spacing: 2px;
        text-align: center;
      }
      .btn, .action-button {
        background-color: #982626 !important;
        color: #fff3e0 !important;
        border: 2px solid #982626 !important;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        font-size: 1.1em;
        border-radius: 8px !important;
        margin: 12px 12px;
        box-shadow: 4px 4px #721313ff;
        transition: background 0.2s;
        min-height: 80px;
        height: 80px;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .start-game-btn {
        background-color: #fff3e0 !important;
        color: #982626 !important;
        border: 3px dashed #982626 !important;
        font-size: 1.3em !important;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        box-shadow: 4px 4px #b71c1c;
        min-height: 48px;
        height: 48px;
        margin: 12px 12px;
        border-radius: 16px !important;
        padding: 8px 32px !important;
        transition: background 0.2s;
      }
      .btn:disabled, .action-button:disabled {
        background-color: #e0e0e0 !important;
        color: #982626 !important;
        border: 2px solid #982626 !important;
        opacity: 0.7;
      }
      .modal-content {
        background-color: #fff !important;
        color: #000 !important;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        border: 3px solid #982626;
        border-radius: 12px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        min-height: 300px;
        text-align: center;
      }
      .modal-footer .btn {
        border-radius: 50% !important;
        width: 110px !important;
        height: 100px !important;
        padding: 0 !important;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 1.5em !important;
      }
      .shiny-text-output {
        font-size: 1.2em;
        font-family: 'Special Elite', 'Courier New', Courier, monospace;
        color: #982626;
        margin-bottom: 10px;
      }
      .card-hidden {
        background-color: #982626 !important;
        color: #fff3e0 !important;
        border: 2px solid #982626 !important;
        opacity: 1;
      }
      .card-revealed {
        background-color: #fff3e0 !important;
        color: #982626 !important;
        border: 2px solid #982626 !important;
        font-weight: bold;
        box-shadow: 0 0 10px #982626;
        opacity: 1;
      }
      .card-matched {
        background: linear-gradient(135deg, #fff3e0 0%, #ffd54f 60%, #982626 100%);
        color: #671717ff !important;
        border: 3px solid #ffd54f !important;
        font-size: 1.3em;
        font-weight: bold;
        text-shadow: 1px 1px 0 #fff3e0, 2px 2px 4px #ffd54f;
        box-shadow: 0 0 16px #ffd54f, 0 0 8px #fff3e0;
        position: relative;
      }
        /* Responsive button row: stack on small screens */
        .taskmaster-btn-row {
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 24px;
          margin-bottom: 12px;
          flex-wrap: wrap;
        }
        @media (max-width: 600px) {
          .taskmaster-btn-row {
            flex-direction: column !important;
            align-items: center !important;
            gap: 8px;
          }
          .taskmaster-btn-row > div {
            width: 100%;
            display: flex;
            justify-content: center;
          }
        }
    "
    ))
  ),
  column(
    width = 10,
    offset = 1,
    titlePanel(
      div("Shoe Who?", class = "taskmaster-title"),
      windowTitle = "Fleur's Birthday Game"
    ),
    div(
      "I mean Guess Shoe? That is, guess who said or did the thing in Taskmaster",
      class = "taskmaster-subtitle"
    ),
    div(
      class = "taskmaster-btn-row",
      div(
        actionButton(
          "new_game",
          "I want to start again!",
          class = "start-game-btn"
        ),
        style = "flex: 0 0 auto;"
      ),
      div(
        actionButton(
          "show_instructions",
          "I forgot what the task is!",
          class = "start-game-btn"
        ),
        style = "flex: 0 0 auto;"
      ),
      div(
        textOutput("timer"),
        class = "taskmaster-timer",
        style = "flex: 0 0 auto;"
      )
    ),
    uiOutput("card_grid_ui"),
    confettiOutput("end_party"),
    tags$footer(
      style = "width: 100%; background: #fff3e0; color: #982626; text-align: center; padding: 18px 0 10px 0; font-family: 'Special Elite', 'Courier New', Courier, monospace; font-size: 1.1em; border-top: 2px solid #982626; margin-top: 32px; position: relative; z-index: 1;",
      HTML(
        "Made with ♥︎ and R Shiny by <a href='https://tinarozsos.github.io/' style='color: #982626; text-decoration: underline;' target='_blank'>Tina</a>. If you see something weird, let her know and she will try to fix it. Data comes from the <a href='https://taskmaster.fandom.com/wiki/Taskmaster_Wiki' style='color: #982626; text-decoration: underline;' target='_blank'>Taskmaster Wiki</a>."
      )
    )
  )
)

server <- function(input, output, session) {
  # Game state: grid, pairs, card states, and selected cards
  game_grid <- reactiveVal()
  game_pairs <- reactiveVal()
  card_states <- reactiveVal()
  selected_cards <- reactiveVal(character(0))
  start_time <- reactiveVal(Sys.time())

  # Track if win modal is shown
  win_shown <- reactiveVal(FALSE)

  # Reactive trigger for new game
  new_game_trigger <- reactiveVal(0)

  # Show instructions modal on launch
  observe({
    showModal(
      modalDialog(
        title = "Play Fleur's Birthday Game",
        HTML(description()),
        footer = tagList(modalButton(HTML("Start<br>Playing"))),
        easyClose = TRUE
      )
    )
  })

  # Show instructions modal when button is clicked
  observeEvent(input$show_instructions, {
    showModal(
      modalDialog(
        title = "Play Fleur's Birthday Game",
        HTML(description()),
        footer = tagList(modalButton(HTML("Start<br>Playing"))),
        easyClose = TRUE
      )
    )
  })

  # New game setup logic
  start_new_game <- function() {
    selected_pairs <- taskmaster_pairs |>
      slice_sample(n = 8)
    card_contents <- c(selected_pairs$quote, selected_pairs$name) |>
      sample()
    card_grid <- expand_grid(
      row = 1:4,
      col = 1:4
    ) |>
      mutate(
        card_id = paste0("card_", row, "_", col),
        content = card_contents
      )
    game_grid(card_grid)
    game_pairs(selected_pairs)
    card_states(setNames(rep("hidden", 16), card_grid$card_id))
    selected_cards(character(0))
    win_shown(FALSE)
  }

  # Timer UI
  output$timer <- renderText({
    states <- card_states()
    # Only update timer if game not won
    if (is.null(states) || !all(states == "matched")) {
      invalidateLater(1000, session)
    }
    elapsed <- as.numeric(difftime(Sys.time(), start_time(), units = "secs"))
    mins <- floor(elapsed / 60)
    secs <- floor(elapsed %% 60)
    sprintf("Your time started %02d:%02d seconds ago", mins, secs)
  })

  # Observe new_game button
  observeEvent(
    input$new_game,
    {
      new_game_trigger(new_game_trigger() + 1)
    },
    ignoreInit = FALSE
  )

  # Observe trigger to start new game
  observeEvent(
    new_game_trigger(),
    {
      start_new_game()
      # get start time
      start_time(Sys.time())
    },
    ignoreInit = FALSE
  )

  # Create card observers once (not inside a reactive block)
  card_ids <- paste0("card_", rep(1:4, each = 4), "_", rep(1:4, times = 4))
  walk(card_ids, function(id) {
    observeEvent(
      input[[id]],
      {
        states <- card_states()
        selected <- selected_cards()
        # Only allow revealing if less than 2 cards are currently revealed and this card is hidden
        if (
          !is.null(states) && states[[id]] == "hidden" && length(selected) < 2
        ) {
          states[[id]] <- "revealed"
          card_states(states)
          selected <- c(selected, id)
          selected_cards(selected)
        }

        # If two cards are revealed, check for match
        if (length(selected_cards()) == 2) {
          grid_now <- game_grid()
          pairs_now <- game_pairs()
          ids_now <- selected_cards()
          contents_now <- grid_now |>
            filter(card_id %in% ids_now) |>
            pull(content)

          # Check if one is quote and one is name, and if they are a pair
          is_match <- FALSE
          if (
            contents_now[1] %in%
              pairs_now$quote &&
              contents_now[2] %in% pairs_now$name
          ) {
            idx <- which(pairs_now$quote == contents_now[1])
            is_match <- pairs_now$name[idx] == contents_now[2]
          } else if (
            contents_now[2] %in%
              pairs_now$quote &&
              contents_now[1] %in% pairs_now$name
          ) {
            idx <- which(pairs_now$quote == contents_now[2])
            is_match <- pairs_now$name[idx] == contents_now[1]
          }

          # Prepare new states vector
          states_new <- card_states()
          if (is_match) {
            states_new[ids_now] <- "matched"
          } else {
            states_new[ids_now] <- "hidden"
          }

          # Only update reactiveVals inside later
          later::later(
            function() {
              card_states(states_new)
              selected_cards(character(0))
            },
            delay = 0.7
          )
        }
      },
      ignoreInit = TRUE
    )
  })

  # Show win modal when all cards are matched
  observe({
    states <- card_states()
    if (!is.null(states) && all(states == "matched") && !win_shown()) {
      output$end_party <- renderConfetti({
        confetti(particle_count = 500, spread = 180)
      })
      win_shown(TRUE)
      showModal(
        modalDialog(
          title = "You win!",
          HTML(str_c(
            'Congratulations! As your reward, enjoy an out-of-context Taskmaster moment:<br><br>',
            sample(reward, 1)
          )),
          footer = tagList(
            modalButton("Close")
          ),
          easyClose = TRUE
        )
      )
    }
  })

  output$card_grid_ui <- renderUI({
    grid <- game_grid()
    states <- card_states()
    if (is.null(grid) || is.null(states)) {
      return()
    }
    tagList(
      map(unique(grid$row), function(r) {
        fluidRow(
          map(unique(grid$col), function(c) {
            card <- grid |>
              filter(row == r, col == c)
            id <- card$card_id
            state <- states[[id]]
            if (state == "matched") {
              label <- HTML(str_replace_all(card$content, "\n", "<br>"))
              disabled <- TRUE
              btn_class <- "card-matched"
            } else if (state == "revealed") {
              label <- HTML(str_replace_all(card$content, "\n", "<br>"))
              disabled <- FALSE
              btn_class <- "card-revealed"
            } else {
              label <- "Reveal Card"
              disabled <- FALSE
              btn_class <- "card-hidden"
            }
            column(
              width = 3,
              actionButton(
                inputId = id,
                label = label,
                width = "100%",
                disabled = disabled,
                class = btn_class
              )
            )
          })
        )
      })
    )
  })
}

shinyApp(ui, server)
