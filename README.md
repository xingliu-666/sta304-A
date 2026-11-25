# ==============================================================
# Who Pays for Generative AI?  -  STA304H5F, Group sta304-A
# Interactive website for our survey results (R Shiny)
# ==============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# --------------------------------------------------------------
# 1. DATA
# --------------------------------------------------------------
# survey_data.csv is our cleaned survey data (student IDs removed).
df <- read.csv("survey_data.csv")

# Variables used for the three research questions
df$pays <- ifelse(df$paid_access == 2, 1, 0)   # 1 = personally pays
df$cs   <- ifelse(df$program == 1, 1, 0)       # 1 = Computer Science

# Readable labels for categories (taken from the codebook)
df$program_label <- factor(df$program, levels = 1:3,
  labels = c("Computer Science", "Statistics, Math, or Data Science", "Other"))
df$access_label <- factor(df$paid_access, levels = 0:2,
  labels = c("Free versions only", "Paid access, someone else pays", "Pays personally"))

# Readable names for the numeric variables (used in menus, axes and text)
num_labels <- c(
  ai_days_week   = "Days per week using GenAI for coursework",
  ai_usefulness  = "Perceived usefulness of GenAI (1-10)",
  spending_money = "Monthly spending money (CAD)",
  wtp_monthly    = "Maximum willingness to pay per month (CAD)"
)
cat_labels <- c(
  program_label = "Program of study",
  access_label  = "Access to paid GenAI tools"
)

tool_cols <- c(tool_chatgpt = "ChatGPT", tool_claude = "Claude",
               tool_gemini = "Google Gemini", tool_deepseek = "DeepSeek",
               tool_other = "Other tool")
act_cols <- c(act_explain = "Explaining course concepts",
              act_outline = "Outlining ideas",
              act_writing = "Writing or editing",
              act_coding = "Coding or debugging",
              act_practice = "Practice questions or study materials",
              act_summarize = "Summarizing readings or notes",
              act_other = "Other coursework-related activity",
              act_none = "Did not use GenAI for coursework")

# Colour sets the user can choose for the graphs
palettes <- list(
  "Night sky (default)"   = c("#4DD0E1", "#FFD166", "#B39DDB"),
  "Colour-blind friendly" = c("#56B4E9", "#E69F00", "#CC79A7"),
  "Greyscale"             = c("#F1F3F5", "#ADB5BD", "#6C757D")
)

msg_empty <- "No respondents match the current filter. Please select at least one program."

# --------------------------------------------------------------
# 2. STATISTICAL TESTS (same methods as in our proposal)
# --------------------------------------------------------------
p_text <- function(p) if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)

# RQ1: one-sample z-test for a proportion, H0: p = 0.5, H1: p > 0.5, plus 95% CI
run_rq1 <- function(d) {
  n <- nrow(d)
  if (n < 1) return(NULL)
  x <- sum(d$pays)
  p <- x / n
  z <- (p - 0.5) / sqrt(0.5 * 0.5 / n)
  se <- sqrt(p * (1 - p) / n)
  list(n = n, x = x, p = p, z = z, pval = 1 - pnorm(z),
       lo = max(0, p - 1.96 * se), hi = min(1, p + 1.96 * se))
}

# RQ2: two-sample z-test for proportions, H0: p_CS = p_nonCS, H1: p_CS > p_nonCS
run_rq2 <- function(d) {
  a <- d[d$cs == 1, ]
  b <- d[d$cs == 0, ]
  n1 <- nrow(a); n2 <- nrow(b)
  if (n1 < 1 || n2 < 1) return(NULL)
  x1 <- sum(a$pays); x2 <- sum(b$pays)
  p1 <- x1 / n1;     p2 <- x2 / n2
  pp <- (x1 + x2) / (n1 + n2)
  se <- sqrt(pp * (1 - pp) * (1 / n1 + 1 / n2))
  if (se == 0) return(NULL)
  z <- (p1 - p2) / se
  ci <- function(p, n) {
    s <- sqrt(p * (1 - p) / n)
    c(max(0, p - 1.96 * s), min(1, p + 1.96 * s))
  }
  list(n1 = n1, n2 = n2, x1 = x1, x2 = x2, p1 = p1, p2 = p2,
       z = z, pval = 1 - pnorm(z), ci1 = ci(p1, n1), ci2 = ci(p2, n2))
}

# RQ3: logistic regression, paying (yes / no) ~ days + usefulness + spending money
# (as in our proposal). Spending money is in units of $100 so the odds ratio is easy to read.
run_rq3 <- function(d) {
  d$spend100 <- d$spending_money / 100
  dd <- d[complete.cases(d[, c("pays", "ai_days_week", "ai_usefulness", "spend100")]), ]
  if (nrow(dd) < 20 || length(unique(dd$pays)) < 2) return(NULL)
  fit <- glm(pays ~ ai_days_week + ai_usefulness + spend100, data = dd, family = binomial)
  co <- summary(fit)$coefficients[-1, ]    # drop the intercept
  ci <- confint.default(fit)[-1, ]         # 95% confidence intervals
  tab <- data.frame(
    term = c("Days per week using GenAI", "Perceived usefulness (1-10)",
             "Monthly spending money (per $100)"),
    unit = c("extra day per week of use", "extra point of usefulness",
             "extra $100 of spending money"),
    or   = exp(co[, 1]),
    lo   = exp(ci[, 1]),
    hi   = exp(ci[, 2]),
    z    = co[, 3],
    pval = co[, 4],
    stringsAsFactors = FALSE)
  list(n = nrow(dd), events = sum(dd$pays), table = tab)
}

# One plain-language sentence for one predictor of the logistic regression
or_sentence <- function(t) {
  if (t$or >= 1) direction <- "higher" else direction <- "lower"
  if (t$pval < 0.05) ending <- "This is statistically significant."
  else ending <- "The interval includes 1, so this could be chance."
  sprintf("%s: each %s goes with %.0f%% %s odds of paying (odds ratio %.2f, 95%% CI %.2f to %.2f). %s",
          t$term, t$unit, abs((t$or - 1) * 100), direction, t$or, t$lo, t$hi, ending)
}

# Results for the full sample (used on the Home page)
all_rq1 <- run_rq1(df)
all_rq2 <- run_rq2(df)
all_rq3 <- run_rq3(df)

# --------------------------------------------------------------
# 3. USER INTERFACE
# --------------------------------------------------------------

# Light colour for text, points and lines so they show up on the dark plots
light <- "#E6ECF8"

# Numbers used in the written text. They are computed from the full
# sample, so the text always matches the data.
n_all  <- nrow(df)
pc     <- function(x) sprintf("%.1f%%", 100 * x)
n_prog <- table(df$program)
tool_pct <- 100 * colMeans(df[, names(tool_cols)])
act_pct  <- 100 * colMeans(df[, names(act_cols)])
med_pay    <- median(df$spending_money[df$pays == 1], na.rm = TRUE)
med_nopay  <- median(df$spending_money[df$pays == 0], na.rm = TRUE)
share_wtp0 <- mean(df$wtp_monthly == 0)


# Page style: starry night background (stars are small dots drawn with CSS) and dark cards
star_css <- tags$style(HTML("
body { background-color: #060B1F !important;
       background-image:
         radial-gradient(2.5px 2.5px at 65px 168px, #B8F3FA, transparent),
         radial-gradient(3px 3px at 336px 57px, #FFFFFF, transparent),
         radial-gradient(2px 2px at 323px 289px, #B8F3FA, transparent),
         radial-gradient(2.5px 2.5px at 185px 256px, #E0D4FF, transparent),
         radial-gradient(2px 2px at 230px 127px, #FFFFFF, transparent),
         radial-gradient(3px 3px at 20px 340px, #CFE8FF, transparent),
         radial-gradient(2px 2px at 112px 40px, #CFE8FF, transparent),
         radial-gradient(2.5px 2.5px at 290px 360px, #B8F3FA, transparent),
         radial-gradient(ellipse at 15% 0%, rgba(77,208,225,0.20), transparent 55%),
         radial-gradient(ellipse at 90% 8%, rgba(124,92,255,0.25), transparent 50%),
         linear-gradient(180deg, #060B1F 0%, #0A1130 60%, #0D0F2B 100%);
       background-size: 380px 380px, 380px 380px, 380px 380px, 380px 380px, 380px 380px,
                        380px 380px, 380px 380px, 380px 380px, auto, auto, auto;
       background-attachment: fixed; }
.card { border: 1px solid rgba(255,255,255,0.22); box-shadow: 0 4px 18px rgba(0,0,0,0.35); }
.card-hd { background: linear-gradient(90deg, #32498F, #4A3A9C) !important; color: #FFFFFF !important;
           border-bottom: 2px solid #4DD0E1 !important; font-weight: 600; }
.hero { background: linear-gradient(135deg, rgba(22,37,79,0.92), rgba(48,28,102,0.90));
        border: 1px solid rgba(77,208,225,0.40); color: #FFFFFF; }
.alert-success { background: rgba(77,208,225,0.12); border-color: rgba(77,208,225,0.45); color: #D9F7FB; }
.alert-info { background: rgba(124,140,248,0.14); border-color: rgba(124,140,248,0.45); color: #E1E6FF; }
.alert-secondary { background: rgba(255,255,255,0.07); border-color: rgba(255,255,255,0.20); color: #D5DCEC; }
.selectize-input, .selectize-dropdown, .form-select, .form-control { background: #1A2A57 !important; color: #FFFFFF !important; border-color: #4A609C !important; }
.selectize-input > .item, .selectize-input input { color: #E6ECF8 !important; }
.selectize-dropdown .active { background: #2A3C70 !important; color: #FFFFFF !important; }
.irs--shiny .irs-line { background: #4A609C; border-color: #4A609C; }
.irs--shiny .irs-bar { background: #4DD0E1; border-color: #4DD0E1; }
.irs--shiny .irs-handle { background: #4DD0E1; border-color: #0A1130; }
.irs--shiny .irs-min, .irs--shiny .irs-max { background: #2A3A63; color: #CDD7EE; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #4DD0E1; color: #06101F; }
.irs--shiny .irs-grid-text { color: #9FB0D0; }
.btn-default { background: #16234A; color: #E6ECF8; border-color: #4DD0E1; }
table.dataTable, table.dataTable tbody tr, table.dataTable tbody td { background-color: transparent !important; color: #E6ECF8 !important; }
table.dataTable tbody tr.odd td { background-color: rgba(255,255,255,0.04) !important; }
table.dataTable thead th { border-bottom: 1px solid #4DD0E1 !important; color: #FFFFFF !important; }
.dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate { color: #B8C4DC !important; }
.dataTables_wrapper .dataTables_paginate .paginate_button { color: #E6ECF8 !important; }
.dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select { background: #16234A; color: #E6ECF8; border: 1px solid #2F4070; }
.site-footer { color: #9FB0D0; }
.tab-content { padding-left: 7vw !important; padding-right: 7vw !important; padding-bottom: 1rem; }
.sub { color: #4DD0E1; font-weight: 700; margin-top: 1rem; margin-bottom: 0.25rem; }
.navbar { background: linear-gradient(90deg, #2A2D9E, #5B2FA8) !important; border-bottom: 3px solid #4DD0E1; }
.navbar .navbar-brand { color: #FFFFFF !important; font-weight: 700; font-size: 1.1rem; white-space: nowrap; }
.navbar .nav-link { color: #FFFFFF !important; font-weight: 600; opacity: 0.85; font-size: 0.88rem; padding-left: 0.4rem; padding-right: 0.4rem; white-space: nowrap; }
.navbar .nav-link:hover { color: #4DD0E1 !important; opacity: 1; }
.navbar .nav-link.active { color: #4DD0E1 !important; opacity: 1; border-bottom: 3px solid #4DD0E1; }
.navbar .navbar-toggler { border-color: #FFFFFF; background: rgba(255,255,255,0.15); }
.selectize-control.multi .selectize-input.items > .item { background: #2A3C70 !important; color: #FFFFFF !important; border-radius: 999px; padding: 2px 10px; border: 1px solid #4DD0E1; }
.selectize-control.multi .selectize-input > div.active { background: #4DD0E1 !important; color: #06101F !important; }
.selectize-control.multi .selectize-input > div .remove { color: #FFFFFF; border-left: none; }
.hero h1 { color: #FFFFFF; text-shadow: 0 2px 10px rgba(0,0,0,0.5); }
"))

# Small heading inside a card
sub <- function(x) div(class = "sub", x)
# Median of a variable for students who do not pay (g = 0) or who pay (g = 1)
med_by <- function(v, g) median(df[[v]][df$pays == g], na.rm = TRUE)

# Card header with a colour band
hd <- function(txt) card_header(txt, class = "card-hd")

# Methods table shown on the Project page (same methods as our proposal)
methods_df <- data.frame(
  Question = c("RQ1", "RQ2", "RQ3"),
  Method = c("One-sample z-test for a proportion, plus a 95% confidence interval",
             "Two-sample z-test for comparing two proportions",
             "Logistic regression"),
  Hypotheses = c("H0: p = 0.5  vs  H1: p > 0.5, where p is the proportion who personally pay",
                 "H0: p_CS = p_nonCS  vs  H1: p_CS > p_nonCS",
                 "Response: pays (yes / no). Predictors: days of use per week, perceived usefulness, monthly spending money"),
  stringsAsFactors = FALSE)

ui <- page_navbar(
  title = "Do STA304 Students Pay for GenAI?",
  id = "main_nav",
  fillable = FALSE,
  padding = c("16px", "7vw"),
  footer = div(class = "text-center py-3 site-footer",
               "STA304H5F, Group sta304-A  |  University of Toronto Mississauga  |  Fall 2026"),
  bg = "#2A2D9E",
  inverse = TRUE,
  theme = bs_theme(version = 5, bg = "#223665", fg = "#F4F7FF",
                   primary = "#4DD0E1", secondary = "#8E9AB8",
                   success = "#4DD0E1", info = "#7C8CF8",
                   "border-radius" = "0.8rem"),

  # ---- Filter bar: only shown on the pages that use the data ----
  header = tagList(star_css, div(style = "padding: 16px 7vw 0 7vw;", conditionalPanel(
    condition = "['data','rq1','rq2','rq3','resp'].indexOf(input.main_nav) >= 0",
    card(
      layout_columns(
        col_widths = c(6, 4, 2),
        div(
          selectizeInput("prog_filter", "Show students from:",
                         choices = setNames(1:3, levels(df$program_label)),
                         selected = 1:3, multiple = TRUE, width = "100%",
                         options = list(placeholder = "Pick at least one program",
                                        plugins = list("remove_button"))),
          helpText("Click \u00d7 on a tag to remove a program, or click the box to add it back.")
        ),
        selectInput("palette", "Graph colours", choices = names(palettes)),
        div(strong("Students included"), br(), textOutput("n_selected"))
      )
    )
  ))),

  # ================= PROJECT =================
  nav_panel(
    "Project", value = "project",
    div(class = "p-4 mb-3 rounded hero",
        layout_columns(
          col_widths = c(8, 4),
          div(
            h1(class = "display-6 fw-bold", "Do STA304 students pay for generative AI?"),
            p(class = "lead mb-0",
              "We surveyed our own class to find out how many students pay for a GenAI subscription,",
              "whether program matters, and whether use, usefulness or spending money can explain who pays.")),
          div(img(src = "hero.jpg", class = "img-fluid rounded shadow",
                  alt = "A student working with a computer and an AI interface")))),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Students who answered", value = n_all,
                p("STA304H5F, Fall 2026")),
      value_box(title = "Personally pay for GenAI", value = pc(all_rq1$p),
                p(sprintf("%d of %d students", all_rq1$x, all_rq1$n))),
      value_box(title = "Survey period", value = "Sep 24 - Oct 9",
                p("Google Form, 2026"))
    ),
    card(
      hd("Why we did this survey"),
      p("Many students use generative AI (GenAI) tools like ChatGPT, Claude and Gemini",
        "for their courses. Some of them pay for a subscription, and some only use the",
        "free versions. We wanted to know how common paying is in our own class,",
        "and what kind of student is more likely to pay."),
      p("Our survey asked STA304H5F students about their program, how often they use GenAI,",
        "how useful they find it, how much spending money they have, and whether they",
        "personally pay for a GenAI subscription.")
    ),
    card(
      hd("Our three research questions"),
      tags$ol(
        tags$li(strong("RQ1:"), "What proportion of STA304H5 students personally pay for a subscription to at least one generative AI tool (for example ChatGPT Plus, Claude Pro, Gemini Advanced)?"),
        tags$li(strong("RQ2:"), "Are Computer Science students more likely to pay for a generative AI subscription than non-CS students?"),
        tags$li(strong("RQ3:"), "Do frequency of use, perceived usefulness for coursework, and monthly discretionary budget predict whether a student pays for a subscription?")
      ),
      p("The three questions build on each other. RQ1 asks how common paying is.",
        "RQ2 asks whether it depends on program. RQ3 asks whether students pay because",
        "they need the tools (use and usefulness) or because they can afford them (spending money).")
    ),
    card(
      hd("How we collected the data"),
      p(strong("Who we wanted to learn about: "),
        "all students taking STA304H5F at the University of Toronto Mississauga in Fall 2026."),
      p(strong("How we planned the sample: "),
        "We planned a stratified random sample with three strata based on program:",
        "Computer Science, Statistics / Mathematics / Data Science, and Other.",
        "We thought program might change how students use AI (for example, CS students",
        "use it for coding), so we wanted enough answers from each group for RQ2."),
      p(strong("What happened in practice: "),
        "Students filled in our Google Form by choice. We stood near the classroom",
        "entrance with a QR code before class, and we also posted the link on Piazza.",
        "Each course-specific ID could only submit once. So our sample is a voluntary",
        "sample and may not represent the whole class. We did not",
        "weight the results."),
      p(strong("Our sample: "),
        sprintf("We got 71 submissions between September 24 and October 9, 2026. Two course IDs submitted twice, so we kept the earliest answer for each. This left %d respondents: %d in Computer Science, %d in Statistics / Mathematics / Data Science, and %d in Other programs.",
                n_all, n_prog[["1"]], n_prog[["2"]], n_prog[["3"]]))
    ),
    card(
      hd("Which method answers which question"),
      tableOutput("methods_table"),
      p("These are the methods we planned in our proposal. We use the same methods here.")
    ),
    card(
      hd("The statistics in plain language"),
      accordion(
        open = FALSE,
        accordion_panel("What is a p-value?",
          p("A p-value tells us how surprising our data would be if there were really no effect.",
            "A small p-value (below 0.05) means the data would be unlikely under 'no effect', so we have evidence for an effect.",
            "A large p-value means we did not find clear evidence. It does not prove that there is no effect.")),
        accordion_panel("What is a 95% confidence interval?",
          p("It is a range of believable values for the true proportion in the whole class.",
            "If the range includes 0.5 (50%), we cannot say whether more or fewer than half of students pay.")),
        accordion_panel("What is a z-test for proportions?",
          p("It compares a sample proportion with a value (RQ1: is it above 50%?) or compares two groups (RQ2: do CS students pay more than other students?).",
            "The z statistic says how far apart the numbers are, measured in standard errors.")),
        accordion_panel("What is an odds ratio (logistic regression)?",
          p("Logistic regression looks at what goes with a yes / no answer (here: pays or does not pay).",
            "An odds ratio of 1 means no relationship. Above 1 means higher values go with higher odds of paying, and below 1 means lower odds.",
            "If the interval for the odds ratio includes 1, we cannot say the relationship is real."))
      )
    ),
    card(
      hd("How to use this website"),
      tags$ul(
        tags$li(strong("The Data"), "shows what our respondents are like. You can pick the variable."),
        tags$li(strong("RQ1, RQ2, RQ3"), "each show the graphs and test for one question, with a plain-language reading."),
        tags$li(strong("Findings"), "puts our answers together and explains what they mean and how sure we are."),
        tags$li(strong("Responses"), "is a table of the answers you can search and sort."),
        tags$li("On the data pages, the bar at the top lets you choose which programs to include and which colours the graphs use. Every graph, test and sentence changes with your choice.")
      )
    )
  ),

  # ================= THE DATA =================
  nav_panel(
    "The Data", value = "data",
    card(
      hd("1. Look at one variable"),
      layout_columns(
        col_widths = c(4, 8),
        div(
          selectInput("exp_var", "Choose a variable to display",
                      choices = c(setNames(names(num_labels), num_labels),
                                  setNames(names(cat_labels), cat_labels))),
          uiOutput("exp_controls")
        ),
        plotOutput("exp_plot", height = "380px")
      ),
      uiOutput("exp_text")
    ),
    card(
      hd("2. Which tools and uses are most common?"),
      layout_columns(
        col_widths = c(4, 8),
        div(
          selectInput("exp2_type", "What would you like to see?",
                      choices = c("GenAI tools students have access to" = "tools",
                                  "What students used GenAI for (past 30 days)" = "acts")),
          radioButtons("exp2_unit", "Show the bars as",
                       choices = c("Percent of respondents" = "pct",
                                   "Number of respondents" = "n")),
          helpText("Students could select more than one option, so the percentages add up to more than 100%.")
        ),
        plotOutput("exp2_plot", height = "380px")
      )
    )
  ),

  # ================= RQ1 =================
  nav_panel(
    "RQ1: Pay rate", value = "rq1",
    card(
      hd("RQ1: What proportion of students personally pay for a GenAI subscription?"),
      div(class = "alert alert-info", strong("Method: "), "We count a student as paying if they personally pay for at least one subscription.",
        "We use a one-sample z-test with H0: p = 0.5 and H1: p > 0.5, to see whether more than half pay.",
        "We also give a 95% confidence interval for the proportion.")
    ),
    layout_columns(
      col_widths = c(7, 5),
      navset_card_tab(
        title = "Choose a graph",
        nav_panel("Counts by access type",
                  radioButtons("rq1_unit", "Show the bars as", inline = TRUE,
                               choices = c("Number of students" = "n",
                                           "Percent of students" = "pct")),
                  plotOutput("rq1_bar", height = "340px")),
        nav_panel("Estimate vs. 50%", plotOutput("rq1_ci", height = "320px"))
      ),
      card(
        hd("Result for the students included"),
        tableOutput("rq1_table"),
        uiOutput("rq1_text")
      )
    )
  ),

  # ================= RQ2 =================
  nav_panel(
    "RQ2: CS vs. non-CS", value = "rq2",
    card(
      hd("RQ2: Are Computer Science students more likely to pay than non-CS students?"),
      div(class = "alert alert-info", strong("Method: "), "Non-CS means Statistics / Mathematics / Data Science plus Other programs.",
        "We use a two-sample z-test for proportions with H0: p_CS = p_nonCS and H1: p_CS > p_nonCS.",
        "To compare the groups, keep Computer Science and at least one other program selected in the bar at the top.")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        hd("Percent who personally pay, by program group"),
        sliderInput("rq2_ymax", "Upper limit of the y-axis (%)",
                    min = 30, max = 100, value = 100, step = 5),
        plotOutput("rq2_plot", height = "340px")
      ),
      card(
        hd("Result for the students included"),
        tableOutput("rq2_table"),
        uiOutput("rq2_text")
      )
    )
  ),

  # ================= RQ3 =================
  nav_panel(
    "RQ3: Predictors", value = "rq3",
    card(
      hd("RQ3: Do use, usefulness and spending money predict whether a student pays?"),
      div(class = "alert alert-info", strong("Method: "), "We use logistic regression. The response is paying (yes / no).",
        "The predictors are days per week using GenAI, perceived usefulness (1 to 10),",
        "and monthly spending money (we show the odds ratio for every extra $100).",
        "An odds ratio above 1 means that higher values go with higher odds of paying.")
    ),
    layout_columns(
      col_widths = c(7, 5),
      navset_card_tab(
        title = "Choose a graph",
        nav_panel(
          "Compare payers and non-payers",
          layout_columns(
            col_widths = c(5, 7),
            div(
              selectInput("rq3_var", "Choose a predictor to display",
                          choices = setNames(c("ai_days_week", "ai_usefulness", "spending_money"),
                                             num_labels[c("ai_days_week", "ai_usefulness", "spending_money")])),
              uiOutput("rq3_controls")
            ),
            plotOutput("rq3_plot", height = "380px")
          )
        ),
        nav_panel("Odds ratios", plotOutput("rq3_forest", height = "380px"))
      ),
      card(
        hd("Result for the students included"),
        tableOutput("rq3_table"),
        uiOutput("rq3_text")
      )
    )
  ),

  # ================= FINDINGS =================
  nav_panel(
    "Findings", value = "findings",
    card(
      hd("What we set out to find out"),
      p("Our goal was to find out how common it is for STA304H5F students to pay for a generative AI subscription, and whether we can tell who is more likely to pay.",
        "We asked three questions, one after the other. RQ1 asks how many students pay.",
        "RQ2 asks whether program of study makes a difference.",
        "RQ3 asks whether how much students use GenAI, how useful they find it, or how much spending money they have is related to paying."),
      p("Each answer below leads to the next question. The numbers on this page use all",
        n_all, "respondents, so they do not change with the filter.")
    ),

    card(
      hd("RQ1: How many students pay?"),
      sub("The question"),
      p("What proportion of STA304H5 students personally pay for a subscription to at least one generative AI tool?"),
      sub("Why we asked"),
      p("Many students use GenAI tools for their courses, but some tools cost money. Before we can ask who pays, we need to know how common paying is in our class."),
      sub("What we did"),
      p("We counted a student as paying if they personally pay for at least one subscription.",
        "Students who have a paid subscription that someone else pays for were counted as not paying, because we want to know who pays.",
        "We tested whether more than half of students pay with a one-sample z-test (H0: p = 0.5, H1: p > 0.5), and we found a 95% confidence interval for the proportion."),
      sub("What we found"),
      tags$ul(
        tags$li(sprintf("%d of %d students (%s) personally pay.", all_rq1$x, all_rq1$n, pc(all_rq1$p))),
        tags$li(sprintf("%d students (%s) have a paid subscription but someone else pays, and %d students (%s) only use free versions.",
                        sum(df$paid_access == 1), pc(mean(df$paid_access == 1)),
                        sum(df$paid_access == 0), pc(mean(df$paid_access == 0)))),
        tags$li(sprintf("The 95%% confidence interval for the proportion who pay is %.3f to %.3f (%s to %s).",
                        all_rq1$lo, all_rq1$hi, pc(all_rq1$lo), pc(all_rq1$hi))),
        tags$li(sprintf("The z-test gives z = %.2f and %s.", all_rq1$z, p_text(all_rq1$pval)))
      ),
      sub("What it means"),
      p(sprintf("About half of our respondents pay for GenAI, so paying is common in our class. But the confidence interval is wide. With %d students, the true share could be anywhere from about %s to %s. This range includes 50%%, so we cannot say that more than half of STA304 students pay, and we cannot say that fewer than half pay either.",
                all_rq1$n, pc(all_rq1$lo), pc(all_rq1$hi))),
      div(class = "alert alert-info", strong("Next question: "),
          "If about half of students pay and half do not, what makes the difference? We first looked at program of study.")
    ),

    card(
      hd("RQ2: Does program of study matter?"),
      sub("The question"),
      p("Are Computer Science students more likely to pay for a generative AI subscription than non-CS students?"),
      sub("Why we asked"),
      p("CS students use AI for coding, and coding help is one of the main things people pay for. So we thought CS students might be more likely to pay. This is also the first reason we looked at for why some students pay and others do not."),
      sub("What we did"),
      p(sprintf("We put students into two groups: Computer Science (%d students) and non-CS (%d students, which is Statistics / Mathematics / Data Science plus Other programs).",
                all_rq2$n1, all_rq2$n2),
        "We thought CS students might pay more because they use AI for coding.",
        "We tested this with a two-sample z-test for proportions (H0: p_CS = p_nonCS, H1: p_CS > p_nonCS)."),
      sub("What we found"),
      tags$ul(
        tags$li(sprintf("%d of %d Computer Science students (%s) pay. Among non-CS students, %d of %d (%s) pay.",
                        all_rq2$x1, all_rq2$n1, pc(all_rq2$p1), all_rq2$x2, all_rq2$n2, pc(all_rq2$p2))),
        tags$li(sprintf("The gap is %.1f percentage points, and it goes the other way: CS students paid less often.",
                        abs(100 * (all_rq2$p1 - all_rq2$p2)))),
        tags$li(sprintf("The z-test gives z = %.2f and %s.", all_rq2$z, p_text(all_rq2$pval)))
      ),
      sub("What it means"),
      p(sprintf("We did not find that CS students are more likely to pay. We should be careful with this result, because only %d CS students answered. Each CS student is worth %.1f percentage points, so a few different answers would change the result a lot. So we say that we found no evidence that CS students pay more. We do not say that they pay less.",
                all_rq2$n1, 100 / all_rq2$n1)),
      div(class = "alert alert-info", strong("Next question: "),
          "Program does not seem to explain who pays. So we asked whether the way students use GenAI, or their money, does.")
    ),

    card(
      hd("RQ3: Do use, usefulness or spending money predict paying?"),
      sub("The question"),
      p("Do frequency of use, perceived usefulness for coursework, and monthly discretionary budget predict whether a student pays for a subscription?"),
      sub("Why we asked"),
      p("Program is only one possible reason. A student who uses GenAI every day, or who finds it very useful, may feel that a subscription is worth the price. A student with more spending money may find it easier to pay. We wanted to check these reasons together."),
      sub("What we did"),
      p(sprintf("We used logistic regression because the response is yes / no (pays or does not pay). The three predictors cover two ideas: the need for the tools (days per week of use and perceived usefulness) and the ability to pay (monthly spending money). The model uses %d students, and %d of them pay. Three students left spending money blank, so they are not in the model.",
                all_rq3$n, all_rq3$events)),
      sub("What we found"),
      tags$ul(tags$li(or_sentence(all_rq3$table[1, ])), tags$li(or_sentence(all_rq3$table[2, ])), tags$li(or_sentence(all_rq3$table[3, ]))),
      p(sprintf("For a rough picture, students who pay had a median of %.0f days of use per week, a usefulness rating of %.0f, and $%.0f of spending money. Students who do not pay had %.0f days, a rating of %.0f, and $%.0f.",
                med_by("ai_days_week", 1), med_by("ai_usefulness", 1), med_by("spending_money", 1),
                med_by("ai_days_week", 0), med_by("ai_usefulness", 0), med_by("spending_money", 0))),
      sub("What it means"),
      p(paste0("None of the three variables is statistically significant. All the odds ratios are a little above 1, so students who use GenAI more, find it more useful, or have more spending money were slightly more likely to pay. But every confidence interval includes 1, so this could be chance. In plain words, we cannot tell who pays from how often they use GenAI, how useful they find it, or how much money they have.")),
      div(class = "alert alert-info", strong("Putting it together: "),
          "That answers our three questions. The box below says what they mean together.")
    ),

    card(
      hd("Putting it together"),
      p(strong("Our main answer. "),
        sprintf("About half of the students in our sample (%s) pay for GenAI themselves. Paying is common in our class, but we cannot say that most students pay.",
                pc(all_rq1$p))),
      p(strong("What the three results say together. "),
        "RQ1 shows that about half of students pay. RQ2 and RQ3 looked for reasons.",
        "We looked at program of study, how often students use GenAI, how useful they find it, and how much spending money they have.",
        "None of these clearly separates students who pay from students who do not.",
        "So in our data, the survey answers we collected do not explain who pays."),
      p(strong("What we would do with more time. "),
        "We would collect more answers, especially from Computer Science and Other programs,",
        "because those groups are small. We would also ask students why they decided to pay or not to pay.")
    ),

    card(
      hd("How sure are we? Limits of our results"),
      tags$ul(
        tags$li("Our sample is a voluntary sample, not a random one. The results describe our respondents and may not apply to all STA304H5F students."),
        tags$li(sprintf("The Computer Science group (%d students) and the Other group (%d students) are very small, so results for them are not very reliable.",
                        n_prog[["1"]], n_prog[["3"]])),
        tags$li("All answers are self-reported."),
        tags$li("Spending money was recorded at the middle of the chosen range (or the lowest value for open-ended answers), so it is only approximate."),
        tags$li("The z-tests use a normal approximation, which works less well for small groups."),
        tags$li("This is survey data. We can say that variables are related, but not that one causes the other.")
      )
    )
  ),

  # ================= RESPONSES =================
  nav_panel(
    "Responses", value = "resp",
    card(
      hd("The survey answers (student IDs removed)"),
      helpText("This table shows the students included by the bar at the top. You can sort and search it."),
      div(downloadButton("dl_data", "Download these rows (CSV)")),
      br(),
      DTOutput("data_table")
    )
  )
)

# --------------------------------------------------------------
# 4. SERVER
# --------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Shared reactive objects: every page uses these ----
  filtered <- reactive({
    df[df$program %in% as.integer(input$prog_filter), , drop = FALSE]
  })
  pal <- reactive(palettes[[input$palette]])

  output$methods_table <- renderTable(methods_df, striped = TRUE, spacing = "s")

  output$n_selected <- renderText({
    paste0(nrow(filtered()), " of ", nrow(df))
  })

  # Common look for all plots
  plot_theme <- theme_minimal(base_size = 14) +
    theme(text = element_text(colour = "#E6ECF8"),
          plot.title = element_text(face = "bold", colour = "#FFFFFF"),
          axis.text = element_text(colour = "#DCE4F5"),
          panel.grid.major = element_line(colour = "#40578F"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#223665", colour = NA),
          panel.background = element_rect(fill = NA, colour = NA))
  n_note <- function(d) paste0("n = ", nrow(d), " respondents (current filter)")
  wrap_txt <- function(x, w = 60) paste(strwrap(x, w), collapse = "\n")

  # ================= Explore: plot 1 =================
  output$exp_controls <- renderUI({
    v <- input$exp_var
    if (v %in% names(num_labels)) {
      tagList(
        sliderInput("exp_bins", "Number of bins", min = 5, max = 30, value = 10, step = 1),
        uiOutput("exp_xlim_ui")
      )
    } else {
      helpText("This is a categorical variable, so we show how many respondents are in each category.")
    }
  })

  # The x-axis slider depends on the variable
  output$exp_xlim_ui <- renderUI({
    v <- input$exp_var
    req(v %in% names(num_labels))
    r <- range(df[[v]], na.rm = TRUE)
    lo <- floor(r[1]); hi <- ceiling(r[2] * 1.05 + 0.5)
    sliderInput("exp_xlim", "Range of the x-axis", min = lo, max = hi,
                value = c(lo, hi), step = if (hi - lo > 20) 1 else 0.5)
  })

  output$exp_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    v <- input$exp_var
    col <- pal()[1]

    if (v %in% names(num_labels)) {
      d$x <- d[[v]]
      d <- d[!is.na(d$x), ]
      validate(need(nrow(d) > 0, "No usable answers for this variable in the current filter."))
      xl <- input$exp_xlim
      if (is.null(xl)) xl <- range(d$x)
      bins <- if (is.null(input$exp_bins)) 10 else input$exp_bins
      ggplot(d, aes(x = x)) +
        geom_histogram(bins = bins, fill = col, colour = "#223665") +
        coord_cartesian(xlim = xl) +
        labs(title = paste("Distribution of:", num_labels[[v]]),
             subtitle = n_note(d), x = num_labels[[v]], y = "Number of students") +
        plot_theme
    } else {
      d$x <- d[[v]]
      tab <- as.data.frame(table(d$x))
      names(tab) <- c("category", "n")
      ggplot(tab, aes(x = category, y = n)) +
        geom_col(fill = col, width = 0.65) +
        geom_text(aes(label = n), vjust = -0.4, colour = light) +
        scale_x_discrete(labels = scales::label_wrap(20)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
        labs(title = paste("Distribution of:", cat_labels[[v]]),
             subtitle = n_note(d), x = cat_labels[[v]], y = "Number of students") +
        plot_theme
    }
  })

  output$exp_text <- renderUI({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    v <- input$exp_var
    if (v %in% names(num_labels)) {
      x <- d[[v]][!is.na(d[[v]])]
      validate(need(length(x) > 0, "No usable answers for this variable in the current filter."))
      p(strong("How to read this: "),
        sprintf("Each bar counts students whose answer falls in that range. For the selected students, the average is %.1f and the middle value (median) is %.1f.",
                mean(x), median(x)))
    } else {
      tab <- table(d[[v]])
      top <- names(tab)[which.max(tab)]
      p(strong("How to read this: "),
        sprintf("Each bar counts students in one category. The largest group is \"%s\" with %d of %d students (%.1f%%).",
                top, max(tab), sum(tab), 100 * max(tab) / sum(tab)))
    }
  })

  # ================= Explore: plot 2 =================
  output$exp2_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    cols <- if (input$exp2_type == "tools") tool_cols else act_cols
    ttl  <- if (input$exp2_type == "tools") "GenAI tools students have access to"
            else "What students used GenAI for in the past 30 days"
    tab <- data.frame(option = unname(cols),
                      n = colSums(d[, names(cols), drop = FALSE]))
    tab$pct <- 100 * tab$n / nrow(d)
    tab$value <- if (input$exp2_unit == "pct") tab$pct else tab$n
    tab$label <- if (input$exp2_unit == "pct") sprintf("%.1f%%", tab$pct) else tab$n
    tab$option <- factor(tab$option, levels = tab$option[order(tab$value)])
    ggplot(tab, aes(x = option, y = value)) +
      geom_col(fill = pal()[1], width = 0.65) +
      geom_text(aes(label = label), hjust = -0.15, colour = light) +
      coord_flip() +
      scale_x_discrete(labels = scales::label_wrap(30)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(title = ttl, subtitle = n_note(d), x = NULL,
           y = if (input$exp2_unit == "pct") "Percent of respondents" else "Number of respondents") +
      plot_theme
  })

  # ================= RQ1 =================
  rq1 <- reactive(run_rq1(filtered()))

  output$rq1_bar <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    tab <- as.data.frame(table(d$access_label))
    names(tab) <- c("access", "n")
    tab$pct <- 100 * tab$n / sum(tab$n)
    show_pct <- identical(input$rq1_unit, "pct")
    tab$value <- if (show_pct) tab$pct else tab$n
    tab$label <- if (show_pct) sprintf("%.1f%%", tab$pct) else tab$n
    fills <- setNames(pal()[c(3, 2, 1)], levels(df$access_label))
    ggplot(tab, aes(x = access, y = value, fill = access)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = label), vjust = -0.4, colour = light) +
      scale_fill_manual(values = fills, guide = "none") +
      scale_x_discrete(labels = scales::label_wrap(16)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = "Access to paid GenAI tools", subtitle = n_note(d),
           x = NULL, y = if (show_pct) "Percent of students" else "Number of students") +
      plot_theme
  })

  output$rq1_ci <- renderPlot({
    r <- rq1()
    validate(need(!is.null(r), msg_empty))
    est <- data.frame(p = r$p, lo = r$lo, hi = r$hi, label = "Students who personally pay")
    ggplot(est, aes(x = p, y = label)) +
      geom_vline(xintercept = 0.5, linetype = "dashed", colour = light) +
      geom_pointrange(aes(xmin = lo, xmax = hi), colour = pal()[1], size = 1, linewidth = 1.2) +
      annotate("text", x = 0.5, y = 1.35, label = "50% (H0)", hjust = -0.1, colour = light) +
      scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
      labs(title = wrap_txt("Estimated proportion who pay, with 95% confidence interval", 55),
           subtitle = wrap_txt(paste0(n_note(filtered()), ". If the interval includes 50%, we cannot say that more (or fewer) than half pay."), 65),
           x = "Proportion of students who personally pay", y = NULL) +
      plot_theme
  })

  output$rq1_table <- renderTable({
    r <- rq1()
    validate(need(!is.null(r), msg_empty))
    data.frame(
      Item = c("Students who pay / all students", "Sample proportion",
               "95% confidence interval", "z statistic", "p-value (H1: p > 0.5)"),
      Value = c(sprintf("%d / %d", r$x, r$n), sprintf("%.3f", r$p),
                sprintf("(%.3f, %.3f)", r$lo, r$hi), sprintf("%.3f", r$z),
                sprintf("%.3f", r$pval)))
  }, striped = TRUE, spacing = "s")

  output$rq1_text <- renderUI({
    r <- rq1(); d <- filtered()
    validate(need(!is.null(r), msg_empty))
    n_else <- sum(d$paid_access == 1); n_free <- sum(d$paid_access == 0)
    tagList(
      p(strong("What the graph shows: "),
        sprintf("%d of %d selected students (%.1f%%) personally pay for at least one GenAI subscription. The other students are split between %d who have a paid subscription that someone else pays for, and %d who only use free versions.",
                r$x, r$n, 100 * r$p, n_else, n_free)),
      p(strong("What the test says: "),
        sprintf("We use a one-sample z-test with H0: p = 0.5 and H1: p > 0.5 (z = %.2f, %s).",
                r$z, p_text(r$pval)),
        if (r$pval < 0.05)
          " A proportion this high would be unlikely if only half of students paid, so we have evidence that more than half pay."
        else
          " We do not have clear evidence that more than half of students pay. This does not prove that exactly half pay."),
      p(strong("How sure we are: "),
        sprintf("The 95%% confidence interval runs from %.1f%% to %.1f%%, so the true share who pay could be anywhere in that range.",
                100 * r$lo, 100 * r$hi),
        if (r$lo <= 0.5 && r$hi >= 0.5)
          " The range includes 50%, so we cannot tell whether the true share is above or below half."
        else if (r$lo > 0.5) " The whole range is above 50%."
        else " The whole range is below 50%."),
      div(class = "alert alert-success", strong("Our answer: "),
        if (r$pval < 0.05)
          sprintf("Yes. More than half of the selected students (%.1f%%) pay for GenAI themselves.", 100 * r$p)
        else
          sprintf("Paying is common (%.1f%% of the selected students), but we cannot say that more than half of students pay.", 100 * r$p)),
      if (r$n < 30)
        div(class = "alert alert-secondary", "Note: fewer than 30 students are selected, so this result is not very reliable.")
    )
  })

  # ================= RQ2 =================
  rq2 <- reactive(run_rq2(filtered()))
  msg_rq2 <- "This comparison needs both Computer Science and non-CS respondents. Please select Computer Science and at least one other program in the filter."

  output$rq2_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    r <- rq2()
    validate(need(!is.null(r), msg_rq2))
    g <- data.frame(
      group = c(paste0("Computer Science (n = ", r$n1, ")"),
                paste0("Non-CS (n = ", r$n2, ")")),
      p  = 100 * c(r$p1, r$p2),
      lo = 100 * c(r$ci1[1], r$ci2[1]),
      hi = 100 * c(r$ci1[2], r$ci2[2]))
    g$group <- factor(g$group, levels = g$group)
    bar_cols <- pal()[1:2]
    ggplot(g, aes(x = group, y = p, fill = group)) +
      geom_col(width = 0.55) +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, colour = light) +
      geom_text(aes(y = hi, label = sprintf("%.1f%%", p)), vjust = -0.6,
                fontface = "bold", colour = light) +
      scale_fill_manual(values = bar_cols, guide = "none") +
      scale_x_discrete(labels = scales::label_wrap(16)) +
      coord_cartesian(ylim = c(0, input$rq2_ymax)) +
      labs(title = wrap_txt("Percent who personally pay for a GenAI subscription", 50),
           subtitle = wrap_txt(paste0(n_note(d), ". Error bars show approximate 95% confidence intervals."), 60),
           x = NULL, y = "Percent who personally pay") +
      plot_theme
  })

  output$rq2_table <- renderTable({
    validate(need(nrow(filtered()) > 0, msg_empty))
    r <- rq2()
    validate(need(!is.null(r), msg_rq2))
    data.frame(
      Item = c("Computer Science: pay / total", "Non-CS: pay / total",
               "z statistic", "p-value (H1: p_CS > p_nonCS)"),
      Value = c(sprintf("%d / %d (%.1f%%)", r$x1, r$n1, 100 * r$p1),
                sprintf("%d / %d (%.1f%%)", r$x2, r$n2, 100 * r$p2),
                sprintf("%.3f", r$z), sprintf("%.3f", r$pval)))
  }, striped = TRUE, spacing = "s")

  output$rq2_text <- renderUI({
    validate(need(nrow(filtered()) > 0, msg_empty))
    r <- rq2()
    validate(need(!is.null(r), msg_rq2))
    tagList(
      p(strong("What the graph shows: "),
        sprintf("%.1f%% of Computer Science students pay (%d of %d), compared with %.1f%% of non-CS students (%d of %d). The CS percentage is %.1f points %s.",
                100 * r$p1, r$x1, r$n1, 100 * r$p2, r$x2, r$n2,
                abs(100 * (r$p1 - r$p2)), ifelse(r$p1 < r$p2, "lower", "higher"))),
      p(strong("What the test says: "),
        sprintf("We use a two-sample z-test for proportions with H0: p_CS = p_nonCS and H1: p_CS > p_nonCS (z = %.2f, %s).",
                r$z, p_text(r$pval)),
        if (r$pval < 0.05)
          " A gap this large would be unlikely if the two groups paid at the same rate, so we have evidence that CS students are more likely to pay."
        else
          " We do not have clear evidence that CS students are more likely to pay."),
      p(strong("How sure we are: "),
        sprintf("There are only %d CS students in this comparison, so each one changes the CS percentage by about %.1f points. A few different answers could change the result a lot.",
                r$n1, 100 / r$n1)),
      div(class = "alert alert-success", strong("Our answer: "),
        if (r$pval < 0.05) "Yes. CS students in our sample are more likely to pay than non-CS students."
        else if (r$p1 < r$p2) "No. We found no evidence that CS students are more likely to pay. In our sample they pay less often than non-CS students, but with so few CS students we do not read much into that."
        else "We found no clear evidence that CS students are more likely to pay."),
      if (min(r$n1, r$n2) < 10)
        div(class = "alert alert-secondary", sprintf("Note: one group has only %d students, so this result is not very reliable.",
                     min(r$n1, r$n2)))
    )
  })

  # ================= RQ3 =================
  rq3 <- reactive(run_rq3(filtered()))
  msg_rq3 <- "Not enough respondents to fit the model (we need at least 20 with complete answers, including some who pay and some who do not). Please select more programs in the filter."

  output$rq3_controls <- renderUI({
    r <- range(df[[input$rq3_var]], na.rm = TRUE)
    lo <- floor(r[1]); hi <- ceiling(r[2])
    sliderInput("rq3_ylim", "Range of the y-axis", min = lo, max = hi,
                value = c(lo, hi), step = if (hi - lo > 20) 1 else 0.5)
  })

  output$rq3_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    v <- input$rq3_var
    d$y <- d[[v]]
    d <- d[!is.na(d$y), ]
    validate(need(nrow(d) > 0, "No usable answers for this variable in the current filter."))
    yl <- input$rq3_ylim
    if (is.null(yl)) yl <- range(d$y)
    n_no  <- sum(d$pays == 0)
    n_yes <- sum(d$pays == 1)
    d$grp <- factor(ifelse(d$pays == 1,
                           paste0("Pays personally\n(n = ", n_yes, ")"),
                           paste0("Does not pay personally\n(n = ", n_no, ")")),
                    levels = c(paste0("Does not pay personally\n(n = ", n_no, ")"),
                               paste0("Pays personally\n(n = ", n_yes, ")")))
    ggplot(d, aes(x = grp, y = y, fill = grp)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA, colour = light) +
      geom_jitter(width = 0.12, height = 0, alpha = 0.6, size = 2, colour = light) +
      scale_fill_manual(values = c(pal()[2], pal()[1]), guide = "none") +
      coord_cartesian(ylim = yl) +
      labs(title = wrap_txt(paste(num_labels[[v]], "by paying status"), 26),
           subtitle = wrap_txt(paste0(n_note(d), ". Each dot is one student."), 30),
           x = NULL, y = num_labels[[v]]) +
      plot_theme
  })

  output$rq3_forest <- renderPlot({
    validate(need(nrow(filtered()) > 0, msg_empty))
    r <- rq3()
    validate(need(!is.null(r), msg_rq3))
    tab <- r$table
    tab$term <- factor(tab$term, levels = rev(tab$term))
    ggplot(tab, aes(x = or, y = term)) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = light) +
      geom_pointrange(aes(xmin = lo, xmax = hi), colour = pal()[1], size = 0.9, linewidth = 1.1) +
      scale_x_log10() +
      scale_y_discrete(labels = scales::label_wrap(22)) +
      labs(title = wrap_txt("Odds ratios from the logistic regression", 50),
           subtitle = wrap_txt(paste0("n = ", r$n, " respondents with complete answers. If a line crosses 1, there is no clear association."), 60),
           x = "Odds ratio of paying (log scale; 1 = no association)", y = NULL) +
      plot_theme
  })

  output$rq3_table <- renderTable({
    validate(need(nrow(filtered()) > 0, msg_empty))
    r <- rq3()
    validate(need(!is.null(r), msg_rq3))
    t <- r$table
    data.frame(
      Predictor = t$term,
      `Odds ratio` = sprintf("%.2f", t$or),
      `95% CI` = sprintf("(%.2f, %.2f)", t$lo, t$hi),
      z = sprintf("%.2f", t$z),
      `p-value` = ifelse(t$pval < 0.001, "< 0.001", sprintf("%.3f", t$pval)),
      check.names = FALSE)
  }, striped = TRUE, spacing = "s")

  output$rq3_text <- renderUI({
    validate(need(nrow(filtered()) > 0, msg_empty))
    r <- rq3()
    validate(need(!is.null(r), msg_rq3))
    t <- r$table
    sig <- t$term[t$pval < 0.05]
    d <- filtered()
    d <- d[complete.cases(d[, c("ai_days_week", "ai_usefulness", "spending_money")]), ]
    med <- function(v, g) median(d[[v]][d$pays == g])
    tagList(
      p(strong("What the model shows: "),
        sprintf("We used %d students with complete answers (%d of them pay). The odds ratio tells us how the odds of paying change when a predictor goes up by one unit:",
                r$n, r$events)),
      tags$ul(tags$li(or_sentence(t[1, ])), tags$li(or_sentence(t[2, ])), tags$li(or_sentence(t[3, ]))),
      if (r$events > 0 && r$events < r$n)
        p(strong("A quick look at the two groups: "),
          sprintf("Students who pay use GenAI a median of %.1f days a week, rate its usefulness %.1f out of 10, and have $%.0f of spending money. For students who do not pay, the medians are %.1f days, %.1f out of 10 and $%.0f.",
                  med("ai_days_week", 1), med("ai_usefulness", 1), med("spending_money", 1),
                  med("ai_days_week", 0), med("ai_usefulness", 0), med("spending_money", 0))),
      p(strong("What the test says: "),
        if (length(sig) == 0)
          "None of the three predictors is statistically significant (all p-values are above 0.05). We do not have clear evidence that any of them is related to paying."
        else
          paste0("These predictors are statistically significant: ", paste(sig, collapse = "; "),
                 ". Their p-values are below 0.05, so a relationship this strong would be unlikely by chance alone. The other predictors are not significant.")),
      div(class = "alert alert-success", strong("Our answer: "),
        if (length(sig) == 0)
          "No. In our sample, how often students use GenAI, how useful they find it, and how much spending money they have do not clearly predict whether they pay."
        else
          "Partly. Some of these variables are related to paying in our sample. This shows an association, not a cause."),
      if (min(r$events, r$n - r$events) < 10)
        div(class = "alert alert-secondary", "Note: fewer than 10 students are in one of the two groups, so the model is not very reliable.")
    )
  })

  # ================= Data table =================
  readable_rows <- function(d) {
    data.frame(
      Respondent = d$respondent,
      Program = d$program_label,
      `Days per week using GenAI` = d$ai_days_week,
      `Access to paid GenAI tools` = d$access_label,
      `Perceived usefulness (1-10)` = d$ai_usefulness,
      `Monthly spending money (CAD)` = d$spending_money,
      `Max. willingness to pay (CAD)` = d$wtp_monthly,
      check.names = FALSE)
  }
  output$dl_data <- downloadHandler(
    filename = function() "genai_survey_selected_rows.csv",
    content = function(file) write.csv(readable_rows(filtered()), file, row.names = FALSE)
  )
  output$data_table <- renderDT({
    d <- filtered()
    validate(need(nrow(d) > 0, msg_empty))
    show <- readable_rows(d)
    datatable(show, rownames = FALSE, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
