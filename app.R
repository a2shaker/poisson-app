
# Install required packages if you haven't already:
# install.packages(c("shiny", "ggplot2", "patchwork", "dplyr"))

library(shiny)
library(ggplot2)
library(patchwork)
library(dplyr)

# Define User Interface (UI)
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty"), # Clean, modern theme
  
  titlePanel("Interactive Poisson Distribution Visualisation"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Control Panel"),
      p("Adjust the parameters below to see how the mathematical properties change live."),
      
      # Slider for Lambda
      sliderInput("lambda",
                  label = "Mean Rate (λ):",
                  min = 1,
                  max = 50,
                  value = 4,
                  step = 0.5),
      
      # Slider for Simulation Size
      sliderInput("n_sim",
                  label = "Simulated Observations:",
                  min = 100,
                  max = 5000,
                  value = 1000,
                  step = 100)
    ),
    
    mainPanel(
      width = 9,
      # Output the combined grid plot
      plotOutput("poissonPlot", height = "700px")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  output$poissonPlot <- renderPlot({
    set.seed(42) # Seed for consistent simulation behavior
    
    # Dynamic variables from inputs
    lam <- input$lambda
    n_sim <- input$n_sim
    
    # Dynamically scale x-axis limits based on lambda to keep plots focused
    x_min <- max(0, floor(lam - 3.5 * sqrt(lam)))
    x_max <- ceiling(lam + 4 * sqrt(lam))
    x_vals <- x_min:x_max
    
    # -------------------------------------------------------------------------
    # 1. SIMULATED DATA & PMF
    # -------------------------------------------------------------------------
    pmf_data <- data.frame(
      x = x_vals,
      prob = dpois(x_vals, lambda = lam)
    )
    
    sim_data <- data.frame(x = rpois(n_sim, lambda = lam))
    
    p1 <- ggplot() +
      geom_histogram(data = sim_data, aes(x = x, y = after_stat(count/sum(count)), fill = "Simulated"), 
                     binwidth = 0.5, color = "black", alpha = 0.3) +
      geom_linerange(data = pmf_data, aes(x = x, ymin = 0, ymax = prob, color = "Theoretical (PMF)"), size = 1) +
      geom_point(data = pmf_data, aes(x = x, y = prob, color = "Theoretical (PMF)"), size = 3) +
      scale_fill_manual(name = "", values = c("Simulated" = "gray50")) +
      scale_color_manual(name = "", values = c("Theoretical (PMF)" = "#2c3e50")) +
      xlim(x_min - 0.5, x_max + 0.5) +
      labs(title = "PMF & Simulated Data", subtitle = paste("Empirical vs Theoretical frequencies"), x = "Number of Events (x)", y = "Probability") +
      theme_minimal() +
      theme(legend.position = "top")
    
    # -------------------------------------------------------------------------
    # 2. CDF (Cumulative Distribution Function)
    # -------------------------------------------------------------------------
    cdf_data <- data.frame(
      x = x_vals,
      cdf = ppois(x_vals, lambda = lam)
    )
    
    p2 <- ggplot(cdf_data, aes(x = x, y = cdf)) +
      geom_step(color = "#e74c3c", size = 1.2, direction = "vh") +
      geom_point(color = "#e74c3c", size = 2.5) +
      xlim(x_min, x_max) +
      labs(title = "CDF (Cumulative Curve)", subtitle = "Step function of cumulative P(X ≤ x)", x = "x", y = "F(x)") +
      theme_minimal()
    
    # -------------------------------------------------------------------------
    # 3. QUANTILE FUNCTION
    # -------------------------------------------------------------------------
    p_vals <- seq(0.001, 0.999, by = 0.005)
    quantile_data <- data.frame(
      p = p_vals,
      q = qpois(p_vals, lambda = lam)
    )
    
    p3 <- ggplot(quantile_data, aes(x = p, y = q)) +
      geom_step(color = "#9b59b6", size = 1.2) +
      ylim(x_min, x_max) +
      labs(title = "Quantile Function", subtitle = "Inverse CDF: Percentile p to Quantile x", x = "Probability (p)", y = "Quantile (x)") +
      theme_minimal()
    
    # -------------------------------------------------------------------------
    # 4. NORMAL APPROXIMATION
    # -------------------------------------------------------------------------
    approx_data <- data.frame(
      x = x_vals,
      poisson_pmf = dpois(x_vals, lambda = lam),
      normal_pdf = dnorm(x_vals, mean = lam, sd = sqrt(lam))
    )
    
    p4 <- ggplot(approx_data, aes(x = x)) +
      geom_bar(aes(y = poisson_pmf, fill = "Poisson PMF"), stat = "identity", alpha = 0.6, width = 0.6) +
      geom_line(aes(y = normal_pdf, color = "Normal Approx"), size = 1.2) +
      scale_fill_manual(name = "", values = c("Poisson PMF" = "#3498db")) +
      scale_color_manual(name = "", values = c("Normal Approx" = "#e67e22")) +
      xlim(x_min - 0.5, x_max + 0.5) +
      labs(title = "Normal Approximation Check", subtitle = paste("Normal(μ = λ, σ = √λ) overlay"), x = "x", y = "Density / Probability") +
      theme_minimal() +
      theme(legend.position = "top")
    
    # Combine using patchwork layout
    dashboard <- (p1 + p2) / (p3 + p4) + 
      plot_annotation(
        title = paste("Live Analysis for λ =", lam),
        theme = theme(plot_title = element_text(size = 18, face = "bold", hjust = 0.5))
      )
    
    print(dashboard)
  })
}

# Run the Application
shinyApp(ui = ui, server = server)