#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(gapminder)
library(ggrepel)
library(cowplot)
library(stringr)
set.seed(3000)
df <- 
  gapminder %>% 
  filter(year == 2002) %>% 
  filter(continent == "Europe") %>% 
  sample_n(6) %>% 
  mutate(diff_from_mean  = lifeExp - mean(lifeExp)) %>% 
  mutate(square = (diff_from_mean)^2) %>% 
  mutate(gdpPercap_log10 = log10(gdpPercap)) %>% 
  mutate(gdpPercap_1000 = gdpPercap/1000) %>% 
  arrange(lifeExp)

g <- ggplot(df) +
  aes(y = lifeExp, 
      x = 0, 
      label = country) +
  geom_point()  +
  theme_bw(base_size = 15) +
  coord_equal() + 
  labs(x = "", 
       title = "", #"Life expectancy for European countries in 2007", 
       subtitle = "", #"Vis: Gina Reynolds | Source: Gapminder",
       y = "Years") + 
  geom_hline(yintercept = mean(df$lifeExp), lty= "dashed") +
  geom_text_repel()


total_sum_squares <- g + 
  aes(ymin = mean(df$lifeExp),
      ymax = lifeExp,
      xmin = 0,
      xmax = abs(lifeExp - mean(df$lifeExp)),
      fill = factor(lifeExp)) +
  coord_equal(ylim = c(70, 82), xlim = c(0,7)) +
  geom_rect(data = df, 
            alpha = .2) +
  guides(fill = FALSE, col = FALSE) +
  labs(subtitle = "TSS")


variance_w_x <- total_sum_squares + 
  aes(xmin = df$gdpPercap_1000, 
      xmax = df$gdpPercap_1000 + 
        (lifeExp - mean(df$lifeExp)),
      x = df$gdpPercap_1000) +
  geom_point() + 
  geom_vline(xintercept = mean(df$gdpPercap_1000), 
             lty = "dashed") + ylim(c(70,90))


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   headerPanel("Minimize sum of squared residuals ('Manual OLS'):"),
   titlePanel("A Shiny app from Gina Reynolds using + ggplot2 + gapminder + a few more"),
   
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
      # sidebarPanel(
        
         sliderInput("b",
                     "Slope:",
                     min = -1,
                     max = 1,
                     value = 0, step = .001),
         sliderInput("a",
                     "Intercept:",
                     min = 60,
                     max = 80,
                     value = mean(df$lifeExp),
                     step = .001),
      # ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("total_sum_squares"),
         plotOutput("residual_sum_squares")
      )
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      output$total_sum_squares <- renderPlot({
      total_sum_squares
        df$expected <- df$gdpPercap_1000*input$b + 
          input$a
        df$residual <- df$lifeExp - df$expected
        
        lm <- total_sum_squares + 
          aes(x = gdpPercap_1000,
              xend = gdpPercap_1000, yend = df$expected,
              ymin = df$expected,
              xmin = gdpPercap_1000, 
              xmax = gdpPercap_1000 + df$residual
          ) +
          geom_point() + 
          geom_segment(aes(col = factor(lifeExp)), size = 2, clipping = "off") +
          geom_abline(slope = input$b, intercept = input$a, col = "red") +
          coord_equal(ylim = c(70,82), xlim = c(0,45)) +
          labs(subtitle = paste("Scatter plot and RSS:" , round(sum(df$residual^2)),3),
            x = "Per Capita GDP in Dollars (thousands)")
   
        plot_grid(total_sum_squares, lm, rel_widths = c(1,4.35))
        
        })
   
   

   
   output$residual_sum_squares <- renderPlot({
     
     df$expected <- 
       df$gdpPercap_1000*input$b + input$a
     df$residual <- df$lifeExp - df$expected
     
     TSS <- ggplot(df) + 
       aes(y = df$square,
           x = forcats::fct_inorder(factor(str_wrap(country, 12))),
           fill = factor(lifeExp)) +
       geom_col(alpha = .3)  +
       coord_cartesian(ylim = c(0,40)) + 
       theme_bw(base_size = 15) +
       guides(fill = FALSE) + 
       labs(x = "",
            y = "Squared difference from mean", 
            title = "Squared difference from mean", 
            subtitle = paste("Sum =", round(sum(df$square)),3))

     RSS <- TSS + 
       aes(y = df$residual^2) +
       labs(y = "Squared residual",
            title = "Residuals Squared", 
            subtitle = paste("Sum =", round(sum(df$residual^2)),3))
     
     plot_grid(TSS, RSS)
     
   })
   
}

# Run the application 
shinyApp(ui = ui, 
         server = server)

