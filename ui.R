library(shiny)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  HTML("<center><h1>Next Word Prediction App</h1></center>"),
  HTML("<center><h4>Prediction application of TNMayer for the Johns Hopkins University Data Science Capstone Project on Coursera</h4></center>"),
  HTML("<br><br>"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      # textInput("a",""),
      textInput("text", label = h3("Input"), value = "happy birthday"),
      #submitButton('Submit'),
      helpText("Type in a sentence into the input field above, hit enter 
      (or press the button below), and the results will be displayed in the right hand panel."),
      submitButton("Predict next word"),
      HTML("<br><br>")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Results", 
                 br(),
                 h3('You entered:'),
                 verbatimTextOutput('inputValue'),
                 h3('Cleaned string:'),
                 verbatimTextOutput('cleaned'),
                 h3('Predictions:'),
                 verbatimTextOutput('prediction')
                 ),
        tabPanel("Examples",
                 HTML("<br><h4><span style='font-weight:bold'>The word in the [ ] is the predicted next word with the highest probability.<br>
                      Go to the <u><i>Results</i></u> tab for the predicted word<br>
                      Sample predictions by the app are below:<br><br>
                      <font color='red'>would mean the</font> <font color='blue'>[world]</font><br><br>
                      <font color='red'>happy</font> <font color='blue'>[birthday]</font><br><br>
                      <font color='red'>thank you very</font> <font color='blue'>[much]</font><br><br>
                      <font color='red'>how are</font> <font color='blue'>[you]</font><br><br>
                      <font color='red'>love you</font> <font color='blue'>[too]</font></h4></span>")
                 ),
        tabPanel("Model Development & Algorithm", 
                 HTML("<br>"),
                 includeMarkdown("development.md")
                 ),
        tabPanel("Further Improvements", 
                 HTML("<br><span style='font-weight: bold'>
                       <ul>
                        <li>Extend the model to 4- and 5-grams,</li>
                        <li>Preprocess and implement the whole given language corpus in the model,</li>
                        <li>Improve the prediction time,</li>
                        <li>Cross-validate and further improve the prediction performance by
                        comparing it with other natural language processing 
                        (<a href='https://en.wikipedia.org/wiki/Natural_language_processing' target='_blank'>NLP</a>) 
                        approaches like <a href='https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&uact=8&ved=0ahUKEwj7mKSxmYXNAhXFVRQKHQlUBkIQFggkMAE&url=http%3A%2F%2Fwww.cs.cornell.edu%2Fcourses%2Fcs4740%2F2014sp%2Flectures%2Fsmoothing%2Bbackoff.pdf&usg=AFQjCNF3zWHzX4iPZ-X1FdU9XW9d82SuMw' target='_blank'>Katz back-off</a>, 
                        <a href='https://en.wikipedia.org/wiki/Good%E2%80%93Turing_frequency_estimation' target='_blank'>Good-Turing smoothing</a> or just the 
                        <a href='https://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwj94IXdmIXNAhXBwBQKHQjWAssQFggcMAA&url=http%3A%2F%2Fwww.aclweb.org%2Fanthology%2FD07-1090.pdf&usg=AFQjCNFbsZ_e4Nh_Tw8O-7QWHtwUVUP9Cg&bvm=bv.123325700,d.d24' target='_blank'>stupid back-off</a>,</li>
                        <li>Apply the algorithm to other languages like German or French,</li>
                        <li>Implement the user input into the model, thus develop a dynamic model
                        out of a static one.</li>
                      </ul>
                      </span>
                      ")
                 )
      )
    )
  )
  ))