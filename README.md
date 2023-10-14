Central Limit Theorem Visualizations

This Rshiny WebApp is made to visualize the central limit theorem for different distributions through simulations. To access this shiny app, go to [This link](https://9bw6-devstat.shinyapps.io/CLT_viz/) and have fun! 

The central limit theorem says that whenever a sample of sufficiently large size is drawn from any population having a finite mean and a variance, the sample mean will approximately follow a normal distribution. But is this true for any distribution with a finite mean and variance? Let's find out with this shiny app! We can also visualize the weak law of large numbers, the convergence of sample mean to population mean in probability!

Run this app in your `Rstudio` using the following lines of commands: 

    rm(list=ls()
    require(shiny)
    #CLT
    #Auto simulation #If want to run the auto simulation app
    shiny::runGitHub(repo = "Central_Limit_Theorem_Visualizations",username = "K-Deb",subdir = "Auto simulation/",ref="main")
    #Manual simulation #If want to run the manual simulation app
    shiny::runGitHub(repo = "Central_Limit_Theorem_Visualizations",username = "K-Deb",subdir = "Manual simulation/",ref="main")
