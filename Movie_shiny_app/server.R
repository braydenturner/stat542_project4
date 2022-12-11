## server.R

# load functions
# source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

top10 = read.csv("top10.csv", sep = " ")

# define functions
get_user_ratings <- function(value_list) {
  value_list = value_list[names(value_list) != "btn"]
  
  # print(value_list)
  value_list = value_list[! value_list %in% c("NULL", "rm", "FALSE", "")]
  
  ################################################################################
  
  a = sapply(strsplit(names(value_list), "_"), 
             function(x) ifelse(length(x) > 1, x[[2]], NA))
  dat <- data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                     function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))

  n.item = ccount
  new.ratings = rep(NA, n.item)
  if (nrow(dat) < 1){
    return(0)
  }
  for (i in 1:nrow(dat)) {
    if(dat[i]$Rating == FALSE){
      next
    }
    new.ratings[as.numeric(dat[i]$MovieID)] = as.numeric(dat[i]$Rating)
  }

  new.user = matrix(new.ratings, 
                    nrow=1, ncol=n.item,
                    dimnames = list(
                      user=paste('feng'),
                      item=movieIDs
                    ))
  new.Rmat = as(new.user, 'realRatingMatrix')
  recom1 = predict(rec_UBCF, new.Rmat)

  # recom1@items
  # recom1@ratings
  # 
  # recom2 = predict(rec_UBCF, new.Rmat, type = 'ratings')
  # order(as(recom2, "matrix"), decreasing = TRUE)[1:10]
  # as(recom1, "matrix")[order(as(recom1, "matrix"), decreasing = TRUE)[1:10]]
  return(recom1@items[["0"]])
}

# read in data

myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

i = ratings$UserID
j = ratings$MovieID
x = ratings$Rating
rcount = max(i)
ccount = max(j)

tmp = data.frame(i, j, x, stringsAsFactors = TRUE)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
Rmat = Rmat[1:500, ]
rownames(Rmat) = 1:500
colnames(Rmat) = 1:ccount
movieIDs = 1:ccount


Rmat = as(Rmat, 'realRatingMatrix' )
rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score',
                                        method = 'Cosine',
                                        nn = 25))




shinyServer(function(input, output, session) {
  
  d <- reactive({
    dist <- switch(input$mode,
                   rm = rm_selected,
                   gs = gs_selected
                   )
    
    dist(input$n)
  })
  
  # show the books to be rated
  output$ratings <- renderUI(
    if(input$mode == "rm"){
    num_rows <- 20
    row_size <- 6 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:row_size, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * row_size + j], style = "max-height:150")),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * row_size + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * row_size + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  } else {
    selectInput(
      "genre",
      "Select your desired genre:",
      c(
        "Action" = "Action", 
        "Adventure" = "Adventure", 
        "Animation" = "Animation", 
        "Children's" = "Children's", 
        "Comedy" = "Comedy", 
        "Crime" = "Crime",
        "Documentary" = "Documentary", 
        "Drama" = "Drama", 
        "Fantasy" = "Fantasy",
        "Film-Noir" = "Film-Noir", 
        "Horror" = "Horror", 
        "Musical" = "Musical", 
        "Mystery" = "Mystery", 
        "Romance" = "Romance", 
        "Sci-Fi" = "Sci-Fi", 
        "Thriller" = "Thriller", 
        "War" = "War", 
        "Western" = "Western"
      ),
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  
  
  # Calculate recommendations when the submission button is clicked
  df <- eventReactive(input$btn, 
                      
    {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        if(input$mode == "rm"){
          # get the user's rating data
          value_list <- reactiveValuesToList(input)

          user_results <- get_user_ratings(value_list)
          if(length(user_results) < 2){
            user_predicted_ids = movies$MovieID[1:10]
          } else {
            user_predicted_ids = user_results
          }
          recom_results <- data.table(Rank = 1:10, 
                                      MovieID = user_predicted_ids)
          
          # print(user_predicted_ids)
        } else {
          ind = which(top10$Genre == input$genre)
          recom_results <- data.table(
            Rank = 1:10,
            top10[ind, c("MovieID", "Title")]
            )
        }
        
        return(recom_results)   
    }) # still busy
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    row_size <- 5
    recom_result <- df()
    inds = rep(0, 10)
    for (i in 1:10) {
      inds[i] = which(recom_result$MovieID[i] == movies$MovieID)
    }
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:row_size, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * row_size + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[inds[(i - 1) * row_size + j]], height = 150))
            ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[inds[(i - 1) * row_size + j]])
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
