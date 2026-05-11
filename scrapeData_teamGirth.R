## scrapping team rosters (instead of just recruits)

#https://247sports.com/college/arizona/Team/Arizona-Wildcats-Football-146/Roster/

library(rvest)

sport <- "football"
call <- paste0("https://247sports.com/college/arizona/Team/Arizona-Wildcats-Football-146/Roster/")
page <- read_html(call)

scores <- page %>%
  html_nodes(".table-heading , td") %>%
  html_text(trim = TRUE)

#View(scores)
scores

# get rid of "Name" row
scores <- scores[2:length(scores)]

playeNamesEnd <- grep("Jersey", scores)
lastHeader <- grep("Rating", scores)
headers <- 7

i=1
while(i <= playeNamesEnd-1) {
  
  if (i == 1) {
    ## set variables to save
    name <- scores[i]
    jersey <- scores[i+lastHeader]
    pos <- scores[i+1+lastHeader]
    height <- scores[i+2+lastHeader]
    weight <- scores[i+3+lastHeader]
    year <- scores[i+4+lastHeader]
    age <- scores[i+5+lastHeader]
    hs <- scores[i+6+lastHeader]
    rating <- scores[i+7+lastHeader]
    
    store_data <- data.frame(
      Name = name,
      Jersey = jersey,
      Pos = pos,
      Height = height,
      Weight = weight,
      Year = year,
      Age = age,
      HighSchool = hs,
      Rating = rating
      
    )
    print(i)
  } else{
    ## set variables to save
    name <- scores[i]
    jersey <- scores[i+lastHeader+headers*(i-1)]
    pos <- scores[i+1+lastHeader+headers*(i-1)]
    height <- scores[i+2+lastHeader+headers*(i-1)]
    weight <- scores[i+3+lastHeader+headers*(i-1)]
    year <- scores[i+4+lastHeader+headers*(i-1)]
    age <- scores[i+5+lastHeader+headers*(i-1)]
    hs <- scores[i+6+lastHeader+headers*(i-1)]
    rating <- scores[i+7+lastHeader+headers*(i-1)]
    
    ## bind them
    store_data <- rbind(store_data, data.frame(
      Name = name,
      Jersey = jersey,
      Pos = pos,
      Height = height,
      Weight = weight,
      Year = year,
      Age = age,
      HighSchool = hs,
      Rating = rating
    ))
    print(i)
  }

  
  i=i+1
}

store_data









