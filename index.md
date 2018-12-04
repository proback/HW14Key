You should submit a knitted pdf file on Moodle, but be sure to show all
of your R code, in addition to your output, plots, and written
responses.

Factors, Functions, and Vectors
-------------------------------

1.  Given the output below, write out the elements of the vector `y`.

<!-- -->

    #> sort(y)
    #[1] -2 -2 -1  0  3  4  4  7
    #
    #> order(y)
    #[1] 1 8 4 5 6 2 7 3

    y <- c(-2, 4, 7, -1, 0, 3, 4, -2)
    sort(y)

    ## [1] -2 -2 -1  0  3  4  4  7

    order(y)

    ## [1] 1 8 4 5 6 2 7 3

1.  Explain what the following function does and demonstrate by running
    `foo1(x)` with a few appropriately chosen vectors `x`.

<!-- -->

    foo1 <- function(x) {
      diff <- x[-1] - x[1:(length(x) - 1)]
      length(which(diff < 0))
    }
    foo1(y)

    ## [1] 2

foo1 finds the difference between successive values of `x` and counts
how many of those differences are negative (e.g. drop from the 5th value
to the 6th)

1.  Write a function called `greet` using `if`, `else if`, and `else` to
    print out "good morning" if it's before 12 PM, "good afternoon" if
    it's between 12 PM and 5 PM, and "good evening" if it's after 5 PM.
    Your function should work if you input a time like:
    `greet(time = "2018-05-03 17:38:01 CDT")` or if you input the
    current time with `greet(time = Sys.time())`.

<!-- -->

    hours <- parse_number(str_sub(Sys.time(), 12, 13))
    greet <- function(time = Sys.time()) {
      hour <- parse_number(str_sub(time, 12, 13))
      if (hour < 12) {
        print("good morning")
      } else if (hour < 17) {
        print("good afternoon")
      } else {
        print("good evening")
      }
    }
    greet()

    ## [1] "good morning"

    greet(time = "2018-05-03 17:38:01 CDT")

    ## [1] "good evening"

Airbnb Data
-----------

1.  `airbnbData.csv` contains data scraped on 2022 airbnb units in the
    Boston area (65 variables). The following code produces a list
    containing some airbnb data. Show how to pull these data elements
    from `airbnb_list` (that is, your code MUST use `airbnb_list`):

-   the last 10 titles of listings after sorting in alphabetical order
-   the median listing price
-   the latitude and longitude of listing \#3774132
-   the number of listings with a host named Paul
-   print out the sentence "There are XX listings with wheelchair
    access." where XX is automatically filled in

<!-- -->

    airbnb.df <- read.csv("datasets/airbnbData.csv")
    airbnb.df <- as.tibble(airbnb.df) 

    titles <- airbnb.df$Title
    eda_Price <- airbnb.df %>%
        summarise(mean = mean(Price, na.rm = TRUE),
                  median = median(Price, na.rm = TRUE),
                  sd = sd(Price, na.rm = TRUE),
                  iqr = IQR(Price, na.rm = TRUE),
                  n = n())
    lat_long <- airbnb.df %>% select(ListingID, Lat, Long)
    host_names <- airbnb.df %>% select(HostName) %>% distinct() %>% pull(HostName)
    wheelchair <- sum(airbnb.df$A_Wheelchair)
    airbnb_list <- list(titles, eda_Price, lat_long, host_names, wheelchair)

    sort(airbnb_list[[1]])[2013:2022]

    ##  [1] Wonderful charming studio for you! 
    ##  [2] Wonderful room w/ Private Bath     
    ##  [3] Your One Bedroom Home In Cambridge 
    ##  [4] Your Own 800 Sq ft in Modern Duplex
    ##  [5] Your Own Floor /Entrance and Treats
    ##  [6] Your pad - 10 Min to Boston center 
    ##  [7] Your room - 10 Min to Boston center
    ##  [8] Your room next to Cambridge& Boston
    ##  [9] Your Room right in Central Square  
    ## [10] Zen by Design-Central/Harvard/Kenda
    ## 1995 Levels:   Sunlit&Beautiful n Boston's J.P. ...

    airbnb_list[[2]] %>% select(median)

    ## # A tibble: 1 x 1
    ##   median
    ##    <dbl>
    ## 1    110

    airbnb_list[[3]] %>% filter(ListingID == 3774132)

    ## # A tibble: 1 x 3
    ##   ListingID   Lat  Long
    ##       <int> <dbl> <dbl>
    ## 1   3774132  42.3 -71.1

    sum(str_detect(airbnb_list[[4]], "Paul"))

    ## [1] 3

    paste0("There are ", airbnb_list[[5]], " listings with wheelchair access.")

    ## [1] "There are 92 listings with wheelchair access."

1.  The code below shows a function which returns a set of summary
    statistics for a single variable. Note a couple of strange elements
    (e.g. `enquo` and `!!var`) that were needed to get around the way
    tibbles and dplyr handle variable names (for way more detail than
    you probably want on this issue, see
    <https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html>).

<!-- -->

1.  Create a new function called `eda2stats` which produces the same set
    of summary statistics by group. Show your function works by running
    `eda2stats(airbnb.df, Price, S_PropType)`.

2.  Run `eda2stats` again after forming a new Property Type variable
    with only 4 levels: Apartment, House, Bed & Breakfast, and Other.

<!-- -->

    # First remove 5 units with prices over 1000
    airbnb.df <- airbnb.df %>%
      filter(Price < 1000)

    # Find summary stats on a single variable
    eda1stats <- function(df, var) {
      var <- enquo(var)
      df %>%
        summarise(mean = mean(!!var, na.rm = TRUE),
                  median = median(!!var, na.rm = TRUE),
                  sd = sd(!!var, na.rm = TRUE),
                  iqr = IQR(!!var, na.rm = TRUE),
                  n = n())
    }
    eda1stats(airbnb.df, Price)

    ## # A tibble: 1 x 5
    ##    mean median    sd   iqr     n
    ##   <dbl>  <int> <dbl> <dbl> <int>
    ## 1  138.    109  93.0   100  2017

    # Find summary stats by a grouping variable - part (a)
    eda2stats <- function(df, var, groups) {
      var <- enquo(var)
      groups <- enquo(groups)
      df %>%
        group_by(!!groups) %>%
        summarise(mean = mean(!!var, na.rm = TRUE),
                  median = median(!!var, na.rm = TRUE),
                  sd = sd(!!var, na.rm = TRUE),
                  iqr = IQR(!!var, na.rm = TRUE),
                  n = n())
    }
    eda2stats(airbnb.df, Price, S_PropType)

    ## # A tibble: 9 x 6
    ##   S_PropType       mean median     sd   iqr     n
    ##   <fct>           <dbl>  <dbl>  <dbl> <dbl> <int>
    ## 1 /s/Boston--MA   115    115    NA      0       1
    ## 2 Apartment       143.   120    84.7  105    1446
    ## 3 Bed & Breakfast 110.   105    37.9   45      41
    ## 4 Cabin            84.7   85     4.51   4.5     3
    ## 5 Dorm             60     60    NA      0       1
    ## 6 House           128.    85   115.    60     508
    ## 7 Loft            177.   125   116.   118.     11
    ## 8 Not Found        92.5   92.5  17.7   12.5     2
    ## 9 Other            83.8   97.5  56.2   71.2     4

    # Create fewer levels of the grouping variable - part (b)
    airbnb.df <- airbnb.df %>%
      mutate(S_PropType2 = fct_lump(S_PropType, n = 3))
    eda2stats(airbnb.df, Price, S_PropType2)

    ## # A tibble: 4 x 6
    ##   S_PropType2      mean median    sd   iqr     n
    ##   <fct>           <dbl>  <dbl> <dbl> <dbl> <int>
    ## 1 Apartment        143.   120   84.7 105    1446
    ## 2 Bed & Breakfast  110.   105   37.9  45      41
    ## 3 House            128.    85  115.   60     508
    ## 4 Other            132.   102.  95.5  47.5    22

1.  Use `stringr` functions to extract at least one feature from
    `AboutListing` and create a plot to determine if that feature is
    related to `Price`. A couple of examples are illustrated below.

<!-- -->

    # Analyze features of AboutListing description
    foo1 <- airbnb.df$AboutListing[5]
    str_length(foo1)

    ## [1] 249

    str_count(foo1, "!")

    ## [1] 2

    str_detect(foo1, "[Ww]alk")

    ## [1] TRUE

    str_length(airbnb.df$AboutListing)[1:50]   # missings seems to have length 7

    ##  [1]   7 250   7   7 249 250 551   7 242 244 193   7   7 238 250   7 143
    ## [18]   7 103 244   7 277 250 250   7 216 246   7 246   7 249 189   7 237
    ## [35] 105  31  45 245   7 148 120 250 260 235 221 235 237 187 222   7

    airbnb.df <- airbnb.df %>%
      mutate(NumExclaim = str_count(AboutListing, "!"),
             PctExclaim = ifelse(str_length(AboutListing) == 7, NA, 
               str_count(AboutListing, "!") / str_length(AboutListing)),
             Walk = str_detect(AboutListing, "[Ww]alk"))
    airbnb.df %>% group_by(Walk) %>% tally()

    ## # A tibble: 2 x 2
    ##   Walk      n
    ##   <lgl> <int>
    ## 1 FALSE  1476
    ## 2 TRUE    541

    airbnb.df %>% group_by(NumExclaim) %>% tally()

    ## # A tibble: 7 x 2
    ##   NumExclaim     n
    ##        <int> <int>
    ## 1          0  1603
    ## 2          1   314
    ## 3          2    76
    ## 4          3    13
    ## 5          4     6
    ## 6          5     4
    ## 7         11     1

    with(airbnb.df, summary(PctExclaim))

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.0000  0.0000  0.0021  0.0040  0.1250     451

    ggplot(airbnb.df, aes(x = Walk, y = Price)) +
      geom_boxplot() + coord_flip()

![](index_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    ggplot(airbnb.df, aes(x = NumExclaim, y = Price)) +
      geom_point(size = 0.25) + 
      geom_smooth(method = "lm", colour = "red")

![](index_files/figure-markdown_strict/unnamed-chunk-7-2.png)

    ggplot(airbnb.df, aes(x = PctExclaim, y = Price)) +
      geom_point(size = 0.25) + 
      geom_smooth(method = "loess", colour = "red")

![](index_files/figure-markdown_strict/unnamed-chunk-7-3.png)

Extra Credit
============

1.  Create a plot that uses a map of Boston as its backdrop and
    illustrates some trends in the data (assuming the packages ggmap and
    leaflet are loaded on the R server and ggmap is actually working). A
    few examples are shown below.

<!-- -->

    ## Superimpose points on street map of Boston, then size and color 
    ##   points in meaningful ways
    qmplot(Long, Lat, data = airbnb.df, 
           colour = I('red'), size = I(1), darken = .1)  # works

![](index_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    bbox <- c(left = min(airbnb.df$Long), bottom = min(airbnb.df$Lat), 
              right = max(airbnb.df$Long), top = max(airbnb.df$Lat))
    bostonmap <- get_stamenmap(bbox, zoom = 12, maptype = "toner-lite")
    ggmap(bostonmap)

![](index_files/figure-markdown_strict/unnamed-chunk-8-2.png)

    bostonmap2 <- get_stamenmap(bbox, zoom = 12, maptype = "terrain")
    ggmap(bostonmap2)

![](index_files/figure-markdown_strict/unnamed-chunk-8-3.png)

    ggmap(bostonmap) +
      geom_point(data = airbnb.df, aes(x = Long, y = Lat, color = S_PropType,
                                       size = parse_number(S_Accomodates))) +
      scale_size_continuous(range = c(.25, 2.5))

![](index_files/figure-markdown_strict/unnamed-chunk-8-4.png)

    # Density plot of airbnb units
    ggmap(bostonmap, legend = "bottomleft", extent = "device") + 
      stat_density2d(aes(x = Long, y = Lat, fill = ..level..), alpha = .5,
        data = airbnb.df, geom = "polygon")

![](index_files/figure-markdown_strict/unnamed-chunk-8-5.png)

    ggmap(bostonmap) + 
      stat_density2d(aes(x = Long, y = Lat, fill = ..level..), alpha = .5,
        data = airbnb.df, geom = "polygon") +
      scale_fill_gradient(low = "black", high= "red") +
      facet_wrap(~ S_PropType2)

![](index_files/figure-markdown_strict/unnamed-chunk-8-6.png)

    # Interactive map with leaflet
    #   addTiles()   Add background map
    #   setView()    Set where the map should originally zoom to
    leaflet() %>%
        addTiles() %>% 
        setView(lng = mean(airbnb.df$Long), lat = mean(airbnb.df$Lat), 
                zoom = 13) %>% 
        addCircleMarkers(data = airbnb.df,
            lat = ~ Lat, lng = ~ Long,
            radius = ~ parse_number(S_Accomodates), popup = ~ AboutListing, weight = 3,
            color = "red", fillColor = "yellow")
