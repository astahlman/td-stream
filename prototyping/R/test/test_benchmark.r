options(digits=15)

expected <- data.frame(
        rbind(
        c(1418607396778,"demarco murray"),
        c(1418608620457,"dez bryant"),
        c(1418609918817,"dez bryant"),
        c(1418610531857,"chris polk"),
        c(1418614014826,"chris polk"),
        c(1418614584563,"darren sproles"),
        c(1418615124627,"demarco murray"),
        c(1418615763277,"dez bryant")),
        stringsAsFactors=FALSE)
    colnames(expected) <- c("timestamp", "player")
    expected$timestamp <- as.numeric(as.character(expected$timestamp))


test_that("A perfect detection algorithm gets a perfect score", {

    output <- expected
    ## Latencies are 2,4,6...,16 summing to 72. Mean latency is 9 millis.
    latencies <- sapply(c(1:8), function(x) x * 2)
    output$timestamp <- output$timestamp + latencies

    metrics <- benchmark(output, expected)

    expect_that(nrow(metrics$true.pos), equals(8)) ## every td is detected
    expect_that(nrow(metrics$false.neg), equals(0)) ## no td is missed
    expect_that(nrow(metrics$false.pos), equals(0)) ## no false alarms

    expect_that(metrics$true.pos$latency, equals(latencies))
    expect_that(mean(metrics$true.pos$latency), equals(9))
    expect_that(score.benchmark(metrics), equals(1))
})

test_that("An algorithm that really likes Dez scores 3/8", {
    
    output <- data.frame(
        rbind(    
            c(1418607396778,"dez bryant"),
            c(1418608620457,"dez bryant"), ## correct
            c(1418609918817,"dez bryant"), ## correct
            c(1418610531857,"dez bryant"),
            c(1418614014826,"dez bryant"),
            c(1418614584563,"dez bryant"),
            c(1418615124627,"dez bryant"),
            c(1418615763277,"dez bryant")), ## correct
        stringsAsFactors=FALSE)
    
    colnames(output) <- c("timestamp", "player")
    output$timestamp <- as.numeric(as.character(output$timestamp))

    metrics <- benchmark(output, expected, check.players=TRUE)

    expect_that(nrow(metrics$true.pos), equals(3)) 
    expect_that(nrow(metrics$false.neg), equals(5))
    expect_that(nrow(metrics$false.pos), equals(5))
    expect_that(score.benchmark(metrics), equals(.375))    
})

test_that("Two false positives and one false negative scores ~ 0.8235", {

    output <- data.frame(
        rbind(    
            c(1418607396778,"demarco murray"),
            c(1418608620457,"dez bryant"),
            c(1418609918817,"dez bryant"),
            c(1418609918817,"dez bryant"), ## duplicate, false positive
            c(1418610531857,"chris polk"),
            c(1418614014826,"chris polk"),
            c(1418614584563,"darren sproles"),
            c(1418614584564,"darren sproles"), ## another false positive
            ##c(1418615124627,"demarco murray"), ## false negative
            c(1418615763277,"dez bryant")),
        stringsAsFactors=FALSE)
    
    colnames(output) <- c("timestamp", "player")
    output$timestamp <- as.numeric(as.character(output$timestamp))

    ## Latencies is flat 10
    output$timestamp <- output$timestamp + rep(10, nrow(output))

    ## Flat 10, but we missed the 7th touchdown
    expected.latency <- c(10, 10, 10, 10, 10, 10, 10)
    expected.false.pos <- do.call(rbind.data.frame,
                                  list(
                                      list(1418609918817,"dez bryant"),
                                      list(1418614593481,"darren sproles")))
    expected.false.neg <- do.call(rbind.data.frame,
                                  list(
                                      list(1418615124627,"demarco murray")))

    metrics <- benchmark(output, expected)

    expect_that(nrow(metrics$true.pos), equals(7))
    expect_that(nrow(metrics$false.neg), equals(1))
    expect_that(nrow(metrics$false.pos), equals(2))

    expect_that(metrics$true.pos$latency, equals(expected.latency))
    expect_that(score.benchmark(metrics), equals(.8235294118))
})
          
test_that("An implementation that misses everything scores 0", {

    output <- data.frame(timestamp=numeric(0), player=character(0))
    output$timestamp <- as.numeric(as.character(output$timestamp))

    metrics <- benchmark(output, expected)

    expect_that(nrow(metrics$true.pos), equals(0))
    expect_that(nrow(metrics$false.neg), equals(8))
    expect_that(nrow(metrics$false.pos), equals(0))
    expect_that(score.benchmark(metrics), equals(0))
})

test_that("We can toggle whether the player who scored matters", {

    ## The timings here are all correct
    output <- data.frame(
        rbind(    
            c(1418607396778,"lebron james"),
            c(1418608625870,"willy wonka"),
            c(1418609918817,"barack obama"),
            c(1418610531857,"kendrick lamar"),
            c(1418614014826,"steve buscemi"),
            c(1418614584563,"omar little"),
            c(1418615124627,"khaleesi"),
            c(1418615763277,"vladimir putin")),
        stringsAsFactors=FALSE)
    
    colnames(output) <- c("timestamp", "player")
    output$timestamp <- as.numeric(as.character(output$timestamp))

    metrics <- benchmark(output, expected, check.players=FALSE)

    expect_that(nrow(metrics$true.pos), equals(8)) ## every td is detected
    expect_that(nrow(metrics$false.neg), equals(0)) ## no td is missed
    expect_that(nrow(metrics$false.pos), equals(0)) ## no false alarms
    expect_that(score.benchmark(metrics), equals(1))
})
