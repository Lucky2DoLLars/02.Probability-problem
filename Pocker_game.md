Pocker game
================
2DoLLars

포커 게임에서 가능한 패는 ‘Royal flush’, ‘Straight flush’, ‘Four of a
kind’, ‘Full house’, ‘Flush’, ‘Straight’, ‘Three of a kind’, ‘Two pair’,
‘One pair’, ‘No pair’ 으로 구성되어 있다. 이 코드는 포거 게임의 각 패의
확률을 몬테카를로(Monte-carlo)방법을 이용하여 추정하는 것이다.

## 포커 게임 코드

아래의 코드는 포거 게임 1회에 대한 패의 결과를 나타낸다.

``` r
Poker = function()
{
  card = matrix(c(101:113,201:213,301:313,401:413),
                nrow=4, ncol=13, byrow=TRUE)
  
  hand = sample(card, 5, replace=FALSE)
  out = FALSE
  
  hand_matrix = matrix(NA, 2, 5)
  
  for(i in 1:5){
    hand_matrix[,i] = c( hand[i] %/% 100 , hand[i] %% 100 )
  }
  
  hand_unique_suit = unique(hand_matrix[1,])
  hand_unique_number = unique(hand_matrix[2,])
  sort_suit = sort(hand_unique_suit)
  sort_number = sort(hand_unique_number)
  len_suit = length(hand_unique_suit)
  len_number = length(hand_unique_number)
  
  ## Royal Flush
  if( len_suit == 1 ){
    if( all(sort_number == c(1, 10, 11, 12, 13) ) ){
      out = 'Royal flush'
      return(out)
    }
  }
  
  ## Straight Flush
  if( len_suit == 1 && len_number == 5 ){
    if(( sort_number[5] - sort_number[1] ) == 4){
       out = 'Straight flush'
       return(out)
    }
  }
  
  ## Four of a kind
  for(i in 1:len_number){  
    if( sum( hand_unique_number[i] == hand_matrix[2,]) == 4 ){
      out = 'Four of a kind'
      return(out)
    }
  }
    
  ## Full house
  if( len_number == 2){
    if( sum( hand_unique_number[1] == hand_matrix[2,]) %in% c(2,3) ){
      out = 'Full house'
    }
    return(out)
  }
    
  ## Flush
  if( len_suit == 1 ){
    out = 'Flush'
    return(out)
  }
    
  ## Straight
  if((( sort_number[5] - sort_number[1] ) == 4) &&
     ( len_number == 5 ) ){
    out = 'Straight'
    return(out)
  }
    
  ## Three of a kind
  if( len_number == 3 ){
    three_of_a_kind = rep(NA, len_number)
    for(i in 1:len_number){
      three_of_a_kind[i] = sum(hand_unique_number[i] == hand_matrix[2,])
    }
    if( sum(three_of_a_kind %in% c(3,1,1)) == 3 ){
      out = 'Three of a kind'
      return(out)
    }
  }
  
  ## Two pair
  if( len_number == 3 ){
    two_pair = rep(NA, len_number)
    for(i in 1:len_number){
      two_pair[i] = sum(hand_unique_number[i] == hand_matrix[2,])
    }
    if( sum(two_pair %in% c(2,2,1)) == 3 ){
      out = 'Two pair'
      return(out)
    }
  }
  ## One pair
  for(i in 1:len_number){
    if( sum(hand_unique_number[i] == hand_matrix[2,]) == 2 ){
      out = 'One pair'
      return(out)
    }
  }
  
  out = 'No pair'
  
  return(out)
}
```

## 몬테카를로 방법

포커 게임을 10<sup>6</sup> 번 반복하여 각 패가 나올 확률을 추정한다.

``` r
Poker_result = c('Royal flush', 'Straight flush', 'Four of a kind', 'Full house',
                 'Flush', 'Straight', 'Three of a kind', 'Two pair', 'One pair', 'No pair')

ITER = 10^6
result = rep(NA,ITER)
Prob_vec = NULL

for (i in 1:ITER){
  result[i] = Poker()
}

for (i in Poker_result){
  Prob_vec[i] = sum(result == i)
}

Prob = Prob_vec / ITER

print(Prob)
```

    ##     Royal flush  Straight flush  Four of a kind      Full house           Flush 
    ##        0.000000        0.000011        0.000238        0.001461        0.002005 
    ##        Straight Three of a kind        Two pair        One pair         No pair 
    ##        0.003405        0.021336        0.046888        0.423315        0.501341

``` r
par(mfrow = c(1, 1), mai = rep(1.4, 4))
barplot(Prob, axes = TRUE, las = 2, horiz = TRUE,
        main = "The probability of the pocker game",
        col = "yellow")
```

![](/image/pocker_game.png)<!-- -->
