dat <- data.frame(
  time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(14.89, 17.23)
)

# Without coord_flip() - as expected:
p1 <- ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity")

ggplotly(p1)

# With coord_flip() - not right
p2 <- ggplot(data=dat, aes(x=time, y=total_bill)) +
  geom_bar(stat="identity") + coord_flip()

ggplotly(p2)
