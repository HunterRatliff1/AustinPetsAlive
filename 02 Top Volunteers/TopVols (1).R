setwd("~/Google Drive/100 - Publicly hosted/rootR/Austin Pets Alive")
require(dplyr)
require(ggplot2)
require(ggthemes)
# require(scales)
# require(gplots)

users <- read.csv("data/users/users.csv", na.strings="") %>% 
  select(First=FirstName, Last=LastName, 
         Hours=HoursWorked, Login=LastLoginDate, 
         Email=EmailAddress, Status=MemberInfo...Status) %>% 
  filter(Status!="Applicant") %>%
  arrange(desc(Hours)) %>%
  mutate(Name = paste(First, Last)) %>% 
  select(-First, -Last) %>%
  mutate(Rank = n() + 1 - row_number(Hours),
         DenR = dense_rank(Hours),
         HundredRank = 1-round(percent_rank(Hours), 2) + 0.01)
users$Status <- factor(users$Status)



P1 <- users %>%
  filter(HundredRank<0.02) %>%
  mutate(dens = dense_rank(Hours)) %>%
  ggplot(aes (x=Hours, y=Rank, label=Name)) +
  geom_text(aes(color=log(Hours), hjust=0, size=log(Hours), family="Georgia")) + 
  geom_rug(sides = "b", color="#DD5928") +
  scale_y_reverse() + scale_x_reverse() + 
  scale_color_continuous(low="#4D4D4D", high="#819E15") +
  ggtitle("Austin Pets Alive Volunteers:\nTop 1% of Volunteers by All Time Hours") + 
  theme_gdocs() + guides(size=F, color=F) + 
  theme(plot.title = element_text(family="American Typewriter", color = "#DD5928"),
        axis.title = element_text(family="Verdana", color = "#5BC002", face = "bold")
        )





df <- users
df$Group <- "Has no hours"
df$Group[df$Hours > 0] <- "Has hours"
df$Group[df$HundredRank <= 0.25] <- "Top 25%"
df$Group[df$HundredRank <= 0.10] <- "Top 10%"
df$Group[df$HundredRank <= 0.01] <- "Top 1%"

P2 <- qplot(data=df, x=Rank, y=Hours, color=Group, shape=Status, geom="jitter") + 
  labs(x="Rank by All Time Hours", y="All\nTime\nHours\nLogged", 
       title="Breakdown of all APA volunteers") + 
  scale_color_manual(
    values = c("Has no hours"="#4D4D4D", "Has hours"="#E78403", "Top 25%"="#DD5928", 
               "Top 10%"="#A7C539", "Top 1%"="#19679F")) + theme_gdocs() + theme(
  axis.title = element_text(family="Verdana", color = "#5BC002", face = "bold"),
  plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", family="American Typewriter", color = "#DD5928"))
     
P3 <- ggplot(subset(df, Hours>0), aes(x=Group, y=Hours)) +
  geom_jitter(alpha=.2, aes(fill=Status)) + 
  geom_boxplot(alpha=0.5, aes(fill=Status)) + 
  labs(x="", y="Hours\nLogged", title="APA Volunteers: By Status") + 
  facet_wrap("Group", scales="free") + 
  scale_fill_manual(
    values = c("Archived"="#DD5928", "Accepted"="#A7C539")) + theme_gdocs() + 
  theme(
        axis.title = element_text(family="Verdana", color = "#DD5928", face = "bold"),
        plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", family="American Typewriter", color = "#DD5928"),
        strip.background = element_rect(fill="#4D4D4D"),
        strip.text = element_text(family="Verdana", color = "#5BC002", face = "bold")
        )

df2 <- top_n(df, 50, Hours) %>% 
  select(Rank, Name, Hours) %>% 
  mutate(Hours = round(Hours))
P4 <- tableGrob(df2, theme=ttheme_minimal())
grid.arrange(P1, P2, P3, P4, ncol=1)


