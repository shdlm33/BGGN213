# Class 05: Data Visualization

# Today we are going to use ggplot2 package
# First we need to load the package!
# install.packages("ggplot2")
library(ggplot2)

# We will use this inbuilt "cars" dataset first
head(cars)

# All ggplots have at least 3 layers,
# data + aes + geoms
ggplot(data=cars) +
  aes(x=speed, y=dist) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Stopping Distance of Old Cars",
       x="Speed (MPH)",
       y="Stopping Distance (ft)")

# Side note: ggplot is not the only graphics system
# A very popular one is good old "base" R graphics
plot(cars)

url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)
#Q How many genes in this dataset
nrow(genes)
colnames(genes)
ncol(genes)
#Q How many genes are up
table(genes$State)
# To obtain the % of up genes compared to total genes:
round( table(genes$State)/nrow(genes) * 100, 2 )

# Make first basic scatter plot

ggplot(data=genes) +
  aes(x=Condition1, y=Condition2) +
  geom_point()

# Adding a third object, State (genes up or down) and saving it as an object, "p":
p <- ggplot(genes) +
  aes(x=Condition1, y=Condition2, col=State) +
  geom_point()
p

# Changing colors:
p + scale_colour_manual( values=c("blue", "gray", "red") )

# Changing labels:
p + scale_colour_manual( values=c("blue", "gray", "red") ) +
  labs(title="Gene Expression Changes Upon Drug Treatment",
         x="Control (no drug)",
         y="Drug Treatment")

# Let's explore the gapminder dataset
# install.packages("gapminder")
library(gapminder)
head(gapminder)

# Let's make a new plot of year vs lifeExp (we can use boxplot/violin)
ggplot(gapminder) +
  aes(x=year, y=lifeExp, color=continent) +
  geom_jitter(width=0.3,alpha=0.4) +
  geom_violin( aes(group=year), alpha=0.2, draw_quantiles = c(0.5))

# Let's turn it interactive
#Install the plotly package
# library(plotly)
# ggplotly()
# ggplotly(p)
