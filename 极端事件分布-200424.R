####### 一些较为学术的讨论
####### 2020/4/27
####### 主题：概率、极端事件发生

library(dplyr)
library(ggplot2)
library(tidyr)

############# 平均斯坦
x <- seq(0, 3.45, 0.05)

two_x <- x*2

ratio = ((1 -pnorm(x))^2)/(1-pnorm(two_x))

tibble(ratio) %>% 
  cbind(x) %>% 
  rename('k' = x) %>% 
  mutate(k = as.double(k)) %>% 
  arrange(k) %>% 
  ggplot(aes(x = k, y = ratio)) +
  geom_point(color = 'dodgerblue4') +
  hrbrthemes::theme_ipsum_rc(grid = "Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "k之于sigma",
       title = "平均斯坦", 
       subtitle = "S(X)^2/S(2X)比例（从一个标准高斯分布而来）")




############## 平均斯坦的大数定律
samples <- 10000

thin <-rnorm(samples, sd = 20)

fat <- rlnorm(samples, sdlog = 5,meanlog = 0)

cumulative_mean <- function(numbers) {
  x <- seq(1, length(numbers))
  cum_mean <- cumsum(numbers)/x 
  cum_mean
}


thin_cum_mean <- cumulative_mean(thin)

thin_cum_mean %>%
  tibble(running_mean = .) %>% 
  add_rownames(var = 'number_samples') %>% 
  mutate(number_samples = as.double(number_samples)) %>% 
  arrange(number_samples) %>% 
  ggplot(aes(x = number_samples, y = running_mean)) +
  geom_line(color = 'dodgerblue4') +
  geom_hline(yintercept = 0, linetype = 2, color = 'red') +
  hrbrthemes::theme_ipsum_rc(grid = 'Y') +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "# of samples",
       title = "High variance (20) Gaussian", 
       subtitle = "Sample is helpful. Red line is mean.")



######################### 极端斯坦：大数定律失效，Pareto80/20
samples <- 1000

fat <- 1/(runif(samples))^(1/1.13)

fat_cum_mean <- cumulative_mean(fat)

mean <- 1.13/0.13

fat_cum_mean %>% 
  tibble(running_mean = .) %>% 
  add_rownames(var = 'number_samples') %>% 
  mutate(number_samples = as.double(number_samples)) %>% 
  arrange(number_samples) %>% 
  ggplot(aes(x = number_samples, y = running_mean)) +
  geom_line(color = 'dodgerblue4') +
  hrbrthemes::theme_ipsum_rc(grid = 'Y') +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = mean, linetype = 2, color = 'red') +
  labs(title = 'Extremistan, Pareto 80/20',
       subtitle = 'Sample mean is uninformative. Red line is mean')




######################### 中心极限定律-高斯分布的样本平均和真实平均
gaussian_mean <- function(sample_size) {
  
  samples <- rnorm(sample_size)
  return(mean(samples))
}

mean_30 <- purrr::rerun(1000, gaussian_mean(30)) %>% 
  unlist()

mean_1 <- rnorm(1000)

tibble(iteration = seq(1, 1000, 1), mean_30, mean_1) %>% 
  pivot_longer(cols = starts_with("mean")) %>%  
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = 0.6) +
  hrbrthemes::theme_ipsum_rc() +
  hrbrthemes::scale_fill_ft() +
  labs(subtitle = "Distribution compress very quickly",
       title = "Sample distribution for the mean of Gaussian")


############################ 中心极限定律-极端分布的样本平均和真实平均
########## 当分布受其极端值支配时，与平均高斯相比，压缩样本分布的时间要长得多。

pareto_mean <- function(sample_size) {
  
  fat <- 1/(runif(sample_size))^(1/1.13)
  mean(fat)
}

mean_300 <- purrr::rerun(1000, pareto_mean(300)) %>% 
  unlist()

mean_500 <- purrr::rerun(1000, pareto_mean(500)) %>% 
  unlist()

tibble(iteration = seq(1, 1000, 1), mean_300, mean_500) %>% 
  pivot_longer(cols = starts_with("mean")) %>%  
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(limits = c(0, 30)) +
  hrbrthemes::theme_ipsum_rc() +
  hrbrthemes::scale_fill_ft() +
  labs(title = "Pareto 80/20 平均，样本数为300和500",
       subtitle = "样本平均数的抽样分布并没有压缩")


###############
# 根据罕见事件确定分布时，我们几乎永远不会从样本中观察到其真实属性。
# 而且，从研究高斯分布学到的洞见将使我们的感觉变钝：它们使我们处于一种完全相反方向的驯服型随机性中。
###############