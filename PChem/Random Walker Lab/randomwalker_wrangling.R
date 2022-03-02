randomwalker <- randomwalker %>%
  mutate(rms_n_pred = sqrt(n_average))

randomwalker

randomwalker_plot <- ggplot(data = randomwalker, mapping = aes(x = n_average, y = rms_n_average)) +
  geom_point() +
  geom_linerange(aes(
    xmin = n_average - n_error/2, 
    xmax = n_average + n_error/2)) +
  geom_linerange(aes(
    ymin = rms_n_average - rms_n_error/2, 
    ymax = rms_n_average + rms_n_error/2)) +
  labs(
    title = "Root-mean-square # of particles vs Average # of particles",
    x = "Average N",
    y = "RMS N") +
  geom_function(color = "blue", fun = "sqrt")
  

randomwalker_plot