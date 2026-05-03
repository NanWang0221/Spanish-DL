

# calculate VOT-FO correlation
df <- read.csv("/Users/nanwang/Dropbox/pr2_tan-jaeger/stimuli/recorded/cor-vot-f0.csv")
df = na.omit(df)

model <- lm(F0 ~ VOT_ms, data = df)
new_data <- data.frame(VOT_ms = c(130))
predicted_VOT <- predict(model, newdata = new_data)
print(predicted_VOT)


