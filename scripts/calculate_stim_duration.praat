n = numberOfSelected("Sound")

# store selection so it isn't lost inside the loop
for i to n
    sound[i] = selected("Sound", i)
endfor

# first pass: durations, total, mean
total = 0
writeInfoLine: "Per-sound durations"
appendInfoLine: "-------------------"
for i to n
    selectObject: sound[i]
    name$ = selected$("Sound")
    dur[i] = Get total duration
    total = total + dur[i]
    appendInfoLine: name$, tab$, fixed$(dur[i], 3), " s"
endfor
mean = total / n

# second pass: sum of squared deviations
sumsq = 0
for i to n
    sumsq = sumsq + (dur[i] - mean) ^ 2
endfor

if n > 1
    sd = sqrt(sumsq / (n - 1))
else
    sd = undefined
endif

appendInfoLine: "-------------------"
appendInfoLine: "Count: ", n
appendInfoLine: "Total: ", fixed$(total, 3), " s"
appendInfoLine: "Average: ", fixed$(mean, 3), " s"
appendInfoLine: "SD: ", fixed$(sd, 3), " s"