# Start of environment setup code
# The level of detail in the information messages
ve <- 2

# Test data is read
l <- c(
    "If you think I’m wrong, send me a link to where it’s happened",
    "We’re about 90percent done with this room",
    "“This isn’t how I wanted it between us.”",
    "Almost any “cute” breed can become ornamental",
    "Once upon a time there was a kingdom with a castle…",
    "That's not a thing any of us are granted'",
    "“Why are you being so difficult?” she asks."
)
# The expected results
res <- c(
    "if you think wrong send me a link to where its happened",
    "were about percent done with this room",
    "this how i wanted it between us",
    "almost any cute breed can become ornamental",
    "once upon a time there was a kingdom with a castle",
    "thats not a thing any of us are granted",
    "why are you being so difficult she asks"
)
# The DataCleaner object is created
dc <- DataCleaner$new(ve = ve)
# The line is cleaned
cl <- dc$clean_lines(l)
# The cleaned lines are printed
print(cl)
