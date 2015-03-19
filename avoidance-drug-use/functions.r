
run = function(x){
  fit = lm(paste(x, "~ avoid.c * social.c * problem.c + cesd + maqna"),
           data=df)
  newfit = update(fit, . ~ . + I(avoid.c^2) + I(social.c^2) + I(problem.c^2))
  newnewfit = update(newfit, . ~ . + I(avoid.c^3) + I(social.c^3) +
                     I(problem.c^3))
  print(summary(newfit))
}

run.avoid = function(x){
  fit = lm(paste(x, "~ avoid.c  + cesd + maqna"), data=df)
  newfit = update(fit, . ~ . + I(avoid.c^2))
  newnewfit = update(newfit, . ~ . + I(avoid.c^3))
  print(summary(newfit))
}

run.problem = function(x){
  fit = lm(paste(x, "~ problem.c  + cesd + maqna"), data=df)
  newfit = update(fit, . ~ . + I(problem.c^2))
  newnewfit = update(newfit, . ~ . + I(problem.c^3))
  print(summary(newfit))
}

run.social = function(x){
  fit = lm(paste(x, "~ social.c + cesd + maqna"), data=df)
  newfit = update(fit, . ~ . + I(social.c^2))
  newnewfit = update(newfit, . ~ . + I(social.c^3))
  print(summary(newfit))
}
