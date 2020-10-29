############# Wenzhou ##############
synth.tables.wenzhou <- synth.tab(
  dataprep.res = dataprep.out.wenzhou,
  synth.res = synth.out.wenzhou)
# --- Table 2 for wenzhou ---
synth.tables.wenzhou$tab.pred
# write.csv(synth.tables.wenzhou$tab.pred, "wenzhoupred1007.csv")

# --- Table S2 ---
print(synth.tables.wenzhou$tab.w[order(synth.tables.wenzhou$tab.w$w.weights), ]) #weight
# write.csv(synth.tables.wenzhou$tab.w[order(synth.tables.wenzhou$tab.w$w.weights), ], "wenzhou1007.csv")
wenzhou.data[!duplicated(wenzhou.data$ctname),] # variable
# write.csv(wenzhou.data[!duplicated(wenzhou.data$ctname),], "wenzhouvariable.csv")


############# Shanghai ##############
synth.tables.shanghai <- synth.tab(
  dataprep.res = dataprep.out.shanghai,
  synth.res = synth.out.shanghai)
# --- Table 2 for shanghai ---
synth.tables.shanghai$tab.pred
# write.csv(synth.tables.shanghai$tab.pred, "shanghaipred1007.csv")

# --- Table S3 ---
print(synth.tables.shanghai$tab.w[order(synth.tables.shanghai$tab.w$w.weights), ]) #weight
# write.csv(synth.tables.shanghai$tab.w[order(synth.tables.shanghai$tab.w$w.weights), ], "shanghai1007.csv")
shanghai.data[!duplicated(shanghai.data$ctname),]
# write.csv(shanghai.data[!duplicated(shanghai.data$ctname),], "shanghaivariable.csv")

