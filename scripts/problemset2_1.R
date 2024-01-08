ct = table(nepal621$trt, nepal621$status)
addmargins(ct)
prop.table(ct, margin = 1)
nepal.plac = filter(nepal621, trt=="Placebo")
nepal.vit = filter(nepal621, trt=="Vit A")

ct = table(nepal.plac$sex, nepal.plac$status)
addmargins(ct)
prop.table(ct, margin = 1)

ct = table(nepal.vit$sex, nepal.vit$status)
addmargins(ct)
prop.table(ct, margin = 1)


head(nepal621)
nepal.fem.plac = filter(nepal621, trt=="Placebo" & sex=="Female") 
nepal.fem.vit = filter(nepal621, trt=="Vit A" & sex=="Female")
nepaltable = table(nepal.fem.plac$age, nepal.fem.plac$status)
addmargins(nepaltable)
prop.table(nepaltable, margin=1)
nepaltable2 = table(nepal.fem.vit$age, nepal.fem.vit$status) 
addmargins(nepaltable2)