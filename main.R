dataset = read.csv("dataset.csv")

#equivalenti (nome completo e sigla)
unique(dataset$club)
unique(dataset$club_abb)

unique(dataset$birth_country)

model = lm(data = dataset, X2021.guaranteed.comp. ~ - name - club_abb -club - birth_city - birth_country - foot - position - X2022.guar..comp..)
summary(model)
