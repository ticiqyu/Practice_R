#install.packages("devtools")
#devtools::install_github("bdemeshev/rlms")

library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")

"
hh5 Пол респондента 
  1 МУЖСКОЙ 
  2 ЖЕНСКИЙ

h_marst СЕМЕЙНОЕ ПОЛОЖЕНИЕ 
 1 Никогда в браке не состояли
 2 Состоите в зарегистрированном браке
 3 Живете вместе, но не зарегистрированы
 4 Разведены и в браке не состоите
 5 Bдовец (вдова)
 6 ОФИЦИАЛЬНО ЗАРЕГИСТРИРОВАНЫ, НО ВМЕСТЕ НЕ ПРОЖИВАЮТ
h_diplom ЗАКОНЧЕННОЕ ОБРАЗОВАНИЕ (ГРУППА) 
 1 окончил 0 - 6 классов
 2 незаконч среднее образование (7 - 8 кл)
 3 незаконч среднее образование (7 - 8 кл) + что-то еще
 4 законч среднее образование
 5 законч среднее специальное образование
 6 законч высшее образование и выше
status ТИП НАСЕЛЕННОГО ПУНКТА 
 1 областной центр
 2 город
 3 ПГТ
 4 село
"
data <- rlms_read("C:\\Users\\Admin\\Documents\\R\\r12i_os26b.sav")


data = select(data, hh5, h_age, h_marst, h_diplom, status, hj13.2, hj6.2)
data = na.omit(data)
glimpse(data)

data2 = select(data,) #Новая база данных для нормализованных значений

#Возраст
age = data$h_age
data2["age"] = (age - mean(age)) / sqrt(var(age))
glimpse(data2["age"])

#Пол
data2["sex"] = 0
data2$sex[which(data$hh5 == 1)] <- 1
glimpse(data2["sex"])

#Семейное положение:

#Никогда не состоял/ла в браке?
data2$wed3 = 0
data2$wed3[which(data$h_marst==1)] <- 1
glimpse(data2["wed3"])

#Состоит ли в зарегестрированном браке?
data2$wed1 = 0
data2$wed1[which(data$h_marst==2)] <- 1
data2$wed1[which(data$h_marst==6)] <- 1
glimpse(data2["wed1"])

#Разведён или вдовец?
data2$wed2 = 0
data2$wed2[which(data$h_marst==4)] <- 1
data2$wed2[which(data$h_marst==5)] <- 1
glimpse(data2["wed2"])

# Проверка на отсутствие зависимости
vif(lm(data$hj13.2 ~ data2$wed1 + data2$wed2 + data2$wed3)) 

#Наличие высшего образования
data2$higher_educ = 0
data2$higher_educ[which(data$h_diplom==6)] <- 1
glimpse(data2["higher_educ"])

#Живёт в городе?
data2$city_status = 0
data2$city_status[which(data$status==1)] <- 1
data2$city_status[which(data$status==2)] <- 1
glimpse(data2["city_status"])

#Нормализованное среднее число рабочих часов в неделю
working_hours = data$hj6.2
data2$working_hours = (working_hours - mean(working_hours)) / sqrt(var(working_hours))
glimpse(data2["working_hours"])

#Нормализованная средняя зарплата
salary = data$hj13.2
data2$salary = (salary - mean(salary)) / sqrt(var(salary))
glimpse(data2["salary"])


# 1. Постройте линейную регрессию зарплаты на все параметры, которые Вы выделили из данных мониторинга. Не забудьте оценить коэффициент вздутия дисперсии VIF.

model1 = lm(data = data2, salary ~ sex + age + wed1 + wed2 + wed3 + higher_educ + city_status + working_hours)
vif(model1)#зависимость между регрессорами-отсутствует
summary(model1)#R^2~0.1747,wed1 и wed2- не имеют звёзд(плохая p-статистика)

model1 = lm(data = data2, salary ~ sex + age + wed3 + higher_educ + city_status + working_hours)
vif(model1)#зависимость между регрессорами-отсутствует
summary(model1)#р-статистика--отличная,R^2~0.1744(зависимость-нелинейная)

# 2. Поэкспериментируйте с функциями вещественных параметров: используйте логарифм и степени (хотя бы от 0.1 до 2 с шагом 0.1).

#sex,wed3,higher_educ,city_status-имеют значения только 0 и 1->не имеет смысла использовать с ними логарифмирования и возведение в степень

# с логарифмами:
model1 = lm(data = data2, salary ~ sex + working_hours + age +  wed3 + higher_educ + city_status + I(log(working_hours)) + I(log(age)))
vif(model1)#vif<5 у всех регрессоров,age,wed3 и оба логарифма имеют плохую р-статистику
summary(model1)#R^2~0.2164(зависимость нелинейная)

model2 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(log(age)))
vif(model2)#зависимость между регрессорами-отсутствует
summary(model2)#R^2~0.1958 p-статистика плохая у wed3 и I(log(age))

model3 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status +  I(log(working_hours)))
vif(model3)#зависимость между регрессорами-отсутствует
summary(model3)#R^2~0.1916 p-статистика плохая у I(log(working_hours))

#со степенями
power = 0.1
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)
summary(model1)#R^2~0.2155,плохая р-статистика у переменных со словами age и working_hours

power = 0.2
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1) #есть переменные у которых vif>5
summary(model1)#R^2~0.2146

power = 0.3
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1) #есть переменные у которых vif>5
summary(model1)#R^2~0.2138

power = 0.4
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>10
summary(model1)#R^2~0.2129

power = 0.5
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>15
summary(model1)#R^2~0.2122

power = 0.6
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>25
summary(model1)#R^2~0.2114

power = 0.7
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>45
summary(model1)#R^2~0.2108


power = 0.8
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>100
summary(model1)#R^2~0.2103

power = 0.9
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>480
summary(model1)#R^2~0.2098

power = 1.1
model1 = lm(data = data2, salary ~ sex + working_hours + wed3 + higher_educ + city_status + I(age^power))
vif(model1)#есть переменные у которых vif>520
summary(model1)#R^2~0.209

power = 1.2
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>130
summary(model1)#R^2~0.2088

power = 1.3
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>60
summary(model1)#R^2~0.2085

power = 1.4
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>35
summary(model1)#R^2~0.2084

power = 1.5
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>20
summary(model1)#R^2~0.2082

#R^2 изменяется очень медленно,перейдём сразу к power=1.9

power = 1.9
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#есть переменные у которых vif>9
summary(model1)#R^2~0.208

power = 2
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)#vif у всех переменных<1,5
summary(model1)#R^2~0.183




# 3.Выделите наилучшие модели из построенных: по значимости параметров,включённых в зависимости, и по объяснённому с помощью построенных зависимостей разбросу adjusted R2 - R2adj.

#сравним лучшие модели из пункта 2 
power = 2 #наилучшая p-статистика
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)
summary(model1)
#Multiple R-squared:  0.183,	Adjusted R-squared:  0.1809

power = 0.1 #наибольший R^2
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1)
summary(model1)
#Multiple R-squared:  0.2155,	Adjusted R-squared:  0.2028 

power = 0.2#p-статистика и R^2 схожая с model1 при power=0.1
model1 = lm(data = data2, salary ~ sex + working_hours + age + wed3 + higher_educ + city_status + I(working_hours^power) + I(age^power))
vif(model1) 
summary(model1)
#Multiple R-squared:  0.2146,	Adjusted R-squared:  0.2019

# Разброс R2 - R2_adj у model1 при power=2 - наименьший, а R^2 больше для степени 0.1

#Итог:среди моделей с наименьшей линейной зависимостью,с наилучшими по сравнению с остальными показателями p-статистики у регрессоров,лучшей по R^2 оказалась модель для степени 0.1



# 4. Сделайте вывод о том, какие индивиды получают наибольшую зарплату.

#Согласно этой модели больше всего зарабатывают молодые(ненадёжная p-статистика) мужчины с высшим образованием, проживающие в городах, работающие много часов в неделю.

# 5. Оцените регрессии для подмножества индивидов:

#1)Не вступавшие в брак, без высшего образования

power = 0.1
data3 = subset(data2, higher_educ  == 0)
data3 = subset(data3, wed3 == 1) 
model1 = lm(data = data2, salary ~ sex + working_hours + age + city_status + I(working_hours^power) + I(age^power))
summary(model1)#R^2~0.1688
#Больше всего зарабатывают молодые(ненадёжная p-статистика) мужчины,работающие много,проживающие в городе 

# 2) Городские жители, состоящие в браке
power = 0.1
data3 = subset(data2, city_status  == 1)
data3 = subset(data3, wed2 == 1) 
model1 = lm(data = data2, salary ~ sex + working_hours + age + higher_educ + I(working_hours^power) + I(age^power))
summary(model1)#R^2 ~ 0.16
# Наибольшая зарплата у мужчин с высшим образованием молодого(ненадёжная p-статистика) возраста, работающих много


