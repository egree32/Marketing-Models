# preparation -------------------------------------------------------------------
library(data.table)
library(dummies)
library(zoo)
library(ggplot2)
library(reshape)
library(car)
library(lmtest)

hol = fread('DATA/holiday.csv', header = TRUE)
prod = fread("DATA/product_table_supp.csv", header = TRUE)
promo = fread('DATA/promo_ad.csv', header = TRUE)
seasonality = fread('DATA/seasonality.csv', header = TRUE)
trans = fread("DATA/transaction_table_supp.csv", header = TRUE)

head(trans)
unique(trans$prod_id) # three products to discuss: 138936953 138936952 138936951

# first computed Reach for both TV and Radio. TV and Radio were employed for all 3 products. They were firstly used during the week following '2016-06-05' -------------------------------------------------------------------
head(promo)

tran_wk = unique(promo$tran_wk)
shift_tran_wk = shift(tran_wk)

wk = data.table(cbind(tran_wk, shift_tran_wk))
wk$tran_wk = as.Date(wk$tran_wk)
wk$shift_tran_wk = as.Date(wk$shift_tran_wk)
wk$diff = wk$tran_wk - wk$shift_tran_wk
wk$diff # all 7 days difference, it seems Pernalonga is promoting on a weekly basis

#wk_TV[wk_TV$vehicle == 'TV']
promo[promo$vehicle == 'Radio']

all_wk = data.table(cbind(tran_wk, shift_tran_wk))
all_wk$tran_wk = as.Date(all_wk$tran_wk)
all_wk = data.table(all_wk$tran_wk)
colnames(all_wk) <- 'tran_wk'

# TV, 8-week half life; Radio, 4-week half life
alpha_TV = 1 - 0.5 ** (1 / 8)
alpha_Radio = 1 - 0.5 ** (1 / 4)

unique(promo[promo$vehicle == 'TV', prod_assoc])
unique(promo[promo$vehicle == 'Radio', prod_assoc])
# all TV and Radio promotions are targeted towards all products

promo$tran_wk = as.Date(promo$tran_wk)

## we calculate the GRP and impression of TV for each week
wk_TV = merge(all_wk, promo[promo$vehicle == 'TV'], by = 'tran_wk', all.x = TRUE)
wk_TV = wk_TV[wk_TV$tran_wk >= '2016-06-05']
wk_TV$tran_wk = as.Date(wk_TV$tran_wk)

# we start from 06-05 because this when the first TV campaign took place, and we assume that there is no adstock left before campaign & initialize
wk_TV[is.na(wk_TV$amount), amount := 0] # for NA amounts, change to 0
wk_TV 


# use the half life provided to adstock the GRP with missing weeks filled in. After that, transform the adstock GRP to reach before inputting it in the regression. -------------------------------------------------------------------
alpha_TV = 1 - 0.5 ** (1 / 8) # calculate alpha of TV commercial based on TV half-life
wk_TV$adstock = 0
for (wk in as.character(wk_TV$tran_wk)) {
    if (wk == '2016-06-05') {
        wk_TV[wk_TV$tran_wk == wk, adstock := alpha_TV * amount]
        pre_adstock <- wk_TV[wk_TV$tran_wk == wk, adstock]
    } else {
        grp <- wk_TV[wk_TV$tran_wk == wk, amount]
        update <- (1 - alpha_TV) * pre_adstock + alpha_TV * grp
        wk_TV[wk_TV$tran_wk == wk, 'adstock'] <- update
        pre_adstock <- wk_TV[wk_TV$tran_wk == wk, adstock]
    }
}
wk_TV[, reach := 0.95 * (1 - exp(-0.02 * adstock))] # convert adstocked GRP to reach
wk_TV$tran_wk = as.Date(wk_TV$tran_wk)
wk_TV

## we calculate the GRP and impression of radio for each week
wk_Radio = merge(all_wk, promo[promo$vehicle == 'Radio'], by = 'tran_wk', all.x = TRUE)
wk_Radio = wk_Radio[wk_Radio$tran_wk >= '2016-06-05']
wk_Radio[is.na(wk_Radio$amount), amount := 0]
wk_Radio$adstock = 0

alpha_Radio = 1 - 0.5 ** (1 / 4) #  calculate alpha of radio commercial based on radio half-life
for (wk in as.character(wk_Radio$tran_wk)) {
    if (wk == '2016-06-05') {
        wk_Radio[wk_Radio$tran_wk == wk, adstock := alpha_Radio * amount]
        pre_adstock <- wk_Radio[wk_Radio$tran_wk == wk, adstock]
    } else {
        grp <- wk_Radio[wk_Radio$tran_wk == wk, amount]
        update <- (1 - alpha_Radio) * pre_adstock + alpha_Radio * grp
        wk_Radio[wk_Radio$tran_wk == wk, 'adstock'] <- update
        pre_adstock <- wk_Radio[wk_Radio$tran_wk == wk, adstock]
    }
}
wk_Radio[, reach := 0.9 * (1 - exp(-0.025 * adstock))] # convert adstocked GRP to reach
wk_Radio$tran_wk = as.Date(wk_Radio$tran_wk)
wk_Radio


# Prepare the weekly data from transaction table, seasonality and holiday table. In terms of holidays, split it into two variables, holiday and important holiday comprising Christmas and New Year. When plotting the sales across weeks, we found there are patterns at the turnning point of each year that are very different from other holidays. Taking 138936952 for example, its sales in this period plummeted for some reason. ----------------------------------------------------------
data <- trans[, .(tran_dt, prod_id, tran_prod_sale_amt, tran_prod_sale_qty, tran_prod_discount_amt, prod_unit_price)]
data$tran_dt = as.Date(data$tran_dt)
all_wk$week = all_wk$tran_wk

data = merge(data, all_wk, by.x = 'tran_dt', by.y = 'tran_wk', all.x = TRUE, all.y = TRUE)
data = data[data$tran_dt > '2015-12-27']
data$week = as.character(data$week)

# list price, discount
data = data[order(tran_dt)]
data[data$tran_dt <= '2016-01-02', week := '2015-12-27']
data$week = na.locf(na.locf(data$week))

data[, wkly_price := sum(tran_prod_sale_amt) / sum(tran_prod_sale_qty), by = .(week, prod_id)]
data[, wkly_dct := sum(tran_prod_discount_amt) / sum(tran_prod_sale_qty), by = .(week, prod_id)]
data

# seasonality, holiday
data = merge(data, seasonality, by.x = 'week', by.y = 'tran_wk')

# Take 138936952 for example, its sales in the turnning point of each year plummeted.
ggplot(data[data$prod_id == 138936953, list(sales = sum(tran_prod_sale_amt)), by = week], aes(x = week, y = sales)) + geom_point()
hol[hol$holiday %in% c('XMAS', 'NEWYEAR'), imt_hol := 1]
hol[is.na(imt_hol), imt_hol := 0]

hol[, holiday := 1]
hol[imt_hol == 1, holiday := 0]
hol = unique(hol)

data = merge(data, hol, all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
data[is.na(holiday), holiday := 0]
data[is.na(imt_hol), imt_hol := 0]
data = data[, list(sales_qty = sum(tran_prod_sale_qty)),
     by = .(week, prod_id, seas_index, holiday, imt_hol, wkly_price, wkly_dct)]
data[, list(count = .N), by = prod_id]

data[, list(count = .N), by = prod_id]
data$week = as.Date(data$week)


# Now we discuss the DueTos for three products separately: first # 138936951 ---------------------------------------------------------------
prod_is = 138936951 # this product has no Store Display
model = data[prod_id == prod_is]

dim(model)
ggplot(model, aes(x = week, y = sales_qty)) + geom_line()

# merge with promo + TV + Radio
prod_promo = promo[(prod_assoc %in% c('ALL', as.character(prod_is))) & (!(vehicle %in% c('TV', 'Radio'))), .(tran_wk, vehicle, amount)]

# Flyer, Store Display, Email, Web Display, Paid Search
unique(prod_promo$vehicle)
prod_promo = cast(prod_promo, form = tran_wk ~ vehicle, value = 'amount')
model = merge(model, prod_promo, by.x = 'week', by.y = 'tran_wk')

# TV, Radio
typeof(wk_TV$tran_wk)

model = merge(model, wk_TV[, .(tran_wk, reach)], all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
colnames(model)[colnames(model) == 'reach'] <- 'TV'

model = merge(model, wk_Radio[, .(tran_wk, reach)], all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
colnames(model)[colnames(model) == 'reach'] <- 'Radio'
model$base_price <- model[week == '2015-12-27', wkly_price] # use the earlist price as base price

# transform y
model[, max_sales_qty := max(sales_qty) * 1.1] # we are making an assumption that the maximum sales quantity is 10% more than historical maximum
model[, sales_trfm := log(sales_qty / (max_sales_qty - sales_qty))] # It is bounded and replace 1 with max_sales_qty*1.1 to ensure it is positive inside logarithm
model[is.na(model)] <- 0 # format
colnames(model) <- c("week", "prod_id", "seas_index", "holiday", "imt_hol", "wkly_price", "wkly_dct", "sales_qty", "Email", "Flyer", "Paid_Search", "Web_Display", "TV", "Radio", "base_price", "max_sales_qty", "sales_trfm")

# Run regressions for each product. Merge the trans information of a specific product with all the promotion advertisements associated with it.
formula <- sales_trfm ~ wkly_price + wkly_dct + Flyer + Email + Web_Display + Paid_Search + TV + Radio + seas_index + holiday + imt_hol # no Store Display for 138936951
lm <- lm(formula, data = model)
summary(lm)

# We decided to apply logit function because it is bounded, which is very suitable for our situation, and it helps depict complex implicit interaction, which is very likely when the audience is exosed to a lot of similar media such as TV and radio, so we transformed y with the logit function. 
model$pred <- predict(lm, newdata = model)
model[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

# DueTo Decomposition of # 138936951 ---------------------------------------------------------------
# the baseline is base price + seasonailty + holiday, Use the price at the beginning of the two-year period as the base price with other variables being replaced with 0
model_pred <- copy(model)
model_pred[, wkly_price := base_price]

model_pred[, c('wkly_dct', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'TV', 'Radio')] <- 0
model$base <- predict(lm, newdata = model_pred)
model[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# Due_to_Price is the difference between the quantities estimated using the real price in that period and the quantities estimated with the base price in place of the weekly price while the value of the other variables remain the same
model_pred <- copy(model)
model_pred[, wkly_price := base_price] # replace weekly price with base price, with other attributes fixed
model$due_to_base_price <- predict(lm, newdata = model_pred)
model[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
model[, due_to_Price := pred - due_to_base_price]
model[, due_to_base_price := NULL]

# Similar to Due_to_Price, the base promotion is 0
model_pred <- copy(model)
model_pred[, wkly_dct := 0] # make promo = 0
model$due_to_Promo <- predict(lm, newdata = model_pred)
model[, due_to_Promo := max_sales_qty * exp(due_to_Promo) / (exp(due_to_Promo) + 1)]
model[, due_to_Promo := pred - due_to_Promo]
model

# Due TO Media, Flyer + Email + Web_Display + Paid_Search + TV + Radio. Similar to DueTos above, the base value for promotional advertisements is 0
for (media in c('Flyer', 'Email', 'Web_Display', 'Paid_Search', 'TV', 'Radio')) {
    model_pred <- copy(model)
    model_pred[, c(media) := 0] # make promo = 0
    col_name = paste('due_to', media, sep = '_')
    model[, c(col_name) := predict(lm, newdata = model_pred)]
    model[, c(col_name)] <- model$max_sales_qty * exp(model[, .SD, .SDcols = c(col_name)]) / (exp(model[, .SD, .SDcols = c(col_name)]) + 1)
    model[, c(col_name)] <- model$pred - model[, .SD, .SDcols = c(col_name)]
}

# After decomposition, we rescaled the DueTos back to the exact quantities proportionally.
model[, sum := base + due_to_Price + due_to_Promo + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]
model[, .(sum, sales_qty)]

for (col in c('base', 'due_to_Price', 'due_to_Promo', 'due_to_Flyer', 'due_to_Email', 'due_to_Web_Display', 'due_to_Paid_Search', 'due_to_TV', 'due_to_Radio')) {
    model[, c(col)] <- model[, .SD, .SDcols = c(col)] / model$sum * model$sales_qty
}

for (col in c('max_sales_qty', 'sales_trfm', 'sum')) {
    model[, c(col)] <- NULL
}

ggplot(model, aes(x = week, y = base)) + geom_line()
ggplot(model, aes(x = week, y = sales_qty)) + geom_line()

write.csv(model, '138936951.csv', row.names = FALSE)

# We conduct Dubin-Watson test to calculate autocorrelation of # 138936951 --------------------------------------------------------------- 
# DW = 1.995, p-value = 0.4725, this means that DW test is relatively certain that it possibly has positive autocorrelation
model$residual = model$sales_trfm - model$pred
dwtest(model$sales_trfm ~ model$residual)



# then we do the same for 138936952 and 138936953 --------------------------------------------------------------- 

# 138936952 ---------------------------------------------------------------
prod_is = 138936952
model = data[prod_id == prod_is]

dim(model)
ggplot(model, aes(x = week, y = sales_qty)) + geom_line()

# merge with promo + TV + Radio
prod_promo = promo[(prod_assoc %in% c('ALL', as.character(prod_is))) & (!(vehicle %in% c('TV', 'Radio'))), .(tran_wk, vehicle, amount)]

# Flyer, Store Display, Email, Web Display, Paid Search
unique(prod_promo$vehicle)
prod_promo = cast(prod_promo, form = tran_wk ~ vehicle, value = 'amount')
model = merge(model, prod_promo, by.x = 'week', by.y = 'tran_wk')
model

# TV, Radio
typeof(wk_TV$tran_wk)

model = merge(model, wk_TV[, .(tran_wk, reach)], all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
colnames(model)[colnames(model) == 'reach'] <- 'TV'

model = merge(model, wk_Radio[, .(tran_wk, reach)], all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
colnames(model)[colnames(model) == 'reach'] <- 'Radio'
model$base_price <- model[week == '2015-12-27', wkly_price] # use the earlist price as base price

# transform y
model[, max_sales_qty := max(sales_qty) * 1.1] # Assumption: 10% more than historical maximum
model[, sales_trfm := log(sales_qty / (max_sales_qty - sales_qty))] # It is bounded and replace 1 with max_sales_qty*1.1 to ensure it is positive inside logarithm

# format
model[is.na(model)] <- 0
colnames(model) <- c("week", "prod_id", "seas_index", "holiday", "imt_hol", "wkly_price", "wkly_dct", "sales_qty", "Email", "Flyer", "Paid_Search", "Store_Display", "Web_Display", "TV", "Radio", "base_price", "max_sales_qty", "sales_trfm")

formula <- sales_trfm ~ wkly_price + wkly_dct + Flyer + Email + Web_Display + Paid_Search + Store_Display +  TV + Radio + seas_index + holiday + imt_hol

lm <- lm(formula, data = model)
summary(lm)

model$pred <- predict(lm, newdata = model)
model[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

# base: base price + seasonailty + holiday
model_pred <- copy(model)
model_pred[, wkly_price := base_price]

model_pred[, c('wkly_dct', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'Store_Display', 'TV', 'Radio')] <- 0
model$base <- predict(lm, newdata = model_pred)
model[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# Due To Price
model_pred <- copy(model)
model_pred[, wkly_price := base_price] # replace weekly price with base price, with other attributes fixed
model$due_to_base_price <- predict(lm, newdata = model_pred)
model[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
model[, due_to_Price := pred - due_to_base_price]
model[, due_to_base_price := NULL]

# Due To Promotion
model_pred <- copy(model)
model_pred[, wkly_dct := 0] # make promo = 0
model$due_to_Promo <- predict(lm, newdata = model_pred)
model[, due_to_Promo := max_sales_qty * exp(due_to_Promo) / (exp(due_to_Promo) + 1)]
model[, due_to_Promo := pred - due_to_Promo]
model

# Due TO Media, Flyer + Email + Web_Display + Store_Display + Paid_Search + TV + Radio
for (media in c('Flyer', 'Email', 'Web_Display', 'Store_Display', 'Paid_Search', 'TV', 'Radio')) {
    model_pred <- copy(model)
    model_pred[, c(media) := 0] # make promo = 0
    col_name = paste('due_to', media, sep = '_')
    model[, c(col_name) := predict(lm, newdata = model_pred)]
    model[, c(col_name)] <- model$max_sales_qty * exp(model[, .SD, .SDcols = c(col_name)]) / (exp(model[, .SD, .SDcols = c(col_name)]) + 1)
    model[, c(col_name)] <- model$pred - model[, .SD, .SDcols = c(col_name)]
}

# Rescale
model[, sum := base + due_to_Price + due_to_Promo + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Store_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]
model[, .(sum, sales_qty)]
cor(model[, .(sum, pred, sales_qty)])

for (col in c('base', 'due_to_Price', 'due_to_Promo', 'due_to_Flyer', 'due_to_Email', 'due_to_Web_Display', 'due_to_Paid_Search', 'due_to_TV', 'due_to_Radio')) {
    model[, c(col)] <- model[, .SD, .SDcols = c(col)] / model$sum * model$sales_qty
}

for (col in c('max_sales_qty', 'sales_trfm', 'sum')) {
    model[, c(col)] <- NULL
}
model[, .(pred, sum)]

ggplot(model, aes(x = week, y = base)) + geom_line()
ggplot(model, aes(x = week, y = sales_qty)) + geom_line()

write.csv(model, '138936952.csv', row.names = FALSE)

# We conduct Dubin-Watson test to calculate autocorrelation
model$residual = model$sales_trfm - model$pred
# DW = 1.9406, p-value = 0.3602
dwtest(model$sales_trfm ~ model$residual) 

# 138936953 ---------------------------------------------------------------
prod_is = 138936953
model = data[prod_id == prod_is]

dim(model)
ggplot(model, aes(x = week, y = sales_qty)) + geom_line()

# merge with promo + TV + Radio
prod_promo = promo[(prod_assoc %in% c('ALL', as.character(prod_is))) & (!(vehicle %in% c('TV', 'Radio'))), .(tran_wk, vehicle, amount)]

# Flyer, Store Display, Email, Web Display, Paid Search
unique(prod_promo$vehicle)
prod_promo = cast(prod_promo, form = tran_wk ~ vehicle, value = 'amount')
model = merge(model, prod_promo, by.x = 'week', by.y = 'tran_wk')

# TV, Radio
model = merge(model, wk_TV[, .(tran_wk, reach)], all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
colnames(model)[colnames(model) == 'reach'] <- 'TV'

model = merge(model, wk_Radio[, .(tran_wk, reach)], all.x = TRUE, by.x = 'week', by.y = 'tran_wk')
colnames(model)[colnames(model) == 'reach'] <- 'Radio'
model$base_price <- model[week == '2015-12-27', wkly_price] # use the earlist price as base price

# transform y
model[, max_sales_qty := max(sales_qty) * 1.1] # Assumption: 10% more than historical maximum
model[, sales_trfm := log(sales_qty / (max_sales_qty - sales_qty))] # It is bounded and replace 1 with max_sales_qty*1.1 to ensure it is positive inside logarithm

# format
model[is.na(model)] <- 0
colnames(model) <- c("week", "prod_id", "seas_index", "holiday", "imt_hol", "wkly_price", "wkly_dct", "sales_qty", "Email", "Flyer", "Paid_Search", "Store_Display", "Web_Display", "TV", "Radio", "base_price", "max_sales_qty", "sales_trfm")

formula <- sales_trfm ~ wkly_price + wkly_dct + Flyer + Email + Web_Display + Paid_Search + Store_Display + TV + Radio + seas_index + holiday + imt_hol

lm <- lm(formula, data = model)
summary(lm)

model$pred <- predict(lm, newdata = model)
model[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

# base: base price + seasonailty + holiday
model_pred <- copy(model)
model_pred[, wkly_price := base_price]

model_pred[, c('wkly_dct', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'Store_Display', 'TV', 'Radio')] <- 0
model$base <- predict(lm, newdata = model_pred)
model[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# Due To Price
model_pred <- copy(model)
model_pred[, wkly_price := base_price] # replace weekly price with base price, with other attributes fixed
model$due_to_base_price <- predict(lm, newdata = model_pred)
model[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
model[, due_to_Price := pred - due_to_base_price]
model[, due_to_base_price := NULL]

# Due To Promotion
model_pred <- copy(model)
model_pred[, wkly_dct := 0] # make promo = 0
model$due_to_Promo <- predict(lm, newdata = model_pred)
model[, due_to_Promo := max_sales_qty * exp(due_to_Promo) / (exp(due_to_Promo) + 1)]
model[, due_to_Promo := pred - due_to_Promo]
model

# Due TO Media, Flyer + Email + Web_Display + Store_Display + Paid_Search + TV + Radio
for (media in c('Flyer', 'Email', 'Web_Display', 'Store_Display', 'Paid_Search', 'TV', 'Radio')) {
    model_pred <- copy(model)
    model_pred[, c(media) := 0] # make promo = 0
    col_name = paste('due_to', media, sep = '_')
    model[, c(col_name) := predict(lm, newdata = model_pred)]
    model[, c(col_name)] <- model$max_sales_qty * exp(model[, .SD, .SDcols = c(col_name)]) / (exp(model[, .SD, .SDcols = c(col_name)]) + 1)
    model[, c(col_name)] <- model$pred - model[, .SD, .SDcols = c(col_name)]
}

# Rescale
model[, sum := base + due_to_Price + due_to_Promo + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Store_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]

# correct for base
model[sum < 0, base := base - sum + 0.1]

model[, sum := base + due_to_Price + due_to_Promo + due_to_Flyer + due_to_Email + due_to_Web_Display + due_to_Store_Display + due_to_Paid_Search + due_to_TV + due_to_Radio]

cor(model[, .(sum, pred, sales_qty)])

for (col in c('base', 'due_to_Price', 'due_to_Promo', 'due_to_Flyer', 'due_to_Email', 'due_to_Web_Display', 'due_to_Paid_Search', 'due_to_TV', 'due_to_Radio')) {
    model[, c(col)] <- model[, .SD, .SDcols = c(col)] / model$sum * model$sales_qty
}

for (col in c('max_sales_qty', 'sales_trfm', 'sum')) {
    model[, c(col)] <- NULL
}

ggplot(model, aes(x = week, y = base)) + geom_line()
ggplot(model, aes(x = week, y = sales_qty)) + geom_line()
ggplot(model, aes(x = week, y = pred)) + geom_line()

model[which.max(base)]

write.csv(model, '138936953.csv', row.names = FALSE)

# We conduct Dubin-Watson test to calculate autocorrelation
model$residual = model$sales_trfm - model$pred
# DW = 1.9046, p-value = 0.2892
dwtest(model$sales_trfm ~ model$residual) 
