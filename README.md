# Marketing-Models
Pernalonga, a leading supermarket chain of over 400 stores in Lunitunia, sells over 10 thousand products in over 400 categories.  Pernalonga regularly partners with suppliers to fund promotions and derives about 30% of its sales on promotions.  Until recent experimentation with personalized promotions, most of Pernalonga’s promotions are chain-wide promotions.   The beer category is promoted regularly in Pernalonga’s stores.  Mahou San Miguel, a Spanish brewer, sells three San Miguel beer products in Pernalonga’s stores and regularly partners with Pernalonga to promote its products via weekly flyers and in-store displays.  Mahou San Miguel also employs other marketing vehicles such as email, web (display banners and paid search), and traditional media (TV and Radio).


## Problem
Your analytics consulting firm was selected by Mahou San Miguel (the client) to verify the effectiveness of its promotions and marketing partnership with Pernalonga.  The client is interested in identifying promotion and marketing activities that drive significant incremental sales for continuation into 2018.  The timing is perfect, your Data Science team recently concluded a recommender systems project and a pricing project with Pernalonga, so you are well-versed with Pernalonga’s data.  

Your Data Science team with its experience in marketing mix modeling in the Lunitunia market also have the following insights:

Seasonality of the Beer category
National holidays in Lunitunia in 2016 and 2017
TV advertisements have an 8-week half-life, and GRPs map into target audience 2+ Reach via the following formula
LaTeX: Reach\:=\:0.95\left(1-e^{-0.020\:GRP}\right) R e a c h = 0.95 ( 1 − e − 0.020 G R P )

Radio advertisements have a 4-week half-life, and GRPs map into target audience 2+ Reach via the following formula
LaTeX: Reach\:=\:0.90\left(1-e^{-0.025\:GRP}\right) R e a c h = 0.90 ( 1 − e − 0.025 G R P )

With the above insights and supplemental transaction and product data from Pernalonga, you are ready to demonstrate your capabilities in marketing analytics and help the client identify promotion and marketing vehicles that drive significant results.

## Available Data

The file Pernalonga.zip and SanMiguel.zip contains the following tables:

transaction_table.csv contains transaction history in 2016 and 2017 for close to 8,000 customers
cust_id – Customer ID
tran_id – Transaction ID
tran_dt – Transaction Date
store_id – Store ID
prod_id – Product ID
prod_unit – Product unit of measure: CT for count and KG for kilograms
prod_unit_price – Unit price of the product
tran_prod_sale_qty – Quantity/units of the product in the transaction
tran_prod_sale_amt – Sales amount for the product before discounts in the transaction
tran_prod_discount_amt – Total amount of discounts applied to the product in the transaction
tran_prod_offer_cts – Total number of offers on the product resulting in the total amount of discounts in the transaction
tran_prod_paid_amt – Amount paid for the product after discounts are applied in the transaction
product_table.csv contains the product to subcategory and category mapping and descriptions for about 11,000 products
prod_id – Product ID
subcategory_id – Subcategory ID
category_id – Category ID
sub_category_desc – Subcategory name (in Portuguese)
category_desc – Category name (in Portuguese)
category_desc_eng – Category name (in English)
brand_desc – Brand of the product, including NO LABEL and PRIVATE LABEL
transaction_table_supp.csv contains supplementary transaction history for San Miguel products in 2016 and 2017 with same fields as transaction_table.csv
product_table_supp.csv contains supplementary records for San Miguel products with same fields as product_table.csv
promo_ad.csv contains the promotion and advertising activity records
tran_wk – the date of the Sunday representing the week
vehicle – the promo or advertising vehicle
amount – the amount of advertising in units; a “1” means the promotion (Flyer or Store Display) is on for the week
unit – the unit indicated by the amount
prod_assoc – the Product ID of the product featured in the promo or advertisement; “ALL” means all San Miguel products
seasonality.csv contains the seasonality index for products in the Beer Category
tran_wk – the date of the Sunday representing the week
seas_index – a number indicating seasonality index for the week
holiday.csv contains a list of national holidays celebrated in Lunitunia in 2016 and 2017
tran_wk – the date of the Sunday representing the week
holiday – the name of the holiday; a “Pr” prefix before a holiday indicates a week before the holiday
Note that customer, store, product and promo and marketing information beyond what is available above are not provided.

