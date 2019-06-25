
# 1. Overall sales and units of each Greek yogurt Flavor (using SQL)     
select itemAttributes.Flavor1, sum(randItemSales.Unites), sum(randItemSales.Sales) 
from randItemSales join itemAttributes on randItemSales.Item.Num = itemAttributes.Item.Num 
group by itemAttribute.Flavor1;        

# 2. The Greek yogurt Flavor preference of different cities (using SQL)  
select stores.City, itemAttributes.Flavor1, sum(randItemSales.Unites), sum(randItemSales.Sales) 
from (randItemSales join itemAttributes on randItemSales.Item.Num = itemAttributes.Item.Num)
join stores on stores.StoreNum = randItemSales.Store.Num
group by stores.City, itemAttributes.Flavor1;

# 3. The Greek yogurt consumption of each Flavor based on seasonality (using SQL)  
select quarter(randItemSales.Date), itemAttributes.Flavor1, sum(randItemSales.Unites), sum(randItemSales.Sales)
from randItemSales join itemAttributes on randItemSales.Item.Num = itemAttributes.Item.Num 
group by itemAttributes.Flavor1, quarter(randItemSales.Date);        

# 4. Wegmens’ loyalty card members’ sales and units of each Greek yogurt Flavor (using SQL)  
select itemAttributes.Flavor1, sum(survItemSales.Unites), sum(surItemSales.Sales) 
from survItemSales join itemAttributes on survItemSales.Item.Num = itemAttributes.Item.Num
group by itemAttributes.Flavor1;

# 5. Each Flavor’s top 3 selling brand (using SQL)  
select itemAttributes.Brand, itemAttributes.Flavor1, sum(survItemSales.Unites), sum(surItemSales.Sales) 
from randItemSales join itemAttributes on randItemSales.Item.Num = itemAttributes.Item.Num 
group by itemAttributes.Flavor1, itemAttributes.Brand
order by sum(survItemSales.Unites), sum(surItemSales.Sales);
