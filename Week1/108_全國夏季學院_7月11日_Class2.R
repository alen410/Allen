#這次會用到的資料集
#各國證券市場市值總額佔GDP比率表_NEW https://data.gov.tw/dataset/103952
#臺北市住宅竊盜點位資訊 https://data.gov.tw/dataset/73886

#國內生產總值(GDP)國內生產總值是國民經濟核算的核心指標
#在衡量一個國家或地區經濟狀況和發展水準亦有相當重要性。


#預先安裝好分析環境
install.packages("dplyr")
install.packages("tidyr")
install.packages("rvest")
install.packages("wordcloud")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")
install.packages("stringr")


#確認安裝套件版本
packageVersion("dplyr")
packageVersion("tidyr")
packageVersion("rvest")
packageVersion("wordcloud")
packageVersion("ggplot2")
packageVersion("scales")
packageVersion("stringr")

#顯示必要資訊
R.Version()
sessionInfo()
Sys.time()

#導入函式庫
library('dplyr')
library('tidyr')
library('rvest')
library('wordcloud')
library('ggplot2')
library('scales')
library('stringr')

#設置工作目錄，這屬於暫時性工作目錄，會依照檔案工作目錄變化來修改
setwd("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week1")



#讀取世界GDP比率表，編碼為UTF-8
gdp_rate <- read.table(
    "世界主要證券市場市值總額佔GDP比率表.csv",
    header = TRUE,
    sep = ",",
    col.names = c("年月", "台灣", "紐約", "那斯達克",
                  "日本", "倫敦", "香港", "韓國", "新加坡", "上海"),
    na.strings = c(" "),
    row.names = NULL,
    fill = TRUE,
    encoding = "UTF-8",
)


#讀取台北市住宅竊盜地點資訊，編碼為BIG-5
taipei_crime <- read.table(
    "臺北市1040110806住宅竊盜點位資訊_BIG5.csv",
    header = TRUE,
    sep = ",",
    fill = TRUE,
    encoding = "BIG-5",
)


#探索資料集(Exploring data)


#預先觀看GDP比率資料的外觀
dim(gdp_rate) #觀看資料的維度


#觀看一下GDP比率的外觀
head(gdp_rate)

#使用 select()函數指定我只要看台灣每年的GDP值
select(gdp_rate, "年月", "台灣")


#將分析目標鎖定在台灣
summary(gdp_rate$台灣) #觀看台灣GDP的統計值(方法1)

#觀看台灣GDP的統計值(方法2, 可自訂要看的統計值)
summarise(gdp_rate,
          observes_n = n(),
          variable_n = ncol(gdp_rate))


#Tidying Data
#將民國年轉為西元年
#由於目前的GDP資料僅有西元年但不包含月和日
#故先將犯罪率資料作月和日的清除
#先將日期格式正規化
#接著將字串轉換為整數後加上1911變為西元年
taipei_crime$date <-
    str_pad(
        taipei_crime$date,
        width = 7,
        side = c("left"),
        pad = "0"
    )
taipei_crime$date <- str_sub(taipei_crime$date, 1, 3)
taipei_crime$date <- as.integer(taipei_crime$date) + 1911


#觀察是否順利轉換為西元年
head(taipei_crime)

#Tidying Data
#觀察資料集之後，得知GDP資料集的年分範圍是2000年~2018年
#故犯罪率資料也將年份範圍取為2000年~2018年
taipei_crime <- filter(taipei_crime,
                       between(taipei_crime$date, 2000, 2018))


#觀察犯罪資料是否已經順利篩選出正確的時間範圍
head(taipei_crime)


#使用 mutate() 替 taipei_crime表單新增一個「地區」欄位
taipei_crime <-
    mutate(taipei_crime, area = str_sub(taipei_crime$location, 4, 6))
head(taipei_crime)


#使用arrange() 搭配 desc() 結合 group_by()函數計算出有多少區域是發生一次以上竊案的
group_by_frame <- arrange(count(group_by(taipei_crime, location)),
                          desc(count(group_by(
                              taipei_crime, location
                          ))$n))
head(group_by_frame)
cat("一共有",
    nrow(taipei_crime) - nrow(group_by_frame),
    "個區域發生一次以上竊案")
