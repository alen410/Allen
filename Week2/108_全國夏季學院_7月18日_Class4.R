#這次會用到的資料集
#年度職災案例彙編 https://data.gov.tw/dataset/6644
#職業災害統計行業別與受傷部位統計數據 https://data.gov.tw/dataset/20592


#為何使用這兩組資料集?
#根據助教提點的兩點建議
#1.說明很詳盡，可以利用 Rmd 的特性在程式碼的外面寫說明，不用寫成註解
#2.兩個資料集沒有關聯很可惜
#因此學生再次尋找了兩個可能有關聯的資料集
#以職業災害為主題，學生曾在醫院做過志工，見過一些因為工作導致受傷的患者
#因此兩利用這兩個資料集嘗試分析出我們需要關注的職業災害種類，也許能對社會有幫助
#資料來源受限於政府開放資料集完整性(有一些下載點掛掉了)，唯有2014年的資料仍完整
#謝謝助教幫我把網址修正成可以正確顯示的網址，再次謝謝助教

#預先安裝好分析環境(如果有函式庫無法導入就使用以下的程式碼)
#install.packages("devtools")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("stringr")
#install.packages("rmarkdown")
#install.packages("rmarkdown")
#install.packages("knitr")
#install.packages("colormap")
#install.packages("wesanderson")
#install.packages("RColorBrewer")
#install.packages("treemapify")
#treemapify函式庫已經預告下個版本會合併入ggplot2內，不建議再使用
#install.packages("D:/CRAN/tmcn_0.2-12.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/tm_0.7-6.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/NLP_0.2-0.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/jiebaRD_0.1.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/jiebaR_0.10.99.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/RColorBrewer_1.1-2.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/wordcloud_2.6.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/rvest_0.3.4.zip",
#                 repos = NULL, type = "win.binary")
#install.packages("D:/CRAN/slam_0.1-45.zip",
#                 repos = NULL, type = "win.binary")

#確認安裝套件版本
packageVersion("NLP")
packageVersion("RColorBrewer")
packageVersion("colormap")
packageVersion("devtools")
packageVersion("dplyr")
packageVersion("ggplot2")
packageVersion("jiebaR")
packageVersion("rmarkdown")
packageVersion("rvest")
packageVersion("rvest")
packageVersion("scales")
packageVersion("slam")
packageVersion("stringr")
packageVersion("tidyr")
packageVersion("tidyverse")
packageVersion("tm")
packageVersion("tmcn")
packageVersion("treemapify")
packageVersion("wesanderson")
packageVersion("wordcloud")


#顯示必要資訊
R.Version()
sessionInfo()
Sys.time()
gc() #原來R語言有記憶體清理函式，慎用...。


#導入函式庫
library('NLP')
library('RColorBrewer') #關於顏色的佈景主題
library('colormap') #關於顏色的佈景主題
library('devtools')
library('dplyr')
library('ggplot2')
library('jiebaR')
library('jiebaRD')
library('rvest')
library('rvest')
library('scales')
library('slam')
library('stringr') #方便易用的資料前處理工具
library('tidyr')
library('tidyverse') #其實只要引入tidyverse庫就不用寫前三行了，不過就當作練習
library('tm')
library('tmcn') #中文文本挖掘工具包
library('treemapify') #矩形式樹狀結構繪圖法
library('wesanderson') #關於顏色的佈景主題
library('wordcloud')


#設置工作目錄，這屬於暫時性工作目錄，會依照檔案工作目錄變化來修改
setwd("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week2")


#在R語言中，如果資料是帶有繁體中文的，編碼建議使用BIG-5
#然而UTF-8編碼目前會出錯，原因待查
#年度職災案例彙編，編碼為BIG-5
job_disaster_case_compilation <- read.table(
    "年度職災案例彙編.csv",
    header = TRUE,
    sep = ",",
    na.strings = c(" "),
    row.names = NULL,
    fill = TRUE,
    encoding = "BIG-5",
)


#職業災害統計行業別與受傷部位統計數據，編碼為BIG-5
injured_parts_statistics <- read.table(
    "職業災害統計行業別與受傷部位統計數據.csv",
    header = TRUE,
    sep = ",",
    na.strings = c(" "),
    fill = TRUE,
    encoding = "BIG-5",
)





#解釋為何要先用舊的資料集，因為先求有再求好，求得好後再求精。

#職業災害致命物文字雲繪製
word_cloud_corpus <- data.frame(job_disaster_case_compilation$媒介物)

#變更欄位名稱
colnames(word_cloud_corpus) <- c("媒介物")
#word_cloud_corpus <- toString(word_cloud_corpus$媒介物)

#用table()函數幫我統計出頻率，再用sort()函數幫我做降冪排列
word_cloud_corpus <-
    sort(table(word_cloud_corpus$媒介物), decreasing = TRUE)
word_cloud_corpus <- as.data.frame(word_cloud_corpus)
colnames(word_cloud_corpus) <- c("媒介物", "頻率")

#固定亂數種子
set.seed(0)

#繪製基本文字雲
wordcloud(
    words = word_cloud_corpus$媒介物,
    freq = word_cloud_corpus$頻率,
    min.freq = 1,
    #會決定文字雲外觀的參數，要適當調整
    max.words = 100,
    #會決定文字雲外觀的參數，要適當調整
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)








