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
#install.packages("knitr")
#install.packages("colormap")
#install.packages("wesanderson")
#install.packages("RColorBrewer")
#install.packages("treemapify")
#install.packages("miniCRAN")
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
packageVersion("miniCRAN")
packageVersion("rmarkdown")
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
library('miniCRAN')
library('rvest')
library('scales')
library('slam')
library('stringr') #方便易用的資料前處理工具
library('tidyr')
library('tidyverse') #其實只要引入tidyverse庫就不用寫前三行了，不過就當作練習
library('tm') #英文文本挖掘工具包
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

#用table()函數幫我統計出頻率，再用sort()函數幫我做降冪排列
word_cloud_corpus <-
    sort(table(word_cloud_corpus$媒介物), decreasing = TRUE)
word_cloud_corpus <- as.data.frame(word_cloud_corpus)
colnames(word_cloud_corpus) <- c("媒介物", "頻率")

#固定亂數種子
set.seed(146)

#繪製基本文字雲1
wordcloud(
    words = word_cloud_corpus$媒介物,
    freq = word_cloud_corpus$頻率,
    min.freq = 1,
    #會決定文字雲外觀的參數，要適當調整
    max.words = 100,
    #會決定文字雲外觀的參數，要適當調整
    random.order = FALSE,
    rot.per = 0,
    #要旋轉90度的比例，看個人喜好，設為0也可以
    colors = brewer.pal(12, "Paired"),
)

#文字雲中的色彩參數可以透過該指令查詢更換
brewer.pal.info

#可以看出藍色字體之外的物品都是比較容易造成職業災害的物品


#職業災害類型文字雲繪製
word_cloud_corpus <- data.frame(job_disaster_case_compilation$災害類型)

#變更欄位名稱
colnames(word_cloud_corpus) <- c("災害類型")

#移除標點符號
word_cloud_corpus$災害類型        <-
    str_remove_all(word_cloud_corpus$災害類型, "[、]")


#用table()函數幫我統計出頻率，再用sort()函數幫我做降冪排列
word_cloud_corpus <-
    sort(table(word_cloud_corpus$災害類型), decreasing = TRUE)
word_cloud_corpus <- as.data.frame(word_cloud_corpus)
colnames(word_cloud_corpus) <- c("災害類型", "頻率")


#繪製基本文字雲2
wordcloud(
    words = word_cloud_corpus$災害類型,
    freq = word_cloud_corpus$頻率,
    min.freq = 1,
    #會決定文字雲外觀的參數，要適當調整
    max.words = 100,
    #會決定文字雲外觀的參數，要適當調整
    random.order = FALSE,
    rot.per = 0,
    #要旋轉90度的比例，看個人喜好，設為0也可以
    colors = brewer.pal(12, "Paired"),
)


#R語言套件管理工具CRAN的文字雲
#幾乎每個方便好用的函式庫大多都是由其他函式庫組合而成
#此類的相依性關係也可以繪製成文字雲
pkgDep("devtools", availPkgs = cranJuly2014)

#例如devtools是重要的套件包，因此帶有21個相依套件
pkg_dep_graph <- makeDepGraph("devtools",
                              enhances = TRUE,
                              availPkgs = cranJuly2014)
plot(pkg_dep_graph,
     legendPosition = c(-1, 1),
     vertex.size = 10) #vertex.size是主要套件包的圖示尺寸


#這次使用到的函式庫
used_pkg <- c(
    "NLP",
    "RColorBrewer",
    "colormap",
    "devtools",
    "dplyr",
    "ggplot2",
    "jiebaR",
    "miniCRAN",
    "rmarkdown",
    "rvest",
    "scales",
    "slam",
    "stringr",
    "tidyr",
    "tidyverse",
    "tm",
    "tmcn",
    "treemapify",
    "wesanderson",
    "wordcloud"
)

#有一些函式庫會沒辦法在cranJuly2014內找到
pkgDep(used_pkg, availPkgs = cranJuly2014)


#原因待查，就先畫圖
pkg_dep_graph <- makeDepGraph(used_pkg,
                              enhances = TRUE,
                              availPkgs = cranJuly2014)
plot(pkg_dep_graph,
     legendPosition = c(-1, 1),
     vertex.size = 10) #vertex.size是主要套件包的圖示尺寸

#使用的函式庫越多，就會越像「相依性地獄」


#TF-IDF相關知識練習

#將文字檔案讀入 https://forum.jorsindo.com/portal.php

motor_article_1 <- 
    readLines("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week2/小老婆汽機車資訊網語料庫/1.txt")
motor_article_1 <- toString(motor_article_1)

motor_article_2 <- 
    readLines("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week2/小老婆汽機車資訊網語料庫/2.txt")
motor_article_2 <- toString(motor_article_2)

motor_article_3 <- 
    readLines("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week2/小老婆汽機車資訊網語料庫/3.txt")
motor_article_3 <- toString(motor_article_3)

motor_article_4 <- 
    readLines("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week2/小老婆汽機車資訊網語料庫/4.txt")
motor_article_4 <- toString(motor_article_4)

motor_article_5 <- 
    readLines("C:/Users/user/Documents/GitHub/National-Summer-Academy/Week2/小老婆汽機車資訊網語料庫/5.txt")
motor_article_5 <- toString(motor_article_5)


#使用jiebaR斷詞然後轉為文字矩陣
#先串接全部的文章

word_segment <- worker()
motor_article_all <- paste(
    motor_article_1,
    motor_article_2,
    motor_article_3,
    motor_article_4,
    motor_article_5
)

segmented_article <- segment(code = motor_article_all,
                             jiebar = word_segment,
                             mod = "hmm")
print(segmented_article)
