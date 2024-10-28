
library(CBCgrps)
library(nortest)
data(dt)
head(dt) 
tab1 <-twogrps(dt, gvar = "mort")
tab1
print(tab1, quote = T)
write.csv(tab1,file="tab1.csv")
#这里的核心函数为twogrps()，进行两组间比较，该函数最主要的赋值为一个待分析的数据框dt和分组变量mort，mort目前不是一个factor，仍然可以直接作为分组变量
#得出的表格用print函数来显示，注意别忘了加上quote=T，这样显示出来就是包含双引号的，方便后续直接黏贴到MS Word上，再用文字转化成表格功能进行快速制表。


tab2 <-multigrps(dt, gvar = "type")
print(tab2, quote = T)
#很多时候并不是所有的变量都要进行比较，这时候我们可以选择一部分变量进行比较，同时可以调整变量在表格中的显示顺序，用varlist参数实现该功能。
# 只比较4个变量，并且要求按照基本人口学特征（年龄、性别）、实验室检查（wbc）、治疗（vaso血管活性药物使用）的顺序显示
tabVarlist <-twogrps(dt, gvar = "mort",varlist = c("age", "gender", "wbc", "vaso"))
print(tabVarlist, quote = T)


#上面的显示中我们发现年龄保留了两位小数，似乎没必要，一般年龄保留整数或者最多小数点后一位就行了。随手一段代码就可以搞定。
tabDecimal <-twogrps(dt, gvar = "mort", norm.rd = 1)
print(tabDecimal, quote = T)

#对分类变量保留一位小数点也是轻而易举的事情。
tabCatDecimal <-twogrps(dt, gvar = "mort",cat.rd = 1)

print(tabCatDecimal, quote = T)

#默认状态下p值如果小于0.001就不现实具体数值了，这个我们也可以进行更改，比如小于0.05我们就不显示。
tabPshow <-twogrps(dt, gvar = "mort",p.rd = 2)
print(tabPshow, quote = T)

#人为设定crp和wbc为非正态分布,这时候我们可以人为设定那些变量是非正态分布的，从而采用中位数（四分位间距）来进行统计描述。
tabNormTest <-twogrps(dt, gvar = "mort",skewvar = c("crp", "wbc"))
print(tabNormTest)            

tabStatistic <-twogrps(dt, gvar = "mort", ShowStatistic = T)
print(tabStatistic)
write.csv(tabStatistic$Table,file="tabStatistic.csv")
