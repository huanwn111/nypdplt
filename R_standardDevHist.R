#==人大统计公式整理：== 任务1： ex1: 编写一个函数，要求实现如下功能： （1）给一组数据（1维向量），统计出一共有多少个观测值。 （2）求出最大值、最小值、中位数、平均值、标准差 （3）生成一个直方图
#R语言版
ex1 <- function(){
	set.seed(2)#随机种子，保证每次随机是同一组数，不会每次变化
	vec1 <- rnorm(10,mean=0,sd=0.5)
	#rnorm生成正态分布向量 10个 均值为0 标准差为0.5
	#用=赋值也可
	print("打印随机正态向量vec1：")
	print(vec1)
	
	#==向量个数==
	print("向量个数：")
	sizeVector1 = length(vec1)
	print(sizeVector1 )

	#==最大值、最小值==
	print("最大值，最小值：")
	print(max(vec1))
	print(min(vec1))

	#==中位数==
	print("中位数：")
	print(median(vec1))

	#==均值==
	meanVec1 = mean(vec1)
	print("均值：")
	print(meanVec1)

	#==标准差==
	print("函数版样本标准差：")
	print(sd(vec1))
	#sd算样本标准差 var方差 cor相关系数 cov协方差 mean均值 median中位数
	
	#==数学公式手动计算标准差==
	#公式：每个（xi-x均值）的平方/样本个数 整体开平方根
	#算分子

	sUp = 0 #分子初始化
	for(xi in vec1){
		sUp = sUp + (xi-meanVec1)*(xi-meanVec1)#R语言没有+=
	}
	
	#算总体标准差：分子/样本个数N，样本标准差分母是 样本数n-1
	stdDev = sqrt(sUp/sizeVector1)
	print("总体标准差：")
	print(stdDev)
	stdDev_s = sqrt(sUp/(sizeVector1-1))
	print("样本标准差：")
	print(stdDev_s)

	#==画直方图==
	hist(vec1,xlab='x',labels=TRUE,border='black',col='blue')
	#xlab X轴标量 labels TRUE显示柱数字 border边色 col柱色
}

ex1()

#打印结果
#[1] "打印随机正态向量vec1："
# [1] -0.44845727  0.09242459  0.79392267 -0.56518784 -0.04012588  0.06621014
# [7]  0.35397736 -0.11984901  0.99223697 -0.06939351
#[1] "向量个数："
#[1] 10
#[1] "最大值，最小值："
#[1] 0.992237
#[1] -0.5651878
#[1] "中位数："
#[1] 0.01304213
#[1] "均值："
#[1] 0.1055758
#[1] "函数版样本标准差："
#[1] 0.4924938
#[1] "总体标准差："
#[1] 0.4672207
#[1] "样本标准差："
#[1] 0.4924938
