library(vcd)

set.seed(2014);y=rpois(200,5)
check <- goodfit(x=y, type="poisson", method="ML")




# Câu 1. a) Dùng chi square test: chisq.test(x,p)
# x | 0              1        2          3  4  5
# p | dpois(0,1.7)  dpois(1) dpois(2)  ..     1-dpois(0:4)

# dpois(a) = p(x=a) = f(x)

# Tần số ban đầu là fi. dpois cho ra tần số lý thuyết

# Trong hàm chisq.test, x = vector tần số cho trong bảng ban đầu. p = vector dpois

x = c(8,15,12,9,7,3)
p = c(dpois(0,1.7), dpois(1,1.7), dpois(2,1.7), dpois(3,1.7), dpois(4,1.7), 1 - sum(dpois(0:4,1.7)))
chisq.test(x=x, p=p)

# b) Hàm hợp lý của phân phối Poisson L(lambda) = f(x1) * f(xn) = e^-lambda* lambda^x1 / x1
# Xem ví dụ sách cô gửi, trang 217 (PDF 231)
# 


# Trang 216

data(mtcars)
x <- mtcars$am
L <- function(p,x) prod(dbinom(x, size = 1, prob = p))
optimize(L, interval = c(0,1), x = x, maximum = TRUE)



minuslogL <- function(p,x){
  -sum(dbinom(x, size = 1, prob = p, log = TRUE))
}
optimize(minuslogL, interval = c(0,1), x = x)





library(stats4)
data(PlantGrowth)
x <- PlantGrowth$weight
minuslogL <- function(mu, sigma2){
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE))
}
MaxLikeEst <- mle(minuslogL, start = list(mu = 5, sigma2 = 0.5))
summary(MaxLikeEst)




# Câu 2.
# B1: Kiểm tra xem X, Y có phân phối chuẩn không
# B2: Kiểm tra xem phương sai X, Y có bằng nhau không
# B3: 
# - Nếu thỏa mãn cả 2, dùng t.test(x, y). Phương sai bằng nhau thì var.equal = TRUE, không thì FALSE
# - Nếu không chuẩn, dùng kiểm định phi tham số. VD wilcox.test(x,y)


# Mỗi kiểm định, nhớ viết H0, H1. Tối thiểu là viết H0, H1
# H0: Trọng lượng trước, sau ăn kiêng không thay đổi
# H1: Trọng lượng trước, sau ăn kiêng có thay đổi

# Trình bày mọi thứ trong R. Nộp code trên máy.



# Câu 3
# Xử lý dữ liệu 1 chút
# Loại 1: 70
# Loại 2:  30
# a) Dùng hàm prop.test cho mẫu lớn, binom cho mẫu nhỏ?
# b) nhập ma trận B = 
# (70
#  25
#  5)



# Câu 4
# Test xem dữ liệu có phân phối chuẩn không. Không có thì test theo phi tham số.
# B1: kiểm tra phân phối chuẩn. Dùng shapiro.test
# B2: Kiểm tra xem phương sai X, Y có bằng nhau không
# B3:
# - Nếu thỏa mãn, dùng hàm anova.
# - Nếu không chuẩn, dùng Kruskal Wallis test. kruskal.test 
# - Phương sai không bằng nhau: dùng Welch ANOVA
# B4: Tìm ra sự khác biệt giữa các nhóm: TukeyHSD

# Câu 5
# Đọc file
# File gì? CSV
# Phân tích dữ liệu như nào? Cần chỉ ra những đặc điểm gì?