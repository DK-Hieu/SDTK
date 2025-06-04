# Câu 1
data.c1 <- data.frame(
  trong_luong = c(0.8, 0.9, 1, 1.1, 1.2),
  so_goi = c(1, 2, 5, 3, 2)
)

data.c1.vector <- rep(data.c1$trong_luong, data.c1$so_goi)

# a/ Tìm ước lượng hợp lý cực đại của giá trị trung bình và phương sai
# của trọng lượng gói hàng. 
# Có 2 cách như dưới, cần xác nhận với cô nên dùng cách nào.

# Cách 1: dùng luôn công thức toán học. Tham khảo trang 216
# sách cô gửi (Introduction to Probability and Statistic in R)
n <- length(data.c1.vector)
mu_hat <- mean(data.c1.vector)
var_hat <- var(data.c1.vector) * (n-1) / n

# Cách 2: tính máy bằng hàm tối ưu mle.
# Logic tính tương tự như câu 1b đề thi deminhhoa. Tính hàm nll, rồi đưa vào
# hàm mle
library(stats4)
mu <- mean(data.c1.vector)
sigma2 <- var(data.c1.vector)
minuslogL <- function(mu, sigma2){
  -sum(dnorm(data.c1.vector, mean = mu, sd = sqrt(sigma2), log = TRUE))
}
mle(minuslogl=minuslogL, start=list(mu=mu, sigma2=sigma2))
# mu: 1.02308039 , sigma2: 0.01254663


# b/Tìm ước lượng các giá trị trung bình và phương sai với độ tin cậy 95%.
# Lưu ý: ước lượng có độ tin cậy tức là tìm khoảng tin cậy cho tham số.
# Nếu sử dụng 1 hàm kiểm định 2 đuôi thì sẽ có thông tin về khoảng tin cậy luôn.

# Tham khảo mục Confidence Interval for means, trang 202 sách cô gửi. Code R
# ở cuối mục đó. Trong trường họp này chưa biến độ lệch chuẩn nên thay vì
# dùng z.test, dùng t.test
# Cách dùng t-test cho 1 mẫu: https://www.sthda.com/english/wiki/one-sample-t-test-in-r

# Dùng t.test để tìm ra khoảng tin cậy. t.test yêu cầu có phân phối chuẩn
# cho biến. Đề đã cho biết biến có phân phối chuẩn rồi thì có cần kiểm ra
# lại bằng shapiro.test? Xác nhận với cô.

# Kiểm tra lại phân phối chuẩn
# H0: biến có phân phối chuẩn
shapiro.test(data.c1.vector) # p-value = 0.3902. Không bác bỏ H0. Biến có phân phối chuẩn

# Tìm khoảng tin cậy cho trung bình mẫu
t.test(x=data.c1.vector, conf.level = 0.95) # [0.9526313, 1.0935225]


# Tìm khoảng tin cậy cho phương sai
# Kiểm định dùng cho phương sai là kiểm định khi bình phương (chi-squared).
# Ta có thể sử dụng kiểm định phương sai cho 1 mẫu để tìm ra khoảng tin cậy.
# Ref: https://search.r-project.org/CRAN/refmans/EnvStats/html/varTest.html
library(EnvStats)
varTest(x=data.c1.vector, conf.level= 0.95) # [0.006988013, 0.037031052]


# Câu 2
data.c2.vector <- c(30.1, 32.7, 22.5, 27.5, 27.7, 29.8, 28.9, 31.4, 31.2, 24.3, 26.4, 22.8, 29.1, 33.4, 32.5, 21.7)

# Dựa vào số liệu nói trên có thể cho biết khẳng định của công ty là
# có căn cứ hay không với mức ý nghĩa 5%? 

# Về căn bản, đề bài hỏi là giá trị trung bình của biến khả năng chịu lực có
# phải = 30 không? Thực hiện kiểm định giá trị trung bình cho 1 mẫu. Sử dụng
# t-test cho 1 mẫu.

# Kiểm tra giả định về phân phối chuẩn
# H0: dữ liệu có phân phối chuẩn
shapiro.test(x=data.c2.vector) # p-value = 0.2668. Không bác bỏ H0. Có phân phối chuẩn.

# Thực hiện kiểm định t.test
# H0: mu = 30
# H1: mu < 30
t.test(
  x=data.c2.vector,
  mu = 30,
  conf.level = 1 - 0.05,
  alternative = "less"
) # p-value = 0.04231

# p-value < alpha. Bác bỏ H0. Vậy khả năng chịu lực bé hơn 30 psi



# b/ Hãy ước lượng tỷ lệ đạt chuẩn của công ty theo mẫu trên với α=5%. 
# Tương tự như câu 1b, ước lượng có mức ý nghĩa tức là ước lượng khoảng tin cậy.
# Nếu sử dụng 1 hàm kiểm định 2 đuôi thì sẽ có thông tin về khoảng tin cậy.
# Ở đây dùng kiểm định tỷ lệ cho 1 mẫu.
# Ref: https://www.sthda.com/english/wiki/one-proportion-z-test-in-r

# Kiểm tra giả định: n*p0 > 5 và n*q > 5.
n.c2 <- length(data.c2.vector)
n.c2.success <- length(data.c2.vector[data.c2.vector >= 30])
p0 = n.c2.success / n.c2
q = 1 - p0
n.c2 * p0 > 5 # TRUE
n.c2 * q > 5 # TRUE

# Vậy có thể dùng xấp xỉ phân phối chuẩn. Sử dụng prop.test.
# Lưu ý: sử dụng correct = FALSE để cho biết rằng đang sử dụng xấp xỉ
# phân phối chuẩn cho phân phối nhị thức

# Tính khoảng tin cậy
prop.test(x=n.c2.success, n=n.c2, correct=FALSE) # [0.1848123, 0.6135896]


# c/ Hãy so sánh sức chịu lực của sản phẩm mới với sản phẩm cũ
# mà công ty đang bán trên thị trường 
data.c2.c.vector <- c(22.5, 30.5, 25.4, 32.1, 32.9, 31.7, 30.2, 24.8, 25.3, 22.8,  29, 32, 27)

# So sánh sức chịu lực tức là so sánh giá trị trung bình của sức chịu lực giữa
# mẫu sản phẩm mới và mẫu sản phẩm cũ. Như vậy áp dụng kiểm định t-test cho 2 mẫu.
# Vì 2 mẫu không liên quan nhau (không phải trước và sau) nên sử dụng unpaired t-test.
# Ref: https://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

# Kiểm định giả định về phân phối chuẩn
# H0: dữ liệu có phân phối chuẩn
shapiro.test(x=data.c2.vector) #p-value 0.2668 > 0.05. Không bác bỏ H0.
shapiro.test(x=data.c2.c.vector) #p-value 0.1685 > 0.05. Không bác bỏ H0.

# Vậy cả 2 dữ liệu đều có phân phối chuẩn.

# Kiểm định giả định về phương sai bằng nhau.
# H0: sigma2_1 = sigma2_2
var.test(x=data.c2.vector, y=data.c2.c.vector)
# p-value=0.9518 > 0.05. Không bác bỏ H0. Vậy phương sai bằng nhau.

# Thực hiện kiểm định t-test 2 mẫu.
# Lưu ý sử dụng tham số var.equal=TRUE . Nếu không dùng thì mặc định sẽ là FALSE,
# và hàm sẽ sử dụng xấp xỉ Welch

# H0: mu_1 = mu_2
t.test(x=data.c2.vector, y=data.c2.c.vector, var.equal=TRUE)
# p-value = 0.9545 > 0.05. Không bác bỏ H0. Vậy sức chịu lực của sản phẩm cũ
# bằng sức chịu lực của sản phẩm mới.


# Câu 3
data.c3 <- data.frame(
  ngo = c(17,	18,	28,	28,	28,	22,	17),
  dau_tuong = c(17,	18,	19,	30,	20,	19,	18),
  lua_mi = c(13, 23,	24,	25,	24,	13,	21)
)
# Biến đổi dữ liệu. Hiện tại dữ liệu đang ở dạng wide-form (pivoted form). Cần
# chuyển về dạng long-form (unpivoted form).
# Ref: https://stackoverflow.com/a/2185525/15421880

library(reshape2)
data.c3.long <- melt(
  data.c3,
  variable.name = "plant_type",
  value.name = "output"
)

# Hãy kiểm tra xem sản lượng trung bình của ba loại nông sản trên có giống nhau không?
# Nếu khác nhau hãy tìm ra các cặp khác nhau đó. α=5%.


# Để kiểm tra giá trị trung bình cho nhiều hơn 2 nhóm, ta dùng ANOVA
# Vì các nhóm ở đây đều nằm trong 1 thuộc tính (1 factor) là loại cây,
# nên ta dùng ANOVA one-way
# Ref: https://www.sthda.com/english/wiki/one-way-anova-test-in-r


# Đầu tiên, kiểm tra các giả định của ANOVA:
# - Dữ liệu mỗi nhóm có phân phối chuẩn
# - Dữ liệu mỗi nhóm có chung phương sai

# Dữ liệu mỗi nhóm có phân phối chuẩn
# H0: dữ liệu có phân phối chuẩn
shapiro.test(x=data.c3$ngo) #p-value = 0.02799
shapiro.test(x=data.c3$dau_tuong) #p-value = 0.001287
shapiro.test(x=data.c3$lua_mi) #p-value = 0.02085

# Tất cả đều có p-value < 0.05, do đó bác bỏ H0. Dữ liệu không có phân phối chuẩn.
# Như vậy ở đây giả thiết của ANOVA không thỏa mãn. Ta chuyển sang dùng
# kiểm định Kruskal-Wallis rank sum test.
# Ref:
# - https://www.sthda.com/english/wiki/one-way-anova-test-in-r#non-parametric-alternative-to-one-way-anova-test
# - https://www.sthda.com/english/wiki/kruskal-wallis-test-in-r

# H0: Giá trị sản lượng trung vị giữa 3 nhóm là bằng nhau.
kruskal.test(output ~ plant_type, data = data.c3.long) # p-value = 0.7868

# p-value > 0.05. Không bác bỏ H0. Vậy sản lượng trung vị giữa 3 nhóm là bằng nhau.


# Câu 4

# Tạo dữ liệu và append dòng thành dataframe
data.c4.tren_trung_binh <- data.frame(
  hoc_luc = c("tren_trung_binh", "tren_trung_binh", "tren_trung_binh"),
  y_kien = c("tot", "vua", "xau"),
  so_luong = c(175, 	124, 	92)
)

data.c4.trung_binh <- data.frame(
  hoc_luc = c("trung_binh", "trung_binh", "trung_binh"),
  y_kien = c("tot", "vua", "xau"),
  so_luong = c(118, 	110,	126)
)

data.c4.duoi_trung_binh <- data.frame(
  hoc_luc = c("duoi_trung_binh", "duoi_trung_binh", "duoi_trung_binh"),
  y_kien = c("tot", "vua", "xau"),
  so_luong = c(127, 	82,	147)
)

data.c4 <- rbind(data.c4.tren_trung_binh, data.c4.trung_binh, data.c4.duoi_trung_binh)

# Hãy kiểm chứng ở mức ý nghĩa 5% xem tỷ lệ học sinh
# “đánh giá phương pháp giáo dục mới là tốt” có như nhau trong 3 nhóm
# học sinh trên hay không? Nhóm nào có tỷ lệ đánh giá này là cao nhất.

# Ở đây, đầu tiên ta cần tính toán tỷ lệ ý kiến tốt ở mỗi nhóm học lực,
# sau đó thực hiện kiểm định tỷ lệ bằng nhau giữa các nhóm

# Tính toán tỷ lệ ý kiến tốt ở mỗi nhóm học lực
library(dplyr)
data.c4.updated <- 
  data.c4 %>%
  group_by(hoc_luc) %>%
  mutate(tong_y_kien = sum(so_luong)) %>%
  mutate(ty_le = so_luong / tong_y_kien)

# Sử dụng kiểm định tỷ lệ 2 mẫu
# Ref: https://www.sthda.com/english/wiki/two-proportions-z-test-in-r

# Kiểm tra giả thiết về xấp xỉ phân phối chuẩn: với từng mẫu A và B:
# nA * p > 5 , nA * q > 5. Trong đó nA là số quan sát trong mẫu A,
# p là tỷ lệ thành công trên TỔNG QUAN SÁT (tỷ lệ trên cả A và B).
# q là tỷ lệ thất bại trên tổng quan sát. Tương tự với mẫu B.

data.c4.tot <- data.c4.updated %>%
  filter(y_kien == 'tot')
p <- sum(data.c4.tot$so_luong) / sum(data.c4.tot$tong_y_kien)
q = 1-p

n.duoi <- data.c4.tot %>%
  filter(hoc_luc == 'duoi_trung_binh') %>%
  pull(tong_y_kien)
n.tb <- data.c4.tot %>%
  filter(hoc_luc == 'trung_binh') %>%
  pull(tong_y_kien)
n.tren <- data.c4.tot %>%
  filter(hoc_luc == 'tren_trung_binh') %>%
  pull(tong_y_kien)

n.duoi.success <- data.c4.tot %>%
  filter(hoc_luc == 'duoi_trung_binh') %>%
  pull(so_luong)
n.tb.success <- data.c4.tot %>%
  filter(hoc_luc == 'trung_binh') %>%
  pull(so_luong)
n.tren.success <- data.c4.tot %>%
  filter(hoc_luc == 'tren_trung_binh') %>%
  pull(so_luong)

n.duoi * p > 5 # TRUE
n.tb * p > 5 # TRUE
n.tren * p > 5 # TRUE
n.duoi * q > 5 # TRUE
n.tb * q > 5 # TRUE
n.tren * q > 5 # TRUE


# Điều kiện được thỏa mãn. Ta có thể sử dụng proportion test
# Kiểm định xem tỷ lệ ý kiến tốt giữa các nhóm học lực có khác nhau không
# H0: p.duoi = p.tb = p.tren
# H1: Có ít nhất 1 nhóm học lực có tỷ lệ khác nhau
prop.test(
  x=c(n.duoi.success, n.tb.success, n.tren.success),
  n=c(n.duoi, n.tb, n.tren),
  conf.level = 1 - 0.05
)

# p-value = 0.002968 < 0.05. Bác bỏ H0. Vậy có ít nhất 1 nhóm có tỷ lệ ý kiến tốt
# khác các nhóm kia.

# Kiểm tra từng cặp
# Dưới TB và TB
# H0: p.duoi = p.tb
# H1: p.duoi > p.tb
prop.test(
  x=c(n.duoi.success, n.tb.success),
  n=c(n.duoi, n.tb),
  alternative="greater",
  conf.level = 1 - 0.05
)

# p-value = 0.2819 > 0.05. Không bác bỏ H0. Vậy p.duoi = p.tb

# ---
# # Trên TB và TB
# H0: p.tren = p.tb
# H1: p.tren > p.tb
prop.test(
  x=c(n.tren.success, n.tb.success),
  n=c(n.tren, n.tb),
  alternative="greater",
  conf.level = 1 - 0.05
)

# p-value = 0.0009273 < 0.05. Bác bỏ H0. Vậy tỷ lệ ý kiến tốt ở nhóm trên trung bình
# lớn hơn nhóm trung bình


# ---
# # Trên TB và dưới TB
# H0: p.tren = p.duoi
# H1: p.tren > p.duoi
prop.test(
  x=c(n.tren.success, n.duoi.success),
  n=c(n.tren, n.duoi),
  alternative="greater",
  conf.level = 1 - 0.05
)

# p-value = 0.007107 < 0.05. Bác bỏ H0. Vậy tỷ lệ ý kiến tốt ở nhóm trên trung bình
# lớn hơn nhóm dưới trung bình

# Vậy nhóm học lực trên trung bình có tỷ lệ đánh giá ý kiến tốt cao nhất.