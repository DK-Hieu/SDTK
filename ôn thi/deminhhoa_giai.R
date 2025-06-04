# File deminhoa.docx

# Câu 1
data.c1 = data.frame(
  x = c(0, 1, 2, 3, 4, 5),
  m = c(8, 15, 12, 9, 7, 3)
)
# Biểu diễn dữ liệu dưới dạng vector
data.c1.vector = rep(data.c1$x, data.c1$m)

# a) Kiểm tra xem X có phân bố Poisson(1.7) không với mức ý nghĩa 5%

# Lưu ý: ta có thể kiểm tra xem tập dữ liệu đã cho có phân phối A không
# bằng cách sử dụng goodness of fit test (một loại chi-square test).
# Goodness of fit test cho biết mức độ phù hợp của phân phối A với
# phân phối thực sự của dữ liệu. Chi tiết về goodness of fit nằm ở
# chương 7.6.3 của sách giáo khoa cô gửi đầu kỳ, hoặc tại đây:
# https://whitlockschluter3e.zoology.ubc.ca/Tutorials%20using%20R/R_tutorial_Frequency_data.html

# Có 2 cách sử dụng goodness of fit test: sử dụng luôn hàm chisq.test,
# hoặc thực hiện thông qua hàm goodfit từ thư viện vcd,
# Sau đây sẽ trình bày cả 2 cách

## Giải. Cách 1: chisq.test
# Cách sử dụng chisq.test cho goodness of fit test: sử dụng hàm
# và các tham số như sau: chisq.test(x=data, p=prob)
# Trong đó:
# - data là biến chứa danh sách các giá trị khả thi của dữ liệu.
# - prob là biến chứa danh sách xác suất tương ứng nếu dữ liệu có phân phối A
# Như vậy, data đã được cho trước. Ta cần phải tính ra biến prob để đưa vào.
# Tùy vào loại phân phối mình muốn kiểm tra, sử dụng hàm d... để tính ra
# xác suất tương ứng (dnorm - phân phối chuẩn, dpois - phân phối poisson, ...).
# Để biết cách sử dụng hàm d... tương ứng, sử dụng ?d... Ví dụ ?dpois


# Lambda đề đã cho
lambda <- 1.7

# Tính toán vector xác suất khi dữ liệu có phân phối Poisson.
# Cách dùng hàm dpois: dpois(x, lambda) với số nguyên x là giá trị
# đầu ra xuất hiện trong dữ liệu. Nếu dữ liệu có phân phối poisson,
# hàm dpois trả ra xác suất trả về giá trị đầu ra đó.
# Lưu ý: đối với phân phối cho biến rời rạc, hàm d*** (VD: dbinom, dpois)
# sẽ trả ra xác suất tại 1 điểm. Với các phân phối cho biến liên tục, thì
# hàm d*** tương ứng sẽ trả ra mật độ, thay vì xác suất (VD dnorm). Xem
# sự khác nhau giữa xác suất và mật độ tại đây:
# https://stats.stackexchange.com/questions/336166/what-is-the-difference-between-dbinom-and-dnorm-in-r

# Có 2 cách để tạo biến prob
# Cách 1 là truyền vào từng số nguyên như dưới.
# Với giá trị cuối cùng (x=5), ta lấy 1 - tổng xác suất các giá trị khác 5
# để đảm bảo tổng xác suất tất cả giá trị là 1.
prob1 <- c(
  dpois(x = 0, lambda = lambda),
  dpois(x = 1, lambda = lambda),
  dpois(x = 2, lambda = lambda),
  dpois(x = 3, lambda = lambda),
  dpois(x = 4, lambda = lambda),
  1 - sum(dpois(x = 0:4, lambda = lambda))
)


# Cách 2 là truyền vào cả biến vector. Ở cách này, sau khi truyền cần phải
# điều chỉnh xác suất của giá trị x = 5 với cùng công thức ở trên.
# Sau khi chạy xong, bạn có thể in output để xác nhận prob1 và prob2 giống nhau.
prob2 <- dpois(x = data.c1$x, lambda = lambda)


# Tính tổng xác suất các giá trị, trừ giá trị cuối
prob2_sum = sum(prob2[1: length(prob2) - 1])
# Cập nhật xác suất giá trị cuối
prob2[length(prob2)] = 1 - prob2_sum



# Thực hiện kiểm định.
# LƯU Ý QUAN TRỌNG: phải để x = data, p = prob, nếu không kết quả sẽ ra sai.
# Ví dụ: nếu bạn để chisq.test(data.c1$x, prob2), thì thực chất câu lệnh này
# tương đương chisq.test(x = data.c1$x, y = prob2). Như vậy ở đây không phải
# goodness of fit test nữa, mà sẽ là 1 loại kiểm định khác.

# H0: Dữ liệu có phân phối Poisson. Tức là xác suất của các giá trị tương ứng
# với xác suất giả thiết. Tức là:
# p(0) = prob2[1] = 0.183
# p(1) = prob2[2] = 0.311
# ...
chisq.test(x = data.c1$m, p = prob2)

# Kết quả:
# Chi-squared = 5.9345, p-value = 0.3126 > 0.05
# Kết luận: Không bác bỏ H0. Dữ liệu có phân phối Poisson.


# Cách 2: dùng hàm goodfit từ thư viện vcd
library(vcd)
summary(goodfit(
  x = data.c1.vector,
  type = "poisson",
  method = "MinChisq",
  par = list(lambda=lambda)
))

data.c1.reverse = data.frame(
  m = c(8, 15, 12, 9, 7, 3),
  x = c(0, 1, 2, 3, 4, 5)
)
summary(goodfit(
  x = data.c1.reverse,
  type = "poisson",
  method = "MinChisq",
  par = list(lambda=lambda)
))

# Ra kết quả y hệt như cách 1.





# b/ Giả sử rằng số cuộc điện thoại gọi đến là một biến ngẫu nhiên có phân bố
# Poisson(). Hãy tìm ước lượng hợp lý cực đại cho tham số từ dữ liệu trên.

# Ước lượng hợp lý cực đại lambda mũ của phân phối Poisson là giá trị trung bình
# của tập dữ liệu. Do đó:
mle_lambda = mean(data.c1.vector)

# Cách giải khác không dùng lối tắt
# B1: tính tổng hàm log-likelihood
# Hàm d... sẽ tính ra giá trị hợp lý tại 1 điểm. Lưu ý giá trị input của hàm d...
# phải là vector giá trị thực. 
# Có thể dùng hàm log-likelihood hoặc hàm log-likelihood âm (negative log likelihood
# or NLL). Theo thông lệ, dùng NLL
# B2: tìm giá trị tối ưu (aka giá trị lớn nhất/nhỏ nhất của hàm log-likelihood)
# Tham số interval sẽ chứa danh sách các giá trị có thể xuất hiện trong tập dữ liệu.
# Nói cách khác, đó là danh sách giá trị distinct trong tập dữ liệu mẫu.
minusLogLPois <- function(l, x) {
  -sum(dpois(x, lambda = l, log=TRUE))
}
optimize(minusLogLPois, interval=data.c1$x, x=data.c1.vector)

# Kết quả của mle_lambda và minimum của optimize sẽ là giống nhau, 2.018519
# Lưu ý hàm optimize chỉ dùng được cho phân phối 1 tham số. Nếu nhiều tham số
# thì phải dùng hàm mle từ thư viện stats4.

#-------------------------------------------------------------

# Câu 2
data.c2 = data.frame(
  before = c(65,	70,	80,	71,	73,	72,	65,	68,	67,	69,	70,	71,	90,	89,	89),
  after = c(59,	62,	79,	70,	66,	72,	60,	61,	66,	65,	70,	72,	75,	80,	88)
)

# a) Với mức ý nghĩa 5% có thể kết luận rằng chế độ ăn kiêng 
# có tác dụng làm giảm trọng lượng hay không?
# Trong trường hợp này, sử dụng 2-sample t-test.
# Có 2 loại 2-sample t-test là paired và unpaired. Khác nhau như nào, khi nào chọn
# loại nào: https://www.youtube.com/watch?v=rH9-BbBlXgQ
# Ở đây, ta dùng paired t-test. Tham khảo: https://www.sthda.com/english/wiki/paired-samples-t-test-in-r

# Đầu tiên, cần kiểm tra xem dữ liệu có phân phối chuẩn không, sử dụng Shapiro-wilk test.
# Nếu có, thực hiện t-test.
# Nếu không, dùng kiểm định phi tham số như wilcox.test

# Kiểm tra phân phối chuẩn
shapiro.test(data.c2$before) # p-value = 0.004
shapiro.test(data.c2$after) # p-value = 0.46
shapiro.test(data.c2$after - data.c2$before) # p-value = 0.08

# H0: dữ liệu có phân phối chuẩn
# Hỏi lại cô. Ở đây p-value cho hiệu là 8% nên không bác bỏ H0. Dữ liệu có phân phối chuẩn

# Kiểm định paired t-test 
# Ref thứ tự before, after: https://stackoverflow.com/questions/64998984/what-does-r-assume-regarding-order-in-paired-t-test
t.test(
  x=data.c2$after,
  y=data.c2$before,
  paired=TRUE,
  alternative = "less",
  conf.level = 1 - 0.05
)

# H0: after = before
# H1: after < before
# p-value = 0.001. Bác bỏ H0. After < before.


# b) Hãy tìm khoảng tin cậy 90% cho sự chênh lệch giữa trọng lượng trước và sau khi ăn kiêng.
# Ref: https://www.geeksforgeeks.org/how-to-find-confidence-intervals-in-r/
res = t.test(
  x=data.c2$after,
  y=data.c2$before,
  paired=TRUE,
  conf.level = 1 - 0.10
)
res$conf.int
# [-6.289284 -2.244049]



#-------------------------------------------------------------

# Câu 3


# Tạo dữ liệu và append dòng thành dataframe
data.c3.loai_1 <- data.frame(
  chat_luong = rep("loai_1", 3),
  phan_xuong = c("xuong_1", "xuong_2", "xuong_3"),
  so_luong = c(70,	80,	60)
)

data.c3.loai_2 <- data.frame(
  chat_luong = rep("loai_2", 3),
  phan_xuong = c("xuong_1", "xuong_2", "xuong_3"),
  so_luong = c(25,	20,	15)
)

data.c3.loai_3 <- data.frame(
  chat_luong = rep("loai_3", 3),
  phan_xuong = c("xuong_1", "xuong_2", "xuong_3"),
  so_luong = c(5,	10,	5)
)

data.c3 <- rbind(data.c3.loai_1, data.c3.loai_2, data.c3.loai_3)
cont.table.c3 <- xtabs(so_luong ~ chat_luong + phan_xuong, data=data.c3)

# a/ Hãy so sánh tỷ lệ sản phẩm loại 1 của 3 phân xưởng trên với mức ý nghĩa 10%.

# Ở đây, đầu tiên ta cần tính toán tỷ lệ sản phẩm loại 1 ở mỗi phân xưởng,
# sau đó thực hiện kiểm định tỷ lệ bằng nhau giữa các nhóm

# Tính toán tỷ lệ sản phẩm loại 1 ở mỗi phân xưởng
library(dplyr)
data.c3.a <- 
  data.c3 %>%
  group_by(phan_xuong) %>%
  mutate(tong_san_pham = sum(so_luong)) %>%
  mutate(ty_le = so_luong / tong_san_pham) %>%
  arrange(phan_xuong, chat_luong)

# Sử dụng kiểm định tỷ lệ 2 mẫu
# Ref: https://www.sthda.com/english/wiki/two-proportions-z-test-in-r

# Kiểm tra giả thiết về xấp xỉ phân phối chuẩn: với từng mẫu A và B:
# nA * p > 5 , nA * q > 5. Trong đó nA là số quan sát trong mẫu A,
# p là tỷ lệ thành công trên TỔNG QUAN SÁT (tỷ lệ trên cả A và B).
# q là tỷ lệ thất bại trên tổng quan sát. Tương tự với mẫu B.

data.c3.loai_1 <- data.c3.a %>%
  filter(chat_luong == 'loai_1')
p <- sum(data.c3.loai_1$so_luong) / sum(data.c3.loai_1$tong_san_pham)
q = 1-p

n.1 <- data.c3.loai_1 %>%
  filter(phan_xuong == 'xuong_1') %>%
  pull(tong_san_pham)
n.2 <- data.c3.loai_1 %>%
  filter(phan_xuong == 'xuong_2') %>%
  pull(tong_san_pham)
n.3 <- data.c3.loai_1 %>%
  filter(phan_xuong == 'xuong_3') %>%
  pull(tong_san_pham)

n.1.success <- data.c3.loai_1 %>%
  filter(phan_xuong == 'xuong_1') %>%
  pull(so_luong)
n.2.success <- data.c3.loai_1 %>%
  filter(phan_xuong == 'xuong_2') %>%
  pull(so_luong)
n.3.success <- data.c3.loai_1 %>%
  filter(phan_xuong == 'xuong_3') %>%
  pull(so_luong)

n.1 * p > 5 # TRUE
n.2 * p > 5 # TRUE
n.3 * p > 5 # TRUE
n.1 * q > 5 # TRUE
n.2 * q > 5 # TRUE
n.3 * q > 5 # TRUE

# Điều kiện được thỏa mãn. Ta có thể sử dụng proportion test
# Kiểm định xem tỷ lệ sản phẩm loại 1 giữa các phân xưởng có khác nhau không
# H0: p.1 = p.2 = p.3
# H1: Có ít nhất 1 phân xưởng có tỷ lệ khác nhau
prop.test(
  x=c(n.1.success, n.2.success, n.3.success),
  n=c(n.1, n.2, n.3),
  conf.level = 1 - 0.1
)

# p-value = 0.7539 > 0.1. Không bác bỏ H0. Vậy không có sự khác nhau về
# tỷ lệ sản phẩm loại 1 giữa các phân xưởng.

# b/ Có sự độc lập giữa chất lượng sản phẩm và nơi làm ra nó hay không? α=0.05 
# Sử dụng kiểm định khi bình phương (chi-squared) về biến phụ thuộc
# Ref: https://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

# Đầu tiên, cần tạo bảng 2 chiều (contingency table). Ta đã làm ở trên
# Thực hiện kiểm định
# H0: các biến độc lập
chisq.test(cont.table.c3)
# p-value = 0.5795 > 0.05. Không bác bỏ H0. Vậy có sự độc lập giữa chất lượng
# sản phẩm và nơi sản xuất.



#-------------------------------------------------------------


# Câu 4
data.c4.loai_1 <- data.frame(
  loai_chat_doc = rep("loai_1", 4),
  loai_phuong_phap = c("A", "B", "C", "D"),
  thoi_gian = c(3.1,	8.2,	4.3,	4.5)
)

data.c4.loai_2 <- data.frame(
  loai_chat_doc = rep("loai_2", 4),
  loai_phuong_phap = c("A", "B", "C", "D"),
  thoi_gian = c(4.5,	8.8,	6.3,	6.6)
)

data.c4.loai_3 <- data.frame(
  loai_chat_doc = rep("loai_3", 4),
  loai_phuong_phap = c("A", "B", "C", "D"),
  thoi_gian = c(4.6,	11,	4.5,	7.1)
)

data.c4.loai_4 <- data.frame(
  loai_chat_doc = rep("loai_4", 4),
  loai_phuong_phap = c("A", "B", "C", "D"),
  thoi_gian = c(4.3,	7.2,	7.6,	6.2)
)

data.c4.loai_5 <- data.frame(
  loai_chat_doc = rep("loai_5", 4),
  loai_phuong_phap = c("A", "B", "C", "D"),
  thoi_gian = c(3.6,	9.2,	4.4,	5.6)
)

data.c4 <- rbind(
  data.c4.loai_1,
  data.c4.loai_2,
  data.c4.loai_3,
  data.c4.loai_4,
  data.c4.loai_5
)

#  Với α=5% tiến hành phân tích phương sai (giả sử các điều kiện của tiêu chuẩn ANOVA thỏa
# mãn). Nêu bảng phân tích ANOVA và kết luận có thể thu được; tiến hành phân tích sâu
# để xem xét sự khác biệt giữa các nhóm cụ thể.


# Ở đây có 2 nhóm thuộc tính (2 factor) nên ta sẽ dùng two-way ANOVA.
# Ref: https://www.sthda.com/english/wiki/two-way-anova-test-in-r

# Giả định của ANOVA: theo như link tham khảo + thông tin từ sách giáo khoa
# cô gửi đầu kỳ, trang 513 (trang thật của sách), giả định của ANOVA gồm:
# - Phương sai giữa các mẫu không thay đổi
# - Mỗi mẫu đều có phân phối chuẩn

# Trong link tham khảo, cách test gợi ý với 2 giả định trên:
# - Mẫu có phân phối chuẩn: nếu phần dư có phân phối chuẩn
# - Phương sai bằng nhau: kiểm tra biểu đồ phần dư so với giá trị dự báo.



# Sách giáo khoa có trình bày cách kiểm tra giả định với one-way ANOVA.
# - Mẫu có phân phối chuẩn: xem QQ plot của từng mẫu, tức là kiểm tra phân phối
# chuẩn của từng mẫu.
# - Phương sai bằng nhau: thực hiện kiểm định Levene. Khác với F-test,
# Levene test có thể dùng cho nhiều nhóm cùng lúc. Như vậy ta có thể thực hiện 1 lần.
# Lưu ý. Theo như link tham khảo này: https://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r,
# ta có thể dùng Barlette test nếu dữ liệu có phân phối chuẩn. Nếu không theo
# phân phối chuẩn thì dùng Levene test.



# Ta sẽ thử cả 2 cách gợi ý trên. Xác nhận lại với cô cách nào ổn.
# Cách 1: Từ sách giáo khoa

# Kiểm tra phân phối chuẩn
# H0: dữ liệu có phân phối chuẩn
shapiro.test(x=data.c4[data.c4["loai_phuong_phap"] == "A", ]$thoi_gian) # p-value = 0.3271
shapiro.test(x=data.c4[data.c4["loai_phuong_phap"] == "B", ]$thoi_gian) # p-value = 0.8792
shapiro.test(x=data.c4[data.c4["loai_phuong_phap"] == "C", ]$thoi_gian) # p-value = 0.0999
shapiro.test(x=data.c4[data.c4["loai_phuong_phap"] == "D", ]$thoi_gian) # p-value = 0.8412

# p-value đều > 0.05. Không bác bỏ H0. Dữ liệu có phân phối chuẩn

# Kiểm tra phương sai bằng nhau. Sử dụng Barlette test
# H0: phương sai giữa các nhóm bằng nhau
# H1: có ít nhất 1 nhóm có phương sai khác nhau
bartlett.test(formula=thoi_gian ~ loai_phuong_phap, data=data.c4)
# p-value = 0.4465 > 0.05. Không bác bỏ H0. Vậy phương sai bằng nhau.


# Cách 2: từ link tham khảo
# Ở đây, ta tạo vật thể anova trước, sau đó lấy thông tin từ đó để kiểm tra
c4.aov <- aov(
  thoi_gian ~ loai_chat_doc + loai_phuong_phap,
  data=data.c4
  )

# Kiểm tra phân phối chuẩn trên phần dư
# H0: phần dư có phân phối chuẩn
shapiro.test(x = residuals(c4.aov) )
# p-value = 0.1016 > 0.05. Không bác bỏ H0. Phần dư có phân phối chuẩn

# Kiểm tra đồ thị phần dư và giá trị dự báo
plot(c4.aov, 1)
library(car)
leveneTest(thoi_gian ~ loai_phuong_phap*loai_chat_doc, data = data.c4)

# Không hiểu vì sao p-value ra NaN. Xác nhận với cô.


# Giả sử các giả thiết được thỏa mãn. Bảng phân tích ANOVA như sau:
summary(c4.aov)

#                  Df Sum Sq Mean Sq F value   Pr(>F)    
#loai_chat_doc     4   8.23   2.057   1.761 0.201553    
#loai_phuong_phap  3  62.63  20.876  17.872 0.000101 ***
#Residuals        12  14.02   1.168                     

# Từ bảng ANOVA, ta thấy
# - Biến loai_chat_doc không có ý nghĩa thống kê. Như vậy có nghĩa 
# thời gian sống trung bình không thay đổi theo loại chất độc
# - Biến loai_phuong_phap có ý nghĩa thống kê. Như vậy có ít nhất
# 1 phương pháp có thời gian sống trung bình khác các phương pháp còn lại


# Đi sâu phân tích chi tiết xem phương pháp nào có thời gian trung bình
# lớn nhất
# Ref: https://www.sthda.com/english/wiki/two-way-anova-test-in-r#tukey-multiple-pairwise-comparisons
TukeyHSD(c4.aov, which = "loai_phuong_phap")

#      diff  lwr         upr       p adj
# B-A  4.86  2.83062343  6.8893766 0.0000632
# C-A  1.40 -0.62937657  3.4293766 0.2246295
# D-A  1.98 -0.04937657  4.0093766 0.0566235
# C-B -3.46 -5.48937657 -1.4306234 0.0013733
# D-B -2.88 -4.90937657 -0.8506234 0.0057128
# D-C  0.58 -1.44937657  2.6093766 0.8304987

# Có 3 nhóm có p-value < 0.05 là B-A, C-B và D-B. 
# Như vậy:
# - Thời gian sống trung bình phương pháp B lớn hơn phương pháp A
# - Thời gian sống trung bình phương pháp B lớn hơn phương pháp C
# - Thời gian sống trung bình phương pháp B lớn hơn phương pháp D
# Như vậy, phương pháp B có thời gian sống trung bình lớn nhất.