# Câu 1

# Kiểm tra thư mục làm việc hiện tại với câu lệnh: getwd()
# Bạn cần để file xlsx vào thư mục hiển thị ở câu lệnh trên,
# hoặc đưa hẳn đường dẫn tuyệt đối đến file như ở dưới.
library(readxl)
data.c1 <- data.frame(
  read_excel(
    path="D:/Thac si/03_Nhap mon suy dien thong ke/De thi mau/TuoiThoBongDen.xlsx",
    sheet = "TuoiThoBongDen"
  )
)

# Đặt lại tên cột
names(data.c1) <- c('before', 'after')


# a/ Hãy kiểm tra xem tuổi thọ bóng đèn trước và sau khi cải tiến kỹ thuật
# có tuân theo phân phối chuẩn hay không? Với mức ý nghĩa 5%
# H0: dữ liệu có phân phối chuẩn
shapiro.test(x=data.c1$before) # p-value = 9.82e-07
shapiro.test(x=data.c1$after) # p-value = 0.0001385

# p-value của cả 2 đều bé hơn 0.05. Bác bỏ H0. Dữ liệu không có phân phối chuẩn.


# b/ Hãy kiểm tra xem sau khi cải tiến kỹ thuật tuổi thọ bóng đèn có
# tăng lên thực sự hay không? α=0.05
# Bài toán yêu cầu sử dụng kiểm định giá trị trung bình cho 2 mẫu có liên quan.
# Vì dữ liệu không thỏa mãn giả định phân phối chuẩn của t-test, nên ta phải
# sử dụng kiểm định phi tham số wilcoxon cho 2 mẫu có liên quan
# Ref:
# - https://www.sthda.com/english/wiki/paired-samples-t-test-in-r
# - https://www.sthda.com/english/wiki/paired-samples-wilcoxon-test-in-r

# H0: mu_after = mu_before
# H1: mu_after > mu_before
wilcox.test(
  x=data.c1$after,
  y=data.c1$before,
  paired = TRUE,
  alternative = "greater",
  conf.level = 1 - 0.05
)

# p-value = 0.03431 < 0.05. Bác bỏ H0. Vậy tuổi thọ bóng đèn thực sự tăng sau khi
# có cải tiến kĩ thuật

#--------------------------------------------------------

# Câu 2

# Thư viện nutshell đã bị xóa khỏi CRAN, nên ta phải sử dụng code R để
# tải file nén về và cài từ đó.
# Code R để tải và cài ở dưới. Lưu ý đọc kĩ output message xem download có bị lỗi không.
# Ref: https://stackoverflow.com/questions/64281872/cant-install-package-nutshell-from-oreilly-book-r-in-a-nutshell-onto-mac

# download.file("https://cran.r-project.org/src/contrib/Archive/nutshell.audioscrobbler/nutshell.audioscrobbler_1.0.tar.gz", "nutshell.audioscrobbler_1.0.tar.gz")
# download.file("https://cran.r-project.org/src/contrib/Archive/nutshell.bbdb/nutshell.bbdb_1.0.tar.gz", "nutshell.bbdb_1.0.tar.gz")
# download.file("https://cran.r-project.org/src/contrib/Archive/nutshell/nutshell_2.0.tar.gz", "nutshell_2.0.tar.gz")

# install.packages("nutshell.audioscrobbler_1.0.tar.gz", repos = NULL)
# install.packages("nutshell.bbdb_1.0.tar.gz", repos = NULL)
# install.packages("nutshell_2.0.tar.gz", repos = NULL)

library(nutshell)
data(births2006.smpl)
data.c2 <- births2006.smpl[c("DBWT","DMETH_REC","DOB_WK","SEX")]
# Đổi lại tên cột
names(data.c2) <- c("birth_weight","delivery_method","dob_weekday_num","sex")

# Biến ngày trong tuần thành factor
data.c2$dob_weekday_num <- factor(
  x=data.c2$dob_weekday_num, 
  levels=1:7,
  labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

# a/ Hãy so sánh trọng lượng trung bình cho trẻ sơ sinh ở các bà mẹ sinh thường
# và các bà mẹ sinh mổ có như nhau hay không, với mức ý nghĩa 5%.

# Liệt kê danh sách các phương thức sinh
unique(data.c2$delivery_method)

# Lọc ra chỉ sinh thường (Vaginal) và sinh mổ (C-section)
data.c2.a <- subset(data.c2, delivery_method %in% c("Vaginal","C-section"))
data.c2.vaginal <- data.c2.a[data.c2.a["delivery_method"] == "Vaginal",]
data.c2.csection <- data.c2.a[data.c2.a["delivery_method"] == "C-section",]

# Đề yêu cầu so sánh trọng lượng trung bình ở 2 nhóm. Như vậy đây là bài toán
# kiểm định giá trị trung bình giữa 2 mẫu độc lập. Ta dùng unpaired 2-sample t-test
# Ref: https://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

# Unpaired 2-sample t-test có 2 giả định
# - Dữ liệu từ cả 2 mẫu đều có phân phối chuẩn
# - Phương sai của 2 mẫu bằng nhau

# Kiểm tra phân phối chuẩn
# Lưu ý rằng nếu dùng shapiro.test, ta sẽ gặp lỗi: 
# `Error in shapiro.test() : sample size must be between 3 and 5000`.

# shapiro.test chỉ dùng được cho mẫu tối đa 5_000 quan sát. Tập dữ liệu cho
# quan sát sinh thường là 1.1M dòng, sinh mổ là 528K dòng. Vậy cần phải giải quyết:
# - Chỉ dùng 5K dòng đầu tiên trong tập dữ liệu, hoặc
# - Sử dụng test khác như Anderson-Darling, Kolmogorov-Smirnov. Lưu ý các test này
# đều có đặc điểm là mẫu dữ liệu càng lớn thì càng có khả năng cao để bác bỏ H0,
# tức là càng có khả năng lớn kiểm định cho thấy dữ liệu không có phân phối chuẩn.
# Ref: https://stackoverflow.com/questions/28217306/error-in-shapiro-test-sample-size-must-be-between

# Xác nhận lại với cô
library(nortest)

# H0: dữ liệu có phân phối chuẩn
# Anderson-Darling
ad.test(data.c2.vaginal$birth_weight) # p-value < 2.2e-16
ks.test(data.c2.vaginal$birth_weight, "pnorm") # p-value < 2.2e-16
# p-value rất nhỏ nên bác bỏ H0. Dữ liệu không có phân phối chuẩn.

# hist(data.c2.vaginal$birth_weight)
# qqnorm(data.c2.vaginal$birth_weight)
# qqline(data.c2.vaginal$birth_weight, col = 2)


# Chuyển sang dùng kiểm định phi tham số wilcoxon rank test
# Ref: https://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
# H0: Trong lượng trung vị cho trẻ sơ sinh ở sinh thường và sinh mổ là như nhau
wilcox.test(
  birth_weight ~ delivery_method,
  data=data.c2.a,
  conf.level= 1 - 0.05
  )
# p-value < 2.2e-16 < 0.05. Bác bỏ H0. Có sự chênh lệnh trong trọng lượng trung vị
# cho trẻ giữa sinh thường và sinh mổ.

# b/ Hãy kiểm tra xem phương pháp sinh có phụ thuộc vào các thứ trong tuần không với α=0.1? 
# Sử dụng kiểm định khi bình phương (chi-squared test) để kiểm tra sự phụ thuộc
# giữa các biến
# Ref: https://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

# Cần phải tạo bảng 2 chiều (contingency table) trước
# Tạo cho cả dữ liệu có (data.c2) và không có (data.c2.a) phương pháp sinh Unknown
library(dplyr)
data.c2.count <- data.c2 %>% count(delivery_method, dob_weekday_num)
data.c2.a.count <- data.c2.a %>% count(delivery_method, dob_weekday_num)
cont.table <- xtabs(
  formula = n ~ delivery_method + dob_weekday_num,
  data = data.c2.count
)
cont.table.a <- xtabs(
  formula = n ~ delivery_method + dob_weekday_num,
  data = data.c2.a.count,
  exclude = c("Unknown")
  )

# Thực hiện kiểm định
# H0: 2 thuộc tính là độc lập
chisq.test(cont.table) # p-value < 2.2e-16
chisq.test(cont.table.a) # p-value < 2.2e-16

# p-value < 0.1. Bác bỏ H0. Vậy phương pháp sinh có sự phụ thuộc vào ngày trong tuần.

# Lưu ý rằng giá trị p-value nhìn giống nhau ở đây. Đây không phải bug, mà bởi vì
# giá trị p-value quá bé, và 2.2e-16 là giá trị nhỏ nhất mà R có thể lưu trữ.
# Cho nên ở đây không có vấn đề gì.
# Ref: https://stats.stackexchange.com/questions/78839/how-should-tiny-p-values-be-reported-and-why-does-r-put-a-minimum-on-2-22e-1


# c/ So sánh tỷ lệ nam và nữ trong các bà mẹ sinh thường với độ tin cậy 99%.

# Logic giải bài toán này theo tôi như sau:
# So sánh tỷ lệ nam và nữ = tỷ lệ nam có nhiều hơn tỷ lệ nữ?
# = tỷ lệ nam có nhiều hơn 0.5? (vì chỉ có 2 giới tính nam và nữ, nam nhiều
# hơn nữ tức là tỷ lệ nhiề hơn 0.5)
# = kiểm định tỷ lệ > 0.5.
# Xác nhận lại với cô.

# Ref: https://www.sthda.com/english/wiki/one-proportion-z-test-in-r
n <- nrow(data.c2.vaginal)
n_success <- nrow(data.c2.vaginal[data.c2.vaginal["sex"] == "M", ])
p_0 = n_success / n
p_e = 0.5
q = 1 - p_0

# Kiểm tra giả thiết np0 > 5 và nq > 5
n * p_0 > 5
n * q > 5
# Cả 2 đều TRUE nên ta có thể dùng xấp xỉ phân phối chuẩn

# Thực hiện kiểm định
# H0: p = 0.5
# H1: p > 0.5
# Lưu ý: sử dụng correct = FALSE để cho biết rằng đang sử dụng xấp xỉ
# phân phối chuẩn cho phân phối nhị thức
prop.test(
  x=n_success, 
  n=n, 
  p=p_e, 
  alternative = "greater",
  conf.level=0.99,
  correct=FALSE
  )
# p-value = 2.544e-06 < 0.01. Bác bỏ H0. p > 0.5. Vậy tỷ lệ nam cao hơn nữ.


#--------------------------------------------------------

# Câu 3
data.c3 <- data.frame(
  read_excel(
    path="D:/Thac si/03_Nhap mon suy dien thong ke/De thi mau/Thoigiansong.xlsx",
    sheet = "TuoiThoBongDen"
  )
)