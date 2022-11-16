; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @factorial(i32 %n) {
entry:
  %factorial = alloca i32, align 4
  %i = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %i, align 4
  store i32 0, ptr %factorial, align 4
  store i32 1, ptr %factorial, align 4
  store i32 1, ptr %i, align 4
  %0 = load i32, ptr %i, align 4
  %1 = load i32, ptr %n1, align 4
  %iletmp = icmp ule i32 %0, %1
  %whilecond = icmp ne i1 %iletmp, false
  br i1 %whilecond, label %loop, label %end

loop:                                             ; preds = %entry
  %2 = load i32, ptr %factorial, align 4
  %3 = load i32, ptr %i, align 4
  %imultmp = mul i32 %2, %3
  store i32 %imultmp, ptr %factorial, align 4
  %4 = load i32, ptr %i, align 4
  %iaddtmp = add i32 %4, 1
  store i32 %iaddtmp, ptr %i, align 4
  br label %end

end:                                              ; preds = %loop, %entry
  %5 = load i32, ptr %factorial, align 4
  ret i32 %5
}
