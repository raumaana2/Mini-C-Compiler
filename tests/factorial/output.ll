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
  br label %before

before:                                           ; preds = %loop, %entry
  %i2 = load i32, ptr %i, align 4
  %n3 = load i32, ptr %n1, align 4
  %i32tof = sitofp i32 %i2 to float
  %i32tof4 = sitofp i32 %n3 to float
  %fletmp = fcmp ole float %i32tof, %i32tof4
  %whilecond = icmp ne i1 %fletmp, false
  br i1 %whilecond, label %loop, label %end

loop:                                             ; preds = %before
  %factorial5 = load i32, ptr %factorial, align 4
  %i6 = load i32, ptr %i, align 4
  %imultmp = mul i32 %factorial5, %i6
  store i32 %imultmp, ptr %factorial, align 4
  %i7 = load i32, ptr %i, align 4
  %iaddtmp = add i32 %i7, 1
  store i32 %iaddtmp, ptr %i, align 4
  br label %before
  br label %end

end:                                              ; preds = %loop, %before
  %factorial8 = load i32, ptr %factorial, align 4
  ret i32 %factorial8
  ret i32 0
}
