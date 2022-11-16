; ModuleID = 'mini-c'
source_filename = "mini-c"

define i1 @palindrome(i32 %number) {
entry:
  %result = alloca i1, align 1
  %rmndr = alloca i32, align 4
  %rev = alloca i32, align 4
  %t = alloca i32, align 4
  %number1 = alloca i1, align 1
  store i32 %number, ptr %number1, align 4
  store i32 0, ptr %t, align 4
  store i32 0, ptr %rev, align 4
  store i32 0, ptr %rmndr, align 4
  store i1 false, ptr %result, align 1
  store i32 0, ptr %rev, align 4
  store i1 false, ptr %result, align 1
  %number2 = load i1, ptr %number1, align 1
  %btoi32 = zext i1 %number2 to i32
  store i32 %btoi32, ptr %t, align 4
  br label %before

before:                                           ; preds = %loop, %entry
  %number3 = load i1, ptr %number1, align 1
  %i1tof = uitofp i1 %number3 to float
  %ffttmp = fcmp ogt float %i1tof, 0.000000e+00
  %whilecond = icmp ne i1 %ffttmp, false
  br i1 %whilecond, label %loop, label %end

loop:                                             ; preds = %before
  %number4 = load i1, ptr %number1, align 1
  %imodtmp = urem i1 %number4, i32 10
  %btoi325 = zext i1 %imodtmp to i32
  store i32 %btoi325, ptr %rmndr, align 4
  %rev6 = load i32, ptr %rev, align 4
  %imultmp = mul i32 %rev6, 10
  %rmndr7 = load i32, ptr %rmndr, align 4
  %iaddtmp = add i32 %imultmp, %rmndr7
  store i32 %iaddtmp, ptr %rev, align 4
  %number8 = load i1, ptr %number1, align 1
  %idivtmp = udiv i1 %number8, i32 10
  store i1 %idivtmp, ptr %number1, align 1
  br label %before
  br label %end

end:                                              ; preds = %loop, %before
  %t9 = load i32, ptr %t, align 4
  %rev10 = load i32, ptr %rev, align 4
  %i32tof = uitofp i32 %t9 to float
  %i32tof11 = uitofp i32 %rev10 to float
  %feqtmp = fcmp oeq float %i32tof, %i32tof11
  %ifcond = icmp ne i1 %feqtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %end
  store i1 true, ptr %result, align 1
  br label %end12

else:                                             ; preds = %end
  store i1 false, ptr %result, align 1
  br label %end12

end12:                                            ; preds = %else, %then
  %result13 = load i1, ptr %result, align 1
  ret i1 %result13
}
