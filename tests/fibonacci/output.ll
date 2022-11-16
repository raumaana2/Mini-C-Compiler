; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n) {
entry:
  %total = alloca i32, align 4
  %c = alloca i32, align 4
  %next = alloca i32, align 4
  %second = alloca i32, align 4
  %first = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %first, align 4
  store i32 0, ptr %second, align 4
  store i32 0, ptr %next, align 4
  store i32 0, ptr %c, align 4
  store i32 0, ptr %total, align 4
  %n2 = load i32, ptr %n1, align 4
  %calltmp = call i32 @print_int(i32 %n2)
  store i32 0, ptr %first, align 4
  store i32 1, ptr %second, align 4
  store i32 1, ptr %c, align 4
  store i32 0, ptr %total, align 4
  br label %before

before:                                           ; preds = %end, %entry
  %c3 = load i32, ptr %c, align 4
  %n4 = load i32, ptr %n1, align 4
  %i32tof = uitofp i32 %c3 to float
  %i32tof5 = uitofp i32 %n4 to float
  %flttmp = fcmp olt float %i32tof, %i32tof5
  %whilecond = icmp ne i1 %flttmp, false
  br i1 %whilecond, label %loop, label %end20

loop:                                             ; preds = %before
  %c6 = load i32, ptr %c, align 4
  %i32tof7 = uitofp i32 %c6 to float
  %fletmp = fcmp ole float %i32tof7, 1.000000e+00
  %ifcond = icmp ne i1 %fletmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %loop
  %c8 = load i32, ptr %c, align 4
  store i32 %c8, ptr %next, align 4
  br label %end

else:                                             ; preds = %loop
  %first9 = load i32, ptr %first, align 4
  %second10 = load i32, ptr %second, align 4
  %iaddtmp = add i32 %first9, %second10
  store i32 %iaddtmp, ptr %next, align 4
  %second11 = load i32, ptr %second, align 4
  store i32 %second11, ptr %first, align 4
  %next12 = load i32, ptr %next, align 4
  store i32 %next12, ptr %second, align 4
  br label %end

end:                                              ; preds = %else, %then
  %next13 = load i32, ptr %next, align 4
  %calltmp14 = call i32 @print_int(i32 %next13)
  %c15 = load i32, ptr %c, align 4
  %iaddtmp16 = add i32 %c15, 1
  store i32 %iaddtmp16, ptr %c, align 4
  %total17 = load i32, ptr %total, align 4
  %next18 = load i32, ptr %next, align 4
  %iaddtmp19 = add i32 %total17, %next18
  store i32 %iaddtmp19, ptr %total, align 4
  br label %before
  br label %end20

end20:                                            ; preds = %end, %before
  %total21 = load i32, ptr %total, align 4
  %calltmp22 = call i32 @print_int(i32 %total21)
  %total23 = load i32, ptr %total, align 4
  ret i32 %total23
}
