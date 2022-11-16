; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float()

declare i32 @print_more_ints(i32, i32)

define i32 @addition(i32 %n, i32 %m) {
entry:
  %result = alloca i32, align 4
  %m2 = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 %m, ptr %m2, align 4
  store i32 0, ptr %result, align 4
  %0 = load i32, ptr %n1, align 4
  %1 = load i32, ptr %m2, align 4
  %iaddtmp = add i32 %0, %1
  store i32 %iaddtmp, ptr %result, align 4
  %2 = load i32, ptr %n1, align 4
  %ieqtmp = icmp eq i32 %2, 4
  %ifcond = icmp ne i1 %ieqtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %3 = load i32, ptr %n1, align 4
  %4 = load i32, ptr %m2, align 4
  %iaddtmp3 = add i32 %3, %4
  %calltmp = call i32 @print_int(i32 %iaddtmp3)
  br label %end

else:                                             ; preds = %entry
  %5 = load i32, ptr %n1, align 4
  %6 = load i32, ptr %m2, align 4
  %imultmp = mul i32 %5, %6
  %calltmp4 = call i32 @print_int(i32 %imultmp)
  br label %end

end:                                              ; preds = %else, %then
  %7 = load i32, ptr %result, align 4
  ret i32 %7
}
