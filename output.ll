; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addition(i32 %n, i32 %m) {
entry:
  %result = alloca i32, align 4
  %m2 = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 %m, ptr %m2, align 4
  store i32 0, ptr %result, align 4
  %n3 = load i32, ptr %n1, align 4
  %i32tof = sitofp i32 %n3 to float
  %faddtmp = fadd float %i32tof, 4.000000e+00
  %ftoi32 = fptosi float %faddtmp to i32
  store i32 %ftoi32, ptr %result, align 4
  %n4 = load i32, ptr %n1, align 4
  %i32tof5 = sitofp i32 %n4 to float
  %feqtmp = fcmp oeq float %i32tof5, 4.000000e+00
  %ifcond = icmp ne i1 %feqtmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %n6 = load i32, ptr %n1, align 4
  %m7 = load i32, ptr %m2, align 4
  %iaddtmp = add i32 %n6, %m7
  %calltmp = call i32 @print_int(i32 %iaddtmp)
  br label %end

else:                                             ; preds = %entry
  %n8 = load i32, ptr %n1, align 4
  %m9 = load i32, ptr %m2, align 4
  %imultmp = mul i32 %n8, %m9
  %calltmp10 = call i32 @print_int(i32 %imultmp)
  br label %end

end:                                              ; preds = %else, %then
  %result11 = load i32, ptr %result, align 4
  ret i32 %result11
  ret i32 0
}
