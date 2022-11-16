; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @multiplyNumbers(i32 %n) {
entry:
  %result = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %result, align 4
  store i32 0, ptr %result, align 4
  %n2 = load i32, ptr %n1, align 4
  %i32tof = uitofp i32 %n2 to float
  %fgetmp = fcmp oge float %i32tof, 1.000000e+00
  %ifcond = icmp ne i1 %fgetmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %n3 = load i32, ptr %n1, align 4
  %n4 = load i32, ptr %n1, align 4
  %isubtmp = sub i32 %n4, 1
  %calltmp = call i32 @multiplyNumbers(i32 %isubtmp)
  %imultmp = mul i32 %n3, %calltmp
  store i32 %imultmp, ptr %result, align 4
  br label %end

else:                                             ; preds = %entry
  store i32 1, ptr %result, align 4
  br label %end

end:                                              ; preds = %else, %then
  %result5 = load i32, ptr %result, align 4
  ret i32 %result5
}

define i32 @rfact(i32 %n) {
entry:
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  %n2 = load i32, ptr %n1, align 4
  %calltmp = call i32 @multiplyNumbers(i32 %n2)
  ret i32 %calltmp
}
