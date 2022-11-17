; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @pi() {
entry:
  %i = alloca i32, align 4
  %PI = alloca float, align 4
  %flag = alloca i1, align 1
  store i1 false, ptr %flag, align 1
  store float 0.000000e+00, ptr %PI, align 4
  store i32 0, ptr %i, align 4
  store i1 true, ptr %flag, align 1
  store float 3.000000e+00, ptr %PI, align 4
  store i32 2, ptr %i, align 4
  br label %before

before:                                           ; preds = %end, %entry
  %i1 = load i32, ptr %i, align 4
  %i32tof = uitofp i32 %i1 to float
  %flttmp = fcmp olt float %i32tof, 1.000000e+02
  %whilecond = icmp ne i1 %flttmp, false
  br i1 %whilecond, label %loop, label %end23

loop:                                             ; preds = %before
  %flag2 = load i1, ptr %flag, align 1
  %ifcond = icmp ne i1 %flag2, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %loop
  %PI3 = load float, ptr %PI, align 4
  %i4 = load i32, ptr %i, align 4
  %i5 = load i32, ptr %i, align 4
  %iaddtmp = add i32 %i5, 1
  %imultmp = mul i32 %i4, %iaddtmp
  %i6 = load i32, ptr %i, align 4
  %iaddtmp7 = add i32 %i6, 2
  %imultmp8 = mul i32 %imultmp, %iaddtmp7
  %i32tof9 = uitofp i32 %imultmp8 to float
  %fdivtmp = fdiv float 4.000000e+00, %i32tof9
  %faddtmp = fadd float %PI3, %fdivtmp
  store float %faddtmp, ptr %PI, align 4
  br label %end

else:                                             ; preds = %loop
  %PI10 = load float, ptr %PI, align 4
  %i11 = load i32, ptr %i, align 4
  %i12 = load i32, ptr %i, align 4
  %iaddtmp13 = add i32 %i12, 1
  %imultmp14 = mul i32 %i11, %iaddtmp13
  %i15 = load i32, ptr %i, align 4
  %iaddtmp16 = add i32 %i15, 2
  %imultmp17 = mul i32 %imultmp14, %iaddtmp16
  %i32tof18 = uitofp i32 %imultmp17 to float
  %fdivtmp19 = fdiv float 4.000000e+00, %i32tof18
  %fsubtmp = fsub float %PI10, %fdivtmp19
  store float %fsubtmp, ptr %PI, align 4
  br label %end

end:                                              ; preds = %else, %then
  %flag20 = load i1, ptr %flag, align 1
  %nottmp = xor i1 %flag20, true
  store i1 %nottmp, ptr %flag, align 1
  %i21 = load i32, ptr %i, align 4
  %iaddtmp22 = add i32 %i21, 2
  store i32 %iaddtmp22, ptr %i, align 4
  br label %before
  br label %end23

end23:                                            ; preds = %end, %before
  %PI24 = load float, ptr %PI, align 4
  ret float %PI24
}
