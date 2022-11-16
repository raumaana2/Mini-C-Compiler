; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define float @unary(i32 %n, float %m) {
entry:
  %sum = alloca float, align 4
  %result = alloca float, align 4
  %m2 = alloca float, align 4
  %n1 = alloca float, align 4
  store i32 %n, ptr %n1, align 4
  store float %m, ptr %m2, align 4
  store float 0.000000e+00, ptr %result, align 4
  store float 0.000000e+00, ptr %sum, align 4
  store float 0.000000e+00, ptr %sum, align 4
  %n3 = load float, ptr %n1, align 4
  %m4 = load float, ptr %m2, align 4
  %faddtmp = fadd float %n3, %m4
  store float %faddtmp, ptr %result, align 4
  %result5 = load float, ptr %result, align 4
  %calltmp = call float @print_float(float %result5)
  %sum6 = load float, ptr %sum, align 4
  %result7 = load float, ptr %result, align 4
  %faddtmp8 = fadd float %sum6, %result7
  store float %faddtmp8, ptr %sum, align 4
  %n9 = load float, ptr %n1, align 4
  %m10 = load float, ptr %m2, align 4
  %negtmp = fneg float %m10
  %faddtmp11 = fadd float %n9, %negtmp
  store float %faddtmp11, ptr %result, align 4
  %result12 = load float, ptr %result, align 4
  %calltmp13 = call float @print_float(float %result12)
  %sum14 = load float, ptr %sum, align 4
  %result15 = load float, ptr %result, align 4
  %faddtmp16 = fadd float %sum14, %result15
  store float %faddtmp16, ptr %sum, align 4
  %n17 = load float, ptr %n1, align 4
  %m18 = load float, ptr %m2, align 4
  %negtmp19 = fneg float %m18
  %negtmp20 = fneg float %negtmp19
  %faddtmp21 = fadd float %n17, %negtmp20
  store float %faddtmp21, ptr %result, align 4
  %result22 = load float, ptr %result, align 4
  %calltmp23 = call float @print_float(float %result22)
  %sum24 = load float, ptr %sum, align 4
  %result25 = load float, ptr %result, align 4
  %faddtmp26 = fadd float %sum24, %result25
  store float %faddtmp26, ptr %sum, align 4
  %n27 = load float, ptr %n1, align 4
  %negtmp28 = fneg float %n27
  %m29 = load float, ptr %m2, align 4
  %negtmp30 = fneg float %m29
  %faddtmp31 = fadd float %negtmp28, %negtmp30
  store float %faddtmp31, ptr %result, align 4
  %result32 = load float, ptr %result, align 4
  %calltmp33 = call float @print_float(float %result32)
  %sum34 = load float, ptr %sum, align 4
  %result35 = load float, ptr %result, align 4
  %faddtmp36 = fadd float %sum34, %result35
  store float %faddtmp36, ptr %sum, align 4
  %sum37 = load float, ptr %sum, align 4
  ret float %sum37
}
