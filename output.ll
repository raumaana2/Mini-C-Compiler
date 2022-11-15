; ModuleID = 'mini-c'
source_filename = "mini-c"

@0 = common global i32 0
@1 = common global i32 0

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
}
