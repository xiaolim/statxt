; ModuleID = 'MicroC'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%g\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @f(i32 %n) {
entry:
  %n1 = alloca i32
  store i32 %n, i32* %n1
  br label %while

while:                                            ; preds = %merge, %entry
  %n12 = load i32, i32* %n1
  %tmp13 = icmp ne i32 %n12, 1
  br i1 %tmp13, label %while_body, label %merge14

while_body:                                       ; preds = %while
  %n2 = load i32, i32* %n1
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %n2)
  %n3 = load i32, i32* %n1
  %n4 = load i32, i32* %n1
  %tmp = sdiv i32 %n4, 2
  %tmp5 = mul i32 %tmp, 2
  %tmp6 = icmp ne i32 %n3, %tmp5
  br i1 %tmp6, label %then, label %else

merge:                                            ; preds = %else, %then
  br label %while

then:                                             ; preds = %while_body
  %n7 = load i32, i32* %n1
  %tmp8 = mul i32 3, %n7
  %tmp9 = add i32 %tmp8, 1
  store i32 %tmp9, i32* %n1
  br label %merge

else:                                             ; preds = %while_body
  %n10 = load i32, i32* %n1
  %tmp11 = sdiv i32 %n10, 2
  store i32 %tmp11, i32* %n1
  br label %merge

merge14:                                          ; preds = %while
  ret i32 0
}

define i32 @main() {
entry:
  %f_result = call i32 @f(i32 27)
  ret i32 0
}
