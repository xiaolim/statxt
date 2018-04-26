; ModuleID = 'Statxt'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%c\0A\00"
@tmp = private unnamed_addr constant [12 x i8] c"Hello World\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %x = alloca i8*
  %i = alloca i32
  store i8* getelementptr inbounds ([12 x i8], [12 x i8]* @tmp, i32 0, i32 0), i8** %x
  store i32 1, i32* %i
  %i1 = load i32, i32* %i
  %i2 = load i32, i32* %i
  %tmp = add i32 %i2, 1
  store i32 %tmp, i32* %i
  %i3 = load i32, i32* %i
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %i3)
  ret i32 0
}
