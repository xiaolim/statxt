i32
i32 3
(int : 1)
(int : 2)
(int : 3)
this_array:   %tmp3 = alloca i32, i32 3
0
1
2
where:   %tmp24 = getelementptr i32, i32* %tmp3, i32 0
what: i32 1
where:   %tmp25 = getelementptr i32, i32* %tmp3, i32 1
what: i32 2
where:   %tmp26 = getelementptr i32, i32* %tmp3, i32 2
what: i32 3
array_llvalue:   %i = alloca [3 x i32]
something_else:   %tmp38 = getelementptr inbounds [3 x i32], [3 x i32]* %i, i32 0, i32 0
array_load:   %tmp29 = load i32, i32* %tmp38
; ModuleID = 'Statxt'

%s = type <{ i32 }>

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%c\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %s1 = alloca %s
  %i = alloca [3 x i32]
  %s11 = load %s, %s* %s1
  %tmp = getelementptr inbounds %s, %s* %s1, i32 0, i32 0
  %tmp2 = load i32, i32* %tmp
  %tmp3 = alloca i32, i32 3
  %tmp24 = getelementptr i32, i32* %tmp3, i32 0
  store i32 1, i32* %tmp24
  %tmp25 = getelementptr i32, i32* %tmp3, i32 1
  store i32 2, i32* %tmp25
  %tmp26 = getelementptr i32, i32* %tmp3, i32 2
  store i32 3, i32* %tmp26
  %tmp37 = load i32, i32* %tmp3
  %tmp38 = getelementptr inbounds [3 x i32], [3 x i32]* %i, i32 0, i32 0
  %tmp29 = load i32, i32* %tmp38
  ret i32 0
}
