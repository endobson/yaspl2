.global foo
foo:
call *bar
nop
ret
.zero 200
.global bar
bar:
jmp *foo
jmp foo
nop
ret
