foo:
call *bar
nop
ret
.zero 200
bar:
jmp *foo
jmp foo
nop
ret
