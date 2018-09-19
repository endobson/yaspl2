.global foo
foo:
call *bar
nop
ret
.global bar
bar:
nop
nop
nop
ret
