.global foo
foo:
call *bar
.global bar
bar:
call *foo
bar_2:
ret
