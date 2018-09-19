.global foo
foo:
call bar
.global bar
bar:
call foo
