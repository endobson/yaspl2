.global aaa
aaa:
call ccc
.global bbb
bbb:
call aaa
.global ccc
ccc:
call bbb
