#!/bin/bash
diff -u output1.xxd prog1.xxd && \
diff -u output2.xxd prog2.xxd && \
diff -u output3.xxd prog3.xxd && \
diff -u output4.xxd prog4.xxd && \
diff -u output5.xxd prog5.xxd && \
diff -u output6.xxd prog6.xxd
