#!/bin/bash
diff output1.xxd prog1.xxd && \
diff output2.xxd prog2.xxd && \
diff output3.xxd prog3.xxd && \
diff output4.xxd prog4.xxd
