

;...............................................................................
.define PAGE_0 "BCF FSR, 0x05 \
                BCF FSR, 0x06"

.define PAGE_1 "BSF FSR, 0x05 \
                BCF FSR, 0x06"

.define PAGE_2 "BCF FSR, 0x05 \
                BSF FSR, 0x06"

.define PAGE_3 "BSF FSR, 0x05 \
                BSF FSR, 0x06"
;...............................................................................

