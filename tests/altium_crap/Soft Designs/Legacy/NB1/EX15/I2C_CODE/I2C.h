// SFR mappings
#define ADDR_I2C_CTRL_REG      0xE7
#define I2C_CTRL_REG		      (*(__sfr unsigned char *)0xE7)
#define I2C_CTRL_REG_bit(x)   ((*(__sfr __bitstruct_t *)0xE7).__b ## x )

#define ADDR_I2C_STAT_REG      0xEF
#define I2C_STAT_REG		      (*(__sfr unsigned char *)0xEF)
#define I2C_STAT_REG_bit(x)   ((*(__sfr __bitstruct_t *)0xEF).__b ## x )

#define ADDR_I2C_CLK0_REG      0xD7
#define I2C_CLK0_REG		      (*(__sfr unsigned char *)0xD7)
#define I2C_CLK0_REG_bit(x)   ((*(__sfr __bitstruct_t *)0xD7).__b ## x )

#define ADDR_I2C_CLK1_REG      0xDF
#define I2C_CLK1_REG		      (*(__sfr unsigned char *)0xDF)
#define I2C_CLK1_REG_bit(x)   ((*(__sfr __bitstruct_t *)0xDF).__b ## x )

#define ADDR_I2C_WRIT_REG      0xC7
#define I2C_WRIT_REG		      (*(__sfr unsigned char *)0xC7)
#define I2C_WRIT_REG_bit(x)   ((*(__sfr __bitstruct_t *)0xC7).__b ## x )

#define ADDR_I2C_READ_REG      0xCF
#define I2C_READ_REG		      (*(__sfr unsigned char *)0xCF)
#define I2C_READ_REG_bit(x)   ((*(__sfr __bitstruct_t *)0xCF).__b ## x )



// control register bits
#define I2C_CTRL_EN     0x01
#define I2C_CTRL_IEN    0x02
#define I2C_CTRL_IACK   0x04
#define I2C_CTRL_WR     0x08
#define I2C_CTRL_RD     0x10
#define I2C_CTRL_STOP   0x20
#define I2C_CTRL_START  0x40
#define I2C_CTRL_ACK    0x80

// status register bits
#define I2C_STAT_RXACK  1
