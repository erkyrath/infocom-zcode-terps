/***************************************************************
picfile.h
Mon Mar  7 15:37:36 WET 1988

	(C) 1988 Magnetic Scrolls Ltd

***************************************************************/
#ifndef default_buflen

#define default_buflen 16*3

typedef struct pixel {
  unsigned char r,g,b;
  } pixel;

#define maxpal 32

typedef struct palette {
  int size;
  pixel paldata[maxpal];
} palette;

typedef struct picfile {
  int x;
  int y;
  char *source;
  int is_palette;
  int origin_x;
  int origin_y;
  int origin_width;
  int origin_height;
  int origin_palette;
  int bordered;
  int dither_factor;
  int written;
  int fp;
  char *buf;
  int buflen;
  char *tbuf;
  int in_buf;
  palette *cur_palette;
  int palette_size;
  char *cur_stipple;
  int apple_pic;
  int pictype;
} picfile;

picfile *picfopen();

#define pixelcmp(A,B) (((A)->r==(B)->r)&&((A)->g==(B)->g)&&((A)->b==(B)->b)) 

#if (0)

#define rf 30
#define bf 11
#define gf 59

#endif

#define rf 1
#define gf 1
#define bf 1

#define lum(A) (rf*(A.r)+gf*(A.g)+bf*(A.b))

#define sq(a) (((int)a)*((int)a))

#define df(x,y) (rf*sq((x)->r-(y)->r)+gf*sq((x)->g-(y)->g) \
                +bf*sq((x)->b-(y)->b))


extern char toolname[];

char *make_pic_name();

#endif
