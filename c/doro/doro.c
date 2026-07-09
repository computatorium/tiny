#define _POSIX_C_SOURCE 200809L
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#include <conio.h>
#include <io.h>
#define TTY (_isatty(_fileno(stdin))&&_isatty(_fileno(stdout)))
#define SLP Sleep(70)
static DWORD im,om;static HANDLE ih,oh;
#else
#include <sys/ioctl.h>
#include <sys/select.h>
#include <termios.h>
#include <unistd.h>
#define TTY (isatty(0)&&isatty(1))
#define SLP nanosleep(&(struct timespec){0,70000000},0)
static struct termios ot;
#endif
#define B 35
#define W (B+2)
#define P printf
static int raw;static volatile sig_atomic_t done;

static void end(void){
#ifdef _WIN32
 if(raw){SetConsoleMode(ih,im);SetConsoleMode(oh,om);}
#else
 if(raw)tcsetattr(0,TCSAFLUSH,&ot);
#endif
 P("\033[?25h\033[?1049l\033[0m");
}
static void sig(int x){(void)x;done=1;}

static double now(void){
#ifdef _WIN32
 static LARGE_INTEGER f;LARGE_INTEGER t;if(!f.QuadPart)QueryPerformanceFrequency(&f);
 QueryPerformanceCounter(&t);return(double)t.QuadPart/f.QuadPart;
#else
 struct timespec t;clock_gettime(CLOCK_MONOTONIC,&t);return t.tv_sec+t.tv_nsec/1e9;
#endif
}

static int key(void){
#ifdef _WIN32
 return _kbhit()?_getch():0;
#else
 fd_set f;struct timeval z={0};char c;FD_ZERO(&f);FD_SET(0,&f);
 return select(1,&f,0,0,&z)>0&&read(0,&c,1)>0?c:0;
#endif
}

static void size(int*w,int*h){
#ifdef _WIN32
 CONSOLE_SCREEN_BUFFER_INFO s;GetConsoleScreenBufferInfo(oh,&s);
 *w=s.srWindow.Right-s.srWindow.Left+1;*h=s.srWindow.Bottom-s.srWindow.Top+1;
#else
 struct winsize s={0};ioctl(1,TIOCGWINSZ,&s);*w=s.ws_col?s.ws_col:80;*h=s.ws_row?s.ws_row:24;
#endif
}

static void line(int y,int tw,const char*s,int n){
 int x=(tw-W)/2+1,p=(W-n)/2;if(x<1)x=1;if(p<0)p=0;
 P("\033[%d;%dH%*s%s",y,x,p,"",s);
}

int main(int ac,char**av){
 int m[2]={25,5},g=0,o=0,pa=0,last=-1,tty=TTY,col=0;
 for(int i=1;i<ac;i++)for(char*s=av[i];*s&&g<2;)if(*s>47&&*s<58){long v=strtol(s,&s,10);if(v>0&&v<10000)m[g++]=v;}else s++;
 double len=m[0]*60,t0=now(),rem=len;
 signal(SIGINT,sig);signal(SIGTERM,sig);
 if(tty){
#ifdef _WIN32
  ih=GetStdHandle(STD_INPUT_HANDLE);oh=GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleMode(ih,&im);GetConsoleMode(oh,&om);
  SetConsoleMode(ih,im&~(ENABLE_ECHO_INPUT|ENABLE_LINE_INPUT));
  SetConsoleMode(oh,om|4);
#else
  struct termios t;tcgetattr(0,&ot);t=ot;t.c_lflag&=~(ECHO|ICANON);t.c_cc[VMIN]=t.c_cc[VTIME]=0;tcsetattr(0,TCSAFLUSH,&t);
#endif
  raw=1;atexit(end);P("\033[?1049h\033[?25l");col=!getenv("NO_COLOR");
 }
 while(!done){
  double n=now();if(!pa)rem=len-(n-t0);if(rem<=0)o^=1,len=m[o]*60,t0=n,rem=len;
  switch(tty?key():0){case'q':done=1;continue;case' ':case'p':pa^=1;t0=n-(len-rem);break;case's':o^=1;len=m[o]*60;t0=n;rem=len;break;case'r':t0=n;rem=len;}
  int s=rem+.999,f=B-B*rem/len,tw=80,th=24,y;char b[256],*p=b,*c=col?o?"\033[36m":"\033[32m":"";
  if(!tty){if(s!=last)P("%s %02d:%02d %d/%d%s\n",o?"break":"focus",s/60,s%60,f,B,pa?" paused":""),fflush(stdout),last=s;SLP;continue;}
  size(&tw,&th);y=(th-8)/2;if(y<1)y=1;P("\033[H\033[2J");
  sprintf(b,"%s%s\033[0m",c,o?"break":"focus");line(y,tw,b,5);
  sprintf(b,"\033[1m%02d:%02d\033[0m",s/60,s%60);line(y+2,tw,b,5);
  p=b+sprintf(b,"%s[",c);for(int i=0;i<B;i++)*p++=i<f?'#':'-';sprintf(p,"]\033[0m");line(y+4,tw,b,W);
  sprintf(b,"\033[2m%s\033[0m",pa?"paused":"running");line(y+6,tw,b,pa?6:7);
  line(y+8,tw,"\033[2mp pause · s skip · r reset · q quit\033[0m",35);
  fflush(stdout);SLP;
 }
}
