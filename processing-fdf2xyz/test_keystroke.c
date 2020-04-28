#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XTest.h>

int main(){
  Display *display;
  unsigned int keycode;
  display = XOpenDisplay(NULL);
  keycode = XKeysymToKeycode(display, XK_A);
  XTestFakeKeyEvent(display, keycode, True, 0);
  keycode = XKeysymToKeycode(display, XK_B);
  XTestFakeKeyEvent(display, keycode, True, 0);
  keycode = XKeysymToKeycode(display, XK_C);
  XTestFakeKeyEvent(display, keycode, True, 0);
  keycode = XKeysymToKeycode(display, XK_D);
  XTestFakeKeyEvent(display, keycode, True, 0);
  XTestFakeKeyEvent(display, keycode, False, 0);
  XFlush(display);
}
