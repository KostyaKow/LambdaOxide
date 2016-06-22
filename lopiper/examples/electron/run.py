#!/usr/bin/env python

import sys, time, gui

def setupElectron(index_path, on_event, on_update, update_freq):
   w = gui.Window(100, 100, "Scheme JS", debug=True)
   w.load('index.html')
   w.on_gui_event += on_event

   def update():
      on_update(w)
      return w.timeout(update, update_freq)

   #w.run(update, 1000)
   w.run()

def sleep(x):
   time.sleep(x)

def on_js_event(msg):
   print(msg)
   print(msg['testdict'])
   #adblib.click(msg['x']/prop, msg['y']/prop)

def on_update(w):
   #done = False
   #from multiprocessing import Process
   #p = Process(target=lambda:adblib.screenshot(pic_path))
   #p.start()
   w.exec_js('console.log("hi")');
   #js_call = 'setPic("%s", %f, %f, %f)' % (pic_path, width, height, prop)
   ##print(js_call)
   #w.exec_js(js_call)
   #done = True

def main():
   w = setupElectron('index.html', on_js_event, on_update, 2000)
   w.run()

main()
