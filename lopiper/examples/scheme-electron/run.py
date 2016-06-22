#!/usr/bin/env python

import sys, time, gui

def setupElectron(index_path, on_event):
   w = gui.Window(100, 100, "Scheme JS", debug=True)
   w.load('index.html')
   w.on_gui_event += on_event
   #w.run(update, 1000)
   return w #.run()

def sleep(x):
   time.sleep(x)

def on_js_event(msg):
   print(msg)
   print(msg['testdict'])
   #adblib.click(msg['x']/prop, msg['y']/prop)

def on_update(w):
   #from multiprocessing import Process
   #p = Process(target=lambda:adblib.screenshot(pic_path))
   #p.start()
   line = sys.stdin.readline()
   print('got from scheme: ' + line)
   #line = "alert(JSON.stringify($(\"body\")));"
   #line = 'setText($("body"), "hiiii")'
   w.exec_js(line);


def main():
   w = setupElectron('index.html', on_js_event)
   w.run(on_update, 300)
main()

