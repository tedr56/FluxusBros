#!/usr/bin/env python

import os
import wx
from wx.lib.intctrl import IntCtrl
import liblo

ID_SEND  = 100
APP_SIZE_X = 300
APP_SIZE_Y = 200
DEFAULT_OSC_PORT = 3333
DEFAULT_OSC_PATH = "/osc/test"
DEFAULT_OSC_MESSAGE = "Kromate"


class MyFrame(wx.Frame):
    def __init__(self, parent, ID, title):
        wx.Frame.__init__(self, parent, ID, title, wx.DefaultPosition, wx.Size(APP_SIZE_X, APP_SIZE_Y))
        self.InitUI()
        #self.InitEvt()
    def InitUI(self):
        panel = wx.Panel(self)
        
        font = wx.SystemSettings_GetFont(wx.SYS_SYSTEM_FONT)
        font.SetPointSize(9)
        
        vbox = wx.BoxSizer(wx.VERTICAL)
        
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        portstatic = wx.StaticText(panel, label='Port')
        portstatic.SetFont(font)
        hbox1.Add(portstatic, flag=wx.RIGHT, border=8)
        self.portctrl = IntCtrl(panel)
        self.portctrl.SetValue(DEFAULT_OSC_PORT)
        self.portctrl.SetBounds(2, 65536)
        hbox1.Add(self.portctrl, proportion=1)
        vbox.Add(hbox1, flag=wx.EXPAND|wx.LEFT|wx.RIGHT|wx.TOP, border=10)

        vbox.Add((-1, 10))
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        pathstatic = wx.StaticText(panel, label='Chemin')
        pathstatic.SetFont(font)
        hbox2.Add(pathstatic, flag=wx.RIGHT, border=8)
        self.pathctrl = wx.TextCtrl(panel)
        self.pathctrl.SetValue(DEFAULT_OSC_PATH)
        hbox2.Add(self.pathctrl, proportion=1)
        vbox.Add(hbox2, flag=wx.EXPAND|wx.LEFT|wx.RIGHT|wx.TOP, border=10)
        
        vbox.Add((-1, 10))
        hbox3 = wx.BoxSizer(wx.HORIZONTAL)
        stringstatic = wx.StaticText(panel, label='Message')
        stringstatic.SetFont(font)
        hbox3.Add(stringstatic, flag=wx.RIGHT, border=8)
        self.stringctrl = wx.TextCtrl(panel)
        self.stringctrl.SetValue(DEFAULT_OSC_MESSAGE)
        hbox3.Add(self.stringctrl, proportion=1)
        vbox.Add(hbox3, flag=wx.EXPAND|wx.LEFT|wx.RIGHT|wx.TOP, border=10)
        
        vbox.Add((-1, 10))
        self.buttonsend = wx.Button(panel, ID_SEND, 'Send', (50, 130))
        self.buttonsend.Bind(wx.EVT_BUTTON, self.SendOsc)
        vbox.Add(self.buttonsend, flag=wx.EXPAND|wx.LEFT|wx.RIGHT|wx.TOP, border=10)

        panel.SetSizer(vbox)

    def SendOsc(self,event):
        if self.portctrl.IsInBounds():
            try:
                target = liblo.Address(self.portctrl.GetValue())
            except liblo.AddressError, err:
                print str(err)
                sys.exit()
            liblo.send(target, self.pathctrl.GetValue() , ('s' , self.stringctrl.GetValue()))

class MyApp(wx.App):
    def OnInit(self, *args, **kwargs):
        frame = MyFrame(None, -1, "Simple String OSC Sender")
        frame.Show(True)
        self.SetTopWindow(frame)
        return True

app = MyApp(0)
app.MainLoop()
        
                     
