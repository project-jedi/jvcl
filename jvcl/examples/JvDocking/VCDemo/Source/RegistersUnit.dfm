object RegistersForm: TRegistersForm
  Left = 490
  Top = 233
  Width = 257
  Height = 211
  BorderStyle = bsSizeToolWin
  Caption = 'Registers'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 249
    Height = 184
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      ' EAX = 004178D0 EBX = 7FFDF000'
      ' ECX = 004178D0 EDX = 001334F8'
      ' ESI = 00000000 EDI = 00000000'
      ' EIP = 5F4334D5 ESP = 0012FEF0'
      ' EBP = 0012FF08 EFL = 00000202 CS = 001B'
      ' DS = 0023 ES = 0023 SS = 0023 FS = 0038'
      ' GS = 0000 OV=0 UP=0 EI=1 PL=0 ZR=0 AC=0'
      ' PE=0 CY=0 ST0 = '
      '+0.00000000000000000e+0000'
      ' ST1 = +0.00000000000000000e+0000'
      ' ST2 = +0.00000000000000000e+0000'
      ' ST3 = +0.00000000000000000e+0000'
      ' ST4 = +0.00000000000000000e+0000'
      ' ST5 = +0.00000000000000000e+0000'
      ' ST6 = +0.00000000000000000e+0000'
      ' ST7 = +0.00000000000000000e+0000'
      ' CTRL = 027F STAT = 0000 TAGS = FFFF'
      ' EIP = 00000000 CS = 0000 DS = 0000'
      ' EDO = 00000000')
    ParentFont = False
    TabOrder = 0
    WordWrap = False
  end
  object lbDockClient1: TJvDockClient
    OnFormShow = lbDockClient1FormShow
    OnFormHide = lbDockClient1FormHide
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = True
    ShowHint = True
    EnableCloseButton = True
    EachOtherDock = False
    DockStyle = MainForm.JvDockVCStyle1
    Left = 176
    Top = 64
  end
end
