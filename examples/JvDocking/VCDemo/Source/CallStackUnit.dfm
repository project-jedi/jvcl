object CallStackForm: TCallStackForm
  Left = 338
  Top = 203
  Width = 200
  Height = 202
  BorderStyle = bsSizeToolWin
  Caption = 'Call Stack'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'System'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 192
    Height = 175
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ImeName = #214#208#206#196' ('#188#242#204#229') - '#200#171#198#180
    Lines.Strings = (
      ' CMyAppApp::InitInstance() line 52'
      ' AfxWinMain(HINSTANCE__ * 0x00400000,'
      ' HINSTANCE__ * 0x00000000, char * 0x0013261b,'
      ' int 1) line 39 + 11 bytes'
      ' WinMain(HINSTANCE__ * 0x00400000,'
      ' HINSTANCE__ * 0x00000000, char * 0x0013261b,'
      ' int 1) line 30'
      ' WinMainCRTStartup() line 330 + 54 bytes'
      ' KERNEL32! 77e67903()')
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
    Left = 104
    Top = 96
  end
end
