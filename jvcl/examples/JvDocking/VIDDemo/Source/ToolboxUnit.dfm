object ToolboxForm: TToolboxForm
  Left = 403
  Top = 173
  Width = 202
  Height = 295
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsSizeToolWin
  Caption = 'Toolbox'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #203#206#204#229
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object HTML_Panel: TPanel
    Left = 0
    Top = 1
    Width = 241
    Height = 20
    Caption = 'HTML'
    TabOrder = 0
  end
  object Server_Objects_Panel: TPanel
    Left = 0
    Top = 24
    Width = 241
    Height = 20
    Caption = 'Server Objects'
    TabOrder = 1
  end
  object Design_Time_Controls_Panel: TPanel
    Left = 0
    Top = 46
    Width = 241
    Height = 20
    Caption = 'Design Time Controls'
    TabOrder = 2
  end
  object ActiveX_Controls_Panel: TPanel
    Left = 0
    Top = 69
    Width = 241
    Height = 20
    Caption = 'ActiveX Controls'
    TabOrder = 3
  end
  object General_Panel1: TPanel
    Left = 0
    Top = 91
    Width = 241
    Height = 20
    Caption = 'General Panel1'
    TabOrder = 4
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 72
    Top = 96
  end
end
