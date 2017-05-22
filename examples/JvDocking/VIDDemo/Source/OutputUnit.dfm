object OutputForm: TOutputForm
  Left = 435
  Top = 182
  Width = 220
  Height = 221
  BorderStyle = bsSizeToolWin
  Caption = 'Output'
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
  PixelsPerInch = 96
  TextHeight = 15
  object Shape1: TShape
    Left = 0
    Top = 30
    Width = 212
    Height = 164
    Align = alClient
    Brush.Color = clBtnFace
    Pen.Color = clGrayText
  end
  object ComboBoxPanel: TPanel
    Left = 0
    Top = 0
    Width = 212
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = ComboBoxPanelResize
    object ComboBox: TComboBox
      Left = 0
      Top = 1
      Width = 221
      Height = 23
      ItemHeight = 15
      TabOrder = 0
    end
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 64
    Top = 88
  end
end
