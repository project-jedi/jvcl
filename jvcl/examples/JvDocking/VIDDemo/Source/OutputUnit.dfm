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
  Font.Name = 'ËÎÌו'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 12
  object Shape1: TShape
    Left = 0
    Top = 24
    Width = 212
    Height = 169
    Align = alClient
    Brush.Color = clBtnFace
    Pen.Color = clGrayText
  end
  object ComboBoxPanel: TPanel
    Left = 0
    Top = 0
    Width = 212
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = ComboBoxPanelResize
    object ComboBox: TComboBox
      Left = 0
      Top = 1
      Width = 177
      Height = 20
      ItemHeight = 12
      TabOrder = 0
    end
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 64
    Top = 88
  end
end
