object Form2: TForm2
  Left = 249
  Top = 271
  Width = 339
  Height = 290
  BorderWidth = 4
  Caption = 'Form2'
  Color = clMaroon
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 323
    Height = 255
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Drag this form on top of the other form to dock it (this is the ' +
      'client form).'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    WordWrap = True
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = Form1.JvDockVIDStyle1
    Left = 104
    Top = 48
  end
end
