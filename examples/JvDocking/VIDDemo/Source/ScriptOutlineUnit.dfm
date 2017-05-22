object ScriptOutlineForm: TScriptOutlineForm
  Left = 580
  Top = 233
  Width = 223
  Height = 148
  BorderStyle = bsSizeToolWin
  BorderWidth = 3
  Caption = 'Script Outline'
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
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 209
    Height = 115
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      'Select the HTML editor Source tab to '
      'use the Script Outline.')
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object lbDockClient1: TJvDockClient
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    DockStyle = MainForm.JvDockVIDStyle1
    Left = 56
    Top = 48
  end
end
