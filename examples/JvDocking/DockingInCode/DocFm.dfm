object DocForm: TDocForm
  Left = 456
  Top = 476
  Width = 344
  Height = 262
  Caption = 'Document Form'
  Color = clAqua
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 336
    Height = 33
    Align = alTop
  end
  object sg: TStringGrid
    Left = 0
    Top = 33
    Width = 336
    Height = 195
    Align = alClient
    Color = 16642009
    ColCount = 25
    RowCount = 25
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 40
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Allow Close'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object DockClient: TJvDockClient
    OnFormShow = DockClientFormShow
    OnFormHide = DockClientFormHide
    OnCheckIsDockable = DockClientCheckIsDockable
    LRDockWidth = 100
    TBDockHeight = 100
    DirectDrag = False
    ShowHint = True
    EnableCloseButton = True
    Left = 424
    Top = 304
  end
end
