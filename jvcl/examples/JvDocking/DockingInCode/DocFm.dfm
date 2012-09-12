object DocForm: TDocForm
  Left = 456
  Top = 476
  Caption = 'Document Form'
  ClientHeight = 474
  ClientWidth = 729
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
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnEndDock = FormEndDock
  OnGetSiteInfo = FormGetSiteInfo
  OnShow = FormShow
  OnStartDock = FormStartDock
  OnUnDock = FormUnDock
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 729
    Height = 33
    Align = alTop
  end
  object sg: TStringGrid
    Left = 0
    Top = 33
    Width = 729
    Height = 441
    Align = alClient
    Color = 16642009
    ColCount = 20
    RowCount = 20
    TabOrder = 0
    OnClick = Button1Click
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
  object Button1: TButton
    Left = 224
    Top = 2
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
    OnStartDock = Button1StartDock
  end
  object CheckBox2: TCheckBox
    Left = 376
    Top = 7
    Width = 97
    Height = 17
    Caption = 'allow float'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object DockClient: TJvDockClient
    DirectDrag = False
    OnFormShow = DockClientFormShow
    OnFormHide = DockClientFormHide
    OnCheckIsDockable = DockClientCheckIsDockable
    Left = 424
    Top = 304
  end
end
