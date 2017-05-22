object frmMain: TfrmMain
  Left = 296
  Top = 130
  Width = 399
  Height = 359
  Caption = 'TJvChangeNotify demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 0
    Top = 132
    Width = 391
    Height = 13
    Align = alTop
    Caption = '  Detected changes:'
  end
  object Panel1: TPanel
    Left = 0
    Top = 249
    Width = 391
    Height = 83
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnStart: TSpeedButton
      Left = 294
      Top = 46
      Width = 75
      Height = 25
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'Start'
      OnClick = btnStartClick
    end
    object Label2: TLabel
      Left = 10
      Top = 13
      Width = 78
      Height = 13
      Caption = 'Timeout interval:'
    end
    object Label4: TLabel
      Left = 152
      Top = 12
      Width = 30
      Height = 13
      Caption = 'msecs'
    end
    object btnAdd: TButton
      Left = 110
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Add...'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 190
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object Edit1: TEdit
      Left = 96
      Top = 10
      Width = 33
      Height = 21
      TabOrder = 2
      Text = '100'
    end
    object udInterval: TUpDown
      Left = 129
      Top = 10
      Width = 15
      Height = 21
      Associate = Edit1
      Min = 0
      Max = 1000
      Increment = 50
      Position = 100
      TabOrder = 3
      Wrap = False
    end
    object btnClear: TButton
      Left = 14
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 4
      OnClick = btnClearClick
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 391
    Height = 132
    Align = alTop
    Columns = <
      item
        Caption = 'Directory'
        Width = 100
      end
      item
        Caption = 'Subtrees'
        Width = 80
      end
      item
        Caption = 'Actions'
        Width = 200
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
  end
  object ListBox2: TListBox
    Left = 0
    Top = 145
    Width = 391
    Height = 104
    Align = alClient
    ItemHeight = 13
    TabOrder = 2
  end
  object JvChangeNotify1: TJvChangeNotify
    Notifications = <>
    OnChangeNotify = JvChangeNotify1ChangeNotify
    Left = 192
    Top = 256
  end
end
