object PainterQBF: TPainterQBF
  Left = 443
  Top = 124
  BorderStyle = bsToolWindow
  Caption = 'QuickBack'
  ClientHeight = 359
  ClientWidth = 217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 217
    Height = 113
    Align = alTop
  end
  object qbpresets: TComboBox
    Left = 8
    Top = 75
    Width = 201
    Height = 22
    Style = csOwnerDrawFixed
    Color = 12639424
    ItemHeight = 16
    PopupMenu = presetspop
    Sorted = True
    TabOrder = 0
    OnClick = qbpresetsClick
    OnDrawItem = qbpresetsDrawItem
  end
  object Panel1: TPanel
    Left = 0
    Top = 113
    Width = 217
    Height = 246
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Shape1: TShape
      Left = 104
      Top = 12
      Width = 25
      Height = 15
      Brush.Color = clRed
      Pen.Style = psClear
      Shape = stCircle
    end
    object Shape2: TShape
      Left = 128
      Top = 12
      Width = 25
      Height = 15
      Brush.Color = clGreen
      Pen.Style = psClear
      Shape = stCircle
    end
    object Shape3: TShape
      Left = 152
      Top = 12
      Width = 25
      Height = 15
      Brush.Color = clBlue
      Pen.Style = psClear
      Shape = stCircle
    end
    object QBList: TListBox
      Left = 0
      Top = 0
      Width = 97
      Height = 246
      Align = alLeft
      ItemHeight = 16
      Items.Strings = (
        'Prod'
        'Sum'
        'Sub'
        'Xor'
        'And'
        'OutAnd'
        'InAnd'
        'OutXor'
        'InXor'
        'OutMod'
        'InMod'
        'ProdXor'
        'SumXor'
        'SubXor'
        'ProdAnd'
        'SumAnd'
        'SubAnd'
        'Inner'
        'Outer'
        'OutRed'
        'InRed'
        'OutGreen'
        'InGreen'
        'Outblue'
        'InBlue'
        'InModOut'
        'OutModIn'
        'OutModIn2'
        'ModMod'
        'ModModXor'
        'Mod3'
        'ModModSub'
        'ModModAdd'
        'ModModAnd'
        'ModModOr'
        'Xor3'
        'XOr3Mod'
        'SubXorSum'
        'SubProdSum'
        'ProdProdSum'
        'DrawXor')
      TabOrder = 0
      OnClick = QBListClick
    end
    object trkred: TScrollBar
      Left = 107
      Top = 32
      Width = 20
      Height = 209
      Kind = sbVertical
      Max = 255
      PageSize = 0
      TabOrder = 1
      OnChange = trkRedChange
    end
    object trkgreen: TScrollBar
      Left = 131
      Top = 32
      Width = 20
      Height = 209
      Kind = sbVertical
      Max = 255
      PageSize = 0
      TabOrder = 2
      OnChange = trkGreenChange
    end
    object trkblue: TScrollBar
      Left = 155
      Top = 32
      Width = 20
      Height = 209
      Kind = sbVertical
      Max = 255
      PageSize = 0
      TabOrder = 3
      OnChange = trkBlueChange
    end
    object trkfactor: TScrollBar
      Left = 187
      Top = 32
      Width = 20
      Height = 209
      Kind = sbVertical
      Max = 255
      Min = 16
      PageSize = 0
      Position = 255
      TabOrder = 4
      OnChange = trkFactorChange
    end
  end
  object redradio: TRadioButton
    Left = 8
    Top = 8
    Width = 121
    Height = 17
    Caption = 'Prod'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = redradioClick
  end
  object greenradio: TRadioButton
    Left = 8
    Top = 28
    Width = 121
    Height = 17
    Caption = 'Prod'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = greenradioClick
  end
  object blueradio: TRadioButton
    Left = 8
    Top = 48
    Width = 121
    Height = 17
    Caption = 'Prod'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = blueradioClick
  end
  object presetspop: TPopupMenu
    Left = 136
    Top = 72
    object AddBackdrop1: TMenuItem
      Caption = '&Add Backdrop'
      OnClick = AddBackdrop1Click
    end
    object DeleteBackdrop1: TMenuItem
      Caption = '&Delete Backdrop'
      OnClick = DeleteBackdrop1Click
    end
    object UpdateBackdrop1: TMenuItem
      Caption = '&Update Backdrop'
      OnClick = UpdateBackdrop1Click
    end
  end
end
