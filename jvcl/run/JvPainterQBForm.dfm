object PainterQBForm: TPainterQBForm
  Left = 443
  Top = 124
  BorderStyle = bsToolWindow
  Caption = 'QuickBack'
  ClientHeight = 292
  ClientWidth = 176
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 0
    Width = 176
    Height = 92
    Align = alTop
  end
  object qbpresets: TComboBox
    Left = 7
    Top = 61
    Width = 163
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    PopupMenu = presetspop
    Sorted = True
    TabOrder = 0
    OnClick = qbpresetsClick
    OnDrawItem = qbpresetsDrawItem
  end
  object Panel1: TPanel
    Left = 0
    Top = 92
    Width = 176
    Height = 200
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Shape1: TShape
      Left = 85
      Top = 10
      Width = 20
      Height = 12
      Brush.Color = clRed
      Pen.Style = psClear
      Shape = stCircle
    end
    object Shape2: TShape
      Left = 104
      Top = 10
      Width = 20
      Height = 12
      Brush.Color = clGreen
      Pen.Style = psClear
      Shape = stCircle
    end
    object Shape3: TShape
      Left = 124
      Top = 10
      Width = 20
      Height = 12
      Brush.Color = clBlue
      Pen.Style = psClear
      Shape = stCircle
    end
    object QBList: TListBox
      Left = 0
      Top = 0
      Width = 79
      Height = 200
      Align = alLeft
      ItemHeight = 13
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
      Left = 87
      Top = 26
      Width = 16
      Height = 170
      Kind = sbVertical
      Max = 255
      PageSize = 0
      TabOrder = 1
      OnChange = trkRedChange
    end
    object trkgreen: TScrollBar
      Left = 107
      Top = 26
      Width = 16
      Height = 170
      Kind = sbVertical
      Max = 255
      PageSize = 0
      TabOrder = 2
      OnChange = trkGreenChange
    end
    object trkblue: TScrollBar
      Left = 126
      Top = 26
      Width = 16
      Height = 170
      Kind = sbVertical
      Max = 255
      PageSize = 0
      TabOrder = 3
      OnChange = trkBlueChange
    end
    object trkfactor: TScrollBar
      Left = 152
      Top = 26
      Width = 16
      Height = 170
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
    Left = 7
    Top = 7
    Width = 98
    Height = 13
    Caption = 'Prod'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = redradioClick
  end
  object greenradio: TRadioButton
    Left = 7
    Top = 23
    Width = 98
    Height = 14
    Caption = 'Prod'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = greenradioClick
  end
  object blueradio: TRadioButton
    Left = 7
    Top = 39
    Width = 98
    Height = 14
    Caption = 'Prod'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
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
