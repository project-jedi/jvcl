object JvgLogicItemEditor: TJvgLogicItemEditor
  Left = 310
  Top = 202
  Width = 454
  Height = 438
  Color = clBtnFace
  Constraints.MaxWidth = 454
  Constraints.MinHeight = 100
  Constraints.MinWidth = 454
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 19
    Top = 230
    Width = 22
    Height = 13
    Caption = 'True'
  end
  object Label3: TLabel
    Left = 12
    Top = 144
    Width = 25
    Height = 13
    Caption = 'False'
  end
  object Shape4: TShape
    Left = 0
    Top = 0
    Width = 446
    Height = 1
    Align = alTop
    Pen.Color = clBtnShadow
  end
  object LB: TListBox
    Left = 0
    Top = 74
    Width = 446
    Height = 292
    Style = lbOwnerDrawFixed
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    ItemHeight = 72
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6')
    TabOrder = 2
    OnDrawItem = LBDrawItem
    OnMeasureItem = LBMeasureItem
  end
  object Button1: TButton
    Left = 280
    Top = 382
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 360
    Top = 382
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object pRule_: TPanel
    Left = 376
    Top = 256
    Width = 15
    Height = 13
    BevelOuter = bvNone
    Color = 14737632
    TabOrder = 3
    object spRule: TSpeedButton
      Left = 0
      Top = 0
      Width = 15
      Height = 13
      Cursor = crHandPoint
      Flat = True
      Glyph.Data = {
        D6000000424DD60000000000000076000000280000000C0000000C0000000100
        0400000000006000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        0000888888888888000088888888888800008888888888880000888880888888
        0000888800088888000088800000888800008800000008880000888888888888
        0000888888888888000088888888888800008888888888880000}
      OnClick = spRuleClick
    end
  end
  object pValue: TPanel
    Left = 44
    Top = 251
    Width = 302
    Height = 16
    BevelOuter = bvNone
    TabOrder = 4
    object Panel10: TPanel
      Left = 0
      Top = 0
      Width = 302
      Height = 1
      Align = alTop
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 0
    end
    object Panel11: TPanel
      Left = 0
      Top = 15
      Width = 302
      Height = 1
      Align = alBottom
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 1
    end
    object Panel12: TPanel
      Left = 0
      Top = 1
      Width = 1
      Height = 14
      Align = alLeft
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 2
    end
    object Panel13: TPanel
      Left = 301
      Top = 1
      Width = 1
      Height = 14
      Align = alRight
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 3
    end
    object eValue: TEdit
      Left = 1
      Top = 1
      Width = 300
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 8404992
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnChange = mTrueChange
    end
  end
  object pTrue: TPanel
    Left = 43
    Top = 269
    Width = 302
    Height = 16
    BevelOuter = bvNone
    TabOrder = 5
    object Panel15: TPanel
      Left = 0
      Top = 15
      Width = 302
      Height = 1
      Align = alBottom
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 0
    end
    object Panel16: TPanel
      Left = 0
      Top = 1
      Width = 1
      Height = 14
      Align = alLeft
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 1
    end
    object Panel17: TPanel
      Left = 301
      Top = 1
      Width = 1
      Height = 14
      Align = alRight
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 2
    end
    object mTrue: TEdit
      Left = 1
      Top = 1
      Width = 300
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnChange = mTrueChange
    end
    object Panel14: TPanel
      Left = 0
      Top = 0
      Width = 302
      Height = 1
      Align = alTop
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 4
    end
  end
  object pFalse: TPanel
    Left = 44
    Top = 297
    Width = 302
    Height = 16
    BevelOuter = bvNone
    TabOrder = 6
    object Panel20: TPanel
      Left = 0
      Top = 15
      Width = 302
      Height = 1
      Align = alBottom
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 0
    end
    object Panel21: TPanel
      Left = 0
      Top = 1
      Width = 1
      Height = 14
      Align = alLeft
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 1
    end
    object Panel22: TPanel
      Left = 301
      Top = 1
      Width = 1
      Height = 14
      Align = alRight
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 2
    end
    object mFalse: TEdit
      Left = 1
      Top = 1
      Width = 300
      Height = 14
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 4210816
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnChange = mTrueChange
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 302
      Height = 1
      Align = alTop
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 4
    end
  end
  object Panel18: TPanel
    Left = 0
    Top = 1
    Width = 446
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 7
    object Label1: TLabel
      Left = 9
      Top = 3
      Width = 37
      Height = 13
      Caption = 'Value:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 257
      Top = 3
      Width = 58
      Height = 13
      Caption = 'Condition:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 9
      Top = 35
      Width = 102
      Height = 13
      Caption = 'Variants/Choices:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Shape1: TShape
      Left = 49
      Top = 10
      Width = 193
      Height = 1
      Pen.Color = clBtnShadow
    end
    object Shape2: TShape
      Left = 317
      Top = 10
      Width = 124
      Height = 1
      Pen.Color = clBtnShadow
    end
    object Shape3: TShape
      Left = 115
      Top = 43
      Width = 326
      Height = 1
      Pen.Color = clBtnShadow
    end
    object pExpr: TPanel
      Left = 5
      Top = 17
      Width = 236
      Height = 16
      BevelOuter = bvNone
      TabOrder = 0
      object cbExpr: TComboBox
        Left = -2
        Top = -2
        Width = 241
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 29041
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 0
        OnChange = cbExprChange
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 236
        Height = 1
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
      end
      object Panel7: TPanel
        Left = 0
        Top = 15
        Width = 236
        Height = 1
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 2
      end
      object Panel8: TPanel
        Left = 0
        Top = 1
        Width = 1
        Height = 14
        Align = alLeft
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 3
      end
      object Panel9: TPanel
        Left = 235
        Top = 1
        Width = 1
        Height = 14
        Align = alRight
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 4
      end
    end
    object pRule: TPanel
      Left = 256
      Top = 17
      Width = 185
      Height = 16
      BevelOuter = bvNone
      TabOrder = 1
      object cbRule: TComboBox
        Left = -2
        Top = -2
        Width = 190
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbExprChange
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 1
        Align = alTop
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 1
      end
      object Panel2: TPanel
        Left = 0
        Top = 15
        Width = 185
        Height = 1
        Align = alBottom
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 2
      end
      object Panel3: TPanel
        Left = 0
        Top = 1
        Width = 1
        Height = 14
        Align = alLeft
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 3
      end
      object Panel4: TPanel
        Left = 184
        Top = 1
        Width = 1
        Height = 14
        Align = alRight
        BevelOuter = bvNone
        Color = clGray
        TabOrder = 4
      end
    end
  end
  object TB: TToolBar
    Left = 0
    Top = 52
    Width = 446
    Height = 22
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 38
    Caption = 'TB'
    Color = clBtnFace
    EdgeInner = esNone
    Flat = True
    ParentColor = False
    ShowCaptions = True
    TabOrder = 8
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'Add'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 38
      Top = 0
      Caption = 'Delete'
      ImageIndex = 1
    end
  end
  object pmRule: TPopupMenu
    Left = 264
    Top = 120
  end
end
