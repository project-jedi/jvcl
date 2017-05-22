object JvFormPropsDlg: TJvFormPropsDlg
  Left = 200
  Top = 111
  BorderIcons = [biSystemMenu]
  Caption = 'Form Storage Designer'
  ClientHeight = 525
  ClientWidth = 623
  Color = clBtnFace
  Constraints.MinHeight = 561
  Constraints.MinWidth = 436
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000FF
    FFF000000FFFFF000000FFFFFF00000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000008888800000000000000000
    0000000008800088000000000000000000000000880FFF088000000000000000
    0770000080FFFFF080000000000000000770000080FFFFF08000000000000000
    07700000880FFF08800000000000000000000000088000880000000000000000
    0000000000888880000000000000000000000000000000000000000000000000
    0000000007000007000000000000000777777777777777777777777000000000
    0000000000000000000000000000000000000000070000070000000000000000
    0000000000700070000000000000000000000000000777000000000000000000
    000000000000000000000000000000000000000000000000000000000000C1F8
    3F03F3FC7F9FF1FC7F1FF8FC7E3FFC7C7C7FFE3C78FFFF1C71FFFF8C63FFFFC4
    47FFFFE00FFFFFF01FFFFFF83FFFC000000F8000000780000007800000078000
    0007800000078000000780000007800000078000000780000007800000078000
    000780000007C000000F8120021F8130061FFFF80FFFFFFC1FFFFFFFFFFF}
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBevel: TGroupBox
    Left = 4
    Top = 71
    Width = 614
    Height = 417
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object CompPropsSplitter: TSplitter
      Left = 2
      Top = 211
      Width = 610
      Height = 3
      Cursor = crVSplit
      Align = alTop
      AutoSnap = False
      MinSize = 140
    end
    object CompPropsPanel: TPanel
      Left = 2
      Top = 15
      Width = 610
      Height = 196
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = CompPropsPanelResize
      object ComponentsLabel: TLabel
        Left = 12
        Top = 0
        Width = 68
        Height = 13
        Caption = '&Components   '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        IsControl = True
      end
      object PropertiesLabel: TLabel
        Left = 298
        Top = 0
        Width = 56
        Height = 13
        Caption = '&Properties   '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        IsControl = True
      end
      object AddButton: TButton
        Left = 215
        Top = 86
        Width = 77
        Height = 25
        Caption = '&Add'
        TabOrder = 0
        OnClick = AddButtonClick
      end
      object ComponentsList: TListBox
        Left = 12
        Top = 16
        Width = 197
        Height = 174
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
        OnClick = ListClick
      end
      object PropertiesList: TListBox
        Left = 298
        Top = 16
        Width = 190
        Height = 174
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
        OnDblClick = PropertiesListDblClick
      end
    end
    object StoredPropertiesPanel: TPanel
      Left = 2
      Top = 214
      Width = 610
      Height = 201
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object UpBtn: TSpeedButton
        Left = 539
        Top = 24
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Glyph.Data = {
          DE000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3000333333333333300033330000033330003333066603333000333306660333
          3000333306660333300030000666000030003306666666033000333066666033
          3000333306660333300033333060333330003333330333333000333333333333
          3000}
        OnClick = UpBtnClick
      end
      object DownBtn: TSpeedButton
        Left = 570
        Top = 24
        Width = 25
        Height = 25
        Anchors = [akTop, akRight]
        Glyph.Data = {
          DE000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3000333333333333300033333303333330003333306033333000333306660333
          3000333066666033300033066666660330003000066600003000333306660333
          3000333306660333300033330666033330003333000003333000333333333333
          3000}
        OnClick = DownBtnClick
      end
      object StoredPropertiesLabel: TLabel
        Left = 9
        Top = 8
        Width = 90
        Height = 13
        Caption = '&Stored Properties   '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object StoredList: TListBox
        Left = 9
        Top = 24
        Width = 511
        Height = 170
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = StoredListClick
      end
      object DeleteButton: TButton
        Left = 526
        Top = 138
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        TabOrder = 1
        OnClick = DeleteButtonClick
      end
      object ClearButton: TButton
        Left = 526
        Top = 169
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Cl&ear'
        TabOrder = 2
        OnClick = ClearButtonClick
      end
    end
  end
  object FormBox: TGroupBox
    Left = 4
    Top = 2
    Width = 614
    Height = 63
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Form Properties '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object ActiveCtrlBox: TCheckBox
      Left = 12
      Top = 17
      Width = 129
      Height = 17
      Caption = ' Acti&ve Control'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = ListClick
    end
    object SizeBox: TCheckBox
      Left = 232
      Top = 17
      Width = 137
      Height = 17
      Caption = 'Form &Size'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 2
      OnClick = ListClick
    end
    object StateBox: TCheckBox
      Left = 12
      Top = 40
      Width = 136
      Height = 17
      Caption = ' &Window State'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 1
      OnClick = ListClick
    end
    object LocationBox: TCheckBox
      Left = 232
      Top = 40
      Width = 137
      Height = 17
      Caption = 'Form &Location'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 3
      OnClick = ListClick
    end
  end
  object OkBtn: TButton
    Left = 458
    Top = 494
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 541
    Top = 494
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
