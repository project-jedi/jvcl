object Form1: TForm1
  Left = 197
  Top = 103
  Width = 435
  Height = 346
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 146
    Width = 427
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object PropListView: TListView
    Left = 0
    Top = 25
    Width = 427
    Height = 121
    Align = alTop
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 180
      end
      item
        Caption = 'Default'
        Width = 100
      end>
    ShowWorkAreas = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 427
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ClassName: TLabel
      Left = 16
      Top = 4
      Width = 53
      Height = 13
      Caption = 'ClassName'
    end
    object InstanceSize: TLabel
      Left = 260
      Top = 4
      Width = 61
      Height = 13
      Caption = 'InstanceSize'
    end
    object Label1: TLabel
      Left = 192
      Top = 4
      Width = 64
      Height = 13
      Caption = 'InstanceSize:'
    end
  end
  object MainMenu1: TMainMenu
    Left = 368
    Top = 64
    object AppearanceMenu: TMenuItem
      Caption = '&Appearance'
      object FontItem: TMenuItem
        Caption = '&Font...'
        OnClick = FontItemClick
      end
      object ButtonStyleItem: TMenuItem
        Caption = '&Button Style'
        object Rectangle1: TMenuItem
          Caption = '&Rectangle'
          Checked = True
          Default = True
          RadioItem = True
          ShortCut = 16449
          OnClick = ChangeTVButtonKind
        end
        object X1: TMenuItem
          Tag = 1
          Caption = 'Rhombus'
          RadioItem = True
          ShortCut = 16450
          OnClick = ChangeTVButtonKind
        end
        object Circle1: TMenuItem
          Tag = 2
          Caption = '&Circle'
          RadioItem = True
          ShortCut = 16451
          OnClick = ChangeTVButtonKind
        end
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 368
    Top = 96
  end
end
