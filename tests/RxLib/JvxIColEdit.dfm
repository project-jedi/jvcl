object IconListDialog: TIconListDialog
  Left = 236
  Top = 149
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Icon List Editor'
  ClientHeight = 147
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = UpdateClipboard
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = UpdateClipboard
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 37
    Width = 253
    Height = 20
  end
  object Label1: TLabel
    Left = 9
    Top = 40
    Width = 66
    Height = 13
    Caption = 'Icons Count:  '
  end
  object CntLabel: TLabel
    Left = 78
    Top = 40
    Width = 39
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '12'
  end
  object Label3: TLabel
    Left = 125
    Top = 40
    Width = 79
    Height = 13
    Caption = 'Selected index:  '
  end
  object IdxLabel: TLabel
    Left = 208
    Top = 40
    Width = 41
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '3'
  end
  object OK: TButton
    Left = 268
    Top = 37
    Width = 77
    Height = 24
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Cancel: TButton
    Left = 268
    Top = 65
    Width = 77
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Holder: TPanel
    Left = 4
    Top = 62
    Width = 253
    Height = 59
    BevelInner = bvLowered
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object Slot0: TPanel
      Left = 10
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      Color = clActiveCaption
      TabOrder = 0
      OnMouseDown = ImageMouseDown
      object Image0: TImage
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot1: TPanel
      Tag = 1
      Left = 58
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 1
      OnMouseDown = ImageMouseDown
      object Image1: TImage
        Tag = 1
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot2: TPanel
      Tag = 2
      Left = 106
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      OnMouseDown = ImageMouseDown
      object Image2: TImage
        Tag = 2
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot3: TPanel
      Tag = 3
      Left = 154
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 3
      OnMouseDown = ImageMouseDown
      object Image3: TImage
        Tag = 3
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot4: TPanel
      Tag = 4
      Left = 202
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 4
      OnMouseDown = ImageMouseDown
      object Image4: TImage
        Tag = 4
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
  end
  object JvxSpeedbar: TJvxSpeedBar
    Left = 0
    Top = 0
    Width = 351
    Height = 31
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    BoundLines = [blBottom]
    Options = [sbFlatBtns, sbGrayedBtns, sbTransparentBtns]
    BtnOffsetHorz = 6
    BtnOffsetVert = 3
    BtnWidth = 24
    BtnHeight = 23
    BevelOuter = bvNone
    TabOrder = 3
    InternalVer = 1
    object TJvxSpeedbarSection
      Caption = 'Edit Icon List'
    end
    object TJvxSpeedbarSection
      Caption = 'Clipboard commands'
    end
    object Load: TJvxSpeedItem
      Caption = 'Load Icon'
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        D000DDDDDDDDDDDDD000DDDDD000DDDDD000DDDDD0C0DDDDD000DDDDD0C0DDDD
        D000DD0000C0000DD000DD0CCCCCCC0DD000DD0000C0000DD000DDDDD0C0DDDD
        D000DDDDD0C0DDDDD000DDDDD000DDDDD000DDDDDDDDDDDDD000DDDDDDDDDDDD
        D000}
      Hint = 'Load Icon|Add icon from file'
      Spacing = 1
      Left = 6
      Top = 3
      Visible = True
      OnClick = LoadClick
      SectionName = 'Edit Icon List'
    end
    object Delete: TJvxSpeedItem
      Caption = 'Delete Icon'
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        D000DDDDDDDDDDDDD000DDDDDDDDDDDDD000DDDDDDDDDDDDD000DDDDDDDDDDDD
        D000DD000000000DD000DD0CCCCCCC0DD000DD000000000DD000DDDDDDDDDDDD
        D000DDDDDDDDDDDDD000DDDDDDDDDDDDD000DDDDDDDDDDDDD000DDDDDDDDDDDD
        D000}
      Hint = 'Delete Icon|Delete icon'
      Spacing = 1
      Left = 30
      Top = 3
      Visible = True
      OnClick = DeleteClick
      SectionName = 'Edit Icon List'
    end
    object LoadAni: TJvxSpeedItem
      Caption = 'Load Animated Cursor'
      Glyph.Data = {
        2A010000424D2A010000000000007600000028000000120000000F0000000100
        040000000000B400000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDD000000DDDD0000000000DDDD000000DDDD0D0DDDD0D0DDDD000000DDDD
        0D0DDDD0D0DDDD000000DDDD000DDDD000DDDD000000DDDD0000000000DDDD00
        0000DDDD0D0DDDD0D0DDDD000000DDDD0D0DDDD0D0DDDD000000DDD9000DDDD0
        00DDDD000000D9B9B900000000DDDD000000DB9B9B0DDDD0D0DDDD00000099BB
        B99DDDD0D0DDDD000000DB9B9B0DDDD000DDDD000000D9B9B900000000DDDD00
        0000DDD9DDDDDDDDDDDDDD000000}
      Hint = 'Load Animated Cursor|Load from animated cursor (*.ani)'
      Spacing = 1
      Left = 54
      Top = 3
      Visible = True
      OnClick = LoadAniClick
      SectionName = 'Edit Icon List'
    end
    object Clear: TJvxSpeedItem
      Caption = 'Clear List'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        555555550000555555555555555555550000555555555555555555550000FFFF
        FF555555555555550000FFFFFFFF0005555555550000FFFFFFF09DD055555555
        00005555550DD9DD0555555500005555550BDD9DD0555555000055555509BDD9
        0A0555550000555555509BD022A05555000055555555090AA22A055500005555
        555550AFAA202055000055555555550AFA0B22050000555555555550A0BBB220
        00005555555555550FFBBB22000055555555555550FFBBB20000555555555555
        550FFBBB00005555555555555550FFBB000055555555555555550FFB00005555
        55555555555550FF0000}
      Hint = 'Clear List|Clear icon list'
      Spacing = 1
      Left = 78
      Top = 3
      Visible = True
      OnClick = ClearClick
      SectionName = 'Edit Icon List'
    end
    object Copy: TJvxSpeedItem
      Caption = 'Copy Icon'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDD0000DDDD77DDDDDDDDDDD77D0000DDD700DDDDDDDDDD700D0000DD70
        F0DDDDDDDDD70F0D0000D70FF0DDDDDDDD70FB0D0000D0FFF0DDDDDDDD0FBF0D
        0000D0F0F0DDDDDDDD0B0B0D0000D00FF0DDDD0DDD00BF0D0000D0FFF0DDDD00
        DD0BFB0D0000D000F0D000000D000F0D0000D0FFF0D000000D0BFB0D0000D000
        F0DDDD00DD000F0D0000D0FFF0DDDD0DDD0BFB0D0000D00FF0DDDDDDDD00BF0D
        0000D0F0F0DDDDDDDD0F0B0D0000DD0FF0DDDDDDDDD0BF0D0000DDD0F0DDDDDD
        DDDD0B0D0000DDDD00DDDDDDDDDDD00D0000DDDDD7DDDDDDDDDDDD7D0000DDDD
        DDDDDDDDDDDDDDDD0000}
      Hint = 'Copy Icon|Copy icon to clipboard'
      Spacing = 1
      Left = 110
      Top = 3
      Visible = True
      OnClick = CopyClick
      SectionName = 'Clipboard commands'
    end
    object Paste: TJvxSpeedItem
      Caption = 'Paste Icon'
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDD0000DDDDDDDDDDD00000000D0000DDD777777770EFEFEF0D0000DD00
        00000000F4444E0D0000D0F888888880EFEFEF0D0000D0F888888880F4444E0D
        0000D0F888888880EFEFEF0D0000D0F888000080F44E000D0000D0F888800080
        EFEF00DD0000D0F88800008000000DDD0000D0F88000808807DDDDDD0000D0F8
        0008888807DDDDDD0000D0F88088888807DDDDDD0000D0F88888888807DDDDDD
        0000D0F88888888807DDDDDD0000D0FF000000FF0DDDDDDD0000DD000C4C4000
        DDDDDDDD0000DDDD04C4C0DDDDDDDDDD0000DDDDD0000DDDDDDDDDDD0000DDDD
        DDDDDDDDDDDDDDDD0000}
      Hint = 'Paste Icon|Paste icon from clipboard'
      Spacing = 1
      Left = 134
      Top = 3
      Visible = True
      OnClick = PasteClick
      SectionName = 'Clipboard commands'
    end
  end
  object ScrollBar: TScrollBar
    Left = 4
    Top = 124
    Width = 253
    Height = 17
    LargeChange = 5
    PageSize = 0
    TabOrder = 4
    OnChange = ScrollBarChange
  end
end
