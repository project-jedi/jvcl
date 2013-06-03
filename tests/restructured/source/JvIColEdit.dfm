object IconListDialog: TIconListDialog
  Left = 466
  Top = 230
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
  object ScrollBar: TScrollBar
    Left = 4
    Top = 124
    Width = 253
    Height = 17
    LargeChange = 5
    PageSize = 0
    TabOrder = 3
    OnChange = ScrollBarChange
  end
  object JvSpeedBar1: TJvSpeedBar
    Left = 0
    Top = 0
    Width = 351
    Height = 29
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    BtnOffsetHorz = 3
    BtnOffsetVert = 3
    BtnWidth = 24
    BtnHeight = 23
    TabOrder = 4
    InternalVer = 1
    object JvSpeedbarSection1: TJvSpeedbarSection
      Caption = 'main'
    end
    object Load: TJvSpeedItem
      Caption = 'Load'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADADADADADAD00000000000ADADA003333333330ADAD0B0333333333
        0ADA0FB03333333330AD0BFB03333333330A0FBFB000000000000BFBFBFBFB0A
        DADA0FBFBFBFBF0DADAD0BFB0000000ADADAA000ADADADAD000DDADADADADADA
        D00AADADADAD0DAD0D0DDADADADAD000DADAADADADADADADADAD}
      Hint = 'Load|'
      Spacing = 1
      Left = 3
      Top = 3
      Visible = True
      OnClick = LoadClick
      SectionName = 'main'
    end
    object LoadAni: TJvSpeedItem
      Caption = 'LoadAni'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADADADADADAD00000000000ADADA003333333330ADAD0B0333333333
        0ADA0FB03333333330AD0BFB03333333330A0FBFB000000000000BFBFBFBFB0A
        DADA0FBFBFBFBF0DADAD0BFB0000000ADADAA000ADADADAD000DDADADADADADA
        D00AADADADAD0DAD0D0DDADADADAD000DADAADADADADADADADAD}
      Hint = 'LoadAni|'
      Spacing = 1
      Left = 27
      Top = 3
      Visible = True
      OnClick = LoadAniClick
      SectionName = 'main'
    end
    object Delete: TJvSpeedItem
      Caption = 'Delete'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000FFFFFF000000
        8000008000000080800080000000800080000000000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF000000666600888888888888
        8888888888888888888888868888888888688866688888888888886666888888
        8688888666888888688888886668888668888888866688668888888888666668
        8888888888866688888888888866666888888888866688668888888666688886
        6888886666888888668888666888888888688888888888888888}
      Hint = 'Delete|'
      Spacing = 1
      Left = 51
      Top = 3
      Visible = True
      OnClick = DeleteClick
      SectionName = 'main'
    end
    object Clear: TJvSpeedItem
      Caption = 'Clear'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000FFFFFF000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF000000000000FFFF00FE000000FF00FF00FFFF00007F7F7F008A8A8A8A8A8A
        8A8888080808080808888A80808080808A8888080808080808888A0000000000
        0A888A00111011100A888A00011111000A888A00001110000A888A0001111100
        0A888A00111011100A888A00000000000A888A00000000000A888A00000000AA
        AA888A00000000A0A8888A00000000AA88888AAAAAAAAAA88888}
      Hint = 'Clear|'
      Spacing = 1
      Left = 75
      Top = 3
      Visible = True
      OnClick = ClearClick
      SectionName = 'main'
    end
    object Copy: TJvSpeedItem
      Caption = 'Copy'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADADADADADADADDADADA444444444AADADAD4FFFFFFF4DDADADA4F0000
        0F4A0000004FFFFFFF4D0FFFFF4F00000F4A0F00004FFFFFFF4D0FFFFF4F00F4
        444A0F00004FFFF4F4AD0FFFFF4FFFF44ADA0F00F0444444ADAD0FFFF0F0DADA
        DADA0FFFF00DADADADAD000000DADADADADAADADADADADADADAD}
      Hint = 'Copy|'
      Spacing = 1
      Left = 99
      Top = 3
      Visible = True
      OnClick = CopyClick
      SectionName = 'main'
    end
    object Paste: TJvSpeedItem
      Caption = 'Paste'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAADADAD4444444444D000004FFFFFFFF40737374F444444F40373734FFFFF
        FFF40737374F444F44440373734FFFFF4F4A0737374FFFFF44AD037373444444
        40DA07373737373730AD03700000000770DA07708888880730AD03730B00B073
        70DAA00000BB00000DADDADAD0000ADADADAADADADADADADADAD}
      Hint = 'Paste|'
      Spacing = 1
      Left = 123
      Top = 3
      Visible = True
      OnClick = PasteClick
      SectionName = 'main'
    end
  end
end
