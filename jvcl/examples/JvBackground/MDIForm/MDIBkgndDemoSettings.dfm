object BkgndImageSettings: TBkgndImageSettings
  Left = 485
  Top = 265
  Width = 291
  Height = 254
  Caption = 'Background Image Properties'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Position = poDefaultPosOnly
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 42
    Width = 21
    Height = 13
    Caption = '&Shift'
    FocusControl = ScrollBar1
  end
  object Label2: TLabel
    Left = 16
    Top = 10
    Width = 27
    Height = 13
    Caption = '&Mode'
    FocusControl = Mode
  end
  object ScrollBar1: TScrollBar
    Left = 56
    Top = 42
    Width = 213
    Height = 13
    LargeChange = 8
    Max = 200
    PageSize = 0
    TabOrder = 2
    OnChange = ScrollBar1Change
  end
  object ShiftMode: TRadioGroup
    Left = 8
    Top = 64
    Width = 113
    Height = 69
    Caption = 'ShiftMode'
    ItemIndex = 1
    Items.Strings = (
      'sm&Rows'
      'sm&Columns')
    TabOrder = 3
    OnClick = ShiftModeClick
  end
  object Mode: TComboBox
    Left = 56
    Top = 8
    Width = 93
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ModeChange
    Items.Strings = (
      'bmTile'
      'bmCenter'
      'bmTopLeft'
      'bmTop'
      'bmTopRight'
      'bmLeft'
      'bmBottomLeft'
      'bmRight'
      'bmBottom'
      'bmBottomRight'
      'bmStretch')
  end
  object TranspColor: TButton
    Left = 156
    Top = 8
    Width = 113
    Height = 25
    Caption = 'TransparentCo&lor...'
    TabOrder = 1
    OnClick = TranspColorClick
  end
  object GroupBox1: TGroupBox
    Left = 128
    Top = 64
    Width = 145
    Height = 69
    Caption = 'Tile'
    TabOrder = 4
    object Label3: TLabel
      Left = 16
      Top = 16
      Width = 28
      Height = 13
      Caption = '&Width'
      FocusControl = TileWidth
    end
    object Label4: TLabel
      Left = 16
      Top = 44
      Width = 31
      Height = 13
      Caption = '&Height'
      FocusControl = TileHeight
    end
    object TileWidth: TSpinEdit
      Left = 72
      Top = 12
      Width = 57
      Height = 22
      MaxValue = 1000
      MinValue = 16
      TabOrder = 0
      Value = 16
      OnChange = TileWidthChange
    end
    object TileHeight: TSpinEdit
      Left = 72
      Top = 40
      Width = 57
      Height = 22
      MaxValue = 1000
      MinValue = 16
      TabOrder = 1
      Value = 16
      OnChange = TileHeightChange
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 134
    Width = 265
    Height = 83
    TabOrder = 5
    object ZigZag: TCheckBox
      Left = 152
      Top = 56
      Width = 89
      Height = 17
      Caption = '&ZigZag'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = ZigZagClick
    end
    object Transparent: TCheckBox
      Left = 12
      Top = 56
      Width = 89
      Height = 17
      Caption = '&Transparent'
      TabOrder = 2
      OnClick = TransparentClick
    end
    object Enabled: TCheckBox
      Left = 12
      Top = 16
      Width = 89
      Height = 17
      Caption = '&Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = EnabledClick
    end
    object GrayMapped: TCheckBox
      Left = 12
      Top = 36
      Width = 130
      Height = 17
      Caption = '&GrayMapped'
      Enabled = False
      TabOrder = 1
      OnClick = GrayMappedClick
    end
    object AutoSizeTile: TCheckBox
      Left = 152
      Top = 36
      Width = 97
      Height = 17
      Caption = '&AutoSizeTile'
      TabOrder = 4
      OnClick = AutoSizeTileClick
    end
    object FitPictureSize: TCheckBox
      Left = 152
      Top = 16
      Width = 97
      Height = 17
      Caption = '&FitPictureSize'
      TabOrder = 3
      OnClick = FitPictureSizeClick
    end
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
  end
end
