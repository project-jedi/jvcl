object JvFullColorForm: TJvFullColorForm
  Left = 256
  Top = 233
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Color Spaces Editor, Choose your color'
  ClientHeight = 380
  ClientWidth = 712
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object LabelColorSpace: TLabel
    Left = 8
    Top = 10
    Width = 101
    Height = 13
    Caption = '&Current Color Space :'
  end
  object LabelDrawOld: TLabel
    Left = 240
    Top = 32
    Width = 81
    Height = 41
    AutoSize = False
    Color = clBlack
    ParentColor = False
    OnClick = LabelDrawOldClick
  end
  object LabelDrawNew: TLabel
    Left = 320
    Top = 32
    Width = 81
    Height = 41
    AutoSize = False
    Color = clWhite
    ParentColor = False
  end
  object LabelOld: TLabel
    Left = 240
    Top = 10
    Width = 19
    Height = 13
    Caption = 'Old:'
  end
  object LabelNew: TLabel
    Left = 320
    Top = 10
    Width = 25
    Height = 13
    Caption = 'New:'
  end
  object GroupBoxSettings: TGroupBox
    Left = 8
    Top = 80
    Width = 393
    Height = 249
    Caption = ' Settings ... '
    TabOrder = 1
    object LabelAxis0: TLabel
      Left = 8
      Top = 24
      Width = 51
      Height = 13
      Alignment = taRightJustify
      Caption = 'LabelAxis0'
    end
    object LabelAxis1: TLabel
      Left = 8
      Top = 80
      Width = 51
      Height = 13
      Alignment = taRightJustify
      Caption = 'LabelAxis1'
    end
    object LabelAxis2: TLabel
      Left = 8
      Top = 136
      Width = 51
      Height = 13
      Alignment = taRightJustify
      Caption = 'LabelAxis2'
    end
    object LabelPredefined: TLabel
      Left = 88
      Top = 184
      Width = 85
      Height = 13
      Alignment = taRightJustify
      Caption = '&Predefined colors:'
      FocusControl = ColorBox
    end
    object ScrollBarAxis0: TScrollBar
      Left = 8
      Top = 41
      Width = 281
      Height = 20
      LargeChange = 16
      Max = 255
      PageSize = 0
      TabOrder = 0
      OnChange = ScrollBarChange
    end
    object ScrollBarAxis1: TScrollBar
      Tag = 1
      Left = 8
      Top = 98
      Width = 281
      Height = 20
      LargeChange = 16
      Max = 255
      PageSize = 0
      TabOrder = 2
      OnChange = ScrollBarChange
    end
    object ScrollBarAxis2: TScrollBar
      Tag = 2
      Left = 8
      Top = 154
      Width = 281
      Height = 20
      LargeChange = 16
      Max = 255
      PageSize = 0
      TabOrder = 4
      OnChange = ScrollBarChange
    end
    object SpinEditAxis0: TSpinEdit
      Left = 312
      Top = 40
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = SpinEditChange
    end
    object SpinEditAxis1: TSpinEdit
      Tag = 1
      Left = 312
      Top = 96
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = SpinEditChange
    end
    object SpinEditAxis2: TSpinEdit
      Tag = 2
      Left = 312
      Top = 152
      Width = 65
      Height = 22
      MaxValue = 255
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = SpinEditChange
    end
    object ColorBox: TColorBox
      Left = 88
      Top = 200
      Width = 129
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 6
      OnSelect = ComboBoxPredefinedSelect
    end
  end
  object PanelGraphic: TPanel
    Left = 416
    Top = 8
    Width = 289
    Height = 361
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 2
    Visible = False
    object LabelAxis: TLabel
      Left = 20
      Top = 20
      Width = 22
      Height = 13
      Caption = '&Axis:'
    end
    object JvColorPanel: TJvFullColorPanel
      Left = 8
      Top = 40
      Width = 276
      Height = 276
      FullColor = 83886079
      TabOrder = 1
      OnColorChange = JvColorPanelColorChange
      CrossSize = 10
      CrossCenter = 3
      CrossStyle.Width = 3
      ColorTrackBar = JvFullColorTrackBar
    end
    object JvFullColorTrackBar: TJvFullColorTrackBar
      Left = 8
      Top = 320
      Width = 276
      Height = 21
      FullColor = 83886079
      TabOrder = 2
      ArrowWidth = 10
    end
    object JvColorAxisConfigCombo: TJvFullColorAxisCombo
      Left = 48
      Top = 16
      Width = 225
      Height = 21
      Selected = acXYZ
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBoxAxisChange
    end
  end
  object ButtonGraphics: TButton
    Left = 326
    Top = 344
    Width = 75
    Height = 25
    TabOrder = 6
    OnClick = ButtonGraphicsClick
  end
  object ButtonOK: TButton
    Left = 8
    Top = 344
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object ButtonCancel: TButton
    Left = 114
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Ca&ncel'
    ModalResult = 2
    TabOrder = 4
  end
  object ButtonApply: TButton
    Left = 220
    Top = 344
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 5
    OnClick = ButtonApplyClick
  end
  object JvColorSpaceCombo: TJvFullColorSpaceCombo
    Left = 8
    Top = 32
    Width = 209
    Height = 21
    ColorSpaceID = 28
    ItemHeight = 13
    TabOrder = 0
    OnSelect = JvComboBoxColorSpaceSelect
  end
end
