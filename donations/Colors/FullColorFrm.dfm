object FullColorForm: TFullColorForm
  Left = 255
  Top = 232
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
    FocusControl = ColorSpaceCombo
  end
  object LabelDrawOld: TLabel
    Left = 288
    Top = 32
    Width = 57
    Height = 33
    AutoSize = False
    Color = clBlack
    ParentColor = False
    OnClick = LabelDrawOldClick
  end
  object LabelDrawNew: TLabel
    Left = 344
    Top = 32
    Width = 57
    Height = 33
    AutoSize = False
    Color = clWhite
    ParentColor = False
  end
  object LabelOld: TLabel
    Left = 304
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Old:'
  end
  object LabelNew: TLabel
    Left = 360
    Top = 8
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
      OnChange = ComboBoxPredefinedChange
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
    TabOrder = 3
    Visible = False
    object LabelAxis: TLabel
      Left = 20
      Top = 20
      Width = 22
      Height = 13
      Caption = '&Axis:'
    end
    object ColorPanel: TColorPanel
      Left = 8
      Top = 40
      Width = 261
      Height = 261
      FullColor = 16777376
      AxisConfig = acXZY
      TabOrder = 1
      OnColorChange = ColorPanelColorChange
      CrossSize = 10
      CrossCenter = 3
      CrossStyle.Width = 3
      ColorTrackBar = ColorTrackBar
    end
    object ColorTrackBar: TColorTrackBar
      Left = 8
      Top = 320
      Width = 261
      Height = 20
      FullColor = 16777376
      AxisConfig = acXZY
      TabOrder = 2
      ArrowColor = clBlack
      ArrowWidth = 10
      ArrowPosition = 0
      ColorOrientation = 0
      BarOrientation = boHorizontal
      BarWidth = 10
      FullColorDrawing = True
    end
    object AxisConfigCombo: TAxisConfigCombo
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
    TabOrder = 2
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
    TabOrder = 4
  end
  object ButtonCancel: TButton
    Left = 114
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Ca&ncel'
    ModalResult = 2
    TabOrder = 5
  end
  object ButtonApply: TButton
    Left = 220
    Top = 344
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 6
    OnClick = ButtonApplyClick
  end
  object ColorSpaceCombo: TColorSpaceCombo
    Left = 8
    Top = 32
    Width = 153
    Height = 21
    ColorSpaceID = 0
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBoxColorSpaceChange
  end
end
