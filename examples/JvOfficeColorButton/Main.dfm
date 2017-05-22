object Form1: TForm1
  Left = 206
  Top = 129
  Width = 764
  Height = 569
  Caption = 'New JvOffice pack demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 269
    Top = 15
    Width = 301
    Height = 390
    Anchors = [akRight, akBottom]
    Caption = 'Demo style'
    TabOrder = 0
    object lbl1: TLabel
      Left = 177
      Top = 239
      Width = 63
      Height = 13
      Caption = 'Border Width'
    end
    object chkShowSystemColors: TCheckBox
      Left = 163
      Top = 57
      Width = 130
      Height = 17
      Caption = 'Show System Colors'
      TabOrder = 0
      OnClick = chkShowNoneClick
    end
    object chkShowDefault: TCheckBox
      Left = 163
      Top = 36
      Width = 130
      Height = 17
      Caption = 'Show Default Color'
      TabOrder = 1
      OnClick = chkShowNoneClick
    end
    object chkShowUserColors: TCheckBox
      Left = 163
      Top = 83
      Width = 130
      Height = 17
      Caption = 'Show user Colors'
      TabOrder = 2
      OnClick = chkShowNoneClick
    end
    object chkShowCustom: TCheckBox
      Left = 163
      Top = 102
      Width = 130
      Height = 17
      Caption = 'Show Custom Button'
      TabOrder = 3
      OnClick = chkShowNoneClick
    end
    object chkHoldCustomColor: TCheckBox
      Left = 163
      Top = 123
      Width = 130
      Height = 17
      Caption = 'Hold Custom Color'
      TabOrder = 4
      OnClick = chkShowNoneClick
    end
    object chkShowNone: TCheckBox
      Left = 163
      Top = 14
      Width = 130
      Height = 17
      Caption = 'Show None Color'
      TabOrder = 5
      OnClick = chkShowNoneClick
    end
    object chkFlatBorder: TCheckBox
      Left = 163
      Top = 223
      Width = 131
      Height = 16
      Caption = 'Flat Border'
      TabOrder = 6
      OnClick = chkShowNoneClick
    end
    object JvSpinEdit1: TJvSpinEdit
      Left = 177
      Top = 257
      Width = 75
      Height = 25
      MaxValue = 4.000000000000000000
      TabOrder = 7
      OnChange = chkShowNoneClick
    end
    object grp2: TGroupBox
      Left = 6
      Top = 110
      Width = 142
      Height = 74
      Caption = 'Style'
      TabOrder = 8
      object rbStandard: TRadioButton
        Left = 12
        Top = 18
        Width = 87
        Height = 13
        Caption = 'Standard'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbStandardClick
      end
      object rbOfficeXP: TRadioButton
        Left = 12
        Top = 43
        Width = 87
        Height = 13
        Caption = 'Office XP'
        TabOrder = 1
        OnClick = rbOfficeXPClick
      end
    end
    object grp3: TGroupBox
      Left = 6
      Top = 18
      Width = 142
      Height = 81
      Caption = 'Apply on'
      TabOrder = 9
      object rbColorPanel: TRadioButton
        Left = 12
        Top = 24
        Width = 123
        Height = 13
        Caption = 'JvOfficeColorPanel'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbColorPanelClick
      end
      object rbColorButton: TRadioButton
        Left = 12
        Top = 49
        Width = 123
        Height = 13
        Caption = 'JvOfficeColorButton'
        TabOrder = 1
        OnClick = rbColorPanelClick
      end
    end
    object chkEnableGlyph: TCheckBox
      Left = 163
      Top = 149
      Width = 113
      Height = 17
      Caption = 'Enable glyph'
      TabOrder = 10
      OnClick = chkShowNoneClick
    end
    object chkEnable: TCheckBox
      Left = 163
      Top = 198
      Width = 113
      Height = 17
      Caption = 'Enable'
      TabOrder = 11
      OnClick = chkShowNoneClick
    end
    object chkShowDragBar: TCheckBox
      Left = 163
      Top = 174
      Width = 130
      Height = 16
      Caption = 'Show DragBar'
      TabOrder = 12
      OnClick = chkShowNoneClick
    end
  end
  object jvofcb1: TJvOfficeColorButton
    Left = 6
    Top = 12
    Width = 35
    Height = 22
    TabOrder = 1
    SelectedColor = clDefault
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    HotTrackFontOptions = []
    Properties.NoneColorCaption = 'No Color'
    Properties.DefaultColorCaption = 'Automatic'
    Properties.CustomColorCaption = 'Other Colors...'
    Properties.NoneColorHint = 'No Color'
    Properties.DefaultColorHint = 'Automatic'
    Properties.CustomColorHint = 'Other Colors...'
    Properties.NoneColorFont.Charset = DEFAULT_CHARSET
    Properties.NoneColorFont.Color = clWindowText
    Properties.NoneColorFont.Height = -13
    Properties.NoneColorFont.Name = 'MS Sans Serif'
    Properties.NoneColorFont.Style = []
    Properties.DefaultColorFont.Charset = DEFAULT_CHARSET
    Properties.DefaultColorFont.Color = clWindowText
    Properties.DefaultColorFont.Height = -13
    Properties.DefaultColorFont.Name = 'MS Sans Serif'
    Properties.DefaultColorFont.Style = []
    Properties.CustomColorFont.Charset = DEFAULT_CHARSET
    Properties.CustomColorFont.Color = clWindowText
    Properties.CustomColorFont.Height = -13
    Properties.CustomColorFont.Name = 'MS Sans Serif'
    Properties.CustomColorFont.Style = []
    Properties.FloatWindowCaption = 'Color Window'
    Properties.DragBarHint = 'Drag to float'
    OnColorChange = jvocp1ColorChange
  end
  object jvocp1: TJvOfficeColorPanel
    Left = 111
    Top = 11
    Width = 116
    Height = 124
    SelectedColor = clDefault
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -13
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    HotTrackFontOptions = []
    TabOrder = 2
    Properties.NoneColorCaption = 'No Color'
    Properties.DefaultColorCaption = 'Automatic'
    Properties.CustomColorCaption = 'Other Colors...'
    Properties.NoneColorHint = 'No Color'
    Properties.DefaultColorHint = 'Automatic'
    Properties.CustomColorHint = 'Other Colors...'
    Properties.NoneColorFont.Charset = DEFAULT_CHARSET
    Properties.NoneColorFont.Color = clWindowText
    Properties.NoneColorFont.Height = -13
    Properties.NoneColorFont.Name = 'MS Sans Serif'
    Properties.NoneColorFont.Style = []
    Properties.DefaultColorFont.Charset = DEFAULT_CHARSET
    Properties.DefaultColorFont.Color = clWindowText
    Properties.DefaultColorFont.Height = -13
    Properties.DefaultColorFont.Name = 'MS Sans Serif'
    Properties.DefaultColorFont.Style = []
    Properties.CustomColorFont.Charset = DEFAULT_CHARSET
    Properties.CustomColorFont.Color = clWindowText
    Properties.CustomColorFont.Height = -13
    Properties.CustomColorFont.Name = 'MS Sans Serif'
    Properties.CustomColorFont.Style = []
    OnColorChange = jvocp1ColorChange
  end
  object grp4: TGroupBox
    Left = 6
    Top = 324
    Width = 258
    Height = 81
    Anchors = [akLeft, akBottom]
    Caption = 'Seleted Color'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    object pnlDemoColor: TPanel
      Left = 2
      Top = 15
      Width = 254
      Height = 64
      Align = alClient
      Anchors = [akLeft, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
end
