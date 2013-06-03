object Form1: TForm1
  Left = 230
  Top = 121
  Width = 770
  Height = 575
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 245
    Align = alClient
    Color = clOlive
    Constraints.MinHeight = 183
    FullRepaint = False
    TabOrder = 0
    object JvSpecialProgress1: TJvSpecialProgress
      Left = 8
      Top = 56
      Width = 744
      Height = 55
      Hint = 'TJvSpecialProgress'
      StartColor = clBlack
      EndColor = clWhite
      Step = 0
      Color = clYellow
      TextFont.Charset = DEFAULT_CHARSET
      TextFont.Color = clWindowText
      TextFont.Height = -27
      TextFont.Name = 'Tahoma'
      TextFont.Style = []
      Anchors = [akLeft, akTop, akRight, akBottom]
      ShowHint = True
      HintColor = clYellow
    end
    object Gauge1: TGauge
      Left = 8
      Top = 8
      Width = 744
      Height = 41
      Hint = 'TGauge'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      Progress = 0
      ShowHint = True
    end
    object TrackBar1: TTrackBar
      Left = 8
      Top = 184
      Width = 744
      Height = 45
      Anchors = [akLeft, akRight, akBottom]
      Max = 100
      Orientation = trHorizontal
      Frequency = 100
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TrackBar1Change
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 126
      Width = 744
      Height = 41
      Hint = 'TProgressBar'
      Anchors = [akLeft, akRight, akBottom]
      Min = 0
      Max = 100
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 245
    Width = 762
    Height = 303
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 1
    object rgrStartColor: TRadioGroup
      Left = 8
      Top = 8
      Width = 137
      Height = 209
      Caption = 'Start Color: '
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'clBlack'
        'clMaroon'
        'clGreen'
        'clOlive'
        'clNavy'
        'clPurple'
        'clTeal'
        'clGray'
        'clSilver'
        'clRed'
        'clLime'
        'clYellow'
        'clBlue'
        'clFuchsia'
        'clAqua'
        'clWhite')
      TabOrder = 0
      OnClick = rgrStartColorClick
    end
    object rgrEndColor: TRadioGroup
      Left = 152
      Top = 8
      Width = 137
      Height = 209
      Caption = ' End Color: '
      Columns = 2
      ItemIndex = 15
      Items.Strings = (
        'clBlack'
        'clMaroon'
        'clGreen'
        'clOlive'
        'clNavy'
        'clPurple'
        'clTeal'
        'clGray'
        'clSilver'
        'clRed'
        'clLime'
        'clYellow'
        'clBlue'
        'clFuchsia'
        'clAqua'
        'clWhite')
      TabOrder = 1
      OnClick = rgrEndColorClick
    end
    object rgrBackground: TRadioGroup
      Left = 296
      Top = 8
      Width = 137
      Height = 209
      Caption = ' Background Color: '
      Columns = 2
      ItemIndex = 11
      Items.Strings = (
        'clBlack'
        'clMaroon'
        'clGreen'
        'clOlive'
        'clNavy'
        'clPurple'
        'clTeal'
        'clGray'
        'clSilver'
        'clRed'
        'clLime'
        'clYellow'
        'clBlue'
        'clFuchsia'
        'clAqua'
        'clWhite')
      TabOrder = 2
      OnClick = rgrBackgroundClick
    end
    object rgrPanel: TRadioGroup
      Left = 440
      Top = 8
      Width = 137
      Height = 209
      Caption = ' Panel Color: '
      Color = clBtnFace
      Columns = 2
      ItemIndex = 3
      Items.Strings = (
        'clBlack'
        'clMaroon'
        'clGreen'
        'clOlive'
        'clNavy'
        'clPurple'
        'clTeal'
        'clGray'
        'clSilver'
        'clRed'
        'clLime'
        'clYellow'
        'clBlue'
        'clFuchsia'
        'clAqua'
        'clWhite')
      ParentColor = False
      TabOrder = 3
      OnClick = rgrPanelClick
    end
    object chbSolid: TCheckBox
      Left = 584
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Solid'
      TabOrder = 4
      OnClick = chbSolidClick
    end
    object chbTextVisible: TCheckBox
      Left = 584
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Text Visible'
      TabOrder = 5
      OnClick = chbTextVisibleClick
    end
    object chbTextCentered: TCheckBox
      Left = 584
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Text Centered'
      TabOrder = 6
      OnClick = chbTextCenteredClick
    end
    object chbTransparent: TCheckBox
      Left = 584
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Transparent'
      TabOrder = 7
      OnClick = chbTransparentClick
    end
    object chbGradientBlocks: TCheckBox
      Left = 584
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Gradient Blocks'
      TabOrder = 8
      OnClick = chbGradientBlocksClick
    end
    object rgrFont: TRadioGroup
      Left = 8
      Top = 224
      Width = 425
      Height = 73
      Caption = ' Font: '
      Columns = 4
      Items.Strings = (
        'Font 1'
        'Font 2'
        'Font 3'
        'Font 4'
        'Font 5'
        'Font 6'
        'Font 7'
        'Font 8')
      TabOrder = 9
      OnClick = rgrFontClick
    end
    object Panel3: TPanel
      Left = 584
      Top = 136
      Width = 169
      Height = 161
      TabOrder = 10
      object lblMinimum: TLabel
        Left = 8
        Top = 8
        Width = 44
        Height = 13
        Caption = 'Minimum:'
      end
      object lblMaximum: TLabel
        Left = 8
        Top = 60
        Width = 47
        Height = 13
        Caption = 'Maximum:'
      end
      object lblStep: TLabel
        Left = 8
        Top = 108
        Width = 25
        Height = 13
        Caption = 'Step:'
      end
      object edtMinimum: TEdit
        Left = 8
        Top = 28
        Width = 65
        Height = 21
        TabOrder = 0
        Text = '0'
      end
      object edtMaximum: TEdit
        Left = 8
        Top = 76
        Width = 65
        Height = 21
        TabOrder = 1
        Text = '100'
      end
      object edtStep: TEdit
        Left = 8
        Top = 124
        Width = 65
        Height = 21
        TabOrder = 2
        Text = '10'
      end
      object btnApply: TButton
        Left = 86
        Top = 76
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 3
        OnClick = btnApplyClick
      end
      object btnStepIt: TButton
        Left = 86
        Top = 124
        Width = 75
        Height = 25
        Caption = 'StepIt'
        TabOrder = 4
        OnClick = btnStepItClick
      end
    end
    object chbPanelDoubleBuffered: TCheckBox
      Left = 584
      Top = 104
      Width = 153
      Height = 17
      Caption = 'Panel Double Buffered'
      TabOrder = 11
      OnClick = chbPanelDoubleBufferedClick
    end
  end
end
