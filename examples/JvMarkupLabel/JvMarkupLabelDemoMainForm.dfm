object JvMarkupLabelDemoMainFrm: TJvMarkupLabelDemoMainFrm
  Left = 268
  Top = 150
  Width = 570
  Height = 400
  Caption = 'JvMarkupLabel Demo'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 570
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object JvMarkupLabel1: TJvMarkupLabel
    Left = 10
    Top = 270
    Width = 543
    Height = 86
    MarginLeft = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    ParentColor = False
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 55
    Height = 13
    Caption = 'HTML Text:'
  end
  object Label4: TLabel
    Left = 12
    Top = 250
    Width = 34
    Height = 13
    Caption = 'Result:'
  end
  object Edit1: TMemo
    Left = 8
    Top = 26
    Width = 462
    Height = 169
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      '<font face="arial" color="red">'
      '<b>This text is red, bold and with default size</b>'
      '</font>'
      '<br>'
      '<font face="courier" color="background" size="14">'
      '<i>This line is monospaced, italic</i>'
      '</font>'
      '<br>'
      '<font face="Tahoma" color="$CADEF0" size="9">'
      
        'Unlike HTML, all attributes use Windows format, so font sizes is' +
        ' given in '
      
        'points and colors are either specified in hex or with Delphi col' +
        'or constants'
      'without leading "cl"'
      '</font>')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 480
    Top = 26
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Test'
    TabOrder = 1
    OnClick = Button1Click
  end
  object JvGroupBox1: TJvGroupBox
    Left = 8
    Top = 200
    Width = 545
    Height = 49
    Caption = 'Sizing'
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 17
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object Label3: TLabel
      Left = 240
      Top = 17
      Width = 35
      Height = 13
      Caption = 'Height:'
    end
    object JvTrackBar1: TJvTrackBar
      Left = 44
      Top = 17
      Width = 195
      Height = 28
      Max = 300
      Frequency = 5
      Position = 300
      TabOrder = 0
      ThumbLength = 14
      OnChange = JvTrackBarChange
      ToolTips = True
    end
    object CheckBox1: TCheckBox
      Left = 472
      Top = 17
      Width = 65
      Height = 17
      Caption = 'AutoSize'
      TabOrder = 1
      OnClick = CheckBox1Click
    end
    object JvTrackBar2: TJvTrackBar
      Tag = 1
      Left = 276
      Top = 17
      Width = 195
      Height = 28
      Max = 150
      Frequency = 5
      Position = 150
      TabOrder = 2
      ThumbLength = 14
      OnChange = JvTrackBarChange
      ToolTips = True
    end
  end
end
