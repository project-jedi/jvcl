object Form1: TForm1
  Left = 268
  Top = 150
  Width = 712
  Height = 426
  Caption = 'JvMarkupLabel Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvMarkupLabel1: TJvMarkupLabel
    Left = 10
    Top = 234
    Width = 300
    Height = 150
    BackColor = clWhite
    MarginLeft = 0
    MarginRight = 5
    MarginTop = 5
    Anchors = [akLeft, akBottom]
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 55
    Height = 13
    Caption = 'HTML Text:'
  end
  object Label2: TLabel
    Left = 500
    Top = 238
    Width = 73
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Change width :'
  end
  object Label3: TLabel
    Left = 500
    Top = 302
    Width = 77
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Change height :'
  end
  object Label4: TLabel
    Left = 12
    Top = 210
    Width = 34
    Height = 13
    Caption = 'Result:'
  end
  object JvTrackBar1: TJvTrackBar
    Left = 496
    Top = 258
    Width = 195
    Height = 45
    Anchors = [akRight, akBottom]
    Max = 300
    Orientation = trHorizontal
    Frequency = 5
    Position = 300
    SelEnd = 0
    SelStart = 0
    TabOrder = 0
    ThumbLength = 14
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = JvTrackBarChange
    ToolTips = True
  end
  object Edit1: TMemo
    Left = 12
    Top = 26
    Width = 589
    Height = 167
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    TabOrder = 1
  end
  object Button1: TButton
    Left = 614
    Top = 26
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Test'
    TabOrder = 2
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 500
    Top = 374
    Width = 97
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'AutoSize'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object JvTrackBar2: TJvTrackBar
    Tag = 1
    Left = 496
    Top = 322
    Width = 195
    Height = 45
    Anchors = [akRight, akBottom]
    Max = 150
    Orientation = trHorizontal
    Frequency = 5
    Position = 150
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    ThumbLength = 14
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = JvTrackBarChange
    ToolTips = True
  end
end
