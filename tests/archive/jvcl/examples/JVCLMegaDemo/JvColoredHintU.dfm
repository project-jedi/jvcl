object JvColoredHint: TJvColoredHint
  Left = 348
  Top = 144
  Width = 509
  Height = 466
  Caption = 'HtHints test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 18
    Top = 21
    Width = 447
    Height = 380
    Buttons = [capClose, capHelp]
    Caption = 'Example how to show colored hints (no component)'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object Label1: TLabel
      Left = 48
      Top = 48
      Width = 71
      Height = 13
      Caption = 'Hint for Button:'
    end
    object Button1: TButton
      Left = 48
      Top = 192
      Width = 169
      Height = 25
      Hint = 'Button1 <b>Bold Hint</b> not bold'
      Caption = 'Set colored hint for this button'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 48
      Top = 72
      Width = 329
      Height = 105
      Lines.Strings = (
        'Button1 <b>Bold Hint</b> not bold'
        'Color red = <b><c:Red>Red')
      TabOrder = 1
    end
  end
  object RegAuto1: TJvRegAuto
    RegPath = 'Software\JVCL\ColorHintsTest'
    IniFile = '$HOME/.ColorHintsTest'
    Props.Strings = (
      'Button1.Hint')
    Left = 136
    Top = 144
  end
end
