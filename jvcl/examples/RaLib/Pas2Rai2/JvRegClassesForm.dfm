object JvRegClasses: TJvRegClasses
  Left = 200
  Top = 104
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Classes list'
  ClientHeight = 344
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 224
    Top = 16
    Width = 105
    Height = 49
    AutoSize = False
    Caption = 'This list used to automatically convert variants to objects'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 224
    Top = 80
    Width = 113
    Height = 25
    AutoSize = False
    Caption = 'Feel free to edit this list.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 224
    Top = 112
    Width = 110
    Height = 49
    AutoSize = False
    Caption = 'List stored at file '#39'classes.ini'#39'.'
    WordWrap = True
  end
  object memClasses: TMemo
    Left = 16
    Top = 16
    Width = 193
    Height = 313
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
