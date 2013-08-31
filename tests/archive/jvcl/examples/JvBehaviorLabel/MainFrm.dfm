object Form1: TForm1
  Left = 197
  Top = 107
  BorderStyle = bsDialog
  Caption = 'JvBehaviorLabel Demo'
  ClientHeight = 303
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblCodeBreaker: TJvBehaviorLabel
    Left = 24
    Top = 18
    Width = 259
    Height = 22
    Behavior = 'CodeBreaker'
    BehaviorOptions.DecodedText = 'ENTER THE MATRIX!'
    BehaviorOptions.Interval = 5
    Alignment = taCenter
    AutoSize = False
    Caption = 'BREAK THE CODE'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblAppearing: TJvBehaviorLabel
    Left = 300
    Top = 18
    Width = 259
    Height = 22
    Behavior = 'Appearing'
    BehaviorOptions.Delay = 10
    BehaviorOptions.Pixels = 2
    Alignment = taCenter
    AutoSize = False
    Caption = 'MAKE ME APPEAR'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblBlinking: TJvBehaviorLabel
    Left = 24
    Top = 91
    Width = 259
    Height = 22
    Behavior = 'Blinking'
    BehaviorOptions.Interval = 220
    Alignment = taCenter
    AutoSize = False
    Caption = 'BLINK ME'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblBouncing: TJvBehaviorLabel
    Left = 300
    Top = 91
    Width = 259
    Height = 22
    Behavior = 'Bouncing'
    BehaviorOptions.Interval = 12
    BehaviorOptions.Pixels = 4
    Alignment = taCenter
    AutoSize = False
    Caption = 'BOUNCE ME'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblScrolling: TJvBehaviorLabel
    Left = 24
    Top = 163
    Width = 259
    Height = 22
    Behavior = 'Scrolling'
    BehaviorOptions.Interval = 70
    Alignment = taCenter
    AutoSize = False
    Caption = 'SCROLL ME'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblSpecial: TJvBehaviorLabel
    Left = 300
    Top = 163
    Width = 259
    Height = 22
    Behavior = 'Special'
    BehaviorOptions.Interval = 35
    Alignment = taCenter
    AutoSize = False
    Caption = 'I ACT IN A SPECIAL WAY'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object lblTyping: TJvBehaviorLabel
    Left = 24
    Top = 235
    Width = 259
    Height = 22
    Behavior = 'Typing'
    BehaviorOptions.Interval = 120
    Alignment = taCenter
    AutoSize = False
    Caption = 'TYPE THE TEXT'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clLime
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Button1: TButton
    Left = 38
    Top = 50
    Width = 80
    Height = 25
    Caption = '&CodeBreaker'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 312
    Top = 48
    Width = 80
    Height = 25
    Caption = '&Appearing'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 38
    Top = 121
    Width = 80
    Height = 25
    Caption = '&Blinking'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 312
    Top = 121
    Width = 80
    Height = 25
    Caption = 'B&ouncing'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 38
    Top = 193
    Width = 80
    Height = 25
    Caption = '&Scrolling'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 312
    Top = 193
    Width = 80
    Height = 25
    Caption = 'S&pecial'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 38
    Top = 265
    Width = 80
    Height = 25
    Caption = '&Typing'
    TabOrder = 6
    OnClick = Button7Click
  end
end
