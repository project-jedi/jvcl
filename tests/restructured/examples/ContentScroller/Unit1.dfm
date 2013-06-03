object Form1: TForm1
  Left = 210
  Top = 157
  BorderStyle = bsDialog
  Caption = 'TContentScroller demo'
  ClientHeight = 389
  ClientWidth = 262
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 128
    Top = 344
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ContentScroller1: TJvContentScroller
    Left = 8
    Top = 0
    Width = 225
    Height = 326
    Anchors = [akLeft, akTop, akBottom]
    UseDockManager = True
    BorderStyle = bsSingle
    BorderWidth = 9
    ParentColor = True
    TabOrder = 1
    Active = False
    ScrollAmount = 2
    ScrollIntervall = 10
    ScrollLength = 350
    MediaFile = 'C:\WIN98\MEDIA\The Microsoft Sound.wav'
    LoopMedia = False
    LoopCount = 2
    object Image11: TImage
      Left = 9
      Top = 10
      Width = 208
      Height = 95
      AutoSize = True
    end
    object Label1: TLabel
      Left = 4
      Top = 120
      Width = 213
      Height = 161
      AutoSize = False
      Caption = 
        'This is a demo of the TJvContentScroller component. Load the Tes' +
        't.bmp image into the Image above and just hit the Run button bel' +
        'ow to see it in action! '#13#10#13#10'You can also set the direction of th' +
        'e scroll by checking / unchecking '#13#10'the box marked "Go down"'
      WordWrap = True
    end
  end
  object chkGoDown: TCheckBox
    Left = 16
    Top = 352
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Go down'
    TabOrder = 2
  end
end
