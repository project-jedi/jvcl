object AboutBox: TAboutBox
  Left = 445
  Top = 127
  ActiveControl = OKButton
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 218
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    BevelOuter = bvLowered
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 8
      Top = 8
      Width = 65
      Height = 57
      IsControl = True
    end
    object ProductName: TLabel
      Left = 88
      Top = 16
      Width = 183
      Height = 13
      Caption = 'This form is loaded from about.pas/dfm'
      IsControl = True
    end
    object Version: TLabel
      Left = 88
      Top = 40
      Width = 35
      Height = 13
      Caption = 'Version'
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 80
      Width = 44
      Height = 13
      Caption = 'Copyright'
      IsControl = True
    end
    object Comments: TLabel
      Left = 8
      Top = 104
      Width = 49
      Height = 13
      Caption = 'Comments'
      WordWrap = True
      IsControl = True
    end
  end
  object OKButton: TButton
    Left = 120
    Top = 178
    Width = 65
    Height = 33
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    IsControl = True
  end
end
