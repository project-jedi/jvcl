object YearGridEditF: TYearGridEditF
  Left = 303
  Top = 154
  BorderStyle = bsDialog
  Caption = 'YearGrid Edit'
  ClientHeight = 364
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 331
    Width = 313
    Height = 33
    Align = alBottom
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 176
      Top = 7
      Width = 60
      Height = 20
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 241
      Top = 7
      Width = 60
      Height = 20
      TabOrder = 1
      Kind = bkCancel
    end
    object btnload: TButton
      Left = 13
      Top = 7
      Width = 61
      Height = 20
      Caption = '&Load...'
      TabOrder = 2
      OnClick = btnloadClick
    end
    object btnsave: TButton
      Left = 78
      Top = 7
      Width = 61
      Height = 20
      Caption = '&Save...'
      TabOrder = 3
      OnClick = btnsaveClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 313
    Height = 331
    Align = alClient
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 88
    Top = 104
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 120
    Top = 104
  end
end
