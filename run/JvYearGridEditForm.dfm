object YearGridEditForm: TYearGridEditForm
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
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
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
      Left = 160
      Top = 7
      Width = 65
      Height = 22
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 233
      Top = 7
      Width = 77
      Height = 22
      TabOrder = 1
      Kind = bkCancel
    end
    object BtnLoad: TButton
      Left = 5
      Top = 7
      Width = 60
      Height = 22
      Caption = '&Load...'
      TabOrder = 2
      OnClick = BtnLoadClick
    end
    object BtnSave: TButton
      Left = 70
      Top = 7
      Width = 59
      Height = 22
      Caption = '&Save...'
      TabOrder = 3
      OnClick = BtnSaveClick
    end
  end
  object MemoText: TMemo
    Left = 0
    Top = 0
    Width = 313
    Height = 331
    Align = alClient
    TabOrder = 1
  end
  object OpenDialog: TOpenDialog
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 88
    Top = 104
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Left = 120
    Top = 104
  end
end
